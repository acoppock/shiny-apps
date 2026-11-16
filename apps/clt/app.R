library(shiny)
library(bslib)
library(ggplot2)

# Distributions ----
# Each entry follows the 2016 app: how to draw, the population density, the
# expected value and variance, and the plotted x range.
sims <- 1000

population <- function(dist, input) {
  switch(dist,
    beta = {
      a <- input$beta_a
      b <- input$beta_b
      validate(need(isTruthy(a) && isTruthy(b) && a > 0 && b > 0, "Both shape parameters must be positive."))
      list(
        draw = \(n) rbeta(n, shape1 = a, shape2 = b),
        density = \(x) dbeta(x, shape1 = a, shape2 = b),
        mean = a / (a + b),
        var = (a * b) / ((a + b)^2 * (a + b + 1)),
        xlim = c(0, 1),
        name = paste0("Beta(", a, ", ", b, ")")
      )
    },
    chisq = {
      dof <- input$chisq_df
      validate(need(isTruthy(dof) && dof > 0, "Degrees of freedom must be positive."))
      list(
        draw = \(n) rchisq(n, df = dof),
        density = \(x) dchisq(x, df = dof),
        mean = dof,
        var = 2 * dof,
        xlim = c(0, qchisq(0.99, df = dof)),
        name = paste0("Chi-square with ", signif(dof, 3), " degrees of freedom")
      )
    },
    gamma = {
      shape <- input$gamma_shape
      scale <- input$gamma_scale
      validate(need(isTruthy(shape) && isTruthy(scale) && shape > 0 && scale > 0, "Shape and scale must be positive."))
      list(
        draw = \(n) rgamma(n, shape = shape, scale = scale),
        density = \(x) dgamma(x, shape = shape, scale = scale),
        mean = shape * scale,
        var = shape * scale^2,
        xlim = c(0, qgamma(0.99, shape = shape, scale = scale)),
        name = paste0("Gamma(shape = ", shape, ", scale = ", scale, ")")
      )
    },
    t = {
      dof <- input$t_df
      validate(need(isTruthy(dof) && dof > 0, "Degrees of freedom must be positive."))
      list(
        draw = \(n) rt(n, df = dof),
        density = \(x) dt(x, df = dof),
        mean = if (dof > 1) 0 else NA_real_,
        var = if (dof > 2) dof / (dof - 2) else if (dof > 1) Inf else NA_real_,
        xlim = qt(c(0.005, 0.995), df = dof),
        name = paste0("t with ", dof, if (dof == 1) " degree" else " degrees", " of freedom")
      )
    },
    lognormal = {
      meanlog <- input$meanlog
      sdlog <- input$sdlog
      validate(
        need(is.numeric(meanlog) && !is.na(meanlog), "Enter the mean of the log."),
        need(isTruthy(sdlog) && sdlog > 0, "The SD of the log must be positive.")
      )
      list(
        draw = \(n) rlnorm(n, meanlog = meanlog, sdlog = sdlog),
        density = \(x) dlnorm(x, meanlog = meanlog, sdlog = sdlog),
        mean = exp(meanlog + sdlog^2 / 2),
        var = (exp(sdlog^2) - 1) * exp(2 * meanlog + sdlog^2),
        xlim = c(0, qlnorm(0.99, meanlog = meanlog, sdlog = sdlog)),
        name = paste0("Lognormal(meanlog = ", meanlog, ", sdlog = ", sdlog, ")")
      )
    }
  )
}

fmt <- function(x) {
  if (is.na(x)) return("undefined")
  if (is.infinite(x)) return("infinite")
  format(signif(x, 3), big.mark = ",", scientific = FALSE)
}

# Theme ----
app_theme <- bs_theme(version = 5, preset = "cosmo") |>
  bs_bundle(sass::sass_layer_file("brand.scss"), sass::sass_layer_file("apps.scss"))

crimson <- "#a72f5b"
teal <- "#004c6d"
graphite <- "#3C444C"
parchment <- "#FBFAF7"

# UI ----
ui <- page_sidebar(
  title = "Central Limit Theorem in Action",
  window_title = "Central Limit Theorem",
  theme = app_theme,
  tags$head(tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Source+Sans+3:ital,wght@0,400;0,600;1,400&family=Source+Serif+4:opsz,wght@8..60,400;8..60,600&display=swap"
  )),
  sidebar = sidebar(
    width = 330,
    sliderInput("N", "Sample size", value = 1, min = 1, max = 50, step = 1, ticks = FALSE),
    selectInput("distribution", "Distribution", c(
      "Beta" = "beta",
      "Chi-square" = "chisq",
      "Gamma" = "gamma",
      "t" = "t",
      "Lognormal" = "lognormal"
    )),
    conditionalPanel(
      "input.distribution == 'beta'",
      numericInput("beta_a", "Alpha (first shape)", value = 10, min = 0, max = 100),
      numericInput("beta_b", "Beta (second shape)", value = 0.5, min = 0, max = 100)
    ),
    conditionalPanel(
      "input.distribution == 'chisq'",
      numericInput("chisq_df", "Degrees of freedom", value = 2 / 3, min = 0, max = 100)
    ),
    conditionalPanel(
      "input.distribution == 'gamma'",
      numericInput("gamma_shape", "Shape", value = 2, min = 0, max = 100),
      numericInput("gamma_scale", "Scale", value = 3, min = 0, max = 100)
    ),
    conditionalPanel(
      "input.distribution == 't'",
      numericInput("t_df", "Degrees of freedom", value = 4, min = 0, max = 100)
    ),
    conditionalPanel(
      "input.distribution == 'lognormal'",
      numericInput("meanlog", "Mean of the log", value = 4, min = -100, max = 100),
      numericInput("sdlog", "SD of the log", value = 2, min = 0, max = 100)
    ),
    actionButton("redraw", "Draw new samples", class = "btn-outline-primary")
  ),
  card(
    full_screen = TRUE,
    card_header("Distribution of the sample mean"),
    plotOutput("main_plot", height = "440px"),
    card_footer(textOutput("summary"))
  ),
  accordion(
    open = FALSE,
    accordion_panel(
      "About this app",
      p("The plot shows the sampling distribution of the sample mean in graphite, generated from 1,000 sample means, each the average of a sample of the chosen size. The density of the random variable from which the samples are drawn is shown in teal, and its expected value in crimson."),
      p("With a sample size of 1, the two curves describe the same thing. As the sample size grows, the sample mean converges to the expected value (the weak law of large numbers), and the sampling distribution of the estimator becomes approximately normal (the central limit theorem), however skewed the population distribution."),
      p("The t distribution with 2 or fewer degrees of freedom has infinite variance, and with 1 or fewer it has no mean at all, so neither theorem applies: try 1 degree of freedom and watch the sample mean refuse to settle down.")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  sim <- reactive({
    N <- req(input$N)
    pop <- population(input$distribution, input)
    set.seed(343 + input$redraw)
    ests <- replicate(sims, mean(pop$draw(N)))
    c(pop, list(ests = ests, N = N))
  })

  output$main_plot <- renderPlot(bg = "transparent", {
    s <- sim()
    center <- if (is.na(s$mean)) 0 else s$mean

    # geom_density()'s own computation, done here so the peak can be labeled
    est_density <- density(s$ests, from = min(s$ests), to = max(s$ests), n = 512)
    gg_df <- tibble::tibble(x = est_density$x, y = est_density$y)
    pop_df <- tibble::tibble(x = seq(s$xlim[1], s$xlim[2], length.out = 1000), y = s$density(x))
    pop_df <- pop_df[is.finite(pop_df$y), ]

    in_view <- gg_df$x >= s$xlim[1] & gg_df$x <= s$xlim[2]
    y_top <- max(gg_df$y[in_view], pop_df$y, 0)
    x_span <- diff(s$xlim)
    right_room <- (s$xlim[2] - center) > (center - s$xlim[1])
    side_hjust <- if (right_room) 0 else 1
    side_nudge <- if (right_room) 0.02 * x_span else -0.02 * x_span

    # Sample-means label sits just above its peak, extending toward the side
    # with more room; the expected-value label shares that side at the top.
    peak <- gg_df[in_view, ][which.max(gg_df$y[in_view]), ]
    est_label <- tibble::tibble(x = peak$x + side_nudge, y = peak$y, label = "Sample means")

    # Population label sits on its curve, extending downhill so the text
    # clears the line, with room to the panel edge, and as far as possible
    # from the other two labels.
    slope <- c(diff(pop_df$y), 0)
    candidates <- tibble::tibble(x = pop_df$x, y = pop_df$y, hjust = ifelse(slope > 0, 1, 0))
    room <- ifelse(candidates$hjust == 0, s$xlim[2] - candidates$x, candidates$x - s$xlim[1])
    keep <- candidates$y >= 0.08 * y_top & candidates$y <= 0.85 * y_top & room > 0.18 * x_span
    candidates <- if (any(keep)) candidates[keep, ] else candidates[which.max(candidates$y), ]
    distance <- pmin(
      ((candidates$x - est_label$x) / x_span)^2 + ((candidates$y - est_label$y) / y_top)^2,
      ((candidates$x - center) / x_span)^2 + ((candidates$y - 1.2 * y_top) / y_top)^2
    )
    pop_label <- candidates[which.max(distance), ]
    pop_label$x <- pop_label$x + if (pop_label$hjust == 0) 0.01 * x_span else -0.01 * x_span
    pop_label$label <- "Population"

    mean_text <- if (is.na(s$mean)) "Center (no mean exists)" else paste0("Expected value = ", fmt(s$mean))

    ggplot(gg_df, aes(x, y)) +
      geom_vline(xintercept = center, color = crimson, linewidth = 0.9, linetype = if (is.na(s$mean)) "dashed" else "solid") +
      geom_line(data = pop_df, color = teal, linewidth = 1.1) +
      geom_line(color = graphite, linewidth = 1.3) +
      annotate("label", x = center + side_nudge, y = Inf, label = mean_text, hjust = side_hjust, vjust = 1.4,
        color = crimson, size = 4.4, fill = parchment, border.colour = NA, label.padding = unit(0.1, "lines")) +
      geom_label(data = est_label, aes(label = label), hjust = side_hjust, vjust = -0.5,
        color = graphite, size = 4.4, fill = parchment, border.colour = NA, label.padding = unit(0.1, "lines")) +
      geom_label(data = pop_label, aes(label = label, hjust = hjust), vjust = -0.4,
        color = teal, size = 4.4, fill = parchment, border.colour = NA, label.padding = unit(0.1, "lines")) +
      coord_cartesian(xlim = s$xlim) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
      labs(x = NULL, y = "Density", subtitle = paste0(s$name, ", samples of size ", s$N)) +
      theme_minimal(base_size = 15) +
      theme(
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(color = graphite),
        axis.text = element_text(color = graphite)
      )
  })

  output$summary <- renderText({
    s <- sim()
    se <- if (is.finite(s$var)) sqrt(s$var / s$N) else s$var
    paste0(
      "Theory: expected value ", fmt(s$mean), ", standard error of the mean ", fmt(se), ". ",
      "Simulation: average of the 1,000 sample means ", fmt(mean(s$ests)), ", their standard deviation ", fmt(sd(s$ests)), "."
    )
  })
}

shinyApp(ui, server)
