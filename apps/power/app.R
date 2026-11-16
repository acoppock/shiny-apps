library(shiny)
library(bslib)
library(ggplot2)

# Power formulas ----
# Two-sided test of a difference in means with equal arms. A binary outcome is
# the same calculation with sigma set to sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / 2),
# which reproduces the 2014 app's separate binary formulas exactly.
power_two_arm <- function(N, effect, sigma, alpha, icc = NULL, clusters_per_arm = NULL) {
  crit <- qnorm(1 - alpha / 2)
  if (is.null(icc)) {
    z <- abs(effect) * sqrt(N) / (2 * sigma)
  } else {
    n_per_cluster <- N / (2 * clusters_per_arm)
    z <- abs(effect) * sqrt((clusters_per_arm - 1) * n_per_cluster) /
      sqrt(2 * sigma^2 * (1 + (n_per_cluster - 1) * icc))
    z[n_per_cluster < 1] <- NA
  }
  pnorm(z - crit) + pnorm(-z - crit)
}

n_search_grid <- c(1:9999, seq(10000, 1e8, by = 1000))

# Theme ----
app_theme <- bs_theme(version = 5, preset = "cosmo") |>
  bs_bundle(sass::sass_layer_file("brand.scss"), sass::sass_layer_file("apps.scss"))

crimson <- "#a72f5b"
teal <- "#004c6d"
graphite <- "#3C444C"

# UI ----
ui <- page_sidebar(
  title = "Power Calculator",
  window_title = "Power Calculator",
  theme = app_theme,
  tags$head(tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Source+Sans+3:ital,wght@0,400;0,600;1,400&family=Source+Serif+4:opsz,wght@8..60,400;8..60,600&display=swap"
  )),
  sidebar = sidebar(
    width = 330,
    radioButtons("outcome", "Outcome", c("Continuous" = "continuous", "Binary" = "binary"), inline = TRUE),
    radioButtons("design", "Assignment", c("Individual" = "individual", "Clustered" = "clustered"), inline = TRUE),
    conditionalPanel(
      "input.outcome == 'continuous'",
      numericInput("tau", "Treatment effect", value = 5, min = 0),
      numericInput("sigma", "Standard deviation of the outcome", value = 10, min = 0)
    ),
    conditionalPanel(
      "input.outcome == 'binary'",
      numericInput("p0", "Proportion with outcome = 1, control", value = 0.5, min = 0, max = 1, step = 0.01),
      numericInput("p1", "Proportion with outcome = 1, treatment", value = 0.65, min = 0, max = 1, step = 0.01)
    ),
    conditionalPanel(
      "input.design == 'clustered'",
      sliderInput("icc", "Intra-cluster correlation", value = 0.5, min = 0, max = 1, step = 0.01, ticks = FALSE),
      numericInput("clusters_per_arm", "Clusters per arm", value = 40, min = 2)
    ),
    radioButtons("alpha", "Significance level", c("0.01", "0.05", "0.10"), selected = "0.05", inline = TRUE),
    sliderInput("target", "Power target", value = 0.8, min = 0.05, max = 0.99, step = 0.01, ticks = FALSE),
    numericInput("max_n", "Largest sample size to plot", value = 2000, min = 10)
  ),
  value_box(
    title = "Subjects needed",
    value = textOutput("n_required"),
    textOutput("n_detail"),
    theme = "text-primary",
    max_height = "140px"
  ),
  card(
    full_screen = TRUE,
    card_header("Power by number of subjects"),
    plotOutput("power_plot", height = "420px")
  ),
  accordion(
    open = FALSE,
    accordion_panel(
      "About this calculator",
      p("Power is the probability that a two-sided test of the difference in means comes out statistically significant, given a true treatment effect. Subjects are split evenly between treatment and control. In a clustered design, whole clusters are assigned together, and the intra-cluster correlation says how similar outcomes are within a cluster."),
      p("For a binary outcome, the standard deviation is computed from the two proportions. In a clustered design, adding subjects to a fixed number of clusters runs into a ceiling: past some point, only more clusters raise power."),
      p("The calculation uses the normal approximation, so it is optimistic for very small samples. For designs beyond two arms and a difference in means, simulate the design directly, for example with DeclareDesign.")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  design <- reactive({
    alpha <- as.numeric(input$alpha)
    if (input$outcome == "continuous") {
      validate(
        need(isTruthy(input$tau) && input$tau != 0, "Enter a nonzero treatment effect."),
        need(isTruthy(input$sigma) && input$sigma > 0, "Enter a positive standard deviation.")
      )
      effect <- input$tau
      sigma <- input$sigma
      effect_label <- paste0("effect = ", effect, ", SD = ", sigma)
    } else {
      validate(
        need(isTruthy(input$p0) && isTruthy(input$p1), "Enter both proportions."),
        need(input$p0 >= 0 && input$p0 <= 1 && input$p1 >= 0 && input$p1 <= 1, "Proportions must lie between 0 and 1."),
        need(input$p0 != input$p1, "The two proportions must differ.")
      )
      effect <- input$p1 - input$p0
      sigma <- sqrt((input$p1 * (1 - input$p1) + input$p0 * (1 - input$p0)) / 2)
      effect_label <- paste0("effect = ", round(100 * effect, 1), " percentage points")
    }
    clustered <- input$design == "clustered"
    if (clustered) {
      validate(need(isTruthy(input$clusters_per_arm) && input$clusters_per_arm >= 2, "Use at least two clusters per arm."))
      effect_label <- paste0(effect_label, ", ICC = ", input$icc, ", ", input$clusters_per_arm, " clusters per arm")
    }
    list(
      effect = effect,
      sigma = sigma,
      alpha = alpha,
      icc = if (clustered) input$icc,
      clusters_per_arm = if (clustered) input$clusters_per_arm,
      label = effect_label
    )
  })

  n_required <- reactive({
    d <- design()
    power <- power_two_arm(n_search_grid, d$effect, d$sigma, d$alpha, d$icc, d$clusters_per_arm)
    reached <- which(power >= input$target)
    if (length(reached) == 0) NA else n_search_grid[min(reached)]
  })

  output$n_required <- renderText({
    n <- n_required()
    if (!is.na(n)) return(format(n, big.mark = ","))
    if (is.null(design()$icc)) "More than 100 million" else "Not reachable"
  })

  output$n_detail <- renderText({
    n <- n_required()
    d <- design()
    target <- paste0(round(100 * input$target), "% power")
    if (is.na(n) && !is.null(d$icc)) {
      return(paste0("This design cannot reach ", target, " by adding subjects. Add clusters instead."))
    }
    if (is.na(n)) return(paste0("To reach ", target, "."))
    if (is.null(d$icc)) return(paste0("To reach ", target, ", split evenly between treatment and control."))
    per_cluster <- ceiling(n / (2 * d$clusters_per_arm))
    paste0("To reach ", target, ": about ", format(per_cluster, big.mark = ","), " subjects in each of ", 2 * d$clusters_per_arm, " clusters.")
  })

  output$power_plot <- renderPlot(bg = "transparent", {
    d <- design()
    max_n <- req(input$max_n)
    ns <- if (max_n <= 2000) seq_len(max_n) else round(seq(1, max_n, length.out = 2000))
    clustered <- !is.null(d$icc)

    gg_df <- tibble::tibble(
      N = ns,
      power = power_two_arm(ns, d$effect, d$sigma, d$alpha, d$icc, d$clusters_per_arm),
      series = if (clustered) "Clustered design" else "Individual assignment"
    )
    if (clustered) {
      gg_df <- rbind(gg_df, tibble::tibble(
        N = ns,
        power = power_two_arm(ns, d$effect, d$sigma, d$alpha),
        series = "Same N, individual assignment"
      ))
    }
    label_df <- gg_df[gg_df$N == max(ns), ]
    colors <- c("Individual assignment" = crimson, "Clustered design" = crimson, "Same N, individual assignment" = teal)

    ggplot(gg_df, aes(N, power, color = series)) +
      geom_hline(yintercept = input$target, linetype = "dashed", color = graphite, linewidth = 0.4) +
      annotate("text", x = 0, y = input$target, label = paste0("target: ", round(100 * input$target), "%"),
        hjust = 0, vjust = -0.6, color = graphite, size = 4.2) +
      geom_line(linewidth = 1.3, na.rm = TRUE) +
      geom_text(data = label_df, aes(label = series), hjust = 1, vjust = 1.6, size = 4.4) +
      scale_color_manual(values = colors) +
      scale_y_continuous(limits = c(0, 1), labels = \(x) paste0(round(100 * x), "%"), expand = expansion(mult = c(0, 0.02))) +
      scale_x_continuous(labels = \(x) format(x, big.mark = ",", scientific = FALSE), expand = expansion(mult = c(0, 0.02))) +
      labs(x = "Number of subjects", y = "Power", subtitle = d$label) +
      theme_minimal(base_size = 15) +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(color = graphite),
        axis.text = element_text(color = graphite)
      )
  })
}

shinyApp(ui, server)
