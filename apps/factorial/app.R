library(shiny)
library(bslib)
library(ggplot2)

# Power formulas ----
# N subjects split evenly over the four cells of a 2x2 factorial. The
# interaction is (both - A only) - (B only - neither), so its variance is the
# sum of the four cell-mean variances, 4 * sum(v) / N. The main effect of A,
# averaged over B, has a quarter of that variance, so its SE is half as large.
# Continuous outcomes use v = sigma^2 in every cell; binary outcomes use
# p * (1 - p) cell by cell.
power_from_se <- function(effect, se, alpha) {
  crit <- qnorm(1 - alpha / 2)
  z <- abs(effect) / se
  pnorm(z - crit) + pnorm(-z - crit)
}

n_search_grid <- c(seq(4, 9996, by = 4), seq(10000, 1e8, by = 1000))

# Theme ----
app_theme <- bs_theme(version = 5, preset = "cosmo") |>
  bs_bundle(sass::sass_layer_file("brand.scss"), sass::sass_layer_file("apps.scss"))

crimson <- "#a72f5b"
teal <- "#004c6d"
graphite <- "#3C444C"

# UI ----
ui <- page_sidebar(
  title = "Power for an Interaction in a 2x2 Factorial",
  window_title = "Factorial Power",
  theme = app_theme,
  tags$head(tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Source+Sans+3:ital,wght@0,400;0,600;1,400&family=Source+Serif+4:opsz,wght@8..60,400;8..60,600&display=swap"
  )),
  sidebar = sidebar(
    width = 330,
    radioButtons("outcome", "Outcome", c("Continuous" = "continuous", "Binary" = "binary"), inline = TRUE),
    conditionalPanel(
      "input.outcome == 'continuous'",
      numericInput("interaction", "Interaction effect", value = 0.25, step = 0.05),
      numericInput("sigma", "Standard deviation of the outcome", value = 1, min = 0, step = 0.1)
    ),
    conditionalPanel(
      "input.outcome == 'binary'",
      p(class = "small text-muted mb-2", "Proportion with outcome = 1 in each cell"),
      layout_columns(
        col_widths = c(6, 6),
        numericInput("p00", "Neither", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("p10", "A only", value = 0.6, min = 0, max = 1, step = 0.01),
        numericInput("p01", "B only", value = 0.55, min = 0, max = 1, step = 0.01),
        numericInput("p11", "Both A and B", value = 0.75, min = 0, max = 1, step = 0.01)
      ),
      uiOutput("binary_interaction")
    ),
    radioButtons("alpha", "Significance level", c("0.01", "0.05", "0.10"), selected = "0.05", inline = TRUE),
    sliderInput("target", "Power target", value = 0.8, min = 0.05, max = 0.99, step = 0.01, ticks = FALSE),
    numericInput("max_n", "Largest sample size to plot", value = 2000, min = 20)
  ),
  value_box(
    title = "Subjects needed to detect the interaction",
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
      "About this app",
      p("Subjects are split evenly among the four cells of a 2x2 factorial experiment: neither treatment, A only, B only, and both. The interaction is how much the effect of A changes when B is also present: (both minus A only) minus (B only minus neither). Power is the probability that a two-sided test of the interaction comes out statistically significant."),
      p("Interactions are expensive. The interaction is a difference of two differences, so its standard error is twice that of the main effect of A averaged over B, estimated on the same N. Detecting an interaction therefore takes about four times the sample that a main effect of the same size needs. The teal line shows that comparison."),
      p("For a binary outcome, each cell's variance is p(1 - p) for its own proportion. The calculation uses the normal approximation, so it is optimistic for very small samples.")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  design <- reactive({
    alpha <- as.numeric(input$alpha)
    if (input$outcome == "continuous") {
      validate(
        need(isTruthy(input$interaction) && input$interaction != 0, "Enter a nonzero interaction effect."),
        need(isTruthy(input$sigma) && input$sigma > 0, "Enter a positive standard deviation.")
      )
      return(list(
        effect = input$interaction,
        cell_variances = rep(input$sigma^2, 4),
        alpha = alpha,
        label = paste0("interaction = ", input$interaction, ", SD = ", input$sigma)
      ))
    }
    p <- c(input$p00, input$p10, input$p01, input$p11)
    validate(
      need(length(p) == 4 && !anyNA(p), "Enter all four proportions."),
      need(all(p >= 0 & p <= 1), "Proportions must lie between 0 and 1.")
    )
    effect <- (input$p11 - input$p10) - (input$p01 - input$p00)
    validate(need(abs(effect) > 1e-9, "These four proportions imply no interaction."))
    list(
      effect = effect,
      cell_variances = p * (1 - p),
      alpha = alpha,
      label = paste0("interaction = ", round(100 * effect, 1), " percentage points")
    )
  })

  output$binary_interaction <- renderUI({
    p <- c(input$p00, input$p10, input$p01, input$p11)
    req(length(p) == 4, !anyNA(p))
    p(class = "small",
      "Effect of A without B: ", round(100 * (input$p10 - input$p00), 1), " points. ",
      "With B: ", round(100 * (input$p11 - input$p01), 1), " points. ",
      tags$b("Interaction: ", round(100 * ((input$p11 - input$p01) - (input$p10 - input$p00)), 1), " points."))
  })

  n_required <- reactive({
    d <- design()
    power <- power_from_se(d$effect, sqrt(4 * sum(d$cell_variances) / n_search_grid), d$alpha)
    reached <- which(power >= input$target)
    if (length(reached) == 0) NA else n_search_grid[min(reached)]
  })

  output$n_required <- renderText({
    n <- n_required()
    if (is.na(n)) "More than 100 million" else format(n, big.mark = ",")
  })

  output$n_detail <- renderText({
    n <- n_required()
    target <- paste0(round(100 * input$target), "% power")
    if (is.na(n)) return(paste0("To reach ", target, "."))
    paste0("To reach ", target, ": ", format(n / 4, big.mark = ","), " subjects in each of the four cells.")
  })

  output$power_plot <- renderPlot(bg = "transparent", {
    d <- design()
    max_n <- req(input$max_n)
    ns <- if (max_n <= 2000) seq(4, max_n) else round(seq(4, max_n, length.out = 2000))
    v <- sum(d$cell_variances)

    gg_df <- rbind(
      tibble::tibble(N = ns, power = power_from_se(d$effect, sqrt(4 * v / ns), d$alpha), series = "Interaction"),
      tibble::tibble(N = ns, power = power_from_se(d$effect, sqrt(v / ns), d$alpha), series = "Main effect of the same size")
    )
    label_df <- gg_df[gg_df$N == max(ns), ]

    ggplot(gg_df, aes(N, power, color = series)) +
      geom_hline(yintercept = input$target, linetype = "dashed", color = graphite, linewidth = 0.4) +
      annotate("text", x = 0, y = input$target, label = paste0("target: ", round(100 * input$target), "%"),
        hjust = 0, vjust = -0.6, color = graphite, size = 4.2) +
      geom_line(linewidth = 1.3) +
      geom_text(data = label_df, aes(label = series), hjust = 1, vjust = 1.6, size = 4.4) +
      scale_color_manual(values = c("Interaction" = crimson, "Main effect of the same size" = teal)) +
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
