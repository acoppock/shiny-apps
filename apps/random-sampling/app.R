library(shiny)
library(bslib)
library(ggplot2)

# Data ----
# Simulations from a 2017 DeclareDesign design: draw a simple random sample of
# size n from a population with a binary outcome and estimate the population
# mean with the sample mean and its 95% confidence interval. 200 simulations
# at each sample size; the population is drawn once per sample size.
simulations <- readRDS("data/srs_simulations.rds")

x_limits <- c(0.25, 0.75)

# Theme ----
app_theme <- bs_theme(version = 5, preset = "cosmo") |>
  bs_bundle(sass::sass_layer_file("brand.scss"), sass::sass_layer_file("apps.scss"))

crimson <- "#a72f5b"
teal <- "#004c6d"
graphite <- "#3C444C"

plot_theme <- theme_minimal(base_size = 15) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = graphite)
  )

# UI ----
ui <- page_sidebar(
  title = "Simple Random Sampling",
  window_title = "Simple Random Sampling",
  theme = app_theme,
  tags$head(tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Source+Sans+3:ital,wght@0,400;0,600;1,400&family=Source+Serif+4:opsz,wght@8..60,400;8..60,600&display=swap"
  )),
  sidebar = sidebar(
    width = 330,
    sliderInput("n", "Sample size", min = 100, max = 1000, value = 100, step = 100, ticks = FALSE, animate = TRUE),
    p(class = "text-muted small", "Press play to step through the sample sizes.")
  ),
  layout_columns(
    value_box(
      title = "Standard deviation of the estimates",
      value = textOutput("sd_estimate"),
      theme = "text-primary",
      max_height = "120px"
    ),
    value_box(
      title = "Intervals that cover the truth",
      value = textOutput("coverage"),
      theme = "text-primary",
      max_height = "120px"
    )
  ),
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Sampling distribution of the sample mean"),
      plotOutput("hist_plot", height = "420px")
    ),
    card(
      full_screen = TRUE,
      card_header("95% confidence intervals"),
      plotOutput("ci_plot", height = "420px")
    )
  ),
  accordion(
    open = FALSE,
    accordion_panel(
      "About this app",
      p("Each simulation draws a simple random sample from a population in which about half the units have an outcome of 1, then estimates the population mean with the sample mean. The left panel shows the 200 estimates at the chosen sample size; the dashed line is the true population mean. The right panel shows each simulation's 95% confidence interval, sorted by its midpoint, colored by whether it covers the true mean."),
      p("The sample mean is unbiased at every sample size: the estimates center on the truth. Larger samples make the estimates more precise, so the histogram narrows and the intervals shrink, but about 95% of the intervals cover the truth no matter the sample size."),
      p("The simulations were run with DeclareDesign.")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  sims <- reactive({
    d <- simulations[simulations$n == input$n, ]
    d$covers <- d$ci_lower <= d$estimand & d$ci_upper >= d$estimand
    d$sim_order <- rank((d$ci_lower + d$ci_upper) / 2, ties.method = "first")
    d
  })

  output$sd_estimate <- renderText(format(round(sd(sims()$est), 3), nsmall = 3))

  output$coverage <- renderText({
    d <- sims()
    paste0(sum(d$covers), " of ", nrow(d), " (", round(100 * mean(d$covers)), "%)")
  })

  output$hist_plot <- renderPlot(bg = "transparent", {
    gg_df <- sims()
    truth <- gg_df$estimand[1]

    ggplot(gg_df, aes(est)) +
      geom_histogram(binwidth = 0.01, boundary = 0.005, fill = teal, color = "white", linewidth = 0.2) +
      geom_vline(xintercept = truth, color = crimson, linetype = "dashed", linewidth = 0.8) +
      annotate("text", x = truth, y = Inf, label = paste0("  true mean = ", truth), hjust = 0, vjust = 1.5,
        color = crimson, size = 4.2) +
      coord_cartesian(xlim = x_limits) +
      labs(x = "Sample mean estimates") +
      plot_theme
  })

  output$ci_plot <- renderPlot(bg = "transparent", {
    gg_df <- sims()
    truth <- gg_df$estimand[1]
    n_miss <- sum(!gg_df$covers)

    ggplot(gg_df, aes(y = sim_order, color = covers)) +
      geom_segment(aes(x = ci_lower, xend = ci_upper, yend = sim_order), linewidth = 0.5) +
      geom_vline(xintercept = truth, color = graphite, linetype = "dashed", linewidth = 0.6) +
      annotate("text", x = x_limits[1], y = nrow(gg_df), label = paste0("covers the truth: ", nrow(gg_df) - n_miss),
        hjust = 0, vjust = 1, color = teal, size = 4.2) +
      annotate("text", x = x_limits[1], y = nrow(gg_df) * 0.93, label = paste0("misses: ", n_miss),
        hjust = 0, vjust = 1, color = crimson, size = 4.2) +
      scale_color_manual(values = c("TRUE" = teal, "FALSE" = crimson)) +
      coord_cartesian(xlim = x_limits) +
      labs(x = "95% confidence intervals") +
      plot_theme
  })
}

shinyApp(ui, server)
