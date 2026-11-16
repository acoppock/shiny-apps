library(shiny)
library(bslib)
library(ggplot2)

# Data ----
# Diagnosands from a 2017 DeclareDesign simulation: a 2x2 factorial with a
# standard normal outcome, 1,000 simulations per combination of N and true
# interaction effect, with the interaction estimated as the Z1:Z2 coefficient.
diagnosands <- readRDS("data/factorial_diagnosands.rds")

power_target <- 0.9

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
    sliderInput(
      "beta_3",
      "True interaction effect (standard deviations)",
      min = 0.1,
      max = 0.5,
      value = 0.1,
      step = 0.05,
      ticks = FALSE,
      animate = TRUE
    ),
    p(class = "text-muted small", "Press play to step through the effect sizes.")
  ),
  value_box(
    title = paste0("Subjects needed for ", round(100 * power_target), "% power"),
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
      p("Subjects are randomly assigned to the four cells of a 2x2 factorial experiment. The interaction effect is how much the effect of one treatment changes when the other treatment is also present. Each point is the share of 1,000 simulated experiments in which the estimated interaction is statistically significant at the 0.05 level, given the true interaction shown on the slider."),
      p("Interactions are expensive. The interaction is a difference of two differences, each estimated on half the sample, so its standard error is twice that of a main effect estimated on the same N. Small interactions need very large experiments."),
      p("The simulations were run with DeclareDesign. The gray lines show power at the other effect sizes, for comparison.")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  current <- reactive({
    diagnosands[round(diagnosands$beta_3, 3) == round(input$beta_3, 3), ]
  })

  output$n_required <- renderText({
    d <- current()
    reached <- d$N[d$power >= power_target]
    if (length(reached) == 0) "More than 2,000" else format(min(reached), big.mark = ",")
  })

  output$n_detail <- renderText({
    d <- current()
    reached <- d$N[d$power >= power_target]
    if (length(reached) == 0) {
      return(paste0("At N = 2,000, power is only ", round(100 * d$power[d$N == 2000]), "%."))
    }
    paste0("About ", format(min(reached) / 4, big.mark = ","), " subjects in each of the four cells.")
  })

  output$power_plot <- renderPlot(bg = "transparent", {
    gg_df <- current()
    others_df <- diagnosands[round(diagnosands$beta_3, 3) != round(input$beta_3, 3), ]
    label_df <- gg_df[gg_df$N == max(gg_df$N), ]

    ggplot(gg_df, aes(N, power)) +
      geom_line(data = others_df, aes(group = beta_3), color = graphite, alpha = 0.15, linewidth = 0.6) +
      geom_hline(yintercept = power_target, linetype = "dashed", color = graphite, linewidth = 0.4) +
      annotate("text", x = 200, y = power_target, label = paste0("target: ", round(100 * power_target), "%"),
        hjust = 0, vjust = -0.6, color = graphite, size = 4.2) +
      geom_line(color = crimson, linewidth = 1.3) +
      geom_point(color = crimson, size = 2.6) +
      geom_text(data = label_df, aes(label = paste0("interaction = ", input$beta_3, " SD")),
        hjust = 1, vjust = 2, color = crimson, size = 4.4) +
      scale_x_continuous(breaks = seq(200, 2000, 200), labels = \(x) format(x, big.mark = ",")) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = \(x) paste0(round(100 * x), "%")) +
      labs(x = "Number of subjects", y = "Power") +
      theme_minimal(base_size = 15) +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = graphite)
      )
  })
}

shinyApp(ui, server)
