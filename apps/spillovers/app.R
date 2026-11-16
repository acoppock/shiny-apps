library(shiny)
library(bslib)
library(ggplot2)

# Simulated geography ----
# 50 units scattered on a map, 10 of them treated. Outcomes are generated
# under a true spillover radius of 5km; the user picks the radius the analyst
# assumes. `sample(rep(c(0, 1), c(N - m, m)))` is exactly what
# randomizr::complete_ra(N, m) does, so the draws match the 2015 app's.
N <- 50
m <- 10
n_sims <- 1000
true_radius <- 5

set.seed(343)
coords <- matrix(rnorm(N * 2) * 10, ncol = 2)
distmat <- as.matrix(dist(coords))
assign <- sample(rep(c(0, 1), c(N - m, m)))
Z_mat <- replicate(n_sims, sample(rep(c(0, 1), c(N - m, m))))

# Exposure conditions ----
# "00" control, no spillover; "10" treated, no spillover; "01" control,
# spillover; "11" treated, spillover. A unit is exposed to spillover if any
# other treated unit lies within the radius. Works on a vector or on a matrix
# of assignments (one column per randomization).
adjacency <- function(radius) {
  adjmat <- 1 * (distmat <= radius)
  diag(adjmat) <- 0
  adjmat
}

get_condition <- function(Z, adjmat) {
  exposure <- adjmat %*% Z
  condition <- array("00", dim = dim(exposure))
  condition[Z == 1 & exposure == 0] <- "10"
  condition[Z == 0 & exposure > 0] <- "01"
  condition[Z == 1 & exposure > 0] <- "11"
  condition
}

true_cond_mat <- get_condition(Z_mat, adjacency(true_radius))

# Inverse-probability-weighted estimates ----
# For each randomization, the Hajek (weighted-mean) difference between units in
# condition k and units in "00", weighting each unit by one over its
# probability of being in the condition it is in. Units that never (or always)
# land in either condition are dropped, as in the original. Returns the
# average estimate across the n_sims randomizations; NA when no randomization
# puts any eligible unit in condition k.
average_ipw_estimates <- function(radius, effects) {
  cond_mat <- get_condition(Z_mat, adjacency(radius))
  prob <- sapply(c("00", "01", "10", "11"), \(k) rowMeans(cond_mat == k))
  Y_mat <- matrix(0, N, n_sims)
  for (k in c("01", "10", "11")) Y_mat[true_cond_mat == k] <- effects[[k]]

  hajek_mean <- function(k, eligible) {
    w <- (cond_mat == k & eligible) / prob[, k]
    w[is.nan(w)] <- 0
    colSums(w * Y_mat) / colSums(w)
  }

  estimates <- sapply(c("10", "01", "11"), \(k) {
    eligible <- prob[, "00"] > 0 & prob[, "00"] < 1 & prob[, k] > 0 & prob[, k] < 1
    est <- hajek_mean(k, eligible) - hajek_mean("00", eligible)
    mean(est[!is.nan(est)])
  })
  estimates[is.nan(estimates)] <- NA
  estimates
}

# Theme ----
app_theme <- bs_theme(version = 5, preset = "cosmo") |>
  bs_bundle(sass::sass_layer_file("brand.scss"), sass::sass_layer_file("apps.scss"))

crimson <- "#a72f5b"
teal <- "#004c6d"
graphite <- "#3C444C"

# UI ----
ui <- page_sidebar(
  title = "Geographic Spillovers",
  window_title = "Geographic Spillovers",
  theme = app_theme,
  tags$head(tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Source+Sans+3:ital,wght@0,400;0,600;1,400&family=Source+Serif+4:opsz,wght@8..60,400;8..60,600&display=swap"
  )),
  sidebar = sidebar(
    width = 330,
    sliderInput("radius", "Assumed spillover radius (km)", min = 0, max = 20, value = 3, step = 1, ticks = FALSE),
    p(class = "text-muted small", "Outcomes were generated under a true radius of 5km."),
    numericInput("direct", "Direct effect", value = 2, min = -10, max = 10),
    numericInput("indirect", "Indirect (spillover) effect", value = -2, min = -10, max = 10),
    numericInput("direct_indirect", "Direct + indirect effect", value = -7, min = -10, max = 10)
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(
      full_screen = TRUE,
      card_header("Treated units and their assumed spillover zones"),
      plotOutput("radius_map", height = "480px")
    ),
    card(
      card_header("True effects and the average of 1,000 estimates"),
      tableOutput("estimate_table"),
      p(class = "text-muted small", "Bias is the average estimate minus the true effect. The estimates are unbiased only when the assumed radius equals the true radius of 5km.")
    )
  ),
  accordion(
    open = FALSE,
    accordion_panel(
      "About this app",
      p("Estimates of causal effects depend on the assumed spillover structure. Each of 50 units is either treated or not, and a unit is in a spillover condition if some other treated unit lies within the spillover radius. That gives four conditions: control without spillover, treated without spillover, control with spillover, and treated with spillover. The direct effect compares treated units without spillover to untreated units without spillover; the indirect effect compares untreated units with spillover to the same baseline; the third effect compares treated units with spillover to that baseline."),
      p("Outcomes were generated under a true radius of 5km, with a baseline outcome of zero. The analyst does not know the true radius and must assume one. For each of 1,000 random assignments of 10 treated units, the app classifies units under the assumed radius, computes each unit's probability of landing in its condition across those assignments, and forms inverse-probability-weighted (Hajek) estimates of the three effects. Units whose probability of being in a comparison condition is zero or one are dropped from that comparison. The table reports the average estimate across the 1,000 assignments."),
      p("Set the radius to 5 and the bias disappears. Any other radius misclassifies some units, and the estimates are biased. At a radius of 0, no unit is ever exposed to spillover, so the indirect and combined effects cannot be estimated at all.")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  effects <- reactive({
    validate(need(
      isTruthy(input$direct) && isTruthy(input$indirect) && isTruthy(input$direct_indirect),
      "Enter all three effects."
    ))
    list("10" = input$direct, "01" = input$indirect, "11" = input$direct_indirect)
  })

  output$radius_map <- renderPlot(bg = "transparent", {
    radius <- input$radius
    cond <- get_condition(assign, adjacency(radius))[, 1]

    gg_df <- tibble::tibble(
      x = coords[, 1],
      y = coords[, 2],
      group = factor(
        c("00" = "Control, no spillover", "10" = "Treated, no spillover",
          "01" = "Control, spillover", "11" = "Treated, spillover")[cond],
        levels = c("Control, no spillover", "Treated, no spillover", "Control, spillover", "Treated, spillover")
      )
    )
    theta <- seq(0, 2 * pi, length.out = 120)
    treated <- which(assign == 1)
    circle_df <- tibble::tibble(
      unit = rep(treated, each = length(theta)),
      x = coords[unit, 1] + radius * cos(theta),
      y = coords[unit, 2] + radius * sin(theta)
    )

    ggplot(gg_df, aes(x, y)) +
      geom_path(data = circle_df, aes(group = unit), color = crimson, alpha = 0.5, linewidth = 0.5) +
      geom_point(aes(color = group, shape = group), size = 3.4, stroke = 1.2) +
      scale_color_manual(values = c(teal, crimson, teal, crimson), drop = FALSE) +
      scale_shape_manual(values = c(1, 1, 16, 16), drop = FALSE) +
      coord_equal(xlim = c(-25, 25), ylim = c(-25, 25)) +
      labs(x = NULL, y = NULL, color = NULL, shape = NULL,
        subtitle = paste0("Assumed radius: ", radius, "km. Filled points are exposed to spillover.")) +
      theme_minimal(base_size = 15) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        plot.subtitle = element_text(color = graphite)
      ) +
      guides(color = guide_legend(nrow = 2), shape = guide_legend(nrow = 2))
  })

  output$estimate_table <- renderTable(striped = TRUE, width = "100%", align = "lrrr", na = "Not estimable", {
    truth <- effects()
    estimates <- average_ipw_estimates(input$radius, truth)
    true_values <- c(truth[["10"]], truth[["01"]], truth[["11"]])
    tibble::tibble(
      Effect = c("Direct", "Indirect", "Direct + indirect"),
      True = sprintf("%.2f", true_values),
      `Average estimate` = ifelse(is.na(estimates), NA, sprintf("%.2f", estimates)),
      Bias = ifelse(is.na(estimates), NA, sprintf("%.2f", estimates - true_values))
    )
  })
}

shinyApp(ui, server)
