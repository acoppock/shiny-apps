library(shiny)
library(bslib)
library(ggplot2)

# Corrections ----
# All adjusted p-values come from p.adjust(), as in the 2015 app. Blank inputs
# are dropped, so m counts only the p-values actually entered.
methods <- c(
  "None" = "none",
  "Bonferroni" = "bonferroni",
  "Holm" = "holm",
  "Benjamini-Hochberg" = "BH"
)

# Per-rank cutoff each procedure compares the k-th smallest p-value against.
# A test is significant after correction exactly when its adjusted p-value is
# at or below alpha, which is what colors the points.
rank_threshold <- function(method, alpha, m) {
  k <- seq_len(m)
  switch(method,
    none = rep(alpha, m),
    bonferroni = rep(alpha / m, m),
    holm = alpha / (m + 1 - k),
    BH = k * alpha / m
  )
}

threshold_label <- function(method, alpha, m) {
  alpha <- sprintf("%.2f", alpha)
  switch(method,
    none = paste0("uncorrected cutoff, ", alpha),
    bonferroni = paste0("Bonferroni cutoff, ", alpha, " / ", m),
    holm = paste0("Holm cutoff, ", alpha, " / (", m, " + 1 - k)"),
    BH = paste0("Benjamini-Hochberg cutoff, k × ", alpha, " / ", m)
  )
}

default_ps <- c(0.005, 0.010, 0.025, 0.050, rep(NA, 6))

# Theme ----
app_theme <- bs_theme(version = 5, preset = "cosmo") |>
  bs_bundle(sass::sass_layer_file("brand.scss"), sass::sass_layer_file("apps.scss"))

crimson <- "#a72f5b"
teal <- "#004c6d"
graphite <- "#3C444C"

# UI ----
p_inputs <- lapply(seq_along(default_ps), \(i) {
  numericInput(paste0("p", i), paste("Test", i), value = default_ps[i], min = 0, max = 1, step = 0.001)
})

ui <- page_sidebar(
  title = "Multiple Comparisons Corrections",
  window_title = "Multiple Comparisons Corrections",
  theme = app_theme,
  tags$head(tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Source+Sans+3:ital,wght@0,400;0,600;1,400&family=Source+Serif+4:opsz,wght@8..60,400;8..60,600&display=swap"
  )),
  sidebar = sidebar(
    width = 330,
    radioButtons("alpha", "Significance level", c("0.01", "0.05", "0.10"), selected = "0.05", inline = TRUE),
    radioButtons("method", "Correction", methods, selected = "bonferroni"),
    tags$label(class = "control-label", "p-values (leave blank to omit)"),
    layout_column_wrap(width = 1 / 2, gap = "0.5rem", !!!p_inputs)
  ),
  layout_columns(
    col_widths = c(5, 7),
    card(
      card_header("Unadjusted and adjusted p-values"),
      tableOutput("p_table")
    ),
    card(
      full_screen = TRUE,
      card_header("Sorted p-values against the correction's cutoff"),
      plotOutput("p_plot", height = "420px")
    )
  ),
  accordion(
    open = FALSE,
    accordion_panel(
      "About this app",
      p("Testing many hypotheses at once raises the chance that at least one comes out significant by luck. Each correction below adjusts the p-values so that a fixed significance level means something for the whole family of tests. The adjusted p-values are computed with R's p.adjust() function."),
      p("The Bonferroni correction is very extreme. It multiplies each unadjusted p-value by the total number of tests (equivalently, it compares each p-value to alpha divided by the number of tests). The Bonferroni correction controls the family-wise error rate (FWER), the probability of at least one false rejection, whatever the dependence among the tests."),
      p("The Holm correction also controls the FWER, but is slightly less extreme. It proceeds as follows: order your m p-values from smallest to largest. Find the smallest p-value that satisfies this condition: p_k > alpha / (m + 1 - k), where k is the p-value's rank. This and all larger p-values are insignificant; all smaller p-values are significant."),
      p("The Benjamini-Hochberg procedure controls the false discovery rate (FDR), the expected share of rejections that are false. Like the Holm correction, you begin by ordering the m p-values. Then you find the largest p-value that satisfies p_k <= (k / m) × alpha. This test, and all tests with smaller p-values, are declared significant."),
      p("In the plot, the teal bars mark each rank's cutoff. Holm works up from the smallest p-value and stops at the first one above its bar; Benjamini-Hochberg works down from the largest and stops at the first one below its bar, so a p-value can sit above its own bar and still be significant."),
      p("For more on multiple comparisons, see the ", a("EGAP Methods Guide.", href = "https://egap.org/resource/10-things-to-know-about-multiple-comparisons/", target = "_blank"))
    )
  )
)

# Server ----
server <- function(input, output, session) {
  results <- reactive({
    ps <- vapply(seq_along(default_ps), \(i) {
      value <- input[[paste0("p", i)]]
      if (is.numeric(value) && length(value) == 1) value else NA_real_
    }, numeric(1))
    entered <- which(!is.na(ps))
    validate(
      need(length(entered) > 0, "Enter at least one p-value."),
      need(all(ps[entered] >= 0 & ps[entered] <= 1), "p-values must lie between 0 and 1.")
    )
    alpha <- as.numeric(input$alpha)
    adjusted <- p.adjust(ps[entered], method = input$method)
    tibble::tibble(
      test = entered,
      p = ps[entered],
      significant = p <= alpha,
      adjusted = adjusted,
      adjusted_significant = adjusted <= alpha
    )
  })

  output$p_table <- renderTable(align = "lrcrc", {
    res <- results()
    yes_no <- \(x) ifelse(x, "Yes", "No")
    tibble::tibble(
      Test = as.character(res$test),
      `Unadjusted p` = sprintf("%.3f", res$p),
      `Significant` = yes_no(res$significant),
      `Adjusted p` = sprintf("%.3f", res$adjusted),
      `Significant after correction` = yes_no(res$adjusted_significant)
    )
  })

  output$p_plot <- renderPlot(bg = "transparent", {
    res <- results()
    alpha <- as.numeric(input$alpha)
    m <- nrow(res)

    gg_df <- res[order(res$p), ]
    gg_df$rank <- seq_len(m)
    gg_df$status <- ifelse(gg_df$adjusted_significant, "significant", "not significant")

    threshold_df <- tibble::tibble(rank = seq_len(m), cutoff = rank_threshold(input$method, alpha, m))

    ggplot(gg_df, aes(rank, p)) +
      geom_segment(
        data = threshold_df,
        aes(x = rank - 0.3, xend = rank + 0.3, y = cutoff, yend = cutoff),
        color = teal, linewidth = 1.4
      ) +
      geom_point(aes(shape = status), color = crimson, size = 4, stroke = 1.3, fill = "white") +
      scale_shape_manual(values = c("significant" = 19, "not significant" = 21)) +
      scale_x_continuous(breaks = seq_len(m), labels = paste("Test", gg_df$test), expand = expansion(add = 0.6)) +
      scale_y_sqrt(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
      labs(
        x = "Tests sorted by p-value, smallest to largest (rank k)",
        y = "Unadjusted p-value (square-root scale)",
        subtitle = paste0(
          "Teal bars: ", threshold_label(input$method, alpha, m), "\n",
          "Crimson points: filled if significant after the correction, open if not"
        )
      ) +
      theme_minimal(base_size = 15) +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(color = graphite),
        axis.text = element_text(color = graphite)
      )
  })
}

shinyApp(ui, server)
