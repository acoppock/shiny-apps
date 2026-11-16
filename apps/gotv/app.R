library(shiny)
library(bslib)
library(ggplot2)

# Estimators ----
# Difference in means with its HC2 standard error. With a single binary
# regressor, the HC2 standard error from lm(Y ~ Z) is exactly
# sqrt(var(Y1) / n1 + var(Y0) / n0), which the 2015 app computed with
# sandwich::vcovHC(type = "HC2") and estimatr::lm_robust() reports by default.
diff_in_means_hc2 <- function(Y, Z) {
  Y1 <- Y[Z == 1]
  Y0 <- Y[Z == 0]
  list(
    estimate = mean(Y1) - mean(Y0),
    se = sqrt(var(Y1) / length(Y1) + var(Y0) / length(Y0))
  )
}

# Two-sided power of a difference in means with equal arms (normal approximation).
power_calculator <- function(mu_t, mu_c, sigma, alpha = 0.05, N) {
  z <- abs(mu_t - mu_c) * sqrt(N) / (2 * sigma)
  crit <- qnorm(1 - alpha / 2)
  pnorm(z - crit) + pnorm(-z - crit)
}

# Expands the six counts into individual-level vectors. Y and D are not
# aligned within the treatment group (the counts do not say how many of the
# contacted voted), which is harmless because only group means are used.
counts_to_data <- function(n_Z1, n_Z0, n_Z1_contacted, n_Z0_contacted, n_Z1_voted, n_Z0_voted) {
  list(
    Y = rep(c(0, 1, 0, 1), times = c(n_Z1 - n_Z1_voted, n_Z1_voted, n_Z0 - n_Z0_voted, n_Z0_voted)),
    D = rep(c(1, 0, 0, 1), times = c(n_Z1_contacted, n_Z1 - n_Z1_contacted, n_Z0 - n_Z0_contacted, n_Z0_contacted)),
    Z = rep(c(1, 0), times = c(n_Z1, n_Z0))
  )
}

# ITT, contact rate, and CACE. The CACE standard error divides the ITT
# standard error by the estimated contact rate, treating the contact rate as
# known (Gerber and Green 2012, chapter 5).
noncompliance_estimates <- function(Y, D, Z, scale = 1) {
  itt <- diff_in_means_hc2(Y, Z)
  contact_rate <- mean(D[Z == 1]) - mean(D[Z == 0])
  itt_hat <- itt$estimate * scale
  se_itt <- itt$se * scale
  cace <- itt_hat / contact_rate
  se_cace <- se_itt / contact_rate
  list(
    contact_rate = contact_rate,
    itt_hat = itt_hat,
    se_itt = se_itt,
    cace = cace,
    se_cace = se_cace,
    ci = c(cace - 1.96 * se_cace, cace + 1.96 * se_cace)
  )
}

# Average untreated outcome among compliers: the control-group mean, net of
# always-takers (treated controls) and never-takers (untreated treatment-group
# members), divided by the complier share.
untreated_complier_mean <- function(Y, D, Z, contact_rate) {
  pr_at <- mean(D[Z == 0])
  E_Y_1_at <- if (any(D == 1 & Z == 0)) mean(Y[D == 1 & Z == 0]) else 0
  pr_nt <- mean(D[Z == 1] == 0)
  E_Y_0_nt <- if (any(D == 0 & Z == 1)) mean(Y[D == 0 & Z == 1]) else 0
  (mean(Y[Z == 0]) - pr_at * E_Y_1_at - pr_nt * E_Y_0_nt) / contact_rate
}

# Theme ----
app_theme <- bs_theme(version = 5, preset = "cosmo") |>
  bs_bundle(sass::sass_layer_file("brand.scss"), sass::sass_layer_file("apps.scss"))

crimson <- "#a72f5b"
teal <- "#004c6d"
graphite <- "#3C444C"

# UI ----
ui <- page_sidebar(
  title = "Analysis of Field Experiments",
  window_title = "Analysis of Field Experiments",
  theme = app_theme,
  tags$head(tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Source+Sans+3:ital,wght@0,400;0,600;1,400&family=Source+Serif+4:opsz,wght@8..60,400;8..60,600&display=swap"
  )),
  sidebar = sidebar(
    width = 340,
    radioButtons("mode", "Data", c("Six counts" = "counts", "Upload a dataset" = "upload"), inline = TRUE),
    conditionalPanel(
      "input.mode == 'counts'",
      numericInput("n_Z1", "N assigned to treatment", value = 466, min = 1),
      numericInput("n_Z0", "N assigned to control", value = 297, min = 1),
      numericInput("n_Z1_contacted", "N in treatment who are actually treated", value = 342, min = 0),
      numericInput("n_Z0_contacted", "N in control who are inadvertently treated", value = 0, min = 0),
      numericInput("n_Z1_voted", "N who voted in treatment", value = 86, min = 0),
      numericInput("n_Z0_voted", "N who voted in control", value = 41, min = 0),
      p(class = "text-muted small", "Defaults: Michelson's door-to-door canvassing experiment in Dos Palos, California.")
    ),
    conditionalPanel(
      "input.mode == 'upload'",
      fileInput("file1", "CSV file", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      selectInput("Y", "Outcome", choices = NULL),
      selectInput("D", "Treatment received", choices = NULL),
      selectInput("Z", "Treatment assigned", choices = NULL),
      p(class = "text-muted small", "Three columns: a numeric outcome, treatment received (0 or 1), and treatment assigned (0 or 1). With no noncompliance, choose the assignment column for both treatment variables.")
    )
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(
      full_screen = TRUE,
      card_header(textOutput("plot_title", inline = TRUE)),
      plotOutput("out_plot", height = "380px"),
      p(class = "text-muted small", "Error bars are 95% confidence intervals.")
    ),
    card(
      card_header("Summary of input"),
      tableOutput("summary_table")
    )
  ),
  card(
    card_header("Statistical results"),
    tableOutput("statistical_results")
  ),
  accordion(
    open = FALSE,
    accordion_panel(
      "About this app",
      p("This app analyzes experiments in which only some of the subjects assigned to the treatment group actually receive the treatment, and in which some of the control group may be treated inadvertently. With the six counts, the outcome is dichotomous (for example, voting versus not voting). With an uploaded dataset, the outcome can be any number."),
      p("The six counts are the number of people you (1) assigned to the treatment group, (2) assigned to the control group, (3) successfully treated in the treatment group, (4) inadvertently treated in the control group, (5) found to have voted in the treatment group, and (6) found to have voted in the control group. Melissa Michelson's door-to-door canvassing experiment in Dos Palos, California provides a nice illustration. Prior to the 2001 election, she assigned 466 people with Latino surnames to the treatment group and 297 to the control group. 342 of the people in the treatment group were successfully contacted. No one in the control group was contacted. In the treatment group, 86 people voted, whereas 41 people voted in the control group. The six numbers are therefore: 466, 297, 342, 0, 86, 41.")
    ),
    accordion_panel(
      "Interpretation of results",
      p("The intent-to-treat effect is the effect of assigning subjects to the treatment group, irrespective of whether they are actually treated. With the six counts, the intent-to-treat estimate is expressed in percentage points: an estimate of 5 implies that assignment to the treatment group increases one's probability of voting by 5 percentage points."),
      p("The standard error of this estimate is the standard deviation of the sampling distribution of this estimate; the larger the standard error, the more uncertainty there is about the value of the intent-to-treat parameter."),
      p("The complier average causal effect (CACE) is the effect of the treatment on 'compliers,' i.e., those who are actually treatable if assigned to the treatment group. This quantity is estimated by dividing the intent-to-treat effect by the contact rate. This estimator is equivalent to instrumental variables or two-stage least squares regression. The standard error expresses the uncertainty of this estimate."),
      p("The 95% confidence interval comes from a procedure that brackets the true parameter 95% of the time: if the experiment were repeated a large number of times, 95% of the intervals would encompass the true parameter."),
      p("With the six counts, the app reports a one-tailed p-value: the probability of obtaining an estimated intent-to-treat effect at least as large as what was observed in this sample, if the true effect were zero. With an uploaded dataset, it reports a two-tailed p-value: the probability of obtaining an estimate at least as large in absolute value. When this level is below 0.05, the estimates are conventionally referred to as 'statistically significant.' The test is based on a large-sample approximation. If you are working with small samples and want to conduct an exact significance test, see Gerber and Green (2012), chapter 3."),
      p("The statistical power of an experiment is its probability of rejecting the null hypothesis, given that the true CACE were identical to the CACE estimated from this sample. Suppose, for example, that the estimated CACE were 10 and the power of the study is 50%. For this effect size, the experiment has a 50% chance of rejecting the null hypothesis at the 0.05 level. With the six counts, power refers to a one-tailed test; with an uploaded dataset, to a two-tailed test."),
      p("With an uploaded dataset, the app also reports the average outcome among untreated compliers. It helps interpret the results, since it may differ from the overall average in the control group.")
    ),
    accordion_panel(
      "Credits and further reading",
      p("Statistical procedures by Donald Green and Alan Gerber. App by Alexander Coppock."),
      p("Angrist, Joshua D., Guido W. Imbens, and Donald B. Rubin. 1996. Identification of Causal Effects Using Instrumental Variables. Journal of the American Statistical Association 91(434): 444-455."),
      p("Aronow, Peter M., and Donald P. Green. 2013. Sharp Bounds for Complier Average Potential Outcomes in Experiments with Noncompliance and Incomplete Reporting. Statistics and Probability Letters 83: 677-679."),
      p("Gerber, Alan S., and Donald P. Green. 2012. Field Experiments: Design, Analysis, and Interpretation. W. W. Norton."),
      p("Michelson, Melissa R. 2003. Getting Out the Latino Vote: How Door-to-Door Canvassing Influences Voter Turnout in Rural Central California. Political Behavior 25(3): 247-263.")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  uploaded <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })

  observeEvent(uploaded(), {
    vars <- names(uploaded())
    updateSelectInput(session, "Y", choices = vars, selected = vars[1])
    updateSelectInput(session, "D", choices = vars, selected = vars[min(2, length(vars))])
    updateSelectInput(session, "Z", choices = vars, selected = vars[min(3, length(vars))])
  })

  experiment <- reactive({
    if (input$mode == "counts") {
      counts <- c(input$n_Z1, input$n_Z0, input$n_Z1_contacted, input$n_Z0_contacted, input$n_Z1_voted, input$n_Z0_voted)
      validate(
        need(length(counts) == 6 && !anyNA(counts), "Enter all six counts."),
        need(all(counts >= 0) && all(counts == round(counts)), "Counts must be whole numbers, zero or more."),
        need(input$n_Z1 >= 2 && input$n_Z0 >= 2, "Assign at least two people to each group."),
        need(input$n_Z1_contacted <= input$n_Z1 && input$n_Z1_voted <= input$n_Z1, "Treatment-group counts cannot exceed the number assigned to treatment."),
        need(input$n_Z0_contacted <= input$n_Z0 && input$n_Z0_voted <= input$n_Z0, "Control-group counts cannot exceed the number assigned to control.")
      )
      dat <- counts_to_data(input$n_Z1, input$n_Z0, input$n_Z1_contacted, input$n_Z0_contacted, input$n_Z1_voted, input$n_Z0_voted)
    } else {
      validate(need(isTruthy(input$file1), "Upload a CSV file to begin."))
      dat_raw <- uploaded()
      req(input$Y %in% names(dat_raw), input$D %in% names(dat_raw), input$Z %in% names(dat_raw))
      dat <- list(Y = dat_raw[[input$Y]], D = dat_raw[[input$D]], Z = dat_raw[[input$Z]])
      validate(
        need(is.numeric(dat$Y), "The outcome must be numeric."),
        need(all(dat$D %in% c(0, 1)), "Treatment received can only include 0's and 1's."),
        need(all(dat$Z %in% c(0, 1)), "Treatment assigned can only include 0's and 1's."),
        need(sum(dat$Z == 1) >= 2 && sum(dat$Z == 0) >= 2, "Each assigned group needs at least two subjects.")
      )
    }
    validate(need(
      mean(dat$D[dat$Z == 1]) != mean(dat$D[dat$Z == 0]),
      "The contact rate is zero (treatment and control are treated at the same rate), so the CACE is not defined."
    ))
    dat
  })

  output$plot_title <- renderText({
    if (input$mode == "counts") "Voting rates by assigned group" else "Average outcomes by assigned group"
  })

  output$out_plot <- renderPlot(bg = "transparent", {
    dat <- experiment()
    counts_mode <- input$mode == "counts"

    # The 2015 app used sqrt(p * (1 - p) / n) for the voting rates and
    # sd(Y) / sqrt(n) for uploaded outcomes; both are kept.
    gg_df <- tibble::tibble(group = c("Control", "Treatment"), z = c(0, 1)) |>
      transform(
        y_bar = sapply(z, \(k) mean(dat$Y[dat$Z == k])),
        n = sapply(z, \(k) sum(dat$Z == k)),
        s = sapply(z, \(k) sd(dat$Y[dat$Z == k]))
      ) |>
      transform(se = if (counts_mode) sqrt(y_bar * (1 - y_bar) / n) else s / sqrt(n)) |>
      transform(
        ui = y_bar + 1.96 * se,
        li = y_bar - 1.96 * se,
        label = if (counts_mode) sprintf("%.1f%%", 100 * y_bar) else sprintf("%.2f", y_bar)
      )

    ggplot(gg_df, aes(group, y_bar, color = group)) +
      geom_linerange(aes(ymin = li, ymax = ui), linewidth = 1.4) +
      geom_point(size = 5) +
      geom_text(aes(label = label), hjust = -0.35, size = 4.6) +
      scale_color_manual(values = c(Control = teal, Treatment = crimson)) +
      scale_y_continuous(
        limits = if (counts_mode) c(0, NA),
        labels = if (counts_mode) \(x) paste0(round(100 * x), "%") else waiver()
      ) +
      labs(x = NULL, y = if (counts_mode) "Average voting rate" else "Average outcome") +
      theme_minimal(base_size = 15) +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(color = graphite)
      )
  })

  output$summary_table <- renderTable(rownames = TRUE, striped = TRUE, width = "100%", {
    dat <- experiment()
    Y <- dat$Y
    D <- dat$D
    Z <- dat$Z
    if (input$mode == "counts") {
      tab <- rbind(
        "Percent voting" = c(mean(Y[Z == 1]), mean(Y[Z == 0])) * 100,
        "Percent contacted" = c(mean(D[Z == 1]), mean(D[Z == 0])) * 100
      )
    } else {
      tab <- rbind(
        "Average outcome" = c(mean(Y[Z == 1]), mean(Y[Z == 0])),
        "Percent contacted" = c(mean(D[Z == 1]), mean(D[Z == 0])) * 100
      )
    }
    colnames(tab) <- c("Treatment", "Control")
    format(round(tab, 2), nsmall = 2)
  })

  output$statistical_results <- renderTable(colnames = FALSE, striped = TRUE, width = "100%", align = "lr", {
    dat <- experiment()
    Y <- dat$Y
    D <- dat$D
    Z <- dat$Z
    mu_t <- mean(Y[Z == 1])
    mu_c <- mean(Y[Z == 0])

    if (input$mode == "counts") {
      est <- noncompliance_estimates(Y, D, Z, scale = 100)
      p_label <- "One-tailed p-value"
      p_value <- pnorm(abs(est$itt_hat), sd = est$se_itt, lower.tail = FALSE)
      # One-tailed test at 0.05, approximated by a two-sided test at 0.10.
      power <- power_calculator(mu_t = mu_t, mu_c = mu_c, sigma = sqrt(mu_t * (1 - mu_t)), alpha = 0.1, N = length(Y))
    } else {
      est <- noncompliance_estimates(Y, D, Z, scale = 1)
      p_label <- "Two-tailed p-value"
      p_value <- 2 * pnorm(abs(est$itt_hat), sd = est$se_itt, lower.tail = FALSE)
      power <- power_calculator(mu_t = mu_t, mu_c = mu_c, sigma = sd(Y), alpha = 0.05, N = length(Y))
    }

    results <- tibble::tibble(
      quantity = c(
        "Contact rate",
        "Estimated intent-to-treat effect",
        "Standard error of estimated intent-to-treat effect",
        "Estimated complier average causal effect (CACE)",
        "Standard error of estimated CACE",
        "95% confidence interval for estimated CACE",
        p_label,
        "Statistical power of this experiment"
      ),
      value = c(
        sprintf("%.3f", est$contact_rate),
        sprintf("%.3f", est$itt_hat),
        sprintf("%.3f", est$se_itt),
        sprintf("%.3f", est$cace),
        sprintf("%.3f", est$se_cace),
        paste0(sprintf("%.3f", est$ci[1]), ", ", sprintf("%.3f", est$ci[2])),
        sprintf("%.3f", p_value),
        sprintf("%.3f", power)
      )
    )
    if (input$mode == "upload") {
      results <- rbind(results, tibble::tibble(
        quantity = "Estimated average outcome among untreated compliers",
        value = sprintf("%.3f", untreated_complier_mean(Y, D, Z, est$contact_rate))
      ))
    }
    results
  })
}

shinyApp(ui, server)
