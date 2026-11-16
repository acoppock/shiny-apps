# Verify the factorial interaction power formula ----
# 1. Continuous: against the 2017 DeclareDesign diagnosands the original app
#    plotted (standard normal outcome, 1,000 sims per cell, Z1:Z2 coefficient).
# 2. Binary: a fresh simulation with four cell proportions, estimated by
#    lm_robust (HC2) on the saturated model. Run from the repo root.

library(tidyverse)
library(estimatr)

app_src <- read_lines("apps/factorial/app.R")
eval(parse(text = app_src[str_which(app_src, "^power_from_se <- "):(str_which(app_src, "^n_search_grid") - 1)]))

set.seed(343)

# Continuous ----
continuous <- read_rds("verification/factorial_2017_diagnosands.rds") |>
  as_tibble() |>
  select(N, beta_3, simulated_power = power) |>
  mutate(
    app_power = power_from_se(beta_3, sqrt(4 * 4 / N), 0.05),
    mc_se = sqrt(simulated_power * (1 - simulated_power) / 1000),
    z = (app_power - simulated_power) / mc_se
  )
print(continuous |> summarize(max_abs_diff = max(abs(app_power - simulated_power)), share_within_2se = mean(abs(z) < 2, na.rm = TRUE)))

# Binary ----
simulate_binary <- function(N, p, sims = 2000) {
  cell <- rep(1:4, each = N / 4)
  a <- as.integer(cell %in% c(2, 4))
  b <- as.integer(cell %in% c(3, 4))
  significant <- map_lgl(seq_len(sims), \(i) {
    y <- rbinom(N, 1, p[cell])
    fit <- lm_robust(y ~ a * b, se_type = "HC2")
    fit$p.value["a:b"] < 0.05
  })
  mean(significant)
}

binary <- tribble(
  ~N, ~p,
  400, c(0.5, 0.6, 0.55, 0.75),
  1200, c(0.5, 0.6, 0.55, 0.75),
  800, c(0.2, 0.3, 0.25, 0.25),
  2000, c(0.1, 0.15, 0.1, 0.2)
) |>
  mutate(
    interaction = map_dbl(p, \(x) (x[4] - x[2]) - (x[3] - x[1])),
    app_power = map2_dbl(N, p, \(n, x) power_from_se((x[4] - x[2]) - (x[3] - x[1]), sqrt(4 * sum(x * (1 - x)) / n), 0.05)),
    simulated_power = map2_dbl(N, p, simulate_binary),
    mc_se = sqrt(simulated_power * (1 - simulated_power) / 2000),
    difference = app_power - simulated_power
  )
print(binary |> select(-p), width = Inf)
