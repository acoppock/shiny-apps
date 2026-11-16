# Verify the power calculator against independent references ----
# 1. Analytic: stats::power.t.test and stats::power.prop.test (individual designs).
# 2. Simulation: DeclareDesign, which never touches the app's formula, in all
#    four modes. Run from the repo root: Rscript verification/verify_power.R

library(tidyverse)
library(DeclareDesign)

# Pull power_two_arm() out of the app itself, so the check is on shipped code ----
app_src <- read_lines("apps/power/app.R")
fn_start <- str_which(app_src, "^power_two_arm <- ")
fn_end <- str_which(app_src, "^n_search_grid") - 1
eval(parse(text = app_src[fn_start:fn_end]))

set.seed(343)

# Analytic check ----
analytic <- tibble(N = c(20, 60, 126, 300, 1000)) |>
  mutate(
    app_continuous = power_two_arm(N, effect = 5, sigma = 10, alpha = 0.05),
    t_test = map_dbl(N, \(n) power.t.test(n = n / 2, delta = 5, sd = 10, sig.level = 0.05)$power),
    app_binary = power_two_arm(N, effect = 0.15, sigma = sqrt((0.65 * 0.35 + 0.5 * 0.5) / 2), alpha = 0.05),
    prop_test = map_dbl(N, \(n) power.prop.test(n = n / 2, p1 = 0.5, p2 = 0.65, sig.level = 0.05)$power)
  )
print(analytic, width = Inf)

# Simulation designs ----
continuous_individual <- function(N, tau, sigma) {
  declare_model(N = N, U = rnorm(N, sd = sigma), potential_outcomes(Y ~ tau * Z + U)) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = complete_ra(N)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, .method = difference_in_means, inquiry = "ATE")
}

binary_individual <- function(N, p0, p1) {
  declare_model(N = N, Y_Z_0 = rbinom(N, 1, p0), Y_Z_1 = rbinom(N, 1, p1)) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = complete_ra(N)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, .method = difference_in_means, inquiry = "ATE")
}

# Cluster effect with variance icc * sigma^2, individual noise with the rest.
continuous_clustered <- function(clusters_per_arm, n_per_cluster, tau, sigma, icc) {
  declare_model(
    clusters = add_level(N = 2 * clusters_per_arm, u = rnorm(N, sd = sqrt(icc) * sigma)),
    individuals = add_level(N = n_per_cluster, e = rnorm(N, sd = sqrt(1 - icc) * sigma), potential_outcomes(Y ~ tau * Z + u + e))
  ) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = cluster_ra(clusters = clusters)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, clusters = clusters, .method = difference_in_means, inquiry = "ATE")
}

# Beta-distributed cluster probabilities with mean p have Bernoulli ICC = icc
# when a + b = (1 - icc) / icc.
binary_clustered <- function(clusters_per_arm, n_per_cluster, p0, p1, icc) {
  ab <- (1 - icc) / icc
  declare_model(
    clusters = add_level(N = 2 * clusters_per_arm, q0 = rbeta(N, p0 * ab, (1 - p0) * ab), q1 = rbeta(N, p1 * ab, (1 - p1) * ab)),
    individuals = add_level(N = n_per_cluster, Y_Z_0 = rbinom(N, 1, q0), Y_Z_1 = rbinom(N, 1, q1))
  ) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = cluster_ra(clusters = clusters)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
    declare_estimator(Y ~ Z, clusters = clusters, .method = difference_in_means, inquiry = "ATE")
}

# Scenarios, each at a low-power and a near-target sample size ----
sd_binary <- sqrt((0.65 * 0.35 + 0.5 * 0.5) / 2)

scenarios <- tribble(
  ~mode, ~N, ~design, ~app_power,
  "continuous, individual", 60, continuous_individual(60, 5, 10), power_two_arm(60, 5, 10, 0.05),
  "continuous, individual", 126, continuous_individual(126, 5, 10), power_two_arm(126, 5, 10, 0.05),
  "binary, individual", 150, binary_individual(150, 0.5, 0.65), power_two_arm(150, 0.15, sd_binary, 0.05),
  "binary, individual", 340, binary_individual(340, 0.5, 0.65), power_two_arm(340, 0.15, sd_binary, 0.05),
  "continuous, clustered (ICC 0.1, 20 per arm)", 200, continuous_clustered(20, 5, 5, 10, 0.1), power_two_arm(200, 5, 10, 0.05, 0.1, 20),
  "continuous, clustered (ICC 0.1, 20 per arm)", 800, continuous_clustered(20, 20, 5, 10, 0.1), power_two_arm(800, 5, 10, 0.05, 0.1, 20),
  "binary, clustered (ICC 0.05, 30 per arm)", 300, binary_clustered(30, 5, 0.5, 0.65, 0.05), power_two_arm(300, 0.15, sd_binary, 0.05, 0.05, 30),
  "binary, clustered (ICC 0.05, 30 per arm)", 1200, binary_clustered(30, 20, 0.5, 0.65, 0.05), power_two_arm(1200, 0.15, sd_binary, 0.05, 0.05, 30)
)

sims <- 4000

results <- scenarios |>
  mutate(
    diagnosis = map(design, \(d) diagnose_design(d, sims = sims, bootstrap_sims = 0)$diagnosands_df),
    simulated_power = map_dbl(diagnosis, \(x) x$power),
    mc_se = sqrt(simulated_power * (1 - simulated_power) / sims),
    difference = app_power - simulated_power
  ) |>
  select(mode, N, app_power, simulated_power, mc_se, difference)

print(results, width = Inf)
write_csv(results, "verification/power_simulation_results.csv")
