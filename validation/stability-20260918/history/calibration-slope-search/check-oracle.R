#!/usr/bin/env Rscript
# Test-set noise at perfect population calibration for the lasso reproduction.
# No model is trained: the true linear predictor is used (complexity 1 only).
pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) {
  args[1L]
} else {
  "validation/calibration-slope-search/results/oracle-results.csv"
}
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
set.seed(47L)
dc <- resolve_data_control(NULL, 1L)
tuned <- call_tuner(
  binary_tuning,
  list(
    target_prevalence = 0.05,
    target_performance = 0.75,
    candidate_features = 5L,
    proportion_noise_features = 0,
    .complexity = 1L
  ),
  dc
)
generator <- default_data_generators(list(
  type = "binary",
  args = make_data_args(
    5L,
    0L,
    1L,
    dc,
    extra = list(
      mu_lp = get_param(tuned, "mu_lp"),
      beta_signal = get_param(tuned, "beta_signal"),
      baseline_prob = 0.05
    )
  )
))
slope_at_test <- function(test_n) {
  data <- generator(test_n)
  lp <- get_param(tuned, "mu_lp") +
    get_param(tuned, "beta_signal") * rowSums(data[, -1, drop = FALSE])
  unname(coef(glm(data$y ~ lp, family = binomial()))[2L])
}
# This is exactly the default baseline stage-1 test draw after tuning.
fixed_slope <- slope_at_test(30000L)
rows <- list(data.frame(
  test_n = 30000L,
  replicates = 1L,
  test_draw = "baseline_fixed_seed47",
  equivalent_slope = 1 - abs(1 - fixed_slope),
  ci_low = NA_real_,
  ci_high = NA_real_,
  raw_slope = fixed_slope
))
for (test_n in c(30000L, 120000L)) {
  set.seed(2000000L + test_n)
  slopes <- replicate(1000L, slope_at_test(test_n))
  vals <- sort(-(1 - slopes)^2)
  q <- unname(quantile(vals, 0.2, type = 1))
  ci <- vals[c(
    qbinom(0.025, length(vals), 0.2),
    qbinom(0.975, length(vals), 0.2) + 1L
  )]
  rows[[length(rows) + 1L]] <- data.frame(
    test_n = test_n,
    replicates = 1000L,
    test_draw = "independent_oracle",
    equivalent_slope = 1 - sqrt(-q),
    ci_low = 1 - sqrt(-ci[1]),
    ci_high = 1 - sqrt(-ci[2]),
    raw_slope = NA_real_
  )
  write.csv(do.call(rbind, rows), out, row.names = FALSE)
  cat(test_n, "oracle equivalent slope", 1 - sqrt(-q), "\n")
}
