# Generates the precomputed `pmsims` results shipped as package data.
#
# Running a sample-size search takes minutes to hours, which is far beyond what
# examples, tests or vignettes can do at check time. The four objects created
# here are realistic runs, computed once, so that the documentation and the
# vignette can show real output without recomputing it. See the CRAN cookbook
# on overall check time:
# https://contributor.r-project.org/cran-cookbook/docs_issues.html#overall-checktime
#
# Keep these calls in sync with the @source entries in R/data.R and the
# precomputed examples in vignettes/pmsims.Rmd. The function help pages use
# smaller models with executable searches; see their examples separately.
#
# Re-run with:  Rscript data-raw/precomputed-examples.R

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install pkgload first, e.g. with install.packages('pkgload').")
}
if (!requireNamespace("usethis", quietly = TRUE)) {
  stop("Install usethis first, e.g. with install.packages('usethis').")
}

pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)

# --- Binary outcome ---------------------------------------------------------

set.seed(123)
binary_example <- simulate_binary(
  signal_parameters = 20,
  noise_parameters = 0,
  complexity = 1,
  data_control = list(correlation = 0.3),
  outcome_prevalence = 0.30,
  maximum_achievable_cstatistic = 0.80,
  model = "glm",
  metric = "calibration_slope",
  target_performance = 0.85,
  n_reps_total = 1000,
  mean_or_assurance = "assurance"
)

# --- Continuous outcome -----------------------------------------------------

set.seed(123)
continuous_example <- simulate_continuous(
  signal_parameters = 15,
  noise_parameters = 0,
  complexity = 1,
  data_control = list(correlation = 0.3),
  maximum_achievable_rsquared = 0.50,
  model = "lm",
  metric = "calibration_slope",
  target_performance = 0.95,
  n_reps_total = 1000,
  mean_or_assurance = "assurance"
)

# --- Time-to-event outcome --------------------------------------------------

set.seed(123)
survival_example <- simulate_survival(
  signal_parameters = 15,
  noise_parameters = 0,
  complexity = 1,
  data_control = list(correlation = 0.3),
  maximum_achievable_cindex = 0.70,
  baseline_hazard = 0.01,
  censoring_rate = 0.30,
  model = "coxph",
  metric = "calibration_slope",
  target_performance = 0.90,
  n_reps_total = 1000,
  mean_or_assurance = "assurance"
)

# --- User-supplied simulation -----------------------------------------------

# Defined at the top level so that the closures stored on the result carry the
# global environment (serialised by reference) rather than a captured local one.
custom_data_function <- function(n) {
  x1 <- stats::rnorm(n)
  x2 <- stats::rnorm(n)
  x3 <- stats::rnorm(n)
  y <- (x1 + x2 + x3) / sqrt(3) + stats::rnorm(n)
  data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
}
custom_model_function <- function(dat) {
  stats::lm(y ~ ., data = dat)
}
custom_metric_function <- function(test_data, fit, model) {
  preds <- stats::predict(fit, newdata = test_data)
  unname(stats::coef(stats::lm(test_data$y ~ preds))[2])
}
attr(custom_metric_function, "metric") <- "calibration_slope"
set.seed(123)
custom_example <- simulate_custom(
  data_function = custom_data_function,
  model_function = custom_model_function,
  metric_function = custom_metric_function,
  target_performance = 0.9,
  mean_or_assurance = "assurance",
  min_sample_size = 25,
  max_sample_size = 1000,
  n_reps_total = 1000,
  test_n = 30000,
  progress = FALSE
)

# --- Save -------------------------------------------------------------------

usethis::use_data(
  binary_example,
  continuous_example,
  survival_example,
  custom_example,
  overwrite = TRUE,
  compress = "xz"
)
