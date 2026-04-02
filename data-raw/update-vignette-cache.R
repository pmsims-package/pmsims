if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install pkgload first, e.g. with install.packages('pkgload').")
}

pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)

cache_dir <- file.path("vignettes", "cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(123)
binary_example <- simulate_binary(
  signal_parameters = 20,
  noise_parameters = 0,
  predictor_type = "continuous",
  binary_predictor_prevalence = NULL,
  outcome_prevalence = 0.30,
  maximum_achievable_cstatistic = 0.80,
  model = "glm",
  metric = "calibration_slope",
  target_performance = 0.85,
  n_reps_total = 1000,
  mean_or_assurance = "assurance"
)
saveRDS(binary_example, file = file.path(cache_dir, "binary.rds"))

set.seed(123)
continuous_example <- simulate_continuous(
  signal_parameters = 15,
  noise_parameters = 0,
  predictor_type = "continuous",
  maximum_achievable_rsquared = 0.50,
  model = "lm",
  metric = "calibration_slope",
  target_performance = 0.90,
  n_reps_total = 1000,
  mean_or_assurance = "assurance"
)
saveRDS(continuous_example, file = file.path(cache_dir, "continuous.rds"))
