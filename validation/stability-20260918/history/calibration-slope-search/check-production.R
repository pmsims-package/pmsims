#!/usr/bin/env Rscript
# One unmodified public-API call: no mocks, supplied bounds or stage RNG reset.
# A saved result skips the search on resumption; use an empty folder to rerun.
pkgload::load_all(".", quiet = TRUE)
options(pmsims.confirm_long_runs = FALSE)
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args)) {
  args[1L]
} else {
  "validation/calibration-slope-search/results-production-final"
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
result_file <- file.path(out_dir, "production-result.rds")
result <- if (file.exists(result_file)) {
  readRDS(result_file)
} else {
  set.seed(47L)
  simulate_binary(
    signal_parameters = 15L,
    noise_parameters = 0L,
    complexity = 1L,
    outcome_prevalence = 0.25,
    maximum_achievable_cstatistic = 0.85,
    model = "ridge",
    metric = "calibration_slope",
    target_performance = 0.90,
    n_reps_total = 1000L,
    mean_or_assurance = "assurance",
    progress = FALSE
  )
}
saveRDS(result, result_file)
bounds <- unlist(result$mlpwr_ds$boundaries, use.names = FALSE)
validation_reps <- 1000L
workers <- if (.Platform$OS.type == "windows") 1L else 4L
vals <- unlist(
  parallel::mclapply(
    seq_len(validation_reps),
    function(i) {
      set.seed(3000000L + i)
      test <- result$data_function(30000L)
      train <- result$data_function(result$min_n)
      fit <- result$model_function(train)
      result$metric_function(test, fit, result$model)
    },
    mc.cores = workers,
    mc.set.seed = FALSE
  ),
  use.names = FALSE
)
stopifnot(length(vals) == validation_reps, all(is.finite(vals)))

saveRDS(vals, file.path(out_dir, "production-validation.rds"))
ordered <- sort(vals)
ci <- ordered[c(
  qbinom(0.025, length(vals), 0.2),
  qbinom(0.975, length(vals), 0.2) + 1L
)]
slope <- function(x) 1 - sqrt(pmax(0, -x))
write.csv(
  data.frame(
    scenario = "ridge_reported",
    seed = 47L,
    gp_reps = 1000L,
    min_n = result$min_n,
    perf_n = result$perf_n,
    gp_original_n = as.numeric(result$mlpwr_ds$final$design),
    gp_predicted_equivalent_slope = csse_to_calibration_slope(
      as.numeric(result$mlpwr_ds$final$power),
      "below"
    ),
    confirmation_reps = sum(vapply(
      result$mlpwr_ds$validation$track,
      function(x) length(x$raw),
      integer(1)
    )),
    actual_evaluations = sum(vapply(
      result$mlpwr_ds$data,
      function(x) length(x$y),
      integer(1)
    )),
    lower = bounds[1L],
    upper = bounds[2L],
    seconds = as.numeric(result$simulation_time, units = "secs"),
    validation_reps = length(vals),
    independent_equivalent_slope = slope(unname(quantile(vals, 0.2, type = 1))),
    independent_ci_low = slope(ci[1L]),
    independent_ci_high = slope(ci[2L]),
    target_compatible = ci[2L] >= result$csse_target_performance
  ),
  file.path(out_dir, "production-results.csv"),
  row.names = FALSE
)
print(result)
