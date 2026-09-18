#!/usr/bin/env Rscript
# Apply final confirmation to saved GP fits without redoing unchanged GP work.
# Usage: confirm-saved.R SEARCH_FOLDER OUTPUT_FOLDER
pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
dir.create(args[2L], recursive = TRUE, showWarnings = FALSE)
search <- read.csv(file.path(args[1L], "search-results.csv"))
for (path in Sys.glob(file.path(args[1L], "*-gp1000-fixed-result.rds"))) {
  name <- sub("-seed.*", "", basename(path))
  seed <- as.integer(sub(".*-seed([0-9]+)-.*", "\\1", basename(path)))
  result <- readRDS(path)
  is_csse <- identical(attr(result$metric_function, "metric"), "csse")
  target <- if (is_csse) {
    result$csse_target_performance
  } else {
    result$target_performance
  }
  idx <- which(
    search$scenario == name & search$seed == seed & search$gp_reps == 1000L
  )
  stopifnot(length(idx) == 1L)
  set.seed(6000000L + seed)
  start <- Sys.time()
  checked <- confirm_gp_design(
    as.numeric(result$mlpwr_ds$final$design),
    function(n) {
      test <- result$data_function(30000L)
      train <- result$data_function(n)
      fit <- result$model_function(train)
      result$metric_function(test, fit, result$model)
    },
    target,
    "assurance",
    search$lower[idx],
    search$upper[idx],
    1000L,
    20L
  )
  if (checked$min_n != as.numeric(result$mlpwr_ds$final$design)) {
    # A cached auxiliary metric was evaluated at the original GP N.
    result$metric_2_at_n <- NULL
  }
  result$min_n <- checked$min_n
  result$perf_n <- checked$perf_n
  if (is_csse) {
    result$csse_perf_n <- checked$perf_n
    result$perf_n <- csse_to_calibration_slope(
      checked$perf_n,
      result$csse_direction
    )
  }
  result$mlpwr_ds$validation <- checked$validation
  extra <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  result$simulation_time <- result$simulation_time + extra
  search$min_n[idx] <- result$min_n
  search$perf_n[idx] <- result$perf_n
  search$gp_original_n[idx] <- checked$validation$original_n
  search$confirmation_reps[idx] <- sum(vapply(
    checked$validation$track,
    function(x) length(x$raw),
    integer(1)
  ))
  search$confirmation_seconds[idx] <- extra
  search$seconds[idx] <- search$seconds[idx] + extra
  saveRDS(result, file.path(args[2L], basename(path)))
  write.csv(
    search,
    file.path(args[2L], "search-results.csv"),
    row.names = FALSE
  )
  cat(
    "Confirmed",
    name,
    seed,
    "N",
    result$min_n,
    "original",
    checked$validation$original_n,
    "\n"
  )
}
