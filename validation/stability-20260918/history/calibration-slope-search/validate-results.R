#!/usr/bin/env Rscript
# Re-evaluate saved searches without repeating the GP fit.
# Usage: validate-results.R SEARCH_FOLDER OUTPUT_FOLDER
pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
dir.create(args[2L], recursive = TRUE, showWarnings = FALSE)
paths <- Sys.glob(file.path(args[1L], "*-gp1000-fixed-result.rds"))
stopifnot(length(paths) > 0L)
rows <- list()
for (path in paths) {
  name <- sub("-seed.*", "", basename(path))
  seed <- as.integer(sub(".*-seed([0-9]+)-.*", "\\1", basename(path)))
  result <- readRDS(path)
  for (factor_n in c(0.5, 1, 2)) {
    n <- max(1L, round(result$min_n * factor_n))
    vals_path <- file.path(
      args[2L],
      sprintf("%s-seed%d-n%d-validation.rds", name, seed, n)
    )
    vals <- if (file.exists(vals_path)) {
      readRDS(vals_path)
    } else {
      unlist(
        parallel::mclapply(
          seq_len(1000L),
          function(i) {
            set.seed(4000000L + seed * 10000L + as.integer(factor_n * 1000) + i)
            test <- result$data_function(30000L)
            train <- result$data_function(n)
            fit <- result$model_function(train)
            result$metric_function(test, fit, result$model)
          },
          mc.cores = if (.Platform$OS.type == "windows") 1L else 4L,
          mc.set.seed = FALSE
        ),
        use.names = FALSE
      )
    }
    stopifnot(length(vals) == 1000L, all(is.finite(vals)))
    ordered <- sort(vals)
    q <- unname(quantile(vals, 0.2, type = 1))
    ci <- ordered[c(qbinom(0.025, 1000L, 0.2), qbinom(0.975, 1000L, 0.2) + 1L)]
    is_csse <- identical(attr(result$metric_function, "metric"), "csse")
    target <- if (is_csse) {
      result$csse_target_performance
    } else {
      result$target_performance
    }
    slope <- function(v) if (is_csse) 1 - sqrt(pmax(0, -v)) else v
    rows[[length(rows) + 1L]] <- data.frame(
      scenario = name,
      seed = seed,
      gp_reps = 1000L,
      validation_reps = length(vals),
      factor_n = factor_n,
      n = n,
      internal_target = target,
      internal_q20 = q,
      internal_ci_low = ci[1L],
      internal_ci_high = ci[2L],
      equivalent_slope = slope(q),
      slope_ci_low = slope(ci[1L]),
      slope_ci_high = slope(ci[2L]),
      target_compatible = ci[2L] >= target,
      confidently_below_target = ci[2L] < target
    )
    saveRDS(
      vals,
      file.path(
        args[2L],
        sprintf("%s-seed%d-n%d-validation.rds", name, seed, n)
      )
    )
    write.csv(
      do.call(rbind, rows),
      file.path(args[2L], "validation-results.csv"),
      row.names = FALSE
    )
    cat("Validated", name, seed, "n", n, "equivalent slope", slope(q), "\n")
  }
}
