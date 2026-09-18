#!/usr/bin/env Rscript
# Run from the repository root: Rscript validation/adaptive-stage1/run-study.R glm_stress 298
# Each invocation writes a separate directory; independent cases can run concurrently.
args <- commandArgs(trailingOnly = TRUE)
name <- if (length(args)) args[1L] else "glm_stress"
seed <- if (length(args) >= 2L) as.integer(args[2L]) else 298L
out <- file.path(
  "validation/adaptive-stage1/results",
  paste0(name, "-seed", seed)
)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(".", quiet = TRUE)
options(pmsims.confirm_long_runs = FALSE)
scenarios <- list(
  glm_stress = list(
    fun = simulate_binary,
    args = list(
      signal_parameters = 5L,
      outcome_prevalence = 0.20,
      maximum_achievable_cstatistic = 0.60,
      model = "glm",
      target_performance = 0.95
    )
  ),
  glm_control = list(
    fun = simulate_binary,
    args = list(
      signal_parameters = 5L,
      outcome_prevalence = 0.20,
      maximum_achievable_cstatistic = 0.75,
      model = "glm",
      target_performance = 0.90
    )
  ),
  ridge_reported = list(
    fun = simulate_binary,
    args = list(
      signal_parameters = 15L,
      outcome_prevalence = 0.25,
      maximum_achievable_cstatistic = 0.85,
      model = "ridge",
      target_performance = 0.90
    )
  ),
  continuous_control = list(
    fun = simulate_continuous,
    args = list(
      signal_parameters = 5L,
      maximum_achievable_rsquared = 0.50,
      model = "ridge",
      target_performance = 0.90
    )
  )
)
stopifnot(name %in% names(scenarios), !is.na(seed))
writeLines(
  c(
    paste("Scenario:", name, "seed:", seed),
    "Search: unmodified public API, 1000 GP reps; default stage-1 budget 500, batches of 20, test_n=30000.",
    "Validation: 1000 independent draws at each of lower bound, returned N, upper bound.",
    "Validation RNG seeds: 1000000 + search seed + 1000 * evaluation index.",
    paste("UTC:", format(Sys.time(), tz = "UTC")),
    sub("[[:blank:]]+$", "", capture.output(sessionInfo()))
  ),
  file.path(out, "session-info.txt")
)

run <- function() {
  captured <- NULL
  bounds <- NULL
  engine <- get("calculate_mlpwr", asNamespace("pmsims"))
  adaptive <- get("calculate_adaptive_bounds", asNamespace("pmsims"))
  # These observers delegate exactly once and never draw RNG or replace bounds.
  testthat::local_mocked_bindings(
    calculate_mlpwr = function(...) {
      captured <<- list(...)
      engine(...)
    },
    calculate_adaptive_bounds = function(...) {
      bounds <<- adaptive(...)
      saveRDS(bounds, file.path(out, "stage1.rds"))
      engine_args <- captured[c(
        "data_function",
        "model_function",
        "metric_function",
        "target_performance",
        "value_on_error"
      )]
      saveRDS(engine_args, file.path(out, "functions.rds"))
      bounds
    },
    .package = "pmsims"
  )
  set.seed(seed)
  start <- Sys.time()
  scenario <- scenarios[[name]]
  result <- tryCatch(
    do.call(
      scenario$fun,
      c(
        scenario$args,
        list(
          noise_parameters = 0L,
          complexity = 1L,
          metric = "calibration_slope",
          n_reps_total = 1000L,
          mean_or_assurance = "assurance",
          progress = FALSE
        )
      )
    ),
    error = identity
  )
  ok <- !inherits(result, "error")
  row <- data.frame(
    scenario = name,
    seed = seed,
    status = if (ok) "estimated" else conditionMessage(result),
    stage1_stop = if (is.null(bounds)) NA_character_ else bounds$stop_reason,
    stage1_reps = if (is.null(bounds)) {
      NA_integer_
    } else {
      sum(vapply(bounds$track, function(x) length(x$raw), integer(1)))
    },
    lower = if (is.null(bounds)) NA_real_ else bounds$min_sample_size,
    upper = if (is.null(bounds)) NA_real_ else bounds$max_sample_size,
    n = if (ok) result$min_n else NA_real_,
    predicted_slope = if (ok) result$perf_n else NA_real_,
    actual_gp_reps = if (ok) {
      sum(vapply(result$data, function(x) length(x$y), integer(1)))
    } else {
      0L
    },
    seconds = as.numeric(difftime(Sys.time(), start, units = "secs"))
  )
  write.csv(row, file.path(out, "search.csv"), row.names = FALSE)
  if (!is.null(bounds)) {
    trace <- do.call(
      rbind,
      lapply(bounds$track, function(x) {
        data.frame(
          n = x$n,
          reps = length(x$raw),
          estimate = x$performance,
          ci_low = x$ll,
          ci_high = x$ul
        )
      })
    )
    write.csv(trace, file.path(out, "stage1.csv"), row.names = FALSE)
  }
  print(row)
  if (!ok) {
    return(invisible(NULL))
  }
  saveRDS(result, file.path(out, "search.rds"))

  # Keep the internal metric (including CSSE) and the original Type-7 quantile.
  # No confirmation or correction is applied to the GP answer.
  ns <- unique(c(bounds$min_sample_size, result$min_n, bounds$max_sample_size))
  rows <- vector("list", length(ns))
  for (j in seq_along(ns)) {
    n <- ns[j]
    set.seed(1000000L + seed + 1000L * j)
    vals <- vapply(
      seq_len(1000L),
      function(i) {
        tryCatch(
          {
            test <- captured$data_function(30000L)
            train <- captured$data_function(n)
            fit <- captured$model_function(train)
            captured$metric_function(
              test,
              fit,
              attr(captured$model_function, "model")
            )
          },
          error = function(e) captured$value_on_error
        )
      },
      numeric(1)
    )
    stopifnot(all(is.finite(vals)))
    ordered <- sort(vals)
    q <- unname(quantile(vals, 0.2, type = 7))
    ci <- ordered[c(qbinom(0.025, 1000L, 0.2), qbinom(0.975, 1000L, 0.2) + 1L)]
    csse <- identical(attr(captured$metric_function, "metric"), "csse")
    display <- function(v) if (csse) 1 - sqrt(pmax(0, -v)) else v
    target <- captured$target_performance
    rows[[j]] <- data.frame(
      scenario = name,
      seed = seed,
      n = n,
      reps = length(vals),
      role = if (n == result$min_n) {
        "GP answer"
      } else if (n == bounds$min_sample_size) {
        "lower"
      } else {
        "upper"
      },
      internal_target = target,
      internal_q20 = q,
      internal_ci_low = ci[1L],
      internal_ci_high = ci[2L],
      equivalent_slope = display(q),
      slope_ci_low = display(ci[1L]),
      slope_ci_high = display(ci[2L]),
      evidence = if (ci[2L] < target) {
        "below"
      } else if (ci[1L] > target) {
        "above"
      } else {
        "overlaps target"
      }
    )
    saveRDS(vals, file.path(out, paste0("validation-n", n, ".rds")))
    write.csv(
      do.call(rbind, rows[seq_len(j)]),
      file.path(out, "validation.csv"),
      row.names = FALSE
    )
    print(rows[[j]])
  }
}
run()
