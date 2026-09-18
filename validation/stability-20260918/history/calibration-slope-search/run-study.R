#!/usr/bin/env Rscript
# Run from the repository root. Outputs are reproducible given the baseline
# commit, seeds, package versions and study settings recorded below.
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args)) {
  args[1L]
} else {
  "validation/calibration-slope-search/results"
}
gp_arg <- args[startsWith(args, "--gp-reps=")]
gp_reps <- if (length(gp_arg)) {
  as.integer(sub("--gp-reps=", "", gp_arg[1L]))
} else {
  1000L
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(".", quiet = TRUE)
options(pmsims.confirm_long_runs = FALSE)
baseline_commit <- "a88e701"
baseline_env <- new.env(parent = asNamespace("pmsims"))
source(
  textConnection(system2(
    "git",
    c("show", paste0(baseline_commit, ":R/start_values.R")),
    stdout = TRUE
  )),
  local = baseline_env
)
writeLines(
  c(
    paste("Baseline:", baseline_commit),
    paste("Study UTC:", format(Sys.time(), tz = "UTC")),
    paste(
      "Search:",
      gp_reps,
      "stage-2 replicates, 20 per batch; default tuning and test_n=30000."
    ),
    "GP RNG seed: 10000 + scenario seed. Validation RNG: 1000000 + seed + 1000 * factor_n.",
    "Validation: 1000 independent training/test draws per sample size; search RNG reset separately.",
    capture.output(sessionInfo())
  ),
  file.path(out_dir, "session-info.txt")
)

scenarios <- list(
  lasso_rare = list(
    fun = simulate_binary,
    args = list(
      signal_parameters = 5L,
      outcome_prevalence = 0.05,
      maximum_achievable_cstatistic = 0.75,
      model = "lasso",
      target_performance = 0.95
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
  lasso_control = list(
    fun = simulate_binary,
    args = list(
      signal_parameters = 5L,
      outcome_prevalence = 0.2,
      maximum_achievable_cstatistic = 0.75,
      model = "lasso",
      target_performance = 0.90
    )
  ),
  continuous_control = list(
    fun = simulate_continuous,
    args = list(
      signal_parameters = 5L,
      maximum_achievable_rsquared = 0.5,
      model = "ridge",
      target_performance = 0.90
    )
  )
)
append_summary <- function(row, filename, keys) {
  old <- if (file.exists(filename)) {
    read.csv(filename, stringsAsFactors = FALSE)
  } else {
    row[FALSE, ]
  }
  columns <- union(names(old), names(row))
  for (name in setdiff(columns, names(old))) {
    old[[name]] <- rep(NA, nrow(old))
  }
  for (name in setdiff(columns, names(row))) {
    row[[name]] <- NA
  }
  combined <- rbind(old[columns], row[columns])
  combined <- combined[
    !duplicated(combined[keys], fromLast = TRUE),
    ,
    drop = FALSE
  ]
  write.csv(combined, filename, row.names = FALSE)
}
# Baseline stage 1 only: running its GP inside a known bad range is unnecessary
# and prohibitively costly. The fixed path runs stage 2 and independent checks.
run_one <- function(name, seed, baseline = FALSE) {
  scenario <- scenarios[[name]]
  captured <- NULL
  original_engine <- get("calculate_mlpwr", asNamespace("pmsims"))
  engine <- function(
    data_function,
    model_function,
    metric_function,
    target_performance,
    c_statistic,
    mean_or_assurance,
    test_n,
    n_reps_per,
    min_sample_size,
    max_sample_size,
    ...
  ) {
    start <- compute_start_sample_sizes(
      data_function,
      metric_function,
      target_performance,
      c_statistic,
      mean_or_assurance
    )
    adaptive <- if (baseline) {
      baseline_env$calculate_adaptive_bounds
    } else {
      calculate_adaptive_bounds
    }
    stage_file <- file.path(
      out_dir,
      sprintf(
        "%s-seed%d-%s-stage1.rds",
        name,
        seed,
        if (baseline) "baseline" else "fixed"
      )
    )
    stage1_reused <- file.exists(stage_file)
    bounds <- if (stage1_reused) {
      message("Reusing saved stage-1 trace: ", stage_file)
      readRDS(stage_file)$bounds
    } else {
      adaptive(
        data_function,
        model_function,
        metric_function,
        resolve_value_on_error(metric_function),
        start$start_min_sample_size,
        test_n,
        n_reps_per,
        500,
        target_performance,
        threshold = 0.0001,
        mean_or_assurance = mean_or_assurance,
        verbose = TRUE
      )
    }

    captured <<- list(
      bounds = bounds,
      data_function = data_function,
      model_function = model_function,
      metric_function = metric_function,
      internal_target = target_performance,
      stage1_reused = stage1_reused
    )
    saveRDS(
      captured,
      file.path(
        out_dir,
        sprintf(
          "%s-seed%d-%s-stage1.rds",
          name,
          seed,
          if (baseline) "baseline" else "fixed"
        )
      )
    )
    if (baseline) {
      stop("baseline_stage1_complete", call. = FALSE)
    }
    require_adaptive_bracket(bounds)
    if (name == "lasso_rare") {
      stop("fixed_stage1_complete", call. = FALSE)
    }
    # Separate the GP seed so resuming a saved preliminary trace is identical.
    set.seed(10000L + seed)
    original_engine(
      data_function = data_function,
      model_function = model_function,
      metric_function = metric_function,
      target_performance = target_performance,
      c_statistic = c_statistic,
      mean_or_assurance = mean_or_assurance,
      test_n = test_n,
      n_reps_per = n_reps_per,
      min_sample_size = bounds$min_sample_size,
      max_sample_size = bounds$max_sample_size,
      ...
    )
  }
  testthat::local_mocked_bindings(calculate_mlpwr = engine, .package = "pmsims")
  set.seed(seed)
  time_start <- Sys.time()
  result <- tryCatch(
    do.call(
      scenario$fun,
      c(
        scenario$args,
        list(
          noise_parameters = 0L,
          complexity = 1L,
          metric = "calibration_slope",
          n_reps_total = gp_reps,
          mean_or_assurance = "assurance",
          progress = FALSE
        )
      )
    ),
    error = identity
  )
  bounds <- captured$bounds
  ok <- !inherits(result, "error")
  row <- data.frame(
    scenario = name,
    seed = seed,
    version = if (baseline) "baseline" else "fixed",
    gp_reps = if (baseline || name == "lasso_rare") NA_integer_ else gp_reps,
    stage1_reused = if (is.null(captured)) NA else captured$stage1_reused,
    status = if (ok) "estimated" else conditionMessage(result),
    stop_reason = if (is.null(bounds)) NA_character_ else bounds$stop_reason,
    lower = if (is.null(bounds)) NA_real_ else bounds$min_sample_size,
    upper = if (is.null(bounds)) NA_real_ else bounds$max_sample_size,
    min_n = if (ok) result$min_n else NA_real_,
    perf_n = if (ok) result$perf_n else NA_real_,
    seconds = as.numeric(difftime(Sys.time(), time_start, units = "secs"))
  )
  append_summary(
    row,
    file.path(out_dir, "search-results.csv"),
    c("scenario", "seed", "version", "gp_reps")
  )

  if (!ok) {
    return(invisible(NULL))
  }
  saveRDS(
    result,
    file.path(
      out_dir,
      sprintf("%s-seed%d-gp%d-fixed-result.rds", name, seed, gp_reps)
    )
  )
  # Evaluate the metric actually optimised: for shrinkage, assurance is the
  # 20th percentile of -(1-slope)^2, NOT a one-sided raw-slope percentile.
  for (factor_n in c(0.5, 1, 2)) {
    n <- max(1L, round(result$min_n * factor_n))
    set.seed(1000000L + seed + as.integer(factor_n * 1000))
    vals <- vapply(
      seq_len(1000L),
      function(i) {
        train <- captured$data_function(n)
        test <- captured$data_function(30000L)
        fit <- captured$model_function(train)
        captured$metric_function(
          test,
          fit,
          attr(captured$model_function, "model")
        )
      },
      numeric(1)
    )
    ordered <- sort(vals)
    q <- unname(quantile(vals, 0.2, type = 1))
    ci <- ordered[c(
      qbinom(0.025, length(vals), 0.2),
      qbinom(0.975, length(vals), 0.2) + 1L
    )]
    is_csse <- identical(attr(captured$metric_function, "metric"), "csse")
    slope <- function(v) if (is_csse) 1 - sqrt(pmax(0, -v)) else v
    validation_row <- data.frame(
      scenario = name,
      seed = seed,
      gp_reps = gp_reps,
      validation_reps = length(vals),
      factor_n = factor_n,
      n = n,
      internal_target = captured$internal_target,
      internal_q20 = q,
      internal_ci_low = ci[1],
      internal_ci_high = ci[2],
      equivalent_slope = slope(q),
      slope_ci_low = slope(ci[1]),
      slope_ci_high = slope(ci[2]),
      target_compatible = ci[2] >= captured$internal_target,
      confidently_below_target = ci[2] < captured$internal_target
    )
    append_summary(
      validation_row,
      file.path(out_dir, "validation-results.csv"),
      c("scenario", "seed", "gp_reps", "factor_n")
    )

    saveRDS(
      vals,
      file.path(
        out_dir,
        sprintf("%s-seed%d-gp%d-n%d-validation.rds", name, seed, gp_reps, n)
      )
    )
    cat("Validated", name, seed, "n", n, "equivalent slope", slope(q), "\n")
  }
}
if (
  !("--fixed-only" %in% args) &&
    !file.exists(file.path(out_dir, "lasso_rare-seed47-baseline-stage1.rds"))
) {
  run_one("lasso_rare", 47L, baseline = TRUE)
}
selected_scenarios <- names(scenarios)
scenario_arg <- args[startsWith(args, "--scenarios=")]
if (length(scenario_arg)) {
  selected_scenarios <- strsplit(
    sub("--scenarios=", "", scenario_arg[1L]),
    ","
  )[[1L]]
}
selected_seeds <- c(47L, 48L)
seed_arg <- args[startsWith(args, "--seeds=")]
if (length(seed_arg)) {
  selected_seeds <- as.integer(strsplit(
    sub("--seeds=", "", seed_arg[1L]),
    ","
  )[[1L]])
}
if (!("--baseline-only" %in% args)) {
  for (name in selected_scenarios) {
    for (seed in intersect(
      selected_seeds,
      if (name == "lasso_rare") 47L else selected_seeds
    )) {
      cat("Running", name, "seed", seed, "\n")
      run_one(name, seed)
    }
  }
}
