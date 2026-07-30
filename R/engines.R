#' mlpwr engine
#' @inheritParams simulate_custom
#' @param n_init Integer number of initial sample sizes simulated before the Gaussian process search begins.
#' @param progress Logical flag controlling whether the `mlpwr` progress bar is shown.
#' @param verbose Logical flag passed to `mlpwr`; when `TRUE` verbose output is printed.
#' @param value_on_error Numeric fallback value used if model fitting or metric calculation fails.
#' @param ... Additional options passed to [mlpwr::find.design()].
#' @keywords internal
calculate_mlpwr <- function(
  test_n,
  n_reps_total,
  n_reps_per,
  se_final,
  min_sample_size,
  max_sample_size,
  target_performance,
  c_statistic,
  mean_or_assurance,
  n_init,
  progress = TRUE,
  verbose,
  data_function,
  model_function,
  metric_function,
  value_on_error,
  ...
) {
  # Determine initial start values
  start_values <- tryCatch(
    {
      compute_start_sample_sizes(
        data_function = data_function,
        metric_function = metric_function,
        target_performance = target_performance,
        c_statistic = c_statistic,
        mean_or_assurance = mean_or_assurance
      )
    },
    error = function(e) {
      stop(
        paste("Error when computing start values:", e$message),
        call. = FALSE
      )
    }
  )

  # Adaptive starting values search
  cat("Estimating first stage... (Adaptive starting value search algorithm)\n")
  start_values <- tryCatch(
    {
      calculate_adaptive_bounds(
        data_function = data_function,
        model_function = model_function,
        metric_function = metric_function,
        value_on_error = value_on_error,
        start_n = start_values$start_min_sample_size,
        test_n = test_n,
        n_reps_per = n_reps_per,
        n_reps_total = 500,
        target_performance = target_performance,
        threshold = 0.0001,
        mean_or_assurance = mean_or_assurance,
        verbose = FALSE
      )
    },
    error = function(e) {
      stop(
        paste("Error during adaptive start value search:", e$message),
        call. = FALSE
      )
    }
  )

  start_min_sample_size <- start_values$min_sample_size
  start_max_sample_size <- start_values$max_sample_size

  cat(
    "Starting values determined: min sample size =",
    start_min_sample_size,
    "max sample size =",
    start_max_sample_size,
    "\n"
  )

  # When supplied, use the requested final standard error to control stopping.
  # A deliberately high simulation budget ensures the CI criterion dominates.
  if (!(is.null(se_final))) {
    ci <- se_final * stats::qnorm(0.975) * 2
    n_reps_total <- 10000
  } else {
    ci <- NULL
  }

  # Override adaptive min when provided
  if (!is.null(min_sample_size) && !is.null(max_sample_size)) {
    start_min_sample_size <- min_sample_size
    start_max_sample_size <- max_sample_size
  }

  # Perform search using mlpwr

  cat("Estimating second stage... (Gaussian process algorithm)\n")
  # Progress bar
  orig_print_progress <- NULL
  pb_id <- NULL
  pb_txt <- NULL
  use_cli <- FALSE

  # Try using cli
  if (isTRUE(progress) && requireNamespace("cli", quietly = TRUE)) {
    use_cli <- TRUE

    pb_id <- cli::cli_progress_bar(
      "Estimating second stage (Gaussian process)",
      total = n_reps_total,
      # shows: spinner, bar, "123/1000 sims", ETA
      format = "{cli::pb_spin} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} sims ({cli::pb_eta})"
    )

    # safe patched_print_progress for cli backend
    patched_print_progress <- function(n_updates, evaluations_used, time_used) {
      # Ensure numeric scalar
      # If evaluations_used is NULL/NA/non-numeric/length>1 -> coerce to 0
      safe_eval <- tryCatch(
        {
          if (is.null(evaluations_used)) {
            0L
          } else if (!is.numeric(evaluations_used)) {
            as.numeric(evaluations_used)
          } else {
            evaluations_used
          }
        },
        error = function(e) NA_real_
      )

      # If still NA or NaN, set to 0
      if (!is.finite(safe_eval) || length(safe_eval) != 1) {
        safe_eval <- 0
      }

      # Round / coerce to integer and clamp to [0, total]
      safe_eval <- as.integer(round(safe_eval, 0))
      total_val <- n_reps_total
      if (is.na(total_val) || !is.finite(total_val) || total_val <= 0) {
        # fallback to the n_reps_total captured in closure if available, otherwise don't call
        total_val <- n_reps_total
      }
      safe_eval <- min(max(safe_eval, 0L), as.integer(total_val))

      # Now call cli safely
      tryCatch(
        {
          cli::cli_progress_update(id = pb_id, set = safe_eval)
        },
        error = function(e) {
          # As fallback, attempt a less fancy update (no crash)
          # (we silently swallow errors here because this is only cosmetic)
          invisible(NULL)
        }
      )
    }
  } else if (isTRUE(progress)) {
    # Fallback to txtProgressBar
    pb_txt <- utils::txtProgressBar(
      min = 0,
      max = n_reps_total,
      style = 3
    )

    patched_print_progress <- function(n_updates, evaluations_used, time_used) {
      utils::setTxtProgressBar(pb_txt, evaluations_used)
    }
  }

  # Patch mlpwr::print_progress()
  if (isTRUE(progress)) {
    ns <- asNamespace("mlpwr")
    orig_print_progress <- get("print_progress", envir = ns)
    utils::assignInNamespace("print_progress", patched_print_progress, ns)
  }

  # Ensure cleanup
  on.exit(
    {
      if (!is.null(orig_print_progress)) {
        utils::assignInNamespace("print_progress", orig_print_progress, "mlpwr")
      }
      if (!is.null(pb_txt)) {
        close(pb_txt)
      }
      if (!is.null(pb_id) && use_cli) cli::cli_progress_done(id = pb_id)
    },
    add = TRUE
  )

  # Functions required for mlpwr
  # Calculate metrics for sample size n
  mlpwr_simulation_function <- function(n) {
    tryCatch(
      {
        test_data <- data_function(test_n)
        train_data <- data_function(n)
        fit <- model_function(train_data)
        model <- attr(model_function, "model")
        metric_function(test_data, fit, model)
      },
      error = function(e) {
        return(value_on_error)
      }
    )
  }

  if (mean_or_assurance == "mean") {
    aggregate_fun <- function(x) mean(x, na.rm = TRUE)
  } else if (mean_or_assurance == "assurance") {
    aggregate_fun <- function(x) stats::quantile(x, probs = .2, na.rm = TRUE)
  } else {
    stop("mean_or_assurance must be either 'mean' or 'assurance'")
  }

  # Use a bootstrap to estimate the variance of the estimated quantile
  var_bootstrap <- function(x) {
    stats::var(replicate(
      20,
      aggregate_fun(sample(x, length(x), replace = TRUE))
    ))
  }

  # Calculate bootstrapped quantile variance
  noise_fun <- function(x) var_bootstrap(x$y)

  ds <- tryCatch(
    {
      do.call(
        mlpwr::find.design,
        utils::modifyList(
          list(
            simfun = mlpwr_simulation_function,
            aggregate_fun = aggregate_fun,
            noise_fun = noise_fun,
            boundaries = c(start_min_sample_size, start_max_sample_size),
            power = target_performance,
            surrogate = "gpr",
            setsize = n_reps_per,
            evaluations = n_reps_total,
            ci = ci,
            n.startsets = n_init,
            silent = !isTRUE(progress)
          ),
          list(...)
        )
      )
    },
    error = function(e) {
      stop(
        paste("mlpwr::find.design failed with error:", e$message),
        call. = FALSE
      )
    }
  )

  # Process results from mlpwr
  perfs <- ds$dat
  perfs <- perfs[order(sapply(perfs, "[[", "x"))]
  max_len <- max(sapply(perfs, \(x) length(x$y)))
  results <- matrix(nrow = length(perfs), ncol = max_len)
  rownames(results) <- sapply(perfs, \(x) x$x)
  for (i in seq_along(perfs)) {
    results[i, seq(1, length(perfs[[i]]$y), 1)] <- perfs[[i]]$y
  }

  mlpwr_summaries <- get_summaries(results)

  return(list(
    results = perfs,
    summaries = mlpwr_summaries,
    min_n = as.numeric(ds$final$design),
    perf_n = as.numeric(ds$final$power),
    mlpwr_ds = list(
      data = ds$dat,
      fit = ds$fit,
      boundaries = ds$boundaries,
      final = ds$final,
      aggregate_fun = ds$aggregate_fun
    )
  ))
}

#' The Bisection Engine
#'
#' Runs a bisection search over sample size using repeated simulations and
#' summaries of the chosen performance metric.
#'
#' @inheritParams calculate_mlpwr
#' @param value_on_error Numeric fallback returned when a simulation run fails.
#' @param tol Numeric tolerance controlling when the bisection loop stops.
#' @param parallel Logical; if `TRUE` the per-sample-size simulations run in parallel via `foreach`.
#' @param cores Integer number of cores to use when `parallel = TRUE`.
#' @param budget Logical; if `TRUE` the algorithm halts once the evaluation budget is exhausted instead of using `tol`.
#'
#' @return A list containing the simulation `results`, performance `summaries`,
#'   optional tracking `history`, and the `track_bisection` records.
#' @keywords internal

calculate_bisection <- function(
  data_function = data_function,
  model_function = model_function,
  metric_function = metric_function,
  value_on_error = value_on_error,
  min_sample_size = min_sample_size,
  max_sample_size = max_sample_size,
  test_n = test_n,
  n_reps_total = n_reps_total,
  n_reps_per = n_reps_per,
  target_performance = target_performance,
  c_statistic,
  mean_or_assurance = mean_or_assurance,
  tol = 1e-3,
  parallel = FALSE,
  cores = 20,
  verbose = FALSE,
  budget = TRUE
) {
  # get initial start values

  # Determine start values
  start_values <- compute_start_sample_sizes(
    data_function = data_function,
    metric_function = metric_function,
    target_performance = target_performance,
    c_statistic = c_statistic,
    mean_or_assurance = mean_or_assurance
  )

  start_values <- calculate_adaptive_bounds(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    value_on_error = value_on_error,
    start_n = start_values$start_min_sample_size,
    test_n = test_n,
    n_reps_per = n_reps_per,
    n_reps_total = 500,
    target_performance = target_performance,
    threshold = 0.0001,
    mean_or_assurance = mean_or_assurance,
    verbose = FALSE
  )

  start_min_sample_size <- start_values$min_sample_size
  start_max_sample_size <- start_values$max_sample_size

  max_iter <- round(n_reps_total / n_reps_per)

  # Generate fixed test set once
  test_data <- data_function(test_n)

  # Set up cluster once if parallel requested
  cl <- NULL
  registered_parallel <- FALSE
  if (isTRUE(parallel)) {
    require_optional_packages(
      c("doParallel", "foreach"),
      "parallel bisection simulations"
    )

    # sensible default if user passed an invalid cores
    if (is.null(cores) || !is.numeric(cores) || cores < 1) {
      cores <- parallel::detectCores(logical = FALSE)
    }
    cores_to_use <- min(cores, parallel::detectCores())

    cl <- parallel::makeCluster(cores_to_use)

    # Export the core functions/objects themselves (so workers can call them)
    core_names <- c(
      "data_function",
      "model_function",
      "metric_function",
      "value_on_error",
      "test_data"
    )
    parallel::clusterExport(cl, varlist = core_names, envir = environment())

    # Export everything from the environments of the three functions.
    envs <- unique(list(
      environment(data_function),
      environment(model_function),
      environment(metric_function)
    ))
    for (e in envs) {
      if (!is.null(e)) {
        objs <- ls(envir = e, all.names = TRUE)
        # Avoid exporting names that are internal to base packages.
        if (length(objs) > 0) {
          try(
            parallel::clusterExport(cl, varlist = objs, envir = e),
            silent = TRUE
          )
        }
      }
    }

    # Register backend for foreach
    doParallel::registerDoParallel(cl)
    registered_parallel <- TRUE

    # Ensure cluster is stopped when function exits (even on error)
    on.exit(
      {
        try(parallel::stopCluster(cl), silent = TRUE)
        try(doParallel::stopImplicitCluster(), silent = TRUE)
      },
      add = TRUE
    )
  }

  # Run one simulation.
  single_run <- function(n) {
    tryCatch(
      {
        dat <- data_function(n)
        fit <- model_function(dat)
        metric_function(test_data, fit, attr(model_function, "model"))
      },
      error = function(e) value_on_error
    )
  }

  # Summarise the metric over n_reps_per simulations.
  summary_at_n <- function(n) {
    if (isTRUE(parallel) && registered_parallel) {
      vals <- foreach::`%dopar%`(
        foreach::foreach(i = seq_len(n_reps_per), .combine = c),
        {
          # Each worker will call single_run; single_run closes over data_function, etc.
          tryCatch(
            {
              dat <- data_function(n)
              fit <- model_function(dat)
              metric_function(test_data, fit, attr(model_function, "model"))
            },
            error = function(e) value_on_error
          )
        }
      )
    } else {
      vals <- vapply(
        seq_len(n_reps_per),
        function(i) single_run(n),
        FUN.VALUE = numeric(1)
      )
    }
    s <- get_summaries(matrix(vals, nrow = 1))
    if (mean_or_assurance == "mean") {
      list(y_summary = s$mean_performance, y = vals)
    } else {
      list(y_summary = s$quant20_performance, y = vals)
    }
  }

  # Override adaptive min when provided

  if (!is.null(min_sample_size) && !is.null(max_sample_size)) {
    start_min_sample_size <- min_sample_size
    start_max_sample_size <- max_sample_size
  }

  # Initial bounds
  p_lo <- summary_at_n(start_min_sample_size)$y_summary
  p_hi <- summary_at_n(start_max_sample_size)$y_summary

  iter <- 0
  history <- list()
  track_bisection <- list()

  # Bisection loop with condition depending on 'budget'
  while (
    (budget && iter < max_iter) ||
      (!budget && (p_hi - p_lo) >= tol && iter < max_iter)
  ) {
    mid <- floor((start_min_sample_size + start_max_sample_size) / 2)
    mid_result <- summary_at_n(mid)
    p_mid <- mid_result$y_summary

    track_bisection[[iter + 1]] <- list(x = mid, y = mid_result$y)

    if (verbose) {
      history[[iter + 1]] <- list(iter = iter + 1, mid = mid, p_mid = p_mid)
    }

    if (p_mid >= target_performance) {
      start_max_sample_size <- mid
      p_hi <- p_mid
    } else {
      start_min_sample_size <- mid
      p_lo <- p_mid
    }

    iter <- iter + 1
  }

  # Stop the cluster now; on.exit remains a fallback for early exits.
  if (!is.null(cl)) {
    try(parallel::stopCluster(cl), silent = TRUE)
    try(doParallel::stopImplicitCluster(), silent = TRUE)
  }

  result <- list(
    min_n = start_max_sample_size,
    performance = p_hi,
    min_sample_size_bound = start_min_sample_size,
    min_sample_size_perf = p_lo,
    max_sample_size_bound = start_max_sample_size,
    max_sample_size_perf = p_hi,
    iterations = iter,
    track_bisection = track_bisection
  )

  if (verbose) {
    result$history <- history
  }

  return(result)
}

#' mlpwr-bs Hybrid engine using bisection to determine initial range and mlpwr for search
#' @inheritParams simulate_custom
#' @param progress Logical flag controlling whether the `mlpwr` progress bar is shown.
#' @param verbose Logical flag passed to `mlpwr`; when `TRUE` verbose output is printed.
#' @param value_on_error Numeric fallback value used if model fitting or metric calculation fails.
#' @param ... Additional options passed to [mlpwr::find.design()].
#'
#' @return List containing the combined bisection and mlpwr results (`results`, `summaries`, `min_n`, `perf_n`, and `mlpwr_ds`).
#' @keywords internal
calculate_mlpwr_bs <- function(
  test_n,
  n_reps_total,
  n_reps_per,
  se_final,
  min_sample_size,
  max_sample_size,
  target_performance,
  c_statistic,
  mean_or_assurance,
  progress = TRUE,
  verbose,
  data_function,
  model_function,
  metric_function,
  value_on_error,
  ...
) {
  # Calculate the first stage bisection

  # Determine number of predictors (excluding outcome column)
  # Determine start values
  start_values <- compute_start_sample_sizes(
    data_function = data_function,
    metric_function = metric_function,
    target_performance = target_performance,
    c_statistic = c_statistic,
    mean_or_assurance = mean_or_assurance
  )

  start_values <- calculate_adaptive_bounds(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    value_on_error = value_on_error,
    start_n = start_values$start_min_sample_size,
    test_n = test_n,
    n_reps_per = n_reps_per,
    n_reps_total = 500,
    target_performance = target_performance,
    threshold = 0.0001,
    mean_or_assurance = mean_or_assurance,
    verbose = FALSE
  )

  prev_min_sample_size <- start_values$min_sample_size
  prev_max_sample_size <- start_values$max_sample_size

  # Override adaptive min and max when provided at stage 1
  if (!is.null(min_sample_size) && !is.null(max_sample_size)) {
    prev_min_sample_size <- min_sample_size
    prev_max_sample_size <- max_sample_size
  }

  prev <- calculate_bisection(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    target_performance = target_performance,
    c_statistic = c_statistic,
    min_sample_size = prev_min_sample_size,
    max_sample_size = prev_max_sample_size,
    n_reps_total = 200,
    n_reps_per = n_reps_per,
    mean_or_assurance = mean_or_assurance,
    value_on_error = value_on_error,
    verbose = FALSE,
    parallel = FALSE,
    budget = TRUE,
    test_n = test_n
  )

  # Calculate the second stage mlpwr
  test_data <- data_function(test_n)
  # Calculate the metrics for a sample size n
  mlpwr_simulation_function <- function(n) {
    tryCatch(
      {
        train_data <- data_function(n)
        fit <- model_function(train_data)
        model <- attr(model_function, "model")
        metric_function(test_data, fit, model)
      },
      error = function(e) {
        return(value_on_error)
      }
    )
  }

  if (mean_or_assurance == "mean") {
    aggregate_fun <- function(x) mean(x, na.rm = TRUE)
  } else if (mean_or_assurance == "assurance") {
    aggregate_fun <- function(x) stats::quantile(x, probs = .2, na.rm = TRUE)
  } else {
    stop("mean_or_assurance must be either 'mean' or 'assurance'")
  }

  # Use a bootstrap to estimate the variance of the estimated quantile
  var_bootstrap <- function(x) {
    stats::var(replicate(
      20,
      aggregate_fun(sample(x, length(x), replace = TRUE))
    ))
  }

  # Calculate bootstrapped quantile variance
  noise_fun <- function(x) var_bootstrap(x$y)

  # When supplied, use the requested final standard error to control stopping.
  # A deliberately high simulation budget ensures the CI criterion dominates.
  if (!(is.null(se_final))) {
    ci <- se_final * stats::qnorm(0.975) * 2
    n_reps_total <- 10000
  } else {
    ci <- NULL
  }

  # Perform search using mlpwr
  get_start_bounds <- adaptive_startvalues(
    output = prev,
    aggregate_fun = aggregate_fun,
    var_bootstrap = var_bootstrap,
    target = target_performance,
    ci_q = 0.975
  )

  mlpwrbs_min_sample_size <- get_start_bounds$min_value
  mlpwrbs_max_sample_size <- get_start_bounds$max_value

  # correction for tight bounds

  mlpwrbs_max_sample_size <- ifelse(
    (mlpwrbs_max_sample_size -
      mlpwrbs_min_sample_size) <
      5,
    round(mlpwrbs_min_sample_size * 1.2),
    mlpwrbs_max_sample_size
  )

  # Override adaptive min and max when provided at stage 2
  if (!is.null(min_sample_size) && !is.null(max_sample_size)) {
    mlpwrbs_min_sample_size <- min_sample_size
    mlpwrbs_max_sample_size <- max_sample_size
  }

  ds <- do.call(
    mlpwr::find.design,
    utils::modifyList(
      list(
        simfun = mlpwr_simulation_function,
        aggregate_fun = aggregate_fun,
        noise_fun = noise_fun,
        boundaries = c(mlpwrbs_min_sample_size, mlpwrbs_max_sample_size),
        power = target_performance,
        surrogate = "gpr",
        setsize = n_reps_per,
        evaluations = n_reps_total,
        ci = ci,
        n.startsets = 4,
        silent = !isTRUE(progress)
      ),
      list(...)
    )
  )

  # Process results from mlpwr
  perfs <- ds$dat
  perfs <- perfs[order(sapply(perfs, "[[", "x"))]
  max_len <- max(sapply(perfs, \(x) length(x$y)))
  results <- matrix(nrow = length(perfs), ncol = max_len)
  rownames(results) <- sapply(perfs, \(x) x$x)
  for (i in seq_along(perfs)) {
    results[i, seq(1, length(perfs[[i]]$y), 1)] <- perfs[[i]]$y
  }

  mlpwr_summaries <- get_summaries(results)

  return(list(
    results = perfs,
    summaries = mlpwr_summaries,
    min_n = as.numeric(ds$final$design),
    perf_n = as.numeric(ds$final$power),
    mlpwr_ds = list(
      data = ds$dat,
      fit = ds$fit,
      boundaries = ds$boundaries,
      final = ds$final,
      aggregate_fun = ds$aggregate_fun
    )
  ))
}
