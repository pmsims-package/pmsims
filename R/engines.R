#' mlpwr engine
#' @inheritParams simulate_custom
#' @param n_init Integer number of initial sample sizes simulated before the Gaussian process search begins.
#' @param progress Logical flag controlling whether the `mlpwr` progress bar is shown.
#' @param verbose Logical flag passed to `mlpwr`; when `TRUE` verbose output is printed.
#' @param value_on_error Numeric fallback value used if model fitting or metric calculation fails.
#' @param evaluator Optional evaluator from `new_evaluator()`, shared by every
#'   stage of the search. Created from the data, model and metric functions
#'   when `NULL`.
#' @param max_n Largest sample size the adaptive start-value search may try.
#' @param parallel,cores Run the replicates of each batch in parallel (see
#'   [calculate_bisection()]); with mlpwr, replicates are requested one at a
#'   time, so this mainly affects the adaptive stage and verification.
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
  evaluator = NULL,
  max_n = 1e6,
  parallel = FALSE,
  cores = 1L,
  ...
) {
  # parallel and cores are used by the evaluator (see simulate_custom()); they
  # are named here so they are not passed on to mlpwr::find.design().
  if (is.null(evaluator)) {
    evaluator <- new_evaluator(
      data_function, model_function, metric_function, test_n, value_on_error,
      parallel = parallel, cores = cores
    )
  }

  stage_1 <- search_bounds(
    min_sample_size = min_sample_size,
    max_sample_size = max_sample_size,
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    value_on_error = value_on_error,
    test_n = test_n,
    n_reps_per = n_reps_per,
    target_performance = target_performance,
    c_statistic = c_statistic,
    mean_or_assurance = mean_or_assurance,
    evaluator = evaluator,
    max_n = max_n
  )
  if (!is.null(stage_1$status)) {
    return(stopped_search(stage_1))
  }

  # When supplied, use the requested final standard error to control stopping.
  # A deliberately high simulation budget ensures the CI criterion dominates.
  if (!(is.null(se_final))) {
    ci <- se_final * stats::qnorm(0.975) * 2
    n_reps_total <- 10000
  } else {
    ci <- NULL
  }

  # Extrapolate the timed first stage to the full run and tell the user if it
  # is going to be a long one.
  warn_if_long_run(
    stage_1_secs = stage_1$secs,
    track = stage_1$track,
    n_reps_per = n_reps_per,
    n_reps_total = n_reps_total,
    min_sample_size = stage_1$min,
    max_sample_size = stage_1$max,
    model = attr(model_function, "model", exact = TRUE)
  )

  cli::cli_alert_info("Estimating second stage... (Gaussian process algorithm)")
  search <- run_mlpwr_search(
    evaluator = evaluator,
    boundaries = c(stage_1$min, stage_1$max),
    target_performance = target_performance,
    mean_or_assurance = mean_or_assurance,
    n_reps_per = n_reps_per,
    n_reps_total = n_reps_total,
    ci = ci,
    n_init = n_init,
    progress = progress,
    ...
  )

  mlpwr_output(search, stage_1)
}

# -----------------------------------------------------------------------------
# Shared pieces of the engines
# -----------------------------------------------------------------------------

#' Stage 1: bounds for the main search
#'
#' Uses the user's bounds when both are supplied; otherwise runs the adaptive
#' start-value search on the shared evaluator. When that search reached
#' `max_n` with performance still clearly below the target, the result carries
#' a `status` and the main search is not run (see adaptive_status()).
#' @keywords internal
#' @noRd
search_bounds <- function(
  min_sample_size,
  max_sample_size,
  data_function,
  model_function,
  metric_function,
  value_on_error,
  test_n,
  n_reps_per,
  target_performance,
  c_statistic,
  mean_or_assurance,
  evaluator,
  max_n = 1e6,
  adaptive_reps = 500
) {
  if (!is.null(min_sample_size) && !is.null(max_sample_size)) {
    # A user-defined search space makes the adaptive stage redundant: its bounds
    # would only be replaced. Without a timed first stage there is nothing to
    # extrapolate the runtime from, so the long-run check is skipped too.
    return(list(
      min = min_sample_size,
      max = max_sample_size,
      secs = NA_real_,
      track = NULL,
      adaptive = NULL,
      status = NULL
    ))
  }

  start_values <- tryCatch(
    compute_start_sample_sizes(
      data_function = data_function,
      metric_function = metric_function,
      target_performance = target_performance,
      c_statistic = c_statistic,
      mean_or_assurance = mean_or_assurance
    ),
    error = function(e) {
      stop(paste("Error when computing start values:", e$message), call. = FALSE)
    }
  )

  cli::cli_alert_info(
    "Estimating first stage... (Adaptive starting value search algorithm)"
  )
  stage_1_start <- Sys.time()
  adaptive <- tryCatch(
    calculate_adaptive_bounds(
      data_function = data_function,
      model_function = model_function,
      metric_function = metric_function,
      value_on_error = value_on_error,
      start_n = start_values$start_min_sample_size,
      test_n = test_n,
      n_reps_per = n_reps_per,
      n_reps_total = adaptive_reps,
      target_performance = target_performance,
      threshold = 0.0001,
      mean_or_assurance = mean_or_assurance,
      verbose = FALSE,
      max_n = max_n,
      evaluator = evaluator
    ),
    error = function(e) {
      stop(
        paste("Error during adaptive start value search:", e$message),
        call. = FALSE
      )
    }
  )
  secs <- as.numeric(difftime(Sys.time(), stage_1_start, units = "secs"))

  out <- list(
    min = adaptive$min_sample_size,
    max = adaptive$max_sample_size,
    secs = secs,
    track = adaptive$track,
    adaptive = adaptive,
    status = NULL
  )
  out$status <- adaptive_status(adaptive, target_performance)
  if (is.null(out$status)) {
    cli::cli_alert_info(
      "Starting values determined: min sample size = {out$min}, \\
       max sample size = {out$max}"
    )
  }
  out
}

# Status implied by the adaptive stage, or NULL when the main search should run.
#
# The search is stopped only when the ladder reached max_n with performance
# there still clearly below the target. A plateau is NOT treated as evidence
# that the target is unreachable: with 20-80 replicates per sample size, the
# Monte Carlo error of each summary is often larger than the gain from one
# doubling, so slowly converging but reachable curves "plateau" too (in
# simulations, up to 80% of runs that started 8x below a strict target).
# After a plateau the main search runs as before, and the verification of the
# returned sample size flags an answer that does not meet the target.
adaptive_status <- function(adaptive, target_performance) {
  track <- adaptive$track
  if (!length(track) || !identical(adaptive$stop_reason, "max_n_reached")) {
    return(NULL)
  }
  calls <- vapply(track, function(z) z$call %||% NA_character_, character(1))
  if (any(calls %in% "above")) {
    return(NULL)
  }
  ns <- vapply(track, `[[`, numeric(1), "n")
  perfs <- vapply(track, `[[`, numeric(1), "performance")
  ses <- vapply(track, function(z) z$se %||% 0, numeric(1))
  largest <- which.max(ns)
  if (!isTRUE(perfs[largest] + 2 * ses[largest] < target_performance)) {
    return(NULL)
  }
  list(
    status = "not_bracketed",
    message = sprintf(
      paste(
        "No sample size up to %s reached the target (%s); performance there",
        "was %s. The search stopped at the largest sample size it may try",
        "(max_n); the target may be unreachable."
      ),
      format(max(ns), big.mark = ",", scientific = FALSE),
      format(signif(target_performance, 4)),
      format(signif(perfs[largest], 4))
    ),
    max_achievable_perf = max(perfs, na.rm = TRUE)
  )
}

# Engine output when stage 1 stopped the search.
stopped_search <- function(stage_1) {
  list(
    results = NULL,
    summaries = NULL,
    min_n = NA_real_,
    perf_n = NA_real_,
    mlpwr_ds = NULL,
    gp_restarts = 0L,
    search = list(
      status = stage_1$status$status,
      status_message = stage_1$status$message,
      max_achievable_perf = stage_1$status$max_achievable_perf,
      adaptive_stop_reason = stage_1$adaptive$stop_reason,
      bounds = c(stage_1$min, stage_1$max),
      at_bound = NA_character_,
      mlpwr_warnings = character(0)
    )
  )
}

#' Run mlpwr's Gaussian-process search on the shared evaluator
#'
#' mlpwr's warnings ("No good design found", predictions at the edge of the
#' search space) are collected and returned instead of being printed or lost:
#' the simulate_*() wrappers used to suppress all warnings, so these never
#' reached the user.
#' @keywords internal
#' @noRd
run_mlpwr_search <- function(
  evaluator,
  boundaries,
  target_performance,
  mean_or_assurance,
  n_reps_per,
  n_reps_total,
  ci = NULL,
  n_init = 4,
  progress = FALSE,
  ...
) {
  aggregate_fun <- criterion_function(mean_or_assurance)

  # Bootstrap variance of the criterion, used by mlpwr as the noise variance
  # of each design point.
  var_bootstrap <- function(x) {
    stats::var(replicate(
      20,
      aggregate_fun(sample(x, length(x), replace = TRUE))
    ))
  }
  noise_fun <- function(x) var_bootstrap(x$y)

  warnings_seen <- character(0)
  ds <- with_mlpwr_progress(progress, n_reps_total, {
    withCallingHandlers(
      find_design_with_restarts(
        utils::modifyList(
          list(
            simfun = function(n) evaluator$one(n, "search"),
            aggregate_fun = aggregate_fun,
            noise_fun = noise_fun,
            boundaries = boundaries,
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
      ),
      warning = function(w) {
        warnings_seen <<- c(warnings_seen, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  })

  min_n <- as.numeric(ds$final$design)
  at_bound <- if (length(min_n) == 1L && is.finite(min_n)) {
    if (min_n <= min(boundaries)) {
      "lower"
    } else if (min_n >= max(boundaries)) {
      "upper"
    } else {
      NA_character_
    }
  } else {
    NA_character_
  }

  list(
    ds = ds,
    boundaries = boundaries,
    at_bound = at_bound,
    warnings = unique(warnings_seen)
  )
}

# Engine output from an mlpwr search.
mlpwr_output <- function(search, stage_1 = NULL, extra = list()) {
  ds <- search$ds
  perfs <- ds$dat
  perfs <- perfs[order(sapply(perfs, "[[", "x"))]
  max_len <- max(sapply(perfs, \(x) length(x$y)))
  results <- matrix(nrow = length(perfs), ncol = max_len)
  rownames(results) <- sapply(perfs, \(x) x$x)
  for (i in seq_along(perfs)) {
    results[i, seq(1, length(perfs[[i]]$y), 1)] <- perfs[[i]]$y
  }

  c(
    list(
      results = perfs,
      summaries = get_summaries(results),
      min_n = as.numeric(ds$final$design),
      perf_n = as.numeric(ds$final$power),
      mlpwr_ds = list(
        data = ds$dat,
        fit = ds$fit,
        boundaries = ds$boundaries,
        final = ds$final,
        aggregate_fun = ds$aggregate_fun
      ),
      gp_restarts = attr(ds, "gp_restarts") %||% 0L,
      search = list(
        status = NULL,
        status_message = NULL,
        max_achievable_perf = NA_real_,
        adaptive_stop_reason = stage_1$adaptive$stop_reason,
        bounds = search$boundaries,
        at_bound = search$at_bound,
        mlpwr_warnings = search$warnings
      )
    ),
    extra
  )
}

# Show mlpwr's progress through cli (or a text bar) by temporarily replacing
# mlpwr's internal print_progress().
with_mlpwr_progress <- function(progress, n_reps_total, expr) {
  if (!isTRUE(progress)) {
    return(expr)
  }
  pb_id <- NULL
  pb_txt <- NULL

  if (requireNamespace("cli", quietly = TRUE)) {
    pb_id <- cli::cli_progress_bar(
      "Estimating second stage (Gaussian process)",
      total = n_reps_total,
      format = "{cli::pb_spin} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} sims ({cli::pb_eta})"
    )
    patched_print_progress <- function(n_updates, evaluations_used, time_used) {
      used <- suppressWarnings(as.numeric(evaluations_used))
      if (length(used) != 1L || !is.finite(used)) {
        used <- 0
      }
      used <- min(max(as.integer(round(used)), 0L), as.integer(n_reps_total))
      # Only cosmetic, so a failed update is ignored.
      tryCatch(
        cli::cli_progress_update(id = pb_id, set = used),
        error = function(e) invisible(NULL)
      )
    }
  } else {
    pb_txt <- utils::txtProgressBar(min = 0, max = n_reps_total, style = 3)
    patched_print_progress <- function(n_updates, evaluations_used, time_used) {
      utils::setTxtProgressBar(pb_txt, evaluations_used)
    }
  }

  ns <- asNamespace("mlpwr")
  orig_print_progress <- get("print_progress", envir = ns)
  utils::assignInNamespace("print_progress", patched_print_progress, ns)
  on.exit(
    {
      utils::assignInNamespace("print_progress", orig_print_progress, "mlpwr")
      if (!is.null(pb_txt)) close(pb_txt)
      if (!is.null(pb_id)) cli::cli_progress_done(id = pb_id)
    },
    add = TRUE
  )
  expr
}

#' The Bisection Engine
#'
#' Runs a bisection search over sample size using repeated simulations and
#' summaries of the chosen performance metric.
#'
#' @inheritParams calculate_mlpwr
#' @param value_on_error Numeric fallback returned when a simulation run fails.
#' @param tol Numeric tolerance controlling when the bisection loop stops.
#' @param parallel Logical; if `TRUE` the replicates at each sample size run in
#'   parallel (forked processes; serial on Windows). Results do not depend on
#'   the number of cores.
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
  budget = TRUE,
  evaluator = NULL,
  max_n = 1e6
) {
  if (is.null(evaluator)) {
    if (isTRUE(parallel)) {
      if (is.null(cores) || !is.numeric(cores) || cores < 1) {
        cores <- parallel::detectCores(logical = FALSE)
      }
      cores <- min(cores, parallel::detectCores())
    }
    evaluator <- new_evaluator(
      data_function, model_function, metric_function, test_n, value_on_error,
      parallel = parallel, cores = cores
    )
  }

  stage_1 <- search_bounds(
    min_sample_size = min_sample_size,
    max_sample_size = max_sample_size,
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    value_on_error = value_on_error,
    test_n = test_n,
    n_reps_per = n_reps_per,
    target_performance = target_performance,
    c_statistic = c_statistic,
    mean_or_assurance = mean_or_assurance,
    evaluator = evaluator,
    max_n = max_n
  )
  if (!is.null(stage_1$status)) {
    return(stopped_search(stage_1))
  }
  start_min_sample_size <- stage_1$min
  start_max_sample_size <- stage_1$max
  bounds <- c(start_min_sample_size, start_max_sample_size)

  max_iter <- round(n_reps_total / n_reps_per)
  crit <- criterion_function(mean_or_assurance)

  # Summarise the metric over n_reps_per simulations.
  summary_at_n <- function(n) {
    vals <- evaluator$batch(n, n_reps_per, "search")
    list(y_summary = crit(vals), y = vals)
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

  min_n <- start_max_sample_size
  result <- list(
    min_n = min_n,
    perf_n = p_hi,
    performance = p_hi,
    min_sample_size_bound = start_min_sample_size,
    min_sample_size_perf = p_lo,
    max_sample_size_bound = start_max_sample_size,
    max_sample_size_perf = p_hi,
    iterations = iter,
    track_bisection = track_bisection,
    search = list(
      status = NULL,
      status_message = NULL,
      max_achievable_perf = NA_real_,
      adaptive_stop_reason = stage_1$adaptive$stop_reason,
      bounds = bounds,
      at_bound = if (min_n >= max(bounds)) "upper" else NA_character_,
      mlpwr_warnings = character(0)
    )
  )

  if (verbose) {
    result$history <- history
  }

  return(result)
}

#' mlpwr-bs Hybrid engine using bisection to determine initial range and mlpwr for search
#' @inheritParams calculate_mlpwr
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
  evaluator = NULL,
  max_n = 1e6,
  parallel = FALSE,
  cores = 1L,
  ...
) {
  # parallel and cores are used by the evaluator (see simulate_custom()); they
  # are named here so they are not passed on to mlpwr::find.design().
  if (is.null(evaluator)) {
    evaluator <- new_evaluator(
      data_function, model_function, metric_function, test_n, value_on_error,
      parallel = parallel, cores = cores
    )
  }

  # Stage 1. A user-defined search space makes the adaptive stage redundant,
  # as in calculate_mlpwr(); the bisection below still runs, inside it.
  stage_1 <- search_bounds(
    min_sample_size = min_sample_size,
    max_sample_size = max_sample_size,
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    value_on_error = value_on_error,
    test_n = test_n,
    n_reps_per = n_reps_per,
    target_performance = target_performance,
    c_statistic = c_statistic,
    mean_or_assurance = mean_or_assurance,
    evaluator = evaluator,
    max_n = max_n
  )
  if (!is.null(stage_1$status)) {
    return(stopped_search(stage_1))
  }

  prev <- calculate_bisection(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    target_performance = target_performance,
    c_statistic = c_statistic,
    min_sample_size = stage_1$min,
    max_sample_size = stage_1$max,
    n_reps_total = 200,
    n_reps_per = n_reps_per,
    mean_or_assurance = mean_or_assurance,
    value_on_error = value_on_error,
    verbose = FALSE,
    parallel = FALSE,
    budget = TRUE,
    test_n = test_n,
    evaluator = evaluator
  )

  aggregate_fun <- criterion_function(mean_or_assurance)
  var_bootstrap <- function(x) {
    stats::var(replicate(
      20,
      aggregate_fun(sample(x, length(x), replace = TRUE))
    ))
  }

  # When supplied, use the requested final standard error to control stopping.
  # A deliberately high simulation budget ensures the CI criterion dominates.
  if (!(is.null(se_final))) {
    ci <- se_final * stats::qnorm(0.975) * 2
    n_reps_total <- 10000
  } else {
    ci <- NULL
  }

  # Extrapolate the timed first stage to the full run and tell the user if it
  # is going to be a long one. Done once the replication budget is settled.
  warn_if_long_run(
    stage_1_secs = stage_1$secs,
    track = stage_1$track,
    n_reps_per = n_reps_per,
    n_reps_total = n_reps_total,
    min_sample_size = stage_1$min,
    max_sample_size = stage_1$max,
    model = attr(model_function, "model", exact = TRUE)
  )

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
    (mlpwrbs_max_sample_size - mlpwrbs_min_sample_size) < 5,
    round(mlpwrbs_min_sample_size * 1.2),
    mlpwrbs_max_sample_size
  )

  # Override adaptive min and max when provided at stage 2
  if (!is.null(min_sample_size) && !is.null(max_sample_size)) {
    mlpwrbs_min_sample_size <- min_sample_size
    mlpwrbs_max_sample_size <- max_sample_size
  }

  search <- run_mlpwr_search(
    evaluator = evaluator,
    boundaries = c(mlpwrbs_min_sample_size, mlpwrbs_max_sample_size),
    target_performance = target_performance,
    mean_or_assurance = mean_or_assurance,
    n_reps_per = n_reps_per,
    n_reps_total = n_reps_total,
    ci = ci,
    n_init = 4,
    progress = progress,
    ...
  )

  mlpwr_output(search, stage_1)
}

#' Run mlpwr::find.design(), restarting the search if the GP surrogate fails
#'
#' mlpwr fits its Gaussian-process surrogate with up to 100 attempts of
#' `DiceKriging::km()`, discarding fits that are close to a plane. When the
#' learning curve is nearly linear across the search range, every fit is
#' discarded, many attempts fail inside the optimiser, and if the final attempt
#' fails, mlpwr keeps a `NULL` model and later stops with "no applicable method
#' for `@` applied to an object of class "NULL"". Whether this happens depends
#' on the random state, so restarting the search from the current RNG state
#' usually succeeds. The run stays reproducible for a given seed.
#'
#' Only this surrogate failure is retried; any other error stops immediately,
#' as before.
#'
#' @param args Named list of arguments for [mlpwr::find.design()].
#' @param max_attempts Total number of searches to try.
#' @return The [mlpwr::find.design()] result, with attribute `gp_restarts`
#'   giving the number of restarts that were needed.
#' @keywords internal
#' @noRd
find_design_with_restarts <- function(args, max_attempts = 3L) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(run_find_design(args), error = function(e) e)
    if (!inherits(result, "error")) {
      attr(result, "gp_restarts") <- attempt - 1L
      return(result)
    }
    if (!is_gp_surrogate_failure(result) || attempt == max_attempts) {
      msg <- paste("mlpwr::find.design failed with error:", conditionMessage(result))
      if (is_gp_surrogate_failure(result)) {
        msg <- sprintf("%s (after %d attempts)", msg, attempt)
      }
      stop(msg, call. = FALSE)
    }
    cli::cli_alert_warning(
      "The Gaussian process surrogate could not be fitted; restarting the \\
       search (attempt {attempt + 1} of {max_attempts})."
    )
  }
}

# Separate so tests can replace it.
run_find_design <- function(args) {
  do.call(mlpwr::find.design, args)
}

is_gp_surrogate_failure <- function(e) {
  grepl(
    "applied to an object of class \"NULL\"",
    conditionMessage(e),
    fixed = TRUE
  )
}

