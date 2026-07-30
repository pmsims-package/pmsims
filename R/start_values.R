#' Get Performance
#'
#' @param results Numeric matrix of replicate performance values, with one row
#'   per evaluated sample size.
#' @param p Optional numeric quantile in `(0, 1)` used when `mean = FALSE`.
#' @param mean Logical; if `TRUE`, return row means instead of quantiles.
#'
#' @return Numeric vector of aggregated performance summaries, one value per row
#'   of `results`.
#' @keywords internal
#' @noRd
#'
#' @examples
#' perf <- matrix(c(0.81, 0.83, 0.86, 0.88), nrow = 2, byrow = TRUE)
#' get_perf(perf, mean = TRUE)
get_perf <- function(results, p = NULL, mean = FALSE) {
  if (is.null(p) && !mean) {
    stop("Either p or mean must be specified")
  }
  if (mean) {
    results <- apply(results, FUN = mean, MARGIN = 1, na.rm = TRUE)
  } else {
    results <- apply(
      results,
      FUN = stats::quantile,
      MARGIN = 1,
      probs = p,
      na.rm = TRUE
    )
  }
  return(results)
}

get_summaries <- function(performance_matrix) {
  list(
    mean_performance = get_perf(results = performance_matrix, mean = TRUE),
    median_performance = get_perf(performance_matrix, p = 0.5),
    quant20_performance = get_perf(performance_matrix, p = 0.2),
    quant5_performance = get_perf(performance_matrix, p = 0.05),
    quant95_performance = get_perf(performance_matrix, p = 0.95)
  )
}

#' adaptive_startvalues Derive adaptive sample sizes
#'
#' @param output List-like object containing `track_bisection`, produced by `calculate_bisection()`.
#' @param aggregate_fun Function used to summarise replicate performance values (for example, `mean` or a quantile function).
#' @param var_bootstrap Function returning the bootstrap variance of the aggregated performance.
#' @param target Numeric target performance threshold.
#' @param ci_q Numeric quantile for confidence-interval construction (default 0.975 gives a two-sided 95% interval).
#' @keywords internal
adaptive_startvalues <- function(
  output,
  aggregate_fun,
  var_bootstrap,
  target,
  ci_q = 0.975
) {
  bisection_output <- output$track_bisection
  n_iter <- length(bisection_output)

  # Matrix: n, est, se, ll, ul
  bisection_summary <- matrix(
    NA,
    nrow = n_iter,
    ncol = 5,
    dimnames = list(NULL, c("n", "est", "se", "ll", "ul"))
  )

  for (i in seq_len(n_iter)) {
    results <- bisection_output[[i]]
    n <- results$x
    performance_data <- results$y

    est <- aggregate_fun(performance_data)
    se <- sqrt(var_bootstrap(performance_data))

    ll <- est - se * stats::qnorm(ci_q)
    ul <- est + se * stats::qnorm(ci_q)

    bisection_summary[i, ] <- c(n, est, se, ll, ul)
  }

  # Find the lower bound.
  ordered_by_ul <- bisection_summary[
    order(bisection_summary[, "ul"], decreasing = TRUE),
  ]
  below_target <- ordered_by_ul[ordered_by_ul[, "ul"] < target, , drop = FALSE]

  if (nrow(below_target) == 0) {
    min_value <- min(bisection_summary[, "n"] * 0.8)
  } else {
    min_value <- max(below_target[, "n"])
  }

  # Find the upper bound.
  ordered_by_ll <- bisection_summary[
    order(bisection_summary[, "ll"], decreasing = TRUE),
  ]
  above_target <- ordered_by_ll[ordered_by_ll[, "ll"] > target, , drop = FALSE]

  if (nrow(above_target) == 0) {
    max_value <- max(bisection_summary[, "n"] * 1.2)
  } else {
    max_value <- min(above_target[, "n"])
  }

  return(list(
    summary = bisection_summary,
    min_value = round(min_value),
    max_value = round(max_value)
  ))
}

#' Calculate adaptive start bounds
#'
#' Derive lower and upper sample-size bounds by repeatedly simulating model
#' performance from an initial sample size.
#'
#' @param data_function Function taking a sample size and returning a simulated
#'   training dataset.
#' @param model_function Function fitting a model to a simulated training
#'   dataset.
#' @param metric_function Function evaluating the fitted model on test data.
#' @param value_on_error Numeric fallback used when fitting or evaluation fails.
#' @param start_n Positive integer initial sample size.
#' @param test_n Positive integer size of the fixed test dataset.
#' @param n_reps_per Positive integer simulations performed at each sample size.
#' @param n_reps_total Positive integer total simulation budget.
#' @param target_performance Numeric performance threshold used to define the
#'   search bounds.
#' @param threshold Numeric tolerance around `target_performance`.
#' @param mean_or_assurance Character string selecting the mean or 20th-percentile
#'   performance summary.
#' @param plateau_k Positive integer number of recent iterations used to detect
#'   a performance plateau.
#' @param plateau_tol Numeric maximum change treated as a plateau.
#' @param large_perf_check Logical; whether to probe a large sample size before
#'   beginning the adaptive search.
#' @param large_n Optional positive integer sample size for the preliminary
#'   performance probe.
#' @param large_n_tol Numeric shortfall beyond which the target is considered
#'   unreachable at `large_n`.
#' @param c_statistic Reserved for compatibility with callers that supply an
#'   anticipated discrimination value.
#' @param parallel Logical; whether simulations at each sample size use a
#'   parallel backend.
#' @param cores Positive integer number of parallel workers.
#' @param verbose Logical; whether to report search progress.
#'
#' @noRd
#'
#' @return A list containing lower and upper sample-size bounds, the associated
#'   performance summaries, and the search trace.
calculate_adaptive_bounds <- function(
  data_function,
  model_function,
  metric_function,
  value_on_error,
  start_n,
  test_n,
  n_reps_per,
  n_reps_total,
  target_performance,
  threshold = 0.01,
  mean_or_assurance = "mean",
  plateau_k = 3,
  plateau_tol = 0.005,
  # Optional large-sample pre-check. When enabled, the search first evaluates
  # performance at large_n. If the target is more than large_n_tol above that
  # estimate, it stops and reports the target as unreachable. Otherwise, the
  # result determines the initial search bound and direction.
  #
  # Disabled by default because the preliminary large-sample fit did not work
  # reliably for some machine-learning models. The default search starts at
  # start_n and detects an unreachable target when recent gains plateau.
  large_perf_check = FALSE, # Probe performance at a large sample size first.
  large_n = NULL, # Sample size used for the optional probe.
  large_n_tol = 0.05, # Gap beyond which the target is considered unreachable.
  c_statistic = NULL,
  parallel = FALSE,
  cores = 20,
  verbose = FALSE
) {
  max_iter <- floor(n_reps_total / n_reps_per)
  test_data <- data_function(test_n)

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

  summary_at_n <- function(n) {
    if (parallel) {
      require_optional_packages(
        c("doParallel", "foreach"),
        "parallel adaptive-bound calculations"
      )

      cl <- parallel::makeCluster(cores)
      doParallel::registerDoParallel(cl)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      vals <- foreach::`%dopar%`(
        foreach::foreach(i = seq_len(n_reps_per), .combine = c),
        {
          single_run(n)
        }
      )
    } else {
      vals <- vapply(
        seq_len(n_reps_per),
        function(i) single_run(n),
        FUN.VALUE = numeric(1)
      )
    }

    perf_summary <- if (mean_or_assurance == "mean") {
      mean(vals, na.rm = TRUE)
    } else {
      as.numeric(stats::quantile(vals, probs = 0.20, na.rm = TRUE))
    }

    list(y_summary = perf_summary, y = vals)
  }

  has_plateaued <- function(track, k, tol) {
    if (length(track) < k + 1) {
      return(FALSE)
    }
    recent <- utils::tail(track, k + 1)
    perfs <- sapply(recent, `[[`, "performance")
    gains <- diff(perfs)
    all(abs(gains) < tol)
  }

  vcat <- function(...) if (verbose) message(sprintf(...))

  # Initialise the search state.
  iter <- 0L
  track <- list()
  stop_reason <- "budget_exhausted"
  lower_n <- NA_real_
  lower_perf <- NA_real_
  upper_n <- NA_real_
  upper_perf <- NA_real_
  max_achievable_perf <- NA_real_ # Reported when the target is unreachable.

  # Optionally check reachability at a large sample size before searching.
  # This is not used by default.
  if (isTRUE(large_perf_check)) {
    if (is.null(large_n)) {
      # Default: use the largest n we could reach by doubling from start_n
      # within the iteration budget (leaves room for at least one probe).
      large_n <- start_n * 2L^max(1L, max_iter - 1L)
    }

    vcat("Large-n pre-check at n = %d", large_n)

    res_large <- summary_at_n(large_n)
    perf_large <- res_large$y_summary
    iter <- iter + 1L
    track[[iter]] <- list(
      n = large_n,
      performance = perf_large,
      raw = res_large$y
    )

    vcat(
      "Iter %d | n = %d (large-n probe) | perf = %.4f",
      iter,
      large_n,
      perf_large
    )

    gap <- target_performance - perf_large

    if (gap > large_n_tol) {
      # Target is unreachable even at the large sample size — truncate.
      stop_reason <- "target_unreachable"
      max_achievable_perf <- perf_large
      upper_n <- large_n
      upper_perf <- perf_large
      lower_n <- large_n
      lower_perf <- perf_large

      vcat(
        paste0(
          "Performance at large n (%.4f) is still %.4f below target (%.4f); ",
          "gap exceeds large_n_tol (%.4f). Truncating."
        ),
        perf_large,
        gap,
        target_performance,
        large_n_tol
      )

      return(list(
        min_sample_size = as.numeric(lower_n),
        min_sample_size_perf = lower_perf,
        max_sample_size = as.numeric(upper_n),
        max_sample_size_perf = upper_perf,
        max_achievable_perf = max_achievable_perf,
        stop_reason = stop_reason,
        iterations = iter,
        max_iter = max_iter,
        track = track
      ))
    }

    # Target appears reachable — seed the bracket from the large-n probe
    # and search downward toward the smallest n that still hits the target.
    if (perf_large >= target_performance - threshold) {
      upper_n <- large_n
      upper_perf <- perf_large
      direction <- "down"
      n_current <- large_n
      vcat(
        "Large-n probe meets target. Searching downward from n = %d.",
        large_n
      )
    } else {
      # Within tolerance of being reachable but not yet at target:
      # use large_n as a lower bound and continue searching upward.
      lower_n <- large_n
      lower_perf <- perf_large
      direction <- "up"
      n_current <- large_n
      vcat(
        "Large-n probe close to target. Searching upward from n = %d.",
        large_n
      )
    }
  } else {
    # Start from the configured sample size when no pre-check is requested.
    iter <- iter + 1L
    res <- summary_at_n(start_n)
    perf <- res$y_summary
    track[[iter]] <- list(n = start_n, performance = perf, raw = res$y)

    vcat("Iter %d | n = %d | perf = %.4f", iter, start_n, perf)

    if (perf < target_performance) {
      direction <- "up"
      lower_n <- start_n
      lower_perf <- perf
    } else {
      direction <- "down"
      upper_n <- start_n
      upper_perf <- perf
    }

    n_current <- start_n
  }

  # -----------------------------------------------------------------------
  # Main loop
  # -----------------------------------------------------------------------
  while (iter < max_iter) {
    iter <- iter + 1L

    n_new <- if (direction == "up") {
      n_current * 2L
    } else {
      max(1L, n_current %/% 2L)
    }

    if (n_new == n_current) {
      stop_reason <- "no_movement"
      vcat("No movement in n. Stopping.")
      break
    }

    res <- summary_at_n(n_new)
    perf <- res$y_summary
    track[[iter]] <- list(n = n_new, performance = perf, raw = res$y)

    vcat("Iter %d | n = %d | perf = %.4f", iter, n_new, perf)

    # -- Update brackets -----------------------------------------------------
    if (direction == "up") {
      if (perf >= target_performance - threshold) {
        upper_n <- n_new
        upper_perf <- perf
        stop_reason <- "target_reached"
        vcat("Target reached. Upper bracket = %d", upper_n)
        break
      } else {
        lower_n <- n_new
        lower_perf <- perf
      }
    } else {
      if (perf <= target_performance + threshold) {
        lower_n <- n_new
        lower_perf <- perf
        stop_reason <- "target_reached"
        vcat("Target reached. Lower bracket = %d", lower_n)
        break
      } else {
        upper_n <- n_new
        upper_perf <- perf
      }
    }

    # -- Plateau check -------------------------------------------------------
    if (has_plateaued(track, k = plateau_k, tol = plateau_tol)) {
      vcat(
        paste(
          "Performance plateaued over last %d iterations",
          "(all gains < %.4f). Target unreachable. Stopping."
        ),
        plateau_k,
        plateau_tol
      )
      stop_reason <- "plateau"

      last <- track[[iter]]
      previous <- track[[iter - 1L]]

      upper_n <- last$n
      upper_perf <- last$performance
      lower_n <- previous$n
      lower_perf <- previous$performance

      # Also record the best performance seen so the caller knows the ceiling
      max_achievable_perf <- max(
        sapply(track, `[[`, "performance"),
        na.rm = TRUE
      )
      break
    }

    n_current <- n_new
  }

  list(
    min_sample_size = as.numeric(lower_n),
    min_sample_size_perf = lower_perf,
    max_sample_size = as.numeric(upper_n),
    max_sample_size_perf = upper_perf,
    max_achievable_perf = max_achievable_perf,
    stop_reason = stop_reason,
    iterations = iter,
    max_iter = max_iter,
    track = track
  )
}

#' Get initial starting values before using adaptive searching
#'
#' @param data_function Function that generates data for a requested sample
#'   size.
#' @param metric_function Function used to evaluate performance; must carry a
#'   `"metric"` attribute.
#' @param target_performance Numeric target threshold for the chosen
#'   performance metric.
#' @param c_statistic Optional anticipated discrimination measure used by the
#'   heuristic rules for some outcome types.
#' @param mean_or_assurance Character string selecting whether the search
#'   targets the mean-based or assurance-based criterion.
#'
#' @return A list containing the inferred number of predictors, the detected
#'   metric, and heuristic starting minimum and maximum sample sizes.
#' @keywords internal
#' @noRd
compute_start_sample_sizes <- function(
  data_function,
  metric_function,
  target_performance,
  c_statistic = NULL,
  mean_or_assurance = c("mean", "assurance")
) {
  mean_or_assurance <- match.arg(mean_or_assurance)

  # Infer the number of predictors from the generator formals.
  npar <- formals(data_function)$n_signal_parameters +
    formals(data_function)$noise_parameters
  default_start_value <- max(10L, 10L * npar)

  # 2. Inspect data_function formals to infer outcome type
  formals_list <- formals(data_function)
  args_names <- names(formals_list)

  metric_used <- attr(metric_function, "metric")
  if (is.null(metric_used)) {
    return(
      list(
        npar = npar,
        metric_used = NULL,
        start_min_sample_size = default_start_value,
        start_max_sample_size = NA
      )
    )
  }

  if (metric_used == "csse") {
    metric_used <- "calib_slope"
    target_performance <- 1 - sqrt(abs(target_performance))
  }

  # Survival outcome
  if ("censoring_rate" %in% args_names) {
    censoring_rate <- eval(
      formals_list[["censoring_rate"]],
      environment(data_function)
    )

    if (metric_used == "cindex") {
      prev_min_sample_size <- get_min_sample_size(
        npar = npar,
        prevalence = 1 - censoring_rate,
        c_stat = target_performance,
        calib_slope = NULL,
        epv_value = 3 * (1 - censoring_rate),
        outcome_type = "survival"
      )

      prev_max_sample_size <- 100 * npar
    } else {
      prev_min_sample_size <- get_min_sample_size(
        npar = npar,
        prevalence = 1 - censoring_rate,
        c_stat = c_statistic,
        calib_slope = target_performance,
        epv_value = 10,
        outcome_type = "survival"
      )

      prev_max_sample_size <- 10 * prev_min_sample_size
    }

    # Binary outcome
  } else if ("baseline_prob" %in% args_names) {
    baseline_prob <- eval(
      formals_list[["baseline_prob"]],
      envir = environment(data_function)
    )

    # Validate baseline_prob
    if (
      !is.numeric(baseline_prob) ||
        length(baseline_prob) != 1 ||
        is.na(baseline_prob)
    ) {
      stop("baseline_prob must be a single numeric value (not NA).")
    }
    if (baseline_prob <= 0 || baseline_prob >= 1) {
      stop("baseline_prob must be between 0 and 1 (exclusive).")
    }

    if (metric_used == "auc") {
      epv_val <- 3 * baseline_prob

      prev_min_sample_size <- get_min_sample_size(
        npar = npar,
        prevalence = baseline_prob,
        c_stat = target_performance,
        calib_slope = NULL,
        epv_value = epv_val,
        outcome_type = "binary"
      )

      prev_max_sample_size <- 100 * npar
    } else {
      if (
        baseline_prob <= 0.2 &&
          c_statistic <= 0.7 &&
          mean_or_assurance == "assurance"
      ) {
        epv_val <- 30L

        prev_min_sample_size <- get_min_sample_size(
          npar = npar,
          prevalence = baseline_prob,
          c_stat = c_statistic,
          calib_slope = target_performance,
          epv_value = epv_val,
          outcome_type = "binary"
        )

        prev_max_sample_size <- 5 * prev_min_sample_size
      } else if (baseline_prob <= 0.2) {
        epv_val <- 10L

        prev_min_sample_size <- get_min_sample_size(
          npar = npar,
          prevalence = baseline_prob,
          c_stat = c_statistic,
          calib_slope = target_performance,
          epv_value = epv_val,
          outcome_type = "binary"
        )

        prev_max_sample_size <- 2 * prev_min_sample_size
      } else {
        epv_val <- 10L

        prev_min_sample_size <- get_min_sample_size(
          npar = npar,
          prevalence = baseline_prob,
          c_stat = target_performance,
          calib_slope = NULL,
          epv_value = epv_val,
          outcome_type = "binary"
        )

        prev_max_sample_size <- 10 * prev_min_sample_size
      }
    }

    # Continuous outcome
  } else {
    if (metric_used == "calib_slope") {
      prev_min_sample_size <- get_min_sample_size(
        npar = npar,
        prevalence = NULL,
        c_stat = NULL,
        calib_slope = target_performance,
        outcome_type = "continuous"
      )

      prev_max_sample_size <- 100 * npar
    } else {
      prev_min_sample_size <- get_min_sample_size(
        npar = npar,
        prevalence = NULL,
        c_stat = target_performance,
        calib_slope = NULL,
        outcome_type = "continuous"
      )

      if (target_performance <= 0.5) {
        prev_max_sample_size <- 200 * npar
      } else {
        prev_max_sample_size <- 100 * npar
      }
    }
  }

  # Return results
  list(
    npar = npar,
    metric_used = metric_used,
    start_min_sample_size = prev_min_sample_size,
    start_max_sample_size = prev_max_sample_size
  )
}

#' get_min_sample_size: Heuristic starting-n for binary/continuous/survival prediction
#'
#' @param npar Integer; number of predictors in the model.
#' @param prevalence Numeric in `[0, 1]`; optional event rate or case fraction
#'   used for EPV calculations.
#' @param c_stat Numeric in (0.5, 1]; anticipated discrimination (C-statistic). Lower values inflate the heuristic.
#' @param calib_slope Numeric; anticipated calibration slope. Values below 1 trigger a modest inflation.
#' @param epv_value Numeric; target events-per-variable (EPV) value applied when prevalence is supplied.
#' @param outcome_type Character string; must be one of `"binary"`, `"survival"`, or `"continuous"`.
#' @return Integer recommended starting value from which to calculate the minimum sample size.
#' @keywords internal
get_min_sample_size <- function(
  npar,
  prevalence = NULL,
  c_stat = NULL,
  calib_slope = NULL,
  epv_value = NULL,
  outcome_type = c("binary", "survival", "continuous")
) {
  outcome_type <- match.arg(outcome_type)

  # --- 1) Base rule: 3 * npar (absolute minimum)
  n0 <- 3 * npar

  # --- 2) Outcome-specific rules ---
  if (outcome_type == "binary") {
    # Recommended: ≥10 EPV (Riley et al., 2020)
    epv <- epv_value
    if (!is.null(prevalence) && prevalence > 0 && prevalence < 1) {
      n_epv <- round(epv * npar / prevalence)
      # Optional adjustments:
      if (!is.null(c_stat)) {
        if (c_stat <= 0 || c_stat > 1) {
          warning("c_stat should be between 0 and 1.")
        }
        # Lower c-statistic → require more data (simple heuristic)
        adj <- 1 / max(c_stat, 0.5) # avoid extreme inflation
        n_epv <- round(n_epv * adj)
      }
    } else {
      warning("Prevalence not provided or invalid; assuming 50% events.")
      n_epv <- round(epv * npar / 0.5)
      # Optional adjustments:
      if (!is.null(c_stat)) {
        if (c_stat <= 0 || c_stat > 1) {
          warning("c_stat should be between 0 and 1.")
        }
        # Lower c-statistic → require more data (simple heuristic)
        adj <- 1 / max(c_stat, 0.5) # avoid extreme inflation
        n_epv <- round(n_epv * adj)
      }
    }
    n0 <- max(n0, n_epv)
  } else if (outcome_type == "survival") {
    # Recommended: ≥20 EPV (Riley et al., 2020)

    epv <- epv_value
    if (!is.null(prevalence) && prevalence > 0 && prevalence < 1) {
      n_epv <- round(epv * npar / prevalence)

      # Optional adjustments:
      if (!is.null(c_stat)) {
        if (c_stat <= 0 || c_stat > 1) {
          warning("c_stat should be between 0 and 1.")
        }
        # Lower c-statistic → require more data (simple heuristic)
        adj <- 1 / max(c_stat, 0.5) # avoid extreme inflation
        n_epv <- round(n_epv * adj)
      }
    } else {
      warning("Event proportion not provided; assuming 50% events.")
      n_epv <- round(epv * npar / 0.5)
      # Optional adjustments:
      if (!is.null(c_stat)) {
        if (c_stat <= 0 || c_stat > 1) {
          warning("c_stat should be between 0 and 1.")
        }
        # Lower c-statistic → require more data (simple heuristic)
        adj <- 1 / max(c_stat, 0.5) # avoid extreme inflation
        n_epv <- round(n_epv * adj)
      }
    }

    n0 <- max(n0, n_epv)
  } else if (outcome_type == "continuous") {
    # Continuous outcome: ≥20 obs per predictor (Steyerberg, 2019)
    n_cont <- 3 * npar

    # Optional adjustments:
    if (!is.null(c_stat)) {
      if (c_stat <= 0 || c_stat > 1) {
        warning("c_stat should be between 0 and 1.")
      }
      # Lower c-statistic → require more data (simple heuristic)
      adj <- 1 / max(c_stat, 0.2) # avoid extreme inflation
      n_cont <- round(n_cont * adj)
    }

    if (!is.null(calib_slope)) {
      if (calib_slope > 0 && calib_slope < 1) {
        # Lower slope means more shrinkage needed → increase N slightly
        if (npar > 10) {
          adj <- 1 + (1 - calib_slope)
        } else {
          adj <- 1 + (1 - calib_slope)
        }
        n_cont <- round(n_cont * adj)
      }
    }

    n0 <- max(n0, n_cont)
  }

  return(as.integer(n0))
}
