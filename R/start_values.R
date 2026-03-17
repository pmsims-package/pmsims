#' Get Performance
#'
#' @param results
#' @param p
#' @param mean
#'
#' @returns
#' @export
#'
#' @examples
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

  ## --- Find min value ---
  ordered_by_ul <- bisection_summary[
    order(bisection_summary[, "ul"], decreasing = TRUE),
  ]
  below_target <- ordered_by_ul[ordered_by_ul[, "ul"] < target, , drop = FALSE]

  if (nrow(below_target) == 0) {
    min_value <- min(bisection_summary[, "n"] * 0.8)
  } else {
    min_value <- max(below_target[, "n"])
  }

  ## --- Find max value ---
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


#### New adaptive start values code

#' Adaptive starting value searching (model/metrics) agnostic.
#'
#' @param data_function
#' @param model_function
#' @param metric_function
#' @param value_on_error
#' @param start_n
#' @param test_n
#' @param n_reps_per
#' @param n_reps_total
#' @param target_performance
#' @param threshold
#' @param mean_or_assurance
#' @param c_statistic
#' @param parallel
#' @param cores
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
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
  c_statistic = NULL,
  parallel = FALSE,
  cores = 20,
  verbose = FALSE
) {
  # ---------------------------------------------------------
  # Budget
  # ---------------------------------------------------------
  max_iter <- floor(n_reps_total / n_reps_per)

  # ---------------------------------------------------------
  # Fixed test set
  # ---------------------------------------------------------
  test_data <- data_function(test_n)

  # ---------------------------------------------------------
  # Single run
  # ---------------------------------------------------------
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

  # ---------------------------------------------------------
  # Summary at n
  # ---------------------------------------------------------
  summary_at_n <- function(n) {
    if (parallel) {
      cl <- parallel::makeCluster(cores)
      doParallel::registerDoParallel(cl)

      vals <- foreach::foreach(i = 1:n_reps_per, .combine = c) %dopar%
        {
          single_run(n)
        }

      parallel::stopCluster(cl)
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

  # ---------------------------------------------------------
  # Initial evaluation
  # ---------------------------------------------------------
  iter <- 1
  track <- list()

  res <- summary_at_n(start_n)
  perf <- res$y_summary

  track[[iter]] <- list(n = start_n, performance = perf, raw = res$y)

  if (verbose) {
    message(sprintf("Iter %d | n = %d | perf = %.4f", iter, start_n, perf))
  }

  # ---------------------------------------------------------
  # Decide direction
  # ---------------------------------------------------------
  if (perf < target_performance) {
    direction <- "up"
    lower_n <- start_n
    lower_perf <- perf
    upper_n <- NA
    upper_perf <- NA
  } else {
    direction <- "down"
    upper_n <- start_n
    upper_perf <- perf
    lower_n <- NA
    lower_perf <- NA
  }

  n_current <- start_n

  # ---------------------------------------------------------
  # Adaptive loop
  # ---------------------------------------------------------
  while (iter < max_iter) {
    iter <- iter + 1

    if (direction == "up") {
      n_new <- n_current * 2
    } else {
      n_new <- max(1, floor(n_current / 2))
    }

    # Stop if no movement
    if (n_new == n_current) {
      break
    }

    res <- summary_at_n(n_new)
    perf <- res$y_summary

    track[[iter]] <- list(n = n_new, performance = perf, raw = res$y)

    if (verbose) {
      message(sprintf("Iter %d | n = %d | perf = %.4f", iter, n_new, perf))
    }

    if (direction == "up") {
      if (perf >= target_performance - threshold) {
        upper_n <- n_new
        upper_perf <- perf
        break
      } else {
        lower_n <- n_new
        lower_perf <- perf
      }
    } else {
      if (perf <= target_performance + threshold) {
        lower_n <- n_new
        lower_perf <- perf
        break
      } else {
        upper_n <- n_new
        upper_perf <- perf
      }
    }

    n_current <- n_new
  }

  # ---------------------------------------------------------
  # Return bounds
  # ---------------------------------------------------------
  list(
    min_sample_size = lower_n,
    min_sample_size_perf = lower_perf,
    max_sample_size = upper_n,
    max_sample_size_perf = upper_perf,
    iterations = iter,
    max_iter = max_iter,
    track = track
  )
}


#' Get initial starting values before using adaptive searching
#'
#' @param data_function
#' @param metric_function
#' @param target_performance
#' @param c_statistic
#' @param mean_or_assurance
#'
#' @returns
#' @export
#'
#' @examples
compute_start_sample_sizes <- function(
  data_function,
  metric_function,
  target_performance,
  c_statistic = NULL,
  mean_or_assurance = c("mean", "assurance")
) {
  mean_or_assurance <- match.arg(mean_or_assurance)

  # 1. Number of predictors (exclude outcome column)
  npar <- dim(data_function(1))[2] - 1

  # 2. Inspect data_function formals to infer outcome type
  formals_list <- formals(data_function)
  args_names <- names(formals_list)

  metric_used <- attr(metric_function, "metric")
  if (is.null(metric_used)) {
    stop("metric_function must have a 'metric' attribute.")
  }

  ## -----------------------
  ## SURVIVAL OUTCOME
  ## -----------------------
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

    ## -----------------------
    ## BINARY OUTCOME
    ## -----------------------
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

    ## -----------------------
    ## CONTINUOUS OUTCOME
    ## -----------------------
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
#' @param prevalence Numeric in [0, 1]; optional event rate or case fraction used for EPV calculations.
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
          #adj <- 1 / (1 - calib_slope)
          adj <- 1 + (1 - calib_slope)
        }
        n_cont <- round(n_cont * adj)
      }
    }

    n0 <- max(n0, n_cont)
  }

  return(as.integer(n0))
}
