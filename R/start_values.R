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

#' Evaluate an expression under a fixed RNG seed, restoring the stream after
#'
#' @param seed Integer seed.
#' @param expr Expression to evaluate.
#' @return The value of `expr`.
#' @keywords internal
#' @noRd
with_preserved_seed <- function(seed, expr) {
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit(
      suppressWarnings(rm(".Random.seed", envir = globalenv())),
      add = TRUE
    )
  }
  set.seed(seed)
  force(expr)
}

#' Calculate adaptive start bounds
#'
#' Derive lower and upper sample-size bounds by simulating model performance
#' over a geometric ladder of sample sizes.
#'
#' The bounds this function returns are *search bounds* for the second stage,
#' not estimates in their own right. They are therefore placed at sample sizes
#' whose performance is separated from `target_performance` by several Monte
#' Carlo standard errors, rather than at the first sample size whose noisy
#' estimate happens to cross the target. This is what keeps the bounds stable:
#' with `n_reps_per` replications the standard error of a calibration-slope
#' summary is typically 0.02-0.03, so a bare `estimate >= target` comparison at
#' a sample size near the crossing point is close to a coin flip, and because
#' the ladder is geometric one mis-call moves both bounds by a factor of two.
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
#' @param threshold Numeric minimum half-width of the band around
#'   `target_performance` within which a sample size is treated as too close to
#'   call. The band actually used is
#'   `max(threshold, conf_z * se)`, where `se` is the Monte Carlo standard error
#'   of the performance summary at that sample size.
#' @param mean_or_assurance Character string selecting the mean or
#'   20th-percentile performance summary.
#' @param plateau_k Positive integer number of recent iterations used to detect
#'   a performance plateau.
#' @param plateau_tol Numeric minimum change treated as a plateau. As with
#'   `threshold`, the tolerance actually used is the larger of `plateau_tol` and
#'   twice the Monte Carlo standard error, so that Monte Carlo noise is not
#'   mistaken for a real gain (or a real gain for a plateau).
#' @param seed Optional integer seed. When supplied, the whole stage -- the test
#'   dataset and every replication -- is generated from this seed, so the bounds
#'   are identical on every run regardless of the calling session's RNG state.
#'   The caller's RNG stream is restored on exit. Set to `NULL` to follow the
#'   global stream.
#' @param conf_z Numeric number of standard errors a sample size must be away
#'   from `target_performance` before it is used as a bound.
#' @param max_reps_per Optional positive integer cap on the replications spent
#'   at a single sample size. When a sample size is too close to call, the
#'   search buys more replications there (doubling up to this cap) instead of
#'   deciding on a noisy estimate. Defaults to `4 * n_reps_per`.
#' @param winsorise Logical; whether to winsorise replicate metric values at
#'   the median plus or minus five median absolute deviations before
#'   summarising. This bounds the influence of the occasional separated or
#'   near-degenerate fit, which for ratio-type metrics such as the calibration
#'   slope can otherwise move the mean substantially.
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
#' @return A list containing lower and upper sample-size bounds, the associated
#'   performance summaries, and the search trace. Each element of `track` also
#'   carries the Monte Carlo standard error `se`, the number of replications
#'   `reps` spent, the number of failed replications `n_fail`, and the
#'   classification `call` (`"below"`, `"above"` or `"uncertain"`).
#' @noRd
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
    seed = NULL,
    conf_z = 2,
    max_reps_per = NULL,
    winsorise = TRUE,
    # Optional large-sample pre-check. When enabled, the search first evaluates
    # performance at large_n. If the target is more than large_n_tol above that
    # estimate, it stops and reports the target as unreachable.
    #
    # Disabled by default because the preliminary large-sample fit did not work
    # reliably for some machine-learning models.
    large_perf_check = FALSE,
    large_n = NULL,
    large_n_tol = 0.05,
    c_statistic = NULL,
    parallel = FALSE,
    cores = 20,
    verbose = FALSE
) {
  vcat <- function(...) if (verbose) message(sprintf(...))
  
  # -- Reproducibility ------------------------------------------------------
  # The adaptive stage only produces search bounds for the second stage, so
  # running it under a fixed seed costs nothing statistically and removes the
  # run-to-run variability in the bounds entirely. The caller's RNG stream is
  # left exactly as it was found.
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      caller_seed <- get(".Random.seed", envir = globalenv())
      on.exit(
        assign(".Random.seed", caller_seed, envir = globalenv()),
        add = TRUE
      )
    }
    set.seed(seed)
  }
  
  max_iter <- floor(n_reps_total / n_reps_per)
  max_reps_per <- if (is.null(max_reps_per)) {
    4L * as.integer(n_reps_per)
  } else {
    max(as.integer(max_reps_per), as.integer(n_reps_per))
  }
  
  # The test dataset is drawn once and reused at every sample size, so its own
  # sampling error is a run-level shift applied to the whole performance curve
  # (empirically r > 0.99 between sample sizes) and is not reduced by
  # n_reps_per. Keep it large.
  if (is.finite(test_n) && test_n < 5000) {
    warning(
      sprintf(
        paste(
          "test_n = %d is small for the adaptive start-value search. The test",
          "set is drawn once and reused at every sample size, so its own",
          "sampling error shifts the whole performance curve and is not reduced",
          "by n_reps_per. Values below ~5000 make the search bounds noticeably",
          "run-dependent, particularly for the calibration slope."
        ),
        as.integer(test_n)
      ),
      call. = FALSE
    )
  }
  
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
  
  draw_reps <- function(n, reps) {
    if (parallel) {
      require_optional_packages(
        c("doParallel", "foreach"),
        "parallel adaptive-bound calculations"
      )
      
      cl <- parallel::makeCluster(cores)
      doParallel::registerDoParallel(cl)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      foreach::`%dopar%`(
        foreach::foreach(i = seq_len(reps), .combine = c),
        {
          single_run(n)
        }
      )
    } else {
      vapply(seq_len(reps), function(i) single_run(n), FUN.VALUE = numeric(1))
    }
  }
  
  # -- Summary with its Monte Carlo standard error --------------------------
  summarise_vals <- function(vals, n) {
    ok <- vals[is.finite(vals)]
    m <- length(ok)
    
    if (m == 0L) {
      stop(
        sprintf(
          paste(
            "Adaptive start value search produced a non-finite",
            "performance summary at n = %d"
          ),
          n
        ),
        call. = FALSE
      )
    }
    
    if (isTRUE(winsorise) && m >= 5L) {
      centre <- stats::median(ok)
      spread <- stats::mad(ok)
      if (is.finite(spread) && spread > 0) {
        ok <- pmin(pmax(ok, centre - 5 * spread), centre + 5 * spread)
      }
    }
    
    if (mean_or_assurance == "mean") {
      est <- mean(ok)
      se <- if (m > 1L) stats::sd(ok) / sqrt(m) else 0
    } else {
      est <- as.numeric(stats::quantile(ok, probs = 0.20, type = 7))
      # The 20th percentile has no closed-form standard error here; bootstrap it.
      se <- if (m > 1L) {
        stats::sd(replicate(
          200L,
          stats::quantile(sample(ok, m, replace = TRUE), probs = 0.20, type = 7)
        ))
      } else {
        0
      }
    }
    
    if (length(est) != 1L || !is.finite(est)) {
      stop(
        sprintf(
          paste(
            "Adaptive start value search produced a non-finite",
            "performance summary at n = %d"
          ),
          n
        ),
        call. = FALSE
      )
    }
    
    list(
      n = n,
      est = est,
      se = if (is.finite(se)) se else 0,
      reps = length(vals),
      n_fail = sum(!is.finite(vals)),
      vals = vals
    )
  }
  
  # Replicate values are cached per sample size so that refining a sample size
  # adds replications rather than discarding the ones already paid for.
  cache <- new.env(parent = emptyenv())
  budget <- n_reps_total
  
  cache_key <- function(n) format(n, scientific = FALSE)
  
  cached_vals <- function(n) {
    have <- cache[[cache_key(n)]]
    if (is.null(have)) numeric(0) else have
  }
  
  evaluate_at <- function(n, reps) {
    have <- cached_vals(n)
    if (length(have) < reps) {
      have <- c(have, draw_reps(n, reps - length(have)))
      cache[[cache_key(n)]] <- have
    }
    summarise_vals(have[seq_len(min(reps, length(have)))], n)
  }
  
  classify <- function(s) {
    band <- max(threshold, conf_z * s$se)
    if (s$est > target_performance + band) {
      "above"
    } else if (s$est < target_performance - band) {
      "below"
    } else {
      "uncertain"
    }
  }
  
  # Evaluate a sample size, buying more replications while the call is too
  # close to make confidently and the budget allows.
  assess <- function(n) {
    reps <- as.integer(n_reps_per)
    repeat {
      before <- length(cached_vals(n))
      s <- evaluate_at(n, reps)
      budget <<- budget - (length(cached_vals(n)) - before)
      s$call <- classify(s)
      if (
        s$call != "uncertain" || reps >= max_reps_per || budget < reps
      ) {
        break
      }
      reps <- min(max_reps_per, reps * 2L)
    }
    s
  }
  
  track <- list()
  push <- function(s) {
    track[[length(track) + 1L]] <<- list(
      n = s$n,
      performance = s$est,
      se = s$se,
      reps = s$reps,
      n_fail = s$n_fail,
      call = s$call,
      raw = s$vals
    )
    vcat("n = %s | perf = %.4f (se %.4f) | %s", format(s$n), s$est, s$se, s$call)
  }
  
  have_call <- function(what) {
    any(vapply(track, function(z) identical(z$call, what), logical(1)))
  }
  
  stop_reason <- "budget_exhausted"
  max_achievable_perf <- NA_real_
  
  # -- Optional large-sample pre-check --------------------------------------
  if (isTRUE(large_perf_check)) {
    if (is.null(large_n)) {
      large_n <- start_n * 2^max(1L, max_iter - 1L)
    }
    s <- assess(large_n)
    push(s)
    
    if (target_performance - s$est > large_n_tol) {
      return(list(
        min_sample_size = as.numeric(large_n),
        min_sample_size_perf = s$est,
        max_sample_size = as.numeric(large_n),
        max_sample_size_perf = s$est,
        max_achievable_perf = s$est,
        stop_reason = "target_unreachable",
        iterations = length(track),
        reps_used = n_reps_total - budget,
        max_iter = max_iter,
        track = track
      ))
    }
    n_up <- n_down <- as.numeric(large_n)
  } else {
    s <- assess(start_n)
    push(s)
    n_up <- n_down <- as.numeric(start_n)
  }
  
  # -- Ladder ---------------------------------------------------------------
  # Walk outwards until BOTH a confidently-below and a confidently-above sample
  # size are in hand. Sample sizes that are too close to call belong to neither
  # and so end up inside the returned bracket, which is the conservative
  # direction: a bracket that is one rung too wide costs the second stage a
  # little time, whereas one that excludes the crossing point cannot be
  # recovered from.
  while (budget >= n_reps_per && length(track) < max_iter) {
    if (!have_call("above")) {
      n_up <- n_up * 2
      if (!is.finite(n_up)) {
        stop_reason <- "no_movement"
        break
      }
      push(assess(n_up))
    } else if (!have_call("below")) {
      n_next <- max(1, floor(n_down / 2))
      if (n_next == n_down) {
        stop_reason <- "no_movement"
        break
      }
      n_down <- n_next
      push(assess(n_down))
    } else {
      stop_reason <- "target_bracketed"
      break
    }
    
    # -- Plateau check, judged against the noise level ----------------------
    if (length(track) >= plateau_k + 1L && !have_call("above")) {
      by_n <- track[order(vapply(track, `[[`, numeric(1), "n"))]
      recent <- utils::tail(by_n, plateau_k + 1L)
      gains <- diff(vapply(recent, `[[`, numeric(1), "performance"))
      noise <- max(
        plateau_tol,
        2 * max(vapply(recent, `[[`, numeric(1), "se"), na.rm = TRUE)
      )
      if (all(abs(gains) < noise)) {
        stop_reason <- "plateau"
        max_achievable_perf <- max(
          vapply(track, `[[`, numeric(1), "performance"),
          na.rm = TRUE
        )
        break
      }
    }
  }
  
  # -- Bounds, derived from the whole trace ---------------------------------
  # Taken from the sorted trace rather than from whatever the loop state
  # happened to be when it broke, so the result does not depend on the path
  # taken to get there.
  ns <- vapply(track, `[[`, numeric(1), "n")
  perfs <- vapply(track, `[[`, numeric(1), "performance")
  calls <- vapply(track, `[[`, character(1), "call")
  
  below_ns <- ns[calls == "below"]
  above_ns <- ns[calls == "above"]
  
  lower_n <- if (length(below_ns)) max(below_ns) else max(1, min(ns) / 2)
  upper_n <- if (length(above_ns)) min(above_ns) else max(ns) * 2
  if (!(lower_n < upper_n)) {
    lower_n <- max(1, upper_n / 2)
  }
  
  perf_at <- function(x) {
    i <- which(ns == x)
    if (length(i)) perfs[i[1]] else NA_real_
  }
  
  list(
    min_sample_size = as.numeric(round(lower_n)),
    min_sample_size_perf = perf_at(lower_n),
    max_sample_size = as.numeric(round(upper_n)),
    max_sample_size_perf = perf_at(upper_n),
    max_achievable_perf = max_achievable_perf,
    stop_reason = stop_reason,
    iterations = length(track),
    reps_used = n_reps_total - budget,
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
    metric_used <- "calibration_slope"
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
        calibration_slope = NULL,
        epv_value = 5 * (1 - censoring_rate),
        outcome_type = "survival"
      )
      
      prev_max_sample_size <- 100 * npar
    } else {
      prev_min_sample_size <- get_min_sample_size(
        npar = npar,
        prevalence = 1 - censoring_rate,
        c_stat = c_statistic,
        calibration_slope = target_performance,
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
        calibration_slope = NULL,
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
          calibration_slope = target_performance,
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
          calibration_slope = target_performance,
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
          calibration_slope = NULL,
          epv_value = epv_val,
          outcome_type = "binary"
        )
        
        prev_max_sample_size <- 10 * prev_min_sample_size
      }
    }
    
    # Continuous outcome
  } else {
    if (metric_used == "calibration_slope") {
      prev_min_sample_size <- get_min_sample_size(
        npar = npar,
        prevalence = NULL,
        c_stat = NULL,
        calibration_slope = target_performance,
        outcome_type = "continuous"
      )
      
      prev_max_sample_size <- 100 * npar
    } else {
      prev_min_sample_size <- get_min_sample_size(
        npar = npar,
        prevalence = NULL,
        c_stat = target_performance,
        calibration_slope = NULL,
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
#' @param calibration_slope Numeric; anticipated calibration slope. Values below 1 trigger a modest inflation.
#' @param epv_value Numeric; target events-per-variable (EPV) value applied when prevalence is supplied.
#' @param outcome_type Character string; must be one of `"binary"`, `"survival"`, or `"continuous"`.
#' @return Integer recommended starting value from which to calculate the minimum sample size.
#' @keywords internal
get_min_sample_size <- function(
    npar,
    prevalence = NULL,
    c_stat = NULL,
    calibration_slope = NULL,
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
    
    if (!is.null(calibration_slope)) {
      if (calibration_slope > 0 && calibration_slope < 1) {
        # Lower slope means more shrinkage needed → increase N slightly
        if (npar > 10) {
          adj <- 1 + (1 - calibration_slope)
        } else {
          adj <- 1 + (1 - calibration_slope)
        }
        n_cont <- round(n_cont * adj)
      }
    }
    
    n0 <- max(n0, n_cont)
  }
  
  return(as.integer(n0))
}
