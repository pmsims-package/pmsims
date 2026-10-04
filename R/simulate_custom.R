#' Minimum sample size for custom simulation workflows
#'
#' Compute the minimum sample size required to achieve a target level of
#' predictive performance using user-defined simulation components.
#' `simulate_custom()` is the low-level interface in `pmsims`: users supply a
#' data-generating function, a model-fitting function, and a metric function,
#' and the chosen search engine estimates the smallest \eqn{n} meeting the
#' selected performance criterion.
#'
#' @param data_function Function taking a single argument, `n`, giving the
#'   training sample size, and returning a dataset that can be passed to
#'   `model_function`.
#' @param model_function Function that fits a model to the dataset returned by
#'   `data_function`. It must take the generated dataset as its only argument
#'   and return a fitted model object.
#' @param metric_function Function that evaluates predictive performance on test
#'   data. It must take three positional arguments in the order
#'   `(test_data, fitted_model, model_name)` and return a single numeric value.
#'   Optionally, users may set `attr(metric_function, "value_on_error")` to a
#'   single numeric fallback value to be returned if model fitting or metric
#'   evaluation fails during a simulation run.
#' @param target_performance Numeric target value for the chosen performance
#'   metric. The search aims to find the smallest sample size \eqn{n} for which
#'   the selected criterion is met relative to this threshold.
#' @param c_statistic Optional numeric value used only by the internal
#'   start-value heuristics for some outcome and metric combinations. In most
#'   custom workflows this should be left as `NULL`.
#' @param mean_or_assurance Character string specifying the criterion used to
#'   define the minimum sample size. Must be either `"mean"` or `"assurance"`.
#' @param test_n Integer size of the test dataset used to evaluate model
#'   performance. This should usually be large enough that test-set variability
#'   is negligible relative to the training-sample search.
#' @param min_sample_size Optional integer lower bound for the sample-size
#'   search. If supplied, `max_sample_size` must also be supplied.
#' @param max_sample_size Optional integer upper bound for the sample-size
#'   search. If supplied, `min_sample_size` must also be supplied.
#'   Supplying both bounds defines the search space directly, so the adaptive
#'   starting-value search is skipped. Because the runtime estimate is
#'   extrapolated from that stage, no long-run warning is issued either.
#' @param n_reps_total Integer total number of simulation replications allocated
#'   to the search. The search evaluates approximately
#'   `n_reps_total / n_reps_per` candidate sample sizes.
#' @param n_reps_per Integer number of simulation replications performed at each
#'   candidate sample size.
#' @param method Character string specifying the search engine: `"curve"`
#'   (default; fits a learning curve to all replicates and searches where it
#'   crosses the target, see Details), `"mlpwr"` (adaptive start values, then
#'   mlpwr's Gaussian-process search), `"bisection"` or `"mlpwr-bs"`.
#' @param progress Logical flag controlling whether the `mlpwr` progress bar is
#'   shown for `mlpwr`-based methods.
#' @param verbose Logical flag controlling engine-specific diagnostic output
#'   when supported. For the bisection engine, setting `verbose = TRUE` stores
#'   the iteration history on the returned object.
#' @param verify_reps Integer number of fresh replicates simulated at the
#'   returned sample size to check that it meets the target (see `status`).
#'   Set to `0` to skip the check.
#' @param max_n Largest sample size the search may try. If performance there is
#'   still clearly below the target, the search stops with status
#'   `"not_bracketed"`. Defaults to 200,000 for random forests and xgboost
#'   (one batch of replicates at a million rows would take hours) and
#'   1,000,000 otherwise.
#' @param ... Additional arguments passed to the selected search engine.
#'
#' @return An object of class `"pmsims"` containing the estimated minimum
#'   sample size `min_n` (numeric; `NA` when no sample size could be found)
#'   and a `status`:
#'   \describe{
#'     \item{`"ok"`}{The search found `min_n`, and simulating it again
#'       confirmed the target is met (or the check was skipped).}
#'     \item{`"not_verified"`}{Simulating `min_n` again gave performance
#'       clearly below the target, so `min_n` is likely too small.}
#'     \item{`"not_bracketed"`}{No sample size up to `max_n` met the target,
#'       or the search returned no sample size.}
#'     \item{`"replicates_failed"`}{At least half the simulation replicates
#'       failed to fit or score the model, so no sample size was estimated.}
#'   }
#'   `status_message` explains a status other than `"ok"`, `verification`
#'   holds the check at `min_n`, and `diagnostics` records the search bounds,
#'   whether `min_n` lies on one of them, mlpwr's warnings, Gaussian-process
#'   restarts and failed replicates.
#'
#' @details
#' With `method = "curve"`, every replicate is kept and a monotone learning
#' curve \eqn{C(n) = a - b n^{-c}} is fitted to the criterion at each sample
#' size evaluated (weighted by a smoothed model of the replicate spread; the
#' ceiling \eqn{a} is capped at the metric's maximum). A pilot doubles
#' (or halves) from a heuristic start until the criterion is seen on both sides
#' of the target; each further batch of `n_reps_per` replicates is placed where
#' the fitted curve crosses the target. The search range is therefore never
#' fixed. The answer is where the final fitted curve crosses the target. A
#' target is declared unreachable if a bootstrap upper bound for the curve's
#' ceiling \eqn{a} is below it; the search also stops (status
#' `"not_bracketed"`) when the crossing is very likely beyond `max_n`. These
#' stops extrapolate the fitted curve and should be read as heuristics.
#'
#' The answer is a median-unbiased estimate of where the criterion reaches the
#' target (the mlpwr engine instead picks where its surrogate's mean plus 0.3
#' standard deviations does, which tends to give smaller sample sizes). The
#' interval in `diagnostics$curve$n_ci` reflects Monte Carlo error given the
#' learning-curve shape, not uncertainty about the shape; a shape-free check
#' (`diagnostics$crosscheck_n`) is reported alongside, and flagged when it
#' differs by more than 10%. The verification of the answer has limited power
#' for skewed metrics such as CSSE: passing it does not show that the target
#' is met.
#'
#' @section Random numbers:
#' Each simulation replicate runs on its own random-number stream, derived from
#' a single draw from the session's RNG. Results are therefore reproducible
#' with [set.seed()], and a replicate's data do not depend on how many random
#' numbers other parts of the search consumed.
#'
#' @section How the mlpwr engine picks its answer:
#' mlpwr returns the smallest sample size at which its Gaussian-process
#' surrogate's mean plus 0.3 of its standard deviation reaches the target
#' (fixed inside mlpwr). Where the surrogate is uncertain, this is below the
#' sample size at which the criterion is expected to reach the target: in
#' benchmark simulations against directly simulated reference sample sizes,
#' answers were a median of about 9% too small, and more for penalised and
#' machine-learning models. The verification step (status `"not_verified"`)
#' catches only answers clearly below the target. Whether to correct for this
#' is an open question.
#'
#' @seealso [simulate_binary()], [simulate_continuous()], [simulate_survival()]
#'
#' @examples
#' # Three independent predictors with a population R-squared of 0.5.
#' data_fun <- function(n) {
#'   x1 <- rnorm(n)
#'   x2 <- rnorm(n)
#'   x3 <- rnorm(n)
#'   y <- (x1 + x2 + x3) / sqrt(3) + rnorm(n)
#'   data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
#' }
#'
#' model_fun <- function(dat) {
#'   stats::lm(y ~ ., data = dat)
#' }
#'
#' # Calibration slope evaluated on independent test data.
#' metric_fun <- function(test_data, fit, model) {
#'   preds <- stats::predict(fit, newdata = test_data)
#'   unname(stats::coef(stats::lm(test_data$y ~ preds))[2])
#' }
#' attr(metric_fun, "metric") <- "calibration_slope"
#'
#' \donttest{
#' set.seed(123)
#' est <- simulate_custom(
#'   data_function = data_fun,
#'   model_function = model_fun,
#'   metric_function = metric_fun,
#'   target_performance = 0.9,
#'   mean_or_assurance = "assurance",
#'   min_sample_size = 25,
#'   max_sample_size = 1000,
#'   n_reps_total = 1000,
#'   test_n = 30000,
#'   progress = FALSE
#' )
#' est
#' est$min_n
#' plot(est)
#' }
#' @export
simulate_custom <- function(
  data_function,
  model_function,
  metric_function,
  target_performance,
  c_statistic = NULL,
  mean_or_assurance = "assurance",
  test_n = 30000,
  min_sample_size = NULL,
  max_sample_size = NULL,
  n_reps_total = 1000,
  n_reps_per = 20,
  method = "curve",
  progress = TRUE,
  verbose = FALSE,
  verify_reps = 100,
  max_n = NULL,
  ...
) {
  # Evaluate four initial sample sizes after establishing the search bounds.
  n_init <- 4
  se_final <- NULL # Reserved for internal engine use.

  if (is.null(data_function)) {
    stop("data_function missing")
  }

  if (is.null(n_reps_total)) {
    stop("'n_reps_total' must be specified.")
  }

  # Validate the optional sample-size bounds.
  if (
    (!is.null(min_sample_size) && is.null(max_sample_size)) ||
      (is.null(min_sample_size) && !is.null(max_sample_size))
  ) {
    stop(
      "min_sample_size and max_sample_size must either both be positive integers or both set to NULL"
    )
  }

  if (
    !is.null(min_sample_size) &&
      !is.null(max_sample_size) &&
      min_sample_size > max_sample_size
  ) {
    stop("min_sample_size must be less than max_sample_size")
  }

  if (!is.null(min_sample_size)) {
    cli::cli_alert_info(
      "Using user-specified min_sample_size and max_sample_size. \\
       Adaptive starting values will not be used."
    )
  }

  if ((mean_or_assurance %in% c("mean", "assurance")) == FALSE) {
    stop("mean_or_assurance must be either 'mean' or 'assurance'")
  }

  check_metric_direction(attr(metric_function, "metric", exact = TRUE))
  if (is.null(max_n)) {
    max_n <- default_max_n(attr(model_function, "model", exact = TRUE))
  }

  # Choose the metric-specific fallback used when a simulation fails.
  value_on_error <- resolve_value_on_error(metric_function)
  time_1 <- Sys.time()

  # One evaluator, on keyed random streams, for every stage of the search
  # (see R/simulation_core.R).
  # Replicates of a batch run in parallel with `parallel = TRUE` (forked
  # processes, so not on Windows); random forests and xgboost then use one
  # thread per worker, so the workers do not compete for cores.
  dots <- list(...)
  parallel <- isTRUE(dots$parallel)
  cores <- if (is.numeric(dots$cores)) {
    dots$cores
  } else {
    min(20L, parallel::detectCores(), na.rm = TRUE)
  }
  if (parallel) {
    old_threads <- options(pmsims.threads = 1L)
    on.exit(options(old_threads), add = TRUE)
  }
  streams <- new_simulation_streams()
  evaluator <- new_evaluator(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    test_n = test_n,
    value_on_error = value_on_error,
    streams = streams,
    parallel = parallel,
    cores = if (parallel) cores else 1L
  )

  if (method == "curve") {
    output <- do.call(
      calculate_curve,
      utils::modifyList(
        list(
          test_n = test_n,
          n_reps_total = n_reps_total,
          n_reps_per = n_reps_per,
          se_final = se_final,
          min_sample_size = min_sample_size,
          max_sample_size = max_sample_size,
          target_performance = target_performance,
          c_statistic = c_statistic,
          mean_or_assurance = mean_or_assurance,
          progress = progress,
          verbose = verbose,
          data_function = data_function,
          model_function = model_function,
          metric_function = metric_function,
          value_on_error = value_on_error,
          evaluator = evaluator,
          max_n = max_n
        ),
        list(...)
      )
    )
  } else if (method == "mlpwr") {
    output <- do.call(
      calculate_mlpwr,
      utils::modifyList(
        list(
          test_n = test_n,
          n_reps_total = n_reps_total,
          n_reps_per = n_reps_per,
          se_final = se_final,
          min_sample_size = min_sample_size,
          max_sample_size = max_sample_size,
          target_performance = target_performance,
          c_statistic = c_statistic,
          mean_or_assurance = mean_or_assurance,
          n_init = n_init,
          progress = progress,
          verbose = verbose,
          data_function = data_function,
          model_function = model_function,
          metric_function = metric_function,
          value_on_error = value_on_error,
          evaluator = evaluator,
          max_n = max_n
        ),
        list(...)
      )
    )
  } else if (method == "bisection") {
    output <- do.call(
      calculate_bisection,
      utils::modifyList(
        list(
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
          c_statistic = c_statistic,
          mean_or_assurance = mean_or_assurance,
          tol = 1e-3,
          verbose = verbose,
          budget = TRUE,
          evaluator = evaluator,
          max_n = max_n
        ),
        list(...)
      )
    )
  } else if (method == "mlpwr-bs") {
    output <- do.call(
      calculate_mlpwr_bs,
      utils::modifyList(
        list(
          test_n = test_n,
          n_reps_total = n_reps_total,
          n_reps_per = n_reps_per,
          se_final = se_final,
          min_sample_size = min_sample_size,
          max_sample_size = max_sample_size,
          target_performance = target_performance,
          c_statistic = c_statistic,
          mean_or_assurance = mean_or_assurance,
          progress = progress,
          verbose = verbose,
          data_function = data_function,
          model_function = model_function,
          metric_function = metric_function,
          value_on_error = value_on_error,
          evaluator = evaluator,
          max_n = max_n
        ),
        list(...)
      )
    )
  } else {
    stop("Method not found")
  }
  check <- check_result(
    output = output,
    evaluator = evaluator,
    target_performance = target_performance,
    mean_or_assurance = mean_or_assurance,
    verify_reps = verify_reps,
    describe = describe_value(metric_function)
  )
  time_2 <- Sys.time()

  results_list <- list(
    outcome = attr(data_function, "outcome"),
    min_n = check$min_n,
    perf_n = check$perf_n,
    status = check$status,
    status_message = check$status_message,
    verification = check$verification,
    diagnostics = check$diagnostics,
    mlpwr_ds = output$mlpwr_ds,
    target_performance = target_performance,
    summaries = output$summaries,
    data = output$results,
    train_size = rownames(output$results),
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    model = attr(model_function, "model", exact = TRUE),
    metric = attr(metric_function, "metric", exact = TRUE),
    c_statistic = c_statistic,
    test_n = test_n,
    min_sample_size = min_sample_size,
    max_sample_size = max_sample_size,
    n_reps_total = n_reps_total,
    n_reps_per = n_reps_per,
    method = method,
    progress = progress,
    verbose = verbose,
    simulation_time = difftime(time_2, time_1, units = "secs"),
    # Searches restarted after a Gaussian-process surrogate failure (mlpwr engines).
    gp_restarts = output$gp_restarts %||% 0L,
    rng_base_seed = streams$base_seed,
    mean_or_assurance = mean_or_assurance
  )
  if (!is.null(output$history)) {
    results_list$history <- output$history
  }
  attr(results_list, "class") <- "pmsims"
  return(results_list)
}

# Default largest sample size: lower for random forests and xgboost, where one
# batch of replicates at a million rows would take hours.
default_max_n <- function(model) {
  if (length(model) == 1L && !is.na(model) && model %in% c("rf", "xgboost")) {
    2e5
  } else {
    1e6
  }
}

resolve_value_on_error <- function(metric_function) {
  metric_name <- attr(metric_function, "metric", exact = TRUE)
  custom_value_on_error <- attr(metric_function, "value_on_error", exact = TRUE)
  error_values <- c(
    auc = 0.5,
    cindex = 0.5,
    r2 = 0,
    brier_score_scaled = 0,
    brier_score = 1,
    ibs = 1,
    calibration_slope = 0,
    # CSSE of a calibration slope of 0, matching calibration_slope above. CSSE
    # is <= 0 with 0 perfect, so the generic 0.5 fallback would score a failed
    # fit as better than perfect calibration.
    csse = -1
  )

  if (!is.null(custom_value_on_error)) {
    if (
      !is.numeric(custom_value_on_error) ||
        length(custom_value_on_error) != 1 ||
        is.na(custom_value_on_error)
    ) {
      stop(
        "attr(metric_function, \"value_on_error\") must be a single non-missing numeric value."
      )
    }

    return(as.numeric(custom_value_on_error))
  }

  if (
    length(metric_name) == 1 &&
      !is.na(metric_name) &&
      metric_name %in% names(error_values)
  ) {
    return(unname(error_values[[metric_name]]))
  }

  cli::cli_alert_info(paste(
    "Failed replicates will count as a performance of 0.5. Set",
    "{.code attr(metric_function, \"value_on_error\")} to choose another value."
  ))
  0.5
}

# Fallback for a metric value that is not a single finite number. Metrics can
# fail without raising an error, e.g. a lasso that selects no predictors gives
# constant predictions and an NA calibration slope. Those replicates must count
# as failures, like errors do; otherwise na.rm = TRUE in the summaries drops
# them and the remaining replicates overstate performance.
metric_or_fallback <- function(value, value_on_error) {
  if (is.numeric(value) && length(value) == 1L && is.finite(value)) {
    value
  } else {
    value_on_error
  }
}

#' Check an engine's answer and assign the result status
#'
#' Engines can stop early (the adaptive stage reached max_n without meeting
#' the target). Otherwise the returned sample size is simulated again
#' with fresh replicates: mlpwr's reported performance is its surrogate's
#' prediction, never an observed value, so without this check an answer for an
#' unreachable target -- for example the edge of the search range -- is
#' returned as if it met the target.
#' @keywords internal
#' @noRd
check_result <- function(
  output,
  evaluator,
  target_performance,
  mean_or_assurance,
  verify_reps = 100,
  describe = function(x) format(signif(x, 4))
) {
  search <- output$search %||% list()
  min_n <- suppressWarnings(as.numeric(output$min_n))
  perf_n <- suppressWarnings(as.numeric(output$perf_n))
  if (length(min_n) != 1L) {
    min_n <- NA_real_
  }
  if (length(perf_n) != 1L) {
    perf_n <- NA_real_
  }

  status <- search$status
  status_message <- search$status_message
  verification <- NULL

  # Mostly failed replicates make any answer meaningless, whatever the search
  # returned.
  failed <- failure_status(evaluator)
  if (!is.null(failed)) {
    status <- failed$status
    status_message <- failed$message
  }

  if (is.null(status)) {
    if (!is.finite(min_n)) {
      status <- "not_bracketed"
      status_message <- paste(
        "The search did not return a sample size: no sample size in the",
        "search range was predicted to meet the target."
      )
    } else if (isTRUE(verify_reps > 0)) {
      verification <- verify_sample_size(
        evaluator = evaluator,
        n = min_n,
        target_performance = target_performance,
        mean_or_assurance = mean_or_assurance,
        reps = verify_reps
      )
      if (verification$verified) {
        status <- "ok"
      } else {
        status <- "not_verified"
        status_message <- sprintf(
          paste(
            "Simulating n = %s again gave %s, clearly below the target %s.",
            "The target may be unreachable, or the search range may not",
            "contain the answer; this sample size is likely too small."
          ),
          format(min_n, big.mark = ",", scientific = FALSE),
          describe(verification$performance),
          describe(target_performance)
        )
      }
    } else {
      status <- "ok"
    }
  }

  if (!status %in% c("ok", "not_verified")) {
    min_n <- perf_n <- NA_real_
  }
  if (!identical(status, "ok")) {
    warning(status_message, call. = FALSE)
  } else if (identical(search$at_bound, "lower")) {
    cli::cli_alert_info(paste(
      "The estimate lies on the lower edge of the search range",
      "({search$bounds[1]}-{search$bounds[2]}), so it may overstate the",
      "sample size needed."
    ))
  } else if (identical(search$at_bound, "upper")) {
    cli::cli_alert_info(paste(
      "The estimate lies on the upper edge of the search range",
      "({search$bounds[1]}-{search$bounds[2]}); the sample size needed may",
      "be larger."
    ))
  }
  if (identical(status, "ok")) {
    ci_text <- function(ci) {
      sprintf("%s to %s", round(ci[1]), if (is.finite(ci[2])) round(ci[2]) else "Inf")
    }
    if (isTRUE(search$near_ceiling)) {
      cli::cli_alert_warning(paste(
        "The target is close to the best performance this model can reach:",
        "doubling the sample size from the answer gains less than the Monte",
        "Carlo error, so the sample size is poorly determined."
      ))
    } else if (isTRUE(search$poorly_determined)) {
      cli::cli_alert_warning(
        "The sample size is poorly determined: its interval is {ci_text(search$curve$n_ci)}."
      )
    }
    if (isTRUE(search$crosscheck_disagrees)) {
      cli::cli_alert_warning(paste(
        "A shape-free check (a monotone fit to the simulated points) puts the",
        "answer at {round(search$crosscheck_n)}, more than 10% from the",
        "learning-curve answer."
      ))
    }
  }
  if (length(search$mlpwr_warnings)) {
    cli::cli_alert_warning(
      "mlpwr reported: {paste(search$mlpwr_warnings, collapse = '; ')}"
    )
  }

  failures <- evaluator$failures()
  list(
    min_n = min_n,
    perf_n = perf_n,
    status = status,
    status_message = status_message %||% NA_character_,
    verification = verification,
    diagnostics = list(
      bounds = search$bounds,
      at_bound = search$at_bound %||% NA_character_,
      adaptive_stop_reason = search$adaptive_stop_reason %||% NA_character_,
      max_achievable_perf = search$max_achievable_perf %||% NA_real_,
      mlpwr_warnings = search$mlpwr_warnings %||% character(0),
      gp_restarts = output$gp_restarts %||% 0L,
      curve = search$curve,
      near_ceiling = isTRUE(search$near_ceiling),
      poorly_determined = isTRUE(search$poorly_determined),
      gain_per_doubling = search$gain_per_doubling %||% NA_real_,
      crosscheck_n = search$crosscheck_n %||% NA_real_,
      crosscheck_disagrees = isTRUE(search$crosscheck_disagrees),
      replicates = failures,
      failed_replicates = sum(failures$failed),
      total_replicates = sum(failures$reps)
    )
  )
}

#' Parse and validate input specifications
#'
#' This function validates the provided data, model, and metric specifications,
#' and returns corresponding generator functions for each. It ensures that all
#' required inputs are provided and correctly configured.
#'
#' @param data_spec A list containing two elements:
#'   \describe{
#'     \item{\code{type}}{A character string indicating the outcome type.}
#'     \item{\code{args}}{A list of arguments to be passed to the data-generating function.}
#'   }
#' @param metric A character vector specifying one or more metrics to be used.
#'   Currently, only the first element is used.
#' @param model A character string specifying the model to be used.
#'
#' @return A list containing three elements:
#'   \describe{
#'     \item{\code{data_function}}{The data-generating function.}
#'     \item{\code{model_function}}{The model-generating function.}
#'     \item{\code{metric_function}}{The metric function corresponding to the chosen metric.}
#'   }
#'
#' @details
#' This function calls \code{default_data_generators()}, \code{default_model_generators()},
#' and \code{default_metric_generator()} to construct the appropriate functions based on
#' the supplied inputs.
#'
#' @keywords internal

parse_inputs <- function(data_spec, metric, model) {
  if (is.null(metric)) {
    stop("metric is missing")
  }
  if (is.null(data_spec)) {
    stop("data_spec missing")
  }
  data_function <- default_data_generators(data_spec)
  model_function <- default_model_generators(
    attr(data_function, "outcome"),
    model
  )

  # The current interface uses the first requested metric.
  metric_function <- default_metric_generator(metric[[1]], data_function)
  return(list(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function
  ))
}
