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
#' @param n_reps_total Integer total number of simulation replications allocated
#'   to the search. The search evaluates approximately
#'   `n_reps_total / n_reps_per` candidate sample sizes.
#' @param n_reps_per Integer number of simulation replications performed at each
#'   candidate sample size.
#' @param method Character string specifying the search engine. Defaults to
#'   `"mlpwr"`.
#' @param progress Logical flag controlling whether the `mlpwr` progress bar is
#'   shown for `mlpwr`-based methods.
#' @param verbose Logical flag controlling engine-specific diagnostic output
#'   when supported. For the bisection engine, setting `verbose = TRUE` stores
#'   the iteration history on the returned object.
#' @param ... Additional arguments passed to the selected search engine.
#'
#' @return An object of class `"pmsims"` containing the estimated minimum sample size.
#'
#' @seealso [simulate_binary()], [simulate_continuous()], [simulate_survival()]
#'
#' @examples
#' \dontrun{
#' set.seed(1234)
#'
#' data_fun <- function(n) {
#'   x1 <- rnorm(n)
#'   x2 <- rnorm(n)
#'   x3 <- rnorm(n)
#'   x4 <- rnorm(n)
#'   x5 <- rnorm(n)
#'   y <- 0.35 * x1 - 0.3 * x2 + 0.2 * x3 + 0.1 * x4 - 0.1 * x5 +
#'     rnorm(n, sd = 1)
#'   data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5)
#' }
#'
#' model_fun <- function(dat) {
#'   stats::lm(y ~ ., data = dat)
#' }
#'
#' metric_fun <- function(test_data, fit, model) {
#'   preds <- stats::predict(fit, newdata = test_data)
#'   1 - sum((test_data$y - preds)^2) /
#'     sum((test_data$y - mean(test_data$y))^2)
#' }
#' attr(metric_fun, "metric") <- "r2"
#'
#' maximum_achievable_data <- data_fun(100000)
#' test_data <- data_fun(50000)
#' maximum_achievable_fit <- model_fun(maximum_achievable_data)
#' maximum_achievable_performance <- metric_fun(
#'   test_data,
#'   maximum_achievable_fit,
#'   "lm"
#' )
#'
#' est <- simulate_custom(
#'   data_function = data_fun,
#'   model_function = model_fun,
#'   metric_function = metric_fun,
#'   target_performance = maximum_achievable_performance - 0.02
#' )
#' est
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
  method = "mlpwr",
  progress = TRUE,
  verbose = FALSE,
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
    cat(
      "Using user-specified min_sample_size and max_sample_size. Adaptive starting values will not be used.\n"
    )
  }

  if ((mean_or_assurance %in% c("mean", "assurance")) == FALSE) {
    stop("mean_or_assurance must be either 'mean' or 'assurance'")
  }

  # Choose the metric-specific fallback used when a simulation fails.
  value_on_error <- resolve_value_on_error(metric_function)
  time_1 <- Sys.time()

  if (method == "mlpwr") {
    output <- calculate_mlpwr(
      test_n = test_n,
      n_reps_total = n_reps_total,
      n_reps_per = n_reps_per,
      se_final = se_final,
      min_sample_size = min_sample_size,
      max_sample_size = max_sample_size,
      target_performance = target_performance,
      c_statistic = c_statistic,
      mean_or_assurance,
      n_init = n_init,
      progress = progress,
      verbose = verbose,
      data_function = data_function,
      model_function = model_function,
      metric_function = metric_function,
      value_on_error = value_on_error
    )
  } else if (method == "bisection") {
    output <- calculate_bisection(
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
      parallel = FALSE,
      cores = 20,
      verbose = verbose,
      budget = TRUE,
      ...
    )
  } else if (method == "mlpwr-bs") {
    output <- calculate_mlpwr_bs(
      test_n = test_n,
      n_reps_total = n_reps_total,
      n_reps_per = n_reps_per,
      se_final = se_final,
      min_sample_size = min_sample_size,
      max_sample_size = max_sample_size,
      target_performance = target_performance,
      c_statistic = c_statistic,
      mean_or_assurance,
      progress = progress,
      verbose = verbose,
      data_function = data_function,
      model_function = model_function,
      metric_function = metric_function,
      value_on_error = value_on_error
    )
  } else {
    stop("Method not found")
  }
  time_2 <- Sys.time()
  results_list <- list(
    outcome = attr(data_function, "outcome"),
    min_n = ifelse(
      is.na(output$min_n),
      "Not possible. Increase sample or lower performance",
      output$min_n
    ),
    perf_n = ifelse(
      is.na(output$perf_n),
      "Not possible. Increase sample or lower performance",
      output$perf_n
    ),
    mlpwr_ds = output$mlpwr_ds,
    target_performance = target_performance,
    summaries = output$summaries,
    data = output$results,
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
    mean_or_assurance = mean_or_assurance
  )
  if (!is.null(output$history)) {
    results_list$history <- output$history
  }
  attr(results_list, "class") <- "pmsims"
  return(results_list)
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
    IBS = 1,
    calib_slope = 0
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

  0.5
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
