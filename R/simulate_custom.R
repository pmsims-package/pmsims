#' Simulate Custom
#' 'simulate_custom' is the interface for pmsims at the most basic level. It performs no processing of arguments and allows all possible options to be customised.
#'
#' @param data_function A function that returns datasets. Must have a single argument, `n`, which controls the sample size.
#' @param model_function A function that fits models to the data. Takes the data object returned by `data_function` as its only argument.
#' @param metric_function A function that returns a performance metric. Must take test data, a fitted model and a model function as arguments. Must return a single value.
#' @param target_performance Numeric target performance threshold the algorithm must meet or exceed.
#' @param c_statistic Numeric; anticipated large-sample discrimination used when tuning the data generator.
#' @param mean_or_assurance Character string `"mean"` or `"assurance"` indicating which criterion defines the minimum sample size.
#' @param test_n Integer size of the test datasets used to evaluate performance (should be large).
#' @param min_sample_size Integer lower bound of the sample-size search region.
#' @param max_sample_size Integer upper bound of the sample-size search region.
#' @param n_reps_total Integer total number of simulation replications allocated to the search.
#' @param n_reps_per Integer number of replications evaluated at each candidate sample size.
#' @param se_final Numeric standard error threshold used for early stopping (supply either `n_reps_total` or `se_final`).
#' @param n_init Integer number of initial sample sizes explored before the main search algorithm begins.
#' @param method Character string selecting the search engine; currently `"mlpwr"`, `"bisection"`, or `"mlpwr-bs"`.
#' @param verbose Logical flag controlling printed progress information.
#' @param ... Additional arguments passed to the chosen engine (for example `tol` for bisection or `penalty_weight` for GA-based methods).
#'
#' @return An object of class `"pmsims"` containing the estimated minimum sample size and simulation diagnostics.
#' @keywords internal
#' @export
#'
#' @examples
simulate_custom <- function(
  data_function = NULL,
  model_function = NULL,
  metric_function = NULL,
  target_performance,
  c_statistic,
  mean_or_assurance = "assurance",
  test_n = 30000,
  min_sample_size,
  max_sample_size,
  n_reps_total = NULL,
  n_reps_per = 50,
  se_final = NULL,
  n_init = 4,
  method = "mlpwr",
  verbose = FALSE,
  ...
) {
  if (is.null(data_function)) {
    stop("data_function missing")
  }

  if (
    sum(c(
      is.null(n_reps_total),
      is.null(se_final)
    )) !=
      1
  ) {
    stop("Exactly one of 'n_reps_total' or 'se_final' must be specified.")
  }

  if (
    !is.null(min_sample_size) &&
      !is.null(max_sample_size) &&
      min_sample_size > max_sample_size
  ) {
    stop("min_sample_size must be less than max_sample_size")
  }

  if ((mean_or_assurance %in% c("mean", "assurance")) == FALSE) {
    stop("mean_or_assurance must be either 'mean' or 'assurance'")
  }

  # Define a default metric value if calculations fail; 0.5 for default
  metric_name <- attr(metric_function, "metric")
  error_values <- list(
    auc = 0.5,
    cindex = 0.5,
    r2 = 0,
    brier_score_scaled = 0,
    brier_score = 1,
    IBS = 1,
    calib_slope = 0
  )
  value_on_error <- ifelse(
    metric_name %in% names(error_values),
    error_values[[metric_name]],
    0.5
  )
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
      verbose = FALSE,
      budget = FALSE,
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
    train_size = rownames(output$results),
    data_function = data_function,
    simulation_time = difftime(time_2, time_1, units = "secs")
  )
  attr(results_list, "class") <- "pmsims"
  return(results_list)
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
#' @export
#'
#' @examples
#' \dontrun{
#' data_spec <- list(
#'   type = "binary",
#'   args = list(n = 100, p = 5)
#' )
#' parse_inputs(data_spec, metric = "auc", model = "glm")
#' }
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

  # Set a metric, based on outcome type
  # TODO:
  # Currently, we're selecting the first element in 'metric' only.
  # In future, we will expand this to handle multiple metrics.
  metric_function <- default_metric_generator(metric[[1]], data_function)
  return(list(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function
  ))
}
