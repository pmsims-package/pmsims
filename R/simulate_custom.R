#' Simulate Custom
#' 'simulate_custom' is the interface for pmsims at the most basic level. It performs no processing of arguments and allows all possible options to be customised.
#'
#' @param data_function A function that returns datasets. Must have a single argmument, n, which controls the sample size.
#' @param model_function A function that fits models to the data. Take the data object returned by data_funciton as only argument.
#' @param metric_function A function that returns a performance metric. Must take test data, a fitted model and a model function as arguments. Must return a single value.
#' @param target_performance The minimum desired model performance
#' @param mean_or_assurance Can be either "mean" or "assurance". If mean, sample size is calculated so that average model performance is greater than target_performance. If assurance model performance is greater than target performance 80% of the time.
#' @param test_n The sample size used for test datasets. This should be a large number.
#' @param min_sample_size The minimum sample size assessed. This sets the lower bound of the search region for sample size.
#' @param max_sample_size The maximum sample size assessed. This sets the upper bound of the search region for sample size.
#' @param n_reps_total The total number of simulation reps run by pmsims.
#' @param n_reps_per The number of reps run at each sample size.
#' @param se_final A standard error which can be used as a stopping criteria. Either n_reps_total or se_final should be given.
#' @param n_init The number of initial sample sizes to be used in the search before the algorithm passed in method is used
#' @param method The method used to search for the minimum sample size. Options are "mlpwr", "crude" and "ga".
#' @param verbose A logical controlling output
#' @param penalty_weight This is the weight that balances the objective and the cost of large sample size for the "ga" engine. Value 0 implies the focus is to minimize the abs difference.
#' @param tol This is the minimum difference between performance at i and i+1 for the "bisection" engine, default is 1e-3.
#' @param ... Other argments passed to the method function.
#'
#' @returns
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

  if (!is.null(min_sample_size) && !is.null(max_sample_size) && min_sample_size > max_sample_size) {
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

#' Title
#'
#' @param data_spec A list with two items. The first named type which indicates the type of outcome. The second named args which is a list of arguments to be passed to the data generating function.
#' @param metric A string indicating the metric to be used.
#' @param model A string indicating the model to be used.
#'
#' @return A list containing a data function, a model function, and a metric function.
#' @export
#'
#' @examples
parse_inputs <- function(data_spec, metric, model) {
  if (is.null(metric)) {
    stop("metric is missing")
  }
  if (is.null(data_spec)) {
    stop("data_spec missing")
  }
  # Set data generating function
  data_function <- default_data_generators(data_spec)
  # Set model function, based on outcome type and chosen model
  model_function <- default_model_generators(
    attr(data_function, "outcome"),
    model
  )
  # Set a metric, based on outcome type
  # TODO: handle multiple metrics. Currently selecting first element in
  # 'metric' only.
  metric_function <- default_metric_generator(metric[[1]], data_function)
  return(list(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function
  ))
}
