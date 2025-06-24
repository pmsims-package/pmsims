#' Title
#'
#' @param tune_param The name of the tuning parameter in the data generating model
#' @param max_sample_size The maximum sample size used in simulations
#' @param large_sample_performance The target large sample performance
#' @param data_function The data generating funciton
#' @param model_function The estimation model function
#' @param metric_function The performance metric function the model is to be tuned with.
#' @param tune_args A list of arguments which may include interval, large_n, tolerance, and max_interval_expansion. These will overide defaults

#' @returns The optimal value for the tuning parameter
#' @export
#'
#' @examples
default_tune <- function(
  tune_param,
  max_sample_size,
  large_sample_performance,
  data_function,
  model_function,
  metric_function,
  tune_args = NULL,
  verbose = TRUE
) {
  default_tuning_set_up <- list(
    interval = c(0, 1),
    large_n = set_test_n(max_sample_size),
    tolerance = set_tolerance(large_sample_performance),
    max_interval_expansion = 10
  )
  for (p in names(default_tuning_set_up)) {
    if (is.null(tune_args[[p]])) tune_args[[p]] <- default_tuning_set_up[[p]]
  }
  tune_args <- c(
    tune_args,
    list(
      data_function = data_function,
      model_function = model_function,
      metric_function = metric_function,
      target_performance = large_sample_performance,
      verbose = verbose
    )
  )
  tune_param <- do.call(tune_generate_data, tune_args)
}

#' Title Tune generate data
#'
#' @param data_function A function of two parameters, n and a tuning parameter,
#' that returns data for the model function
#' @param large_n A large sample size used for parameter tuning
#' @param interval 	A vector containing the end-points of the interval to be searched
#' @param model_function A function which takes the object returned by the data
#' generating function and fits the analysis model of interest.
#' @param performance_function A function which takes a a test dataset and
#' model object as arguments and returns a performance metric
#' @param target_performance The desired model performance in a
#' large sample
#' @param tolerance The tolerance in the large sample performance
#' @param max_interval_expansion The maximum number of time the search interval
#' will be expanded before tune generate data quits looking. This prevents
#' getting stuck in impossible searches.
#'
#' @return optimal value for second argument of the data generating function
#' @export
#'
#' @examples
tune_generate_data <- function(
  data_function,
  large_n,
  interval,
  model_function,
  metric_function,
  target_performance,
  tolerance = set_tolerance(target_performance),
  max_interval_expansion = 10,
  verbose = FALSE
) {
  result <- stats::optimise(
    optimise_me, # function defined below
    interval = interval,
    maximum = FALSE,
    tol = tolerance,
    n = large_n,
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    target_performance = target_performance
  )

  optimal_value <- result$minimum
  expand_count <- 1

  # Expand range if solution is close to limits
  check_limits <- function(opt, int) {
    any(
      abs(opt - int[1]) < max(abs(opt - int[1])) / 100,
      abs(opt - int[2]) < max(abs(opt - int[1])) / 100
    )
  }
  while (
    any(
      check_limits(optimal_value, interval),
      result$objective > tolerance
    )
  ) {
    # Interval is too narrow, expand the interval
    if (verbose) {
      print("Expanding search for tuning parameter")
    }
    expand_count <- expand_count + 1
    if (expand_count > max_interval_expansion) {
      stop("cannot find interval containing tuning parameter")
    }
    interval <- c(interval[1] - 1, interval[2] + 1)

    # Call the optimise function with the expanded interval
    result <- stats::optimise(
      optimise_me, # function defined below
      interval = interval,
      maximum = FALSE,
      tol = tolerance,
      n = large_n,
      data_function = data_function,
      model_function = model_function,
      metric_function = metric_function,
      target_performance = target_performance
    )

    # Extract the optimal value and its corresponding objective function value
    optimal_value <- result$minimum
  }

  return(result$minimum)
}

# Update to allow spec. of tuning parameter.
optimise_me <- function(
  tune_var,
  n,
  data_function,
  model_function,
  metric_function,
  target_performance
) {
  # TODO: update to allow choice of tuning parameter.
  data <- data_function(n, tune_var)
  fit <- model_function(data)
  test_data <- data_function(n, tune_var)
  performance <- metric_function(
    data = test_data,
    fit = fit,
    model = attr(model_function, "model")
  )
  delta <- abs(performance - target_performance)
  return(delta)
}
