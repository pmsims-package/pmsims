#' Title Tune generate data
#'
#' @param data_function A function of two parameters, n and a tuning parameter, that returns data for the model function
#' @param large_n A large sample size used for parameter tuning
#' @param min_tune_arg The minimum valaue of the parameter to be tuned
#' @param max_tune_arg The maximum valaue of the parameter to be tuned
#' @param model_function A function which takes the object returned by the data generating function and fits the analysis model of interest.
#' @param performance_function A function which takes a a test dataset and model object as argments and returns a performance metric
#' @param target_large_sample_performance The desired model performance in a large sample
#' @param tolerance The tolerance in the large sample performance
#' @param max_interval_expansion The maximum number of time the search interval will be expanded before tune gererate data quits looking. This prevents getting stuck in impossible searches.
#'
#' @return optimal value for second argument of the data generating function
#' @export
#'
#' @examples
tune_generate_data <- function(
  data_function,
   large_n,
   min_tune_arg,
   max_tune_arg,
   model_function,
   metric_function,
   target_large_sample_performance,
   tolerance = target_large_sample_performance / 100,
   max_interval_expansion = 10,
   verbose = FALSE) {
  interval <- c(min_tune_arg, max_tune_arg)

  # Optimise
  result <- stats::optimise(
    optimise_me, # function defined below
    interval = interval,
    maximum = FALSE,
    tol = tolerance,
    n = large_n,
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    target_large_sample_performance = target_large_sample_performance
  )
  optimal_value <- result$minimum
  expand_count <- 1
  # Expand range if solution is close to limits
  while (abs(optimal_value - interval[1]) < max(abs(optimal_value - interval[1])) / 100 ||
    abs(optimal_value - interval[2]) < max(abs(optimal_value - interval[1])) / 100 ||
    result$objective > tolerance) {

    # Interval is too narrow, expand the interval
    if (verbose) print("Expanding search for tuning parameter")
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
      target_large_sample_performance = target_large_sample_performance
    )

    # Extract the optimal value and its corresponding objective function value
    optimal_value <- result$minimum
  }

  return(result$minimum)
}

# Update to allow spec. of tuning parameter.

optimise_me <- function(tune_var,
                        n,
                        data_function,
                        model_function,
                        metric_function,
                        target_large_sample_performance) {
  data <- data_function(n, tune_var) # TODO: update to allow choice of tuning parameter.
  model <- model_function(data)
  test_data <- data_function(n, tune_var)
  performance <- metric_function(data = test_data, model = model)
  delta <- abs(performance - target_large_sample_performance)
  return(delta)
}
