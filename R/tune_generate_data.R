#' Title Tune generate data
#'
#' @param data_function A function of two parameters, n and a tuning parameter,
#' that returns data for the model function
#' @param large_n A large sample size used for parameter tuning
#' @param min_tune_arg The minimum value of the parameter to be tuned
#' @param max_tune_arg The maximum value of the parameter to be tuned
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
  verbose = FALSE) {

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
  while (any(check_limits(optimal_value, interval),
             result$objective > tolerance)) {
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
      target_performance = target_performance
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
                        target_performance) {
  # TODO: update to allow choice of tuning parameter.
  data <- data_function(n, tune_var)
  fit <- model_function(data)
  test_data <- data_function(n, tune_var)
  performance <- metric_function(
    data = test_data,
    fit = fit,
    model = attr(model_function, "model"))
  delta <- abs(performance - target_performance)
  return(delta)
}
