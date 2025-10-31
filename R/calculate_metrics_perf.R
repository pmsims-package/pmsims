#' Calculate performance metrics
#' Calculates the performetrics for a model given a sample size n.
#' @param n is the sample size.
#' @param value_on_error value to return if error.
#'
#' @return The calculated performance metric
#' @keywords internal

calculate_metrics_perf <- function(
  n,
  data_function,
  model_function,
  metric_function,
  value_on_error
) {
  tryCatch(
    {
      test_data <- data_function(n)
      train_data <- data_function(n)
      fit <- model_function(train_data)
      model <- attr(model_function, "model")
      metric_function(test_data, fit, model)
    },
    error = function(e) {
      return(value_on_error)
    }
  )
}
