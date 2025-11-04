#' Calculate performance metrics
#' Calculates the performance metrics for a model given a sample size n.
#' @param n Integer sample size.
#' @param value_on_error Numeric fallback returned if the metric cannot be computed.
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
