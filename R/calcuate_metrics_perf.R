#' Calculate performance metrics 
#' 'calculate_metrics_perf' calculates the performetrics for a model given a sample size n.
#' @param n is the sample size.
#' @param value_on_error value to return if error.
#'
#'
#' @return
#' @export
#'
#' @examples
calcuate_metrics_perf <- function(n,value_on_error) {
  tryCatch(
    {
      test_data <- data_function(test_n)
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