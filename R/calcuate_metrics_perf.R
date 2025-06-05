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
<<<<<<< HEAD
calcuate_metrics_perf <- function(n,
                                  data_function,
                                  model_function,
                                  metric_function,
                                  value_on_error) {
=======
calcuate_metrics_perf <- function(n, value_on_error) {
>>>>>>> 4e93d588f2e77d9bfd30ef7d3b956c9571f7c982
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

