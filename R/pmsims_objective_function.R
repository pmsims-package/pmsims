#' Objective function for pmsims
#' 'objective_function' is the function that calculate the pmsims objective = |Perf(n) - Target - \lambda*C(n)|.
#' @param n is the sample size.
#' @param penalty_weight This is the weight that balances the objective and the cost of large sample size. Value 0 implies the focus is to minimize the abs difference.
#' @param target_performance The minimum desired model performance.
#' @param min_sample_size The minimum sample size assessed. This sets the lower bound of the search region for sample size.
#' @param max_sample_size The maximum sample size assessed. This sets the upper bound of the search region for sample size.
#' @param value_on_error value to return if error.
#'
#' @return
#' @export
#'
#' @examples
objective_function <- function(
  n,
  penalty_weight,
  target_performance,
  min_sample_size,
  max_sample_size,
  value_on_error
) {
  n <- round(n)
  if (n < min_sample_size) return(-Inf) # Enforce minimum sample size

  tryCatch(
    {
      # Calculate performance metric
      performance <- calcuate_metrics_perf(n, value_on_error)

      # Calculate penalty term (normalized by max sample size)
      penalty <- penalty_weight * (n / max_sample_size)

      # Objective value (minimize difference between performance and target
      # while minimizing sample size)
      objective_value <- -abs(performance - target_performance - penalty)

      return(objective_value)
    },
    error = function(e) {
      return(value_on_error)
    }
  )
}
