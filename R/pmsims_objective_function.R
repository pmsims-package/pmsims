#' Objective function for pmsims
#'
#' Computes the objective value
#' \deqn{\left| M(n) - M^* - \lambda\,C(n) \right|,}
#' where \eqn{M(n)} is performance at sample size \eqn{n}, \eqn{M^*} is the
#' target performance, \eqn{C(n)} is a cost term increasing with \eqn{n}, and
#' \eqn{\lambda} (set via `penalty_weight`) controls the penalty on large samples.
#'
#' @param n Integer sample size.
#' @param penalty_weight Penalty weight \eqn{\lambda} on the cost term \eqn{C(n)};
#'   `0` minimises only the absolute performance gap.
#' @param target_performance Target performance \eqn{M^*} (numeric).
#' @param min_sample_size Minimum sample size considered (lower bound of search).
#' @param max_sample_size Maximum sample size considered (upper bound of search).
#' @param value_on_error Value to return if the objective cannot be evaluated.
#'
#' @return A single numeric: the objective value at \eqn{n}.
#' @keywords internal
#' @export
#'
#' @examples
#' # \dontrun{
#' # Example usage (assuming helper functions exist):
#' # objective_function(
#' #   n = 500,
#' #   penalty_weight = 0.1,
#' #   target_performance = 0.75,
#' #   min_sample_size = 100,
#' #   max_sample_size = 5000,
#' #   value_on_error = Inf
#' # )
#' # }

objective_function <- function(
  n,
  penalty_weight,
  target_performance,
  min_sample_size,
  max_sample_size,
  value_on_error
) {
  n <- round(n)
  if (n < min_sample_size) {
    return(-Inf)
  } # Enforce minimum sample size

  tryCatch(
    {
      # Calculate performance metric
      performance <- calculate_metrics_perf(n, value_on_error)

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
