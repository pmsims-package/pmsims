#' Analytical tuning function for continuous outcome
#' @param r2 Large sample $R^2$ or c-statistic
#' @param candidate_features Number of candidate features
#' @param proportion_noise_features Proportion of noise features
#'
#' @returns The tuned beta value
#' @keywords internal

continuous_tuning <- function(
  r2,
  proportion_noise_features,
  candidate_features
) {
  non_noise_predictors = candidate_features -
    round(candidate_features * proportion_noise_features)
  beta_signal = sqrt(r2 / (non_noise_predictors * (1 - r2)))
  return(beta_signal)
}
