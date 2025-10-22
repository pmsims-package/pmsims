##### Analytical tuning function for continuous outcome

#' Title
#'
#' @param r2 
#' @param proportion_noise_features 
#' @param candidate_features 
#'
#' @returns
#' @export
#'
#' @examples
continuous_tuning <- function(r2, proportion_noise_features, candidate_features){
  
  non_noise_predictors = candidate_features - round(candidate_features*proportion_noise_features)
  
  beta_signal = sqrt(r2/(non_noise_predictors*(1 - r2)))
  
  return(beta_signal)
  
}