sm_linear_extrapolation <- function(results, simulation_parameters, target_performance) {
  median_performance  <-  apply(results, FUN = stats::quantile, MARGIN = 1, probs = 0.5, na.rm = TRUE)
  quant20_performance <- apply(results, FUN = stats::quantile, MARGIN = 1, probs = 0.2, na.rm = TRUE)
  quant5_performance  <-  apply(results, FUN = stats::quantile, MARGIN = 1, probs = 0.05, na.rm = TRUE)
  quant95_performance  <-  apply(results, FUN = stats::quantile, MARGIN = 1, probs = 0.95, na.rm = TRUE)

  # min training size where 80% of AUC are > 0.70:
  # We linearly approximate values between the train sizes and get the min_n from there:
  min_size_func = stats::approxfun(quant20_performance,
                                   simulation_parameters$train_size,
                                   method = "linear")
  min_n  <-  min_size_func(target_performance)

  return_list <- list(min_n = min_n,
                      target = target_performance,
                      summaries = data.frame(
                        median_performance = median_performance,
                        quant20_performance = quant20_performance,
                        quant5_performance = quant5_performance,
                        quant95_performance = quant95_performance),
                      data = results,
                      train_size = simulation_parameters$train_size)
  return(return_list)
}

sm_qgam <- function(results, simulation_parameters, target_performance) {

}
