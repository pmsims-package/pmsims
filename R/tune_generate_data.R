tune_generate_data <- function(data_generating_function,
                               large_n,
                               min_tune_arg,
                               max_tune_arg,
                               model_function,
                               performance_function,
                               target_large_sample_performance,
                               tolerance = target_large_sample_performance/100) {

  #Optimise
  optimal_param <- stats::optimise(optimise_me, # function defined below
                            lower = min_tune_arg,
                            upper = max_tune_arg,
                            maximum = FALSE,
                            tol = tolerance,
                            n = large_n,
                            data_generating_function = data_generating_function,
                            model_function = model_function,
                            performance_function = performance_function,
                            target_large_sample_performance = target_large_sample_performance)
  return(optimal_param$minimum)
}


optimise_me <- function(tune_var,
                        n,
                        data_generating_function,
                        model_function,
                        performance_function,
                        target_large_sample_performance) {
  data <- data_generating_function(n, tune_var)
  model <- model_function(data)
  test_data <- data_generating_function(n, tune_var)
  performance <- performance_function(data = test_data, model = model)
  delta <- abs(performance - target_large_sample_performance)
  return(delta)
}
