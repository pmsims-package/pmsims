calculate_sample_size <- function(data_generating_function, 
                                  model_function,  
                                  performance_function, 
                                  target_performance,
                                  test_n,
                                  tune_param,
                                  min_sample_size,
                                  max_sample_size,
                                  n_reps,
                                  n_sample_sizes = 10 # The number of different sample sizes tried - needs a better name
                                  
                                  ) {
  test_data <- data_generating_function(test_n, tune_param)
  simulation_parameters <- get_simulation_parameters(min_sample_size = min_sample_size,
                                                     max_sample_size = max_sample_size,
                                                     n_reps = n_reps,
                                                     n_sample_sizes = n_sample_sizes) # function defined below
  
  
  # Running the simulations
  #place holder for results across trials (columns) for each train_size(rows)
  results = matrix(nrow = length(simulation_parameters$train_size), ncol = max(simulation_parameters$n_sims))
  for (i in 1:length(simulation_parameters$train_size)) {
    for (j in seq(simulation_parameters$n_sims[i])){
      
      performance <- get_performance_n(n = simulation_parameters$train_size[i],
                                       test_data = test_data,
                                       data_generating_function = data_generating_function,
                                       model_function = model_function,
                                       performance_function = performance_function,
                                       tune_param = tune_param)  # function defined below
      results[i,j] <-  performance
    }
    sim_message <- paste("simulation", i*n_reps/n_sample_sizes, "of", n_reps)
    print(sim_message)
  }
  rownames(results) = simulation_parameters$train_size
  
  # processing results and plotting - functions defined below
  
  results_list <- process_results(results, simulation_parameters, target_performance)
  plot_sample_size_curve(results_list) 
  
  return(results_list)

}

get_simulation_parameters <- function(min_sample_size, max_sample_size, n_reps, n_sample_sizes) {
  train_size = seq(from = min_sample_size, 
                   to = max_sample_size, 
                   length.out = n_sample_sizes)
  n_sims =   rep(n_reps/n_sample_sizes, n_sample_sizes)
  simulation_parameters <- data.frame(train_size = train_size,
                                      n_sims = n_sims)
  return(simulation_parameters)
}

get_performance_n <- function(n, 
                              test_data, 
                              data_generating_function, 
                              model_function,  
                              performance_function,
                              tune_param) {
  train_data <- data_generating_function(n, tune_param)
  model <- model_function(train_data)
  performance <- performance_function(test_data, model)
  return(performance)    
}


process_results <- function(results, simulation_parameters, target_performance) {
  mean_performance  <-  apply(results, FUN = mean, MARGIN = 1, na.rm= TRUE)
  quant20_performance <- apply(results, FUN = quantile, MARGIN = 1, probs = 0.2, na.rm = TRUE)
  quant5_performance  <-  apply(results, FUN = quantile, MARGIN = 1, probs = 0.05, na.rm = TRUE)
  quant95_performance  <-  apply(results, FUN = quantile, MARGIN = 1, probs = 0.95, na.rm = TRUE)
  
  #min training size where 80% of AUC are > 0.70:
  # We linearly approximate values between the train sizes and get the min_n from there:
  min_size_func = approxfun(quant20_performance, simulation_parameters$train_size, method = "linear")
  min_n = min_size_func(target_performance)
  
  return_list <- list(min_n = min_n,
                      target = target_performance,
                      summaries = data.frame(mean_performance= mean_performance, 
                                             quant20_performance = quant20_performance, 
                                             quant5_performance = quant5_performance, 
                                             quant95_performance = quant95_performance),
                      data = results,
                      train_size = simulation_parameters$train_size)
  return(return_list)
}


plot_sample_size_curve <- function(results_summaries) {
  train_size <- results_summaries$train_size
  performance <- results_summaries$summaries
  quant_performance <- performance$quant20_performance
  mean_performance <- performance$mean_performance
  quant5_performance <- performance$quant5_performance
  quant95_performance <- performance$quant95_performance
  
  
  
  
  #Plot 
  plot(train_size, quant_performance, type = "l" , lty = 2, col = "red", 
       main = "AUC by train size with  20th percintile, 5th percentile, & 95th percentile")
  lines(train_size, mean_performance, col = "black")
  lines(train_size, quant5_performance, col = "grey", lty = 1)
  lines(train_size, quant95_performance, col = "grey", lty = 1)
  #abline(h = target_auc, col = 3)
  abline(h = results_summaries$target, col = "green", lty = 3)
  abline(v = results_summaries$min_n, col = "green", lty = 3)
  legend("bottomright", legend = c("AUC mean", "AUC 5th to 95th percentile", "AUC 20th percentile", "Acceptable AUC"), 
         col = c("black", "grey", "red", "green"), lty = c(1,1,2,3))
}
  
