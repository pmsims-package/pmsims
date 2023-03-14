

#' Calculate the minimum sample size required to develop a prediction model
#'
#' @param data_generating_function A function of two parameters, n and a tuning parameter, that returns data for the model function
#' @param model_function A function which takes the object returned by the data generating function and fits the analysis model of interest.
#' @param performance_function A function which takes a a test dataset and model object as argments and returns a performance metric
#' @param target_performance The desired performance of the prediction model
#' @param test_n The sample size used for testing model performance
#' @param tune_param A tuning parameter to be passed to the data generating function
#' @param min_sample_size The minimum sample size used in simualations
#' @param max_sample_size The maximum sample size used in simulations
#' @param n_reps The number of simualtion reps
#' @param n_sample_sizes The number of different sample sizes simulations are carried out at
#'
#' @return A list of results form the simulation
#' @export
#'
#' @examples
#'
#' library(pROC)
#'
#' generate_data <- function(n, beta_signal) {
#'   p_signal <- 10    # number of predictors
#'   prob_p  <-  0.1 # probability of a predictor to be 1
#'   base_prev  <-  0.3 # baseline probability of a positive outcome
#'
#'   alldata <-  rbinom(n*p_signal,1,prob_p)
#'   X <-  matrix(alldata, nrow = n, ncol = p_signal)
#'   W_ <-  rep(beta_signal, p_signal)
#'   b0 <-  log(base_prev/(1- base_prev))
#'   lp <-  X %*% W_ + b0
#'   y_prob <-  1/(1+exp(-lp))
#'
#'   #generate outcome
#'   y <-  rbinom(n,1,y_prob)
#'   data <- cbind(y, X) |> data.frame()
#'   x_names <- paste0("x", 1:(ncol(data)-1))
#'   data_names <- c("y", x_names)
#'   colnames(data) <- data_names
#'   return(data)
#' }
#'
#' fit_model <- function(data) {
#'   logistic_model <- glm("y ~ .", data = data, family = "binomial")
#' }
#'
#' # Get performance must be a function of data and a model objedt
#' get_performance <- function(data, model) {
#'   y <- data[,1]
#'   x <- data[,-1]
#'   y_hat = predict(model, x, type = "response")
#'   auc = pROC::auc(y, as.numeric(y_hat), quiet = TRUE)
#'   return(auc[1])
#' }
#'
#' calculate_sample_size(data_generating_function = generate_data,
#'                       model_function = fit_model,
#'                       performance_function = get_performance,
#'                       target_performance = 0.75,
#'                       test_n = 10000,
#'                       tune_param = 0.7,
#'                       min_sample_size = 100,
#'                       max_sample_size = 3000,
#'                       n_reps = 10,
#'                       n_sample_sizes = 10)

calculate_sample_size <- function(data_generating_function,
                                  model_function,
                                  performance_function,
                                  target_performance,
                                  test_n,
                                  tune_param,
                                  min_sample_size,
                                  max_sample_size,
                                  n_reps,
                                  n_sample_sizes = 10
                                  ) {

  simulation_parameters <- get_simulation_parameters(
                                      min_sample_size = min_sample_size,
                                      max_sample_size = max_sample_size,
                                      n_reps = n_reps,
                                      n_sample_sizes = n_sample_sizes)

  # Running the simulations
  results <- run_simulations(simulation_parameters,
                             test_n = test_n,
                             data_generating_function = data_generating_function,
                             model_function = model_function,
                             performance_function = performance_function,
                             tune_param = tune_param)

  # Applying surrogate modelling and plotting
  results_list <- sm_linear_extrapolation(results, simulation_parameters, target_performance)
  plot_sample_size_curve(results_list)

  return(results_list)
}

#' Get a data frame of sample sizes and reps at each sample size to use in simulation
#'
#'get_simulation_parameters returns a data frame with the first column containing
#'different sample sizes and the second the number of reps to run at each sample size.
#'The sample sizes are equally spaced between the min and max sample sizes. The number
#'of reps  is the total number of simulation reps divided by the number of different
#'sample sizes to run simulations on.
#'
#' @param min_sample_size The minimum sample size
#' @param max_sample_size The maximum sample size
#' @param n_reps The total number of simulation reps
#' @param n_sample_sizes The number of different sample sizes to run simulations on
#'
#' @return
#' @export
#'
#' @examples
get_simulation_parameters <- function(min_sample_size,
                                      max_sample_size,
                                      n_reps,
                                      n_sample_sizes) {
  train_size = seq(from = min_sample_size,
                   to = max_sample_size,
                   length.out = n_sample_sizes) |> as.integer(0)
  n_sims =   rep(n_reps/n_sample_sizes, n_sample_sizes) |> as.integer(0)
  simulation_parameters <- data.frame(train_size = train_size,
                                      n_sims = n_sims)
  return(simulation_parameters)
}

get_performance_n <- function(n,
                              test_n,
                              data_generating_function,
                              model_function,
                              performance_function,
                              tune_param) {
  test_data <- data_generating_function(test_n, tune_param)
  train_data <- data_generating_function(n,
                                         tune_param)
  model <- model_function(train_data)
  performance <- performance_function(test_data, model)
  return(performance)
}

run_simulations <- function(simulation_parameters,
                            test_n = test_n,
                            data_generating_function = data_generating_function,
                            model_function = model_function,
                            performance_function = performance_function,
                            tune_param = tune_param) {

  results <- matrix(nrow = length(simulation_parameters$train_size),
         ncol = max(simulation_parameters$n_sims))
  for (i in 1:length(simulation_parameters$train_size)) {
    for (j in seq(simulation_parameters$n_sims[i])){
      results[i,j] <- get_performance_n(n = simulation_parameters$train_size[i],
                                      test_n = test_n,
                                      data_generating_function = data_generating_function,
                                      model_function = model_function,
                                      performance_function = performance_function,
                                      tune_param = tune_param)  # function defined below
    }
  }
  rownames(results) = simulation_parameters$train_size
  return(results)
}

plot_sample_size_curve <- function(results_summaries) {
  train_size <- results_summaries$train_size
  performance <- results_summaries$summaries
  quant_performance <- performance$quant20_performance
  median_performance <- performance$median_performance
  quant5_performance <- performance$quant5_performance
  quant95_performance <- performance$quant95_performance

  # Plot
  plot(train_size, quant_performance, type = "l" , lty = 2, col = "red",
       main = "AUC by train size with  20th percintile, 5th percentile, & 95th percentile")
  graphics::lines(train_size, median_performance, col = "black")
  graphics::lines(train_size, quant5_performance, col = "grey", lty = 1)
  graphics::lines(train_size, quant95_performance, col = "grey", lty = 1)
  #abline(h = target_auc, col = 3)
  graphics::abline(h = results_summaries$target, col = "green", lty = 3)
  graphics::abline(v = results_summaries$min_n, col = "green", lty = 3)
  graphics::legend("bottomright", legend = c("AUC median", "AUC 5th to 95th percentile", "AUC 20th percentile", "Acceptable AUC"),
         col = c("black", "grey", "red", "green"), lty = c(1,1,2,3))
}
