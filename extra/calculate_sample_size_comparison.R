library(pROC)

generate_data <- function(n, beta_signal) {
  p_signal <- 10    # number of predictors
  prob_p  <-  0.1 # probability of a predictor to be 1
  base_prev  <-  0.3 # baseline probability of a positive outcome
  
  alldata <-  rbinom(n*p_signal,1,prob_p)
  X <-  matrix(alldata, nrow = n, ncol = p_signal)
  W_ <-  rep(beta_signal, p_signal)
  b0 <-  log(base_prev/(1- base_prev))
  lp <-  X %*% W_ + b0
  y_prob <-  1/(1+exp(-lp))
  
  #generate outcome
  y <-  rbinom(n,1,y_prob)
  data <- cbind(y, X) |> data.frame()
  x_names <- paste0("x", 1:(ncol(data)-1))
  data_names <- c("y", x_names)
  colnames(data) <- data_names
  return(data)
}

fit_model <- function(data) {
  logistic_model <- glm("y ~ .", data = data, family = "binomial")
}

# Get performance must be a function of data and a model objedt
get_performance <- function(data, model) {
  y <- data[,1]
  x <- data[,-1]
  y_hat = predict(model, x, type = "response")
  auc = pROC::auc(y, as.numeric(y_hat), quiet = TRUE)
  return(auc[1])
}

a1 = calculate_sample_size(data_generating_function = generate_data,
                      model_function = fit_model,
                      performance_function = get_performance,
                      target_performance = 0.65,
                      test_n = 10000,
                      tune_param = 0.7,
                      min_sample_size = 100,
                      max_sample_size = 3000,
                      n_reps = 100,
                      n_sample_sizes = 10)


if (!require(mlpwr)) {
  devtools::install_github("flxzimmer/mlpwr",ref="dev2")
} 
library(mlpwr)

a2 = calculate_sample_size2(data_generating_function = generate_data,
                      model_function = fit_model,
                      performance_function = get_performance,
                      target_performance = .65,
                      test_n = 10000,
                      tune_param = 0.7,
                      min_sample_size = 100,
                      max_sample_size = 3000,
                      n_reps = 100,
                      n_sample_sizes = 10)



