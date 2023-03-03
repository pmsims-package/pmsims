generate_data <- function(n, beta_signal) {
  p_signal <- 10    # number of predictors
  prob_p  <-  0.1 # probability of a predictor to be 1
  base_prev  <-  0.3 # baseline probability of a positive outcome if all risks are 0

  alldata <-  rbinom(n*p_signal,1,prob_p)
  X <-  matrix(alldata, nrow = n, ncol = p_signal)
  W_ <-  rep(beta_signal, p_signal)
  b0 <-  log(base_prev/(1- base_prev))
  lp <-  X %*% W_ + b0
  y_prob <-  1/(1+exp(-lp))

  #generate outcome from the probabilities calculated based on individual risk
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

simulation_parameters <- get_simulation_parameters(100, 1000, 100, 100)

run_simulations(simulation_parameters,
                data_generating_function = generate_data,
                model_function = fit_model,
                performance_function = get_performance,
                test_n = 10000,
                tune_param = 0.7)

?qgam
