# data generating function returns dataframe not matrix as logistic regression needs df- this is something we need to consider in error checking for the function.
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

test_that("tune_generate_data works", {
  tune_param <- tune_generate_data(data_generating_function = generate_data,
                     large_n = 10000,
                     min_tune_arg = 0,
                     max_tune_arg = 1,
                     model_function = fit_model,
                     performance_function = get_performance,
                     target_large_sample_performance = 0.7)
  expect_equal(length(tune_param), 1)

  train_data <- generate_data(10000, tune_param)
  test_data <- generate_data(10000, tune_param)
  model <- fit_model(train_data)
  performance <- get_performance(test_data, model)

  expect_equal(performance, 0.7, tol = 0.1)

})
