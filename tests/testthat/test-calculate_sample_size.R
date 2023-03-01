
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


test_that("get_simulation_parameters", {
  simulation_parameters <- get_simulation_parameters(100, 1000, 100, 5)
  expect_equal(simulation_parameters,
               data.frame(train_size = c(100, 325, 550, 775, 1000),
                          n_sims = rep(20,5)))
  simulation_parameters <- get_simulation_parameters(100, 3000, 100, 10)
  expect_type(simulation_parameters$train_size, "integer")
  expect_type(simulation_parameters$n_sims, "integer")

})

test_that("get_performance_n", {
  test_data <- generate_data(10000, 0.7)
  performance <- get_performance_n(n = 10000,
                    test_data = test_data,
                    data_generating_function = generate_data,
                    model_function = fit_model,
                    performance_function = get_performance,
                    tune_param = 0.7)
  expect_equal(performance, 0.75, tol = 0.25) # tests that performance is between 0.5 and 1
})

test_that("calculate_sample_size", {
  set.seed(1234)
  sample_size <- calculate_sample_size(data_generating_function = generate_data,
                                       model_function = fit_model,
                                       performance_function = get_performance,
                                       target_performance = 0.75,
                                       test_n = 10000,
                                       tune_param = 0.7,
                                       min_sample_size = 100,
                                       max_sample_size = 3000,
                                       n_reps = 10,
                                       n_sample_sizes = 10)
  expect_equal(length(sample_size), 5)
  expect_equal(length(sample_size$min_n), 1)
  expect_equal(length(sample_size$target), 1)
  expect_equal(ncol(sample_size$summaries), 4)
  expect_equal(length(sample_size$train_size), 10)
})

