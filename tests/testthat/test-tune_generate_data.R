# data generating function returns dataframe not matrix as logistic regression needs df- this is something we need to consider in error checking for the function.
generate_data <- function(n, beta_signal) {
  p_signal <- 10 # number of predictors
  prob_p <- 0.1 # probability of a predictor to be 1
  base_prev <- 0.3 # baseline probability of a positive outcome if all risks are 0

  alldata <- rbinom(n * p_signal, 1, prob_p)
  X <- matrix(alldata, nrow = n, ncol = p_signal)
  W_ <- rep(beta_signal, p_signal)
  b0 <- log(base_prev / (1 - base_prev))
  lp <- X %*% W_ + b0
  y_prob <- 1 / (1 + exp(-lp))

  # generate outcome from the probabilities calculated based on individual risk
  y <- rbinom(n, 1, y_prob)
  data <- cbind(y, X) |> data.frame()
  x_names <- paste0("x", 1:(ncol(data) - 1))
  data_names <- c("y", x_names)
  colnames(data) <- data_names
  return(data)
}

fit_model <- default_model_generators(outcome = "binary", model = "glm")

# Get performance must be a function of data and a model object

test_that("tune_generate_data works", {
  tune_param <- tune_generate_data(
    data_function = generate_data,
    large_n = 10000,
    interval = c(0,1),
    model_function = fit_model,
    metric_function = binary_auc_metric,
    target_performance = 0.7
  )
  expect_equal(length(tune_param), 1)

  train_data <- generate_data(10000, tune_param)
  test_data <- generate_data(10000, tune_param)
  model <- fit_model(train_data)
  performance <- binary_auc_metric(test_data,fit =  model, model = "glm")

  expect_equal(performance, 0.7, tol = 0.1)
})

test_that("interval_expansion works", {
  tune_param <- tune_generate_data(
    data_function = generate_data,
    large_n = 10000,
    interval = c(0,0.1),
    model_function = fit_model,
    metric_function = binary_auc_metric,
    target_performance = 0.7
  )
  expect_equal(length(tune_param), 1)
  
  train_data <- generate_data(10000, tune_param)
  test_data <- generate_data(10000, tune_param)
  model <- fit_model(train_data)
  performance <- binary_auc_metric(test_data,fit =  model, model = "glm")
  
  expect_equal(performance, 0.7, tol = 0.1)
})

test_that("default_tune", {
  set.seed(1234)
  
  large_sample_performance <- 0.8
  
  data_opts <- list(type = "binary", 
                    args = list(
                      n_signal_parameters = 5,
                      noise_parameters = 5,
                      predictor_type = "continuous",
                      baseline_prob = 0.2
                    ))
  data_function <- default_data_generators(data_opts)
  outcome_type <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome_type, model = "glm")
  
  metric_function = default_metric_generator(
    "auc",
    data_function
  )
  
  tuning_parameter <- default_tune(tune_param = beta_signal, 
                                   max_sample_size = 10000,
                                   large_sample_performance = large_sample_performance,
                                   data_function = data_function,
                                   model_function = model_function,
                                   metric_function = metric_function)
  tuned_data_opts <- list(type = "binary", 
                          args = list(
                            n_signal_parameters = 5,
                            noise_parameters = 5,
                            predictor_type = "continuous",
                            baseline_prob = 0.2,
                            beta_signal = tuning_parameter
                          ))
  
  tuned_data_function <- default_data_generators(tuned_data_opts)
  
  
  large_sample <- tuned_data_function(n = 10000)
  fitted_model <- model_function(large_sample)
  performance <- metric_function(large_sample, fit = fitted_model, model = "glm")
  expect_equal(performance, large_sample_performance, tolerance = 0.02)
  
})
  
