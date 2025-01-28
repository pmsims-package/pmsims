test_that("generate_predictors", {
  data <- generate_predictors(n = 100,
                            parameters = 10,
                            type = "continuous")
  expect_equal(nrow(data), 100)
  expect_equal(ncol(data), 10)
  
  data2 <- generate_predictors(n = 100,
                              parameters = 10,
                              type = "binary",
                              predictor_prop = 0.3)
  expect_equal(nrow(data2), 100)
  expect_equal(ncol(data2), 10)
  expect_equal(colnames(data2), paste0("x", 1:10))
  
})

test_that("generate_linear_predictor", {
  X <- generate_predictors(n = 100,
                              parameters = 10,
                              type = "continuous")
  lp <- generate_linear_predictor(X,
                                  n_signal_parameters = 5, 
                                  noise_parameters = 5,
                                  intercept = 1,
                                  beta_signal = 0.5)
  expect_equal(length(lp), 100)
  
  
})


test_that("generate_continuous_data", {
 signal_parameters <- 5
   noise_parameters <- 5
  data <- generate_continuous_data(
   n = 100, 
   n_signal_parameters = signal_parameters, 
   noise_parameters = noise_parameters, 
   predictor_type = "binary",
   predictor_prop = 0.1, 
   beta_signal = 0.1)
 
  expect_equal(nrow(data), 100)
  expect_equal(ncol(data), 11) 
  expect_equal(colnames(data),c("y", paste0("x", 1:(signal_parameters+signal_parameters))))
})

test_that("generate_binary_data", {
  signal_parameters <- 5
  noise_parameters <- 5
  data <- generate_binary_data(
    n = 100, 
    n_signal_parameters = signal_parameters, 
    noise_parameters = noise_parameters, 
    predictor_type = "continuous", 
    beta_signal = 0.1,
    baseline_prob = 0.1)
  
  expect_equal(nrow(data), 100)
  expect_equal(ncol(data), 11) 
  expect_equal(colnames(data),c("y", paste0("x", 1:(signal_parameters+signal_parameters))))
})

test_that("generate_survival_data", {
  signal_parameters <- 5
  noise_parameters <- 5
  data <- generate_survival_data(
    n = 100, 
    n_signal_parameters = signal_parameters, 
    noise_parameters = noise_parameters, 
    predictor_type = "continuous", 
    beta_signal = 0.1,
    baseline_hazard = 0.02,
    censoring_rate = 0.3)
  
  expect_equal(nrow(data), 100)
  expect_equal(ncol(data), signal_parameters+noise_parameters+2) 
  expect_equal(colnames(data), c("time", "event", paste0("x", 1:(signal_parameters+noise_parameters))))
})


test_that("update_arguments", {
  signal_parameters <- 5
  noise_parameters <- 5
  opts <- list(
    args = list(
      n_signal_parameters = signal_parameters, 
       noise_parameters = noise_parameters, 
       predictor_type = "binary",
       predictor_prop = 0.1, 
       beta_signal = 0.1))
  f <- update_arguments(generate_continuous_data, opts)
  
  expect_type(f , "closure")
  data <- f(100)
  
  expect_equal(nrow(data), 100)
  expect_equal(ncol(data), 11) 
  
})




test_that("default_data_generators", {
  signal_parameters <- 5
  noise_parameters <- 5
  opts <- list(
    type = "continuous",
    args = list(
      n_signal_parameters = signal_parameters, 
      noise_parameters = noise_parameters, 
      predictor_type = "binary",
      predictor_prop = 0.1, 
      beta_signal = 0.1))
              
  f <- default_data_generators(opts)
  
  expect_type(f , "closure")
  data <- f(100)
  
  expect_equal(nrow(data), 100)
  expect_equal(ncol(data), 11) 
  expect_equal(attr(f, "outcome"), "continuous")

})


