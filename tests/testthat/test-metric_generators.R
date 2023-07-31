test_that("survival_auc", {
  inputs <- parse_inputs(data_spec = list(type = "survival",
                                          args = list(
                                            signal_parameters = 5, 
                                            noise_parameters = 5, 
                                            predictor_type = "continuous", 
                                            beta_signal = 0.1,
                                            baseline_hazard = 0.01,
                                            censoring_rate = 0.2
                                          )
  ),
  metric="auc")
  set.seed(1234)
  data <- inputs$data_function(1000)
  test_data <- inputs$data_function(1000)
  fit <- inputs$model_function(data)
  metric <- inputs$metric_function(test_data,fit)
  
  expect_false(is.na(metric))
})


test_that("continuous_r2", {
  inputs <- parse_inputs(data_spec = list(type = "continuous",
                                          args = list(
                                            signal_parameters = 5, 
                                            noise_parameters = 5, 
                                            predictor_type = "continuous", 
                                            beta_signal = 0.1
                                          )
  ),
  metric="r2")
  set.seed(1234)
  data <- inputs$data_function(1000)
  test_data <- inputs$data_function(1000)
  fit <- inputs$model_function(data)
  metric <- inputs$metric_function(test_data,fit)
  
  expect_false(is.na(metric))
})

test_that("continuous_calib_slope", {
  inputs <- parse_inputs(data_spec = list(type = "continuous",
                                          args = list(
                                            signal_parameters = 5, 
                                            noise_parameters = 5, 
                                            predictor_type = "continuous", 
                                            beta_signal = 0.1
                                          )
  ),
  metric="calib_slope")
  set.seed(1234)
  data <- inputs$data_function(1000)
  test_data <- inputs$data_function(1000)
  fit <- inputs$model_function(data)
  metric <- inputs$metric_function(test_data,fit)
  
  expect_false(is.na(metric))
})

test_that("continuous_calib_itl", {
  inputs <- parse_inputs(data_spec = list(type = "continuous",
                                          args = list(
                                            signal_parameters = 5, 
                                            noise_parameters = 5, 
                                            predictor_type = "continuous", 
                                            beta_signal = 0.1
                                          )
  ),
  metric="calib_itl")
  set.seed(1234)
  data <- inputs$data_function(1000)
  test_data <- inputs$data_function(1000)
  fit <- inputs$model_function(data)
  metric <- inputs$metric_function(test_data,fit)
  
  expect_false(is.na(metric))
})

