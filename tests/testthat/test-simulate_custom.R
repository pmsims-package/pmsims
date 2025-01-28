test_that("parse_inputs", {
  # Binary
  inputs <- parse_inputs(
    data_spec = list(
      type = "binary",
      args = list(
        signal_parameters = 5,
        noise_parameters = 5,
        predictor_type = "continuous",
        beta_signal = 0.1,
        baseline_prob = 0.1
      )
    ),
    metric = "auc",
    model = "glm"
  )
  expect_equal(length(inputs), 3)
  
  # Continuous
  
  inputs <- parse_inputs(
    data_spec = list(
      type = "continuous",
      args = list(
        n_signal_parameters = 5,
        noise_parameters = 5,
        predictor_type = "continuous",
        beta_signal = 0.1
      )
    ),
    metric = "r2",
    model = "lm"
  )
  expect_equal(length(inputs), 3)
  
  # Survival
  inputs <- parse_inputs(
    data_spec = list(
      type = "survival",
      args = list(
        signal_parameters = 5,
        noise_parameters = 5,
        predictor_type = "continuous",
        beta_signal = 0.1
      )
    ),
    metric = "auc",
    model = "coxph"
    
  )
  expect_equal(length(inputs), 3)
})