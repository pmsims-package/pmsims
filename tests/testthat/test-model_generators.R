test_that("cox_model", {
  inputs <- parse_inputs(
    data_spec = list(
      type = "survival",
      args = list(
        n_signal_parameters = 5,
        noise_parameters = 5,
        predictor_type = "continuous",
        beta_signal = 0.1,
        baseline_hazard = 0.01,
        censoring_rate = 0.2
      )
    ),
    metric = "auc",
    model = "coxph"
  )

  data <- inputs$data_function(1000)
  test_data <- inputs$data_function(1000)
  fit <- inputs$model_function(data)
  metric <- inputs$metric_function(test_data, fit)
  expect_equal(class(fit), "coxph")
})
