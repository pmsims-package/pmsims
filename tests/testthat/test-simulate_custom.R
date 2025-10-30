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

test_that("simulate_custom", {
  set.seed(1234)
  data_opts <- list(
    type = "binary",
    args = list(
      n_signal_parameters = 5,
      noise_parameters = 5,
      predictor_type = "continuous",
      baseline_prob = 0.2
    )
  )
  data_function <- default_data_generators(data_opts)
  outcome_type <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome_type, model = "glm")

  metric_function = default_metric_generator(
    "auc",
    data_function
  )

  tuning_parameter <- default_tune(
    tune_param = beta_signal,
    max_sample_size = 10000,
    large_sample_performance = 0.8,
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function
  )
  tuned_data_opts <- list(
    type = "binary",
    args = list(
      n_signal_parameters = 5,
      noise_parameters = 5,
      predictor_type = "continuous",
      baseline_prob = 0.2,
      beta_signal = tuning_parameter
    )
  )

  tuned_data_function <- default_data_generators(tuned_data_opts)

  sim_results_mlpwr <- simulate_custom(
    data_function = tuned_data_function,
    model_function = model_function,
    metric_function = metric_function,
    target_performance = 0.75,
    test_n = 10000,
    min_sample_size = 75,
    max_sample_size = 500,
    n_reps_total = 100,
    n_reps_per = 10,
    se_final = NULL,
    n_init = 4,
    method = "mlpwr",
    verbose = FALSE
  )

  expect_equal(length(sim_results_mlpwr), 8)
  expect_equal(sim_results_mlpwr$min_n, 130, tolerance = 20)

  sim_results_crude <- simulate_custom(
    data_function = tuned_data_function,
    model_function = model_function,
    metric_function = metric_function,
    target_performance = 0.75,
    test_n = 10000,
    min_sample_size = 75,
    max_sample_size = 500,
    n_reps_total = 100,
    n_reps_per = 10,
    se_final = NULL,
    n_init = 4,
    method = "crude",
    verbose = FALSE
  )

  expect_equal(length(sim_results_crude), 8)
  expect_equal(sim_results_crude$min_n, 130, tolerance = 20)
})
