test_that("parse_inputs", {
  # Binary
  inputs <- parse_inputs(
    data_spec = list(
      type = "binary",
      args = list(
        n_signal_parameters = 5,
        noise_parameters = 5,
        predictor_type = "continuous",
        mu_lp = stats::qlogis(0.1),
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
        n_signal_parameters = 5,
        noise_parameters = 5,
        predictor_type = "continuous",
        beta_signal = 0.1,
        baseline_hazard = 0.01,
        censoring_rate = 0.3
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
      noise_parameters = 0,
      predictor_type = "continuous",
      baseline_prob = 0.2,
      mu_lp = stats::qlogis(0.2),
      beta_signal = 0.5
    )
  )
  data_function <- default_data_generators(data_opts)
  outcome_type <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome_type, model = "glm")

  metric_function = default_metric_generator(
    "auc",
    data_function
  )

  sim_results_mlpwr <- suppressWarnings(simulate_custom(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    target_performance = 0.75,
    c_statistic = 0.8,
    test_n = 2000,
    min_sample_size = 75,
    max_sample_size = 200,
    n_reps_total = 40,
    n_reps_per = 10,
    se_final = NULL,
    n_init = 4,
    method = "mlpwr",
    verbose = FALSE
  ))

  expect_s3_class(sim_results_mlpwr, "pmsims")
  expect_true(is.numeric(sim_results_mlpwr$min_n))
  expect_gt(sim_results_mlpwr$min_n, 0)
  expect_true(is.list(sim_results_mlpwr$summaries))

  sim_results_mlpwr_bs <- suppressWarnings(simulate_custom(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    target_performance = 0.75,
    c_statistic = 0.8,
    test_n = 2000,
    min_sample_size = 75,
    max_sample_size = 200,
    n_reps_total = 40,
    n_reps_per = 10,
    se_final = NULL,
    n_init = 4,
    method = "mlpwr-bs",
    verbose = FALSE
  ))

  expect_s3_class(sim_results_mlpwr_bs, "pmsims")
  expect_true(is.numeric(sim_results_mlpwr_bs$min_n))
  expect_gt(sim_results_mlpwr_bs$min_n, 0)
})
