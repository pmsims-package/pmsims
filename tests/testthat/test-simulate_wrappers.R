mock_simulate_custom <- function(...) {
  args <- list(...)

  list(
    outcome = attr(args$data_function, "outcome"),
    min_n = 80,
    perf_n = args$target_performance,
    target_performance = args$target_performance,
    summaries = list(mean_performance = c(`80` = args$target_performance)),
    results = matrix(
      args$target_performance,
      nrow = 1,
      dimnames = list("80", NULL)
    ),
    mlpwr_ds = NULL
  )
}

test_that("simulate_binary returns a pmsims object", {
  local_mocked_bindings(
    binary_tuning = function(...) c(mu_lp = 0, sigma_sq = 1, beta_signal = 0.3),
    simulate_custom = mock_simulate_custom,
    .package = "pmsims"
  )

  result <- simulate_binary(
    signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "continuous",
    outcome_prevalence = 0.2,
    maximum_achievable_cstatistic = 0.75,
    metric = "calibration_slope",
    target_performance = 0.9,
    n_reps_total = 40,
    mean_or_assurance = "assurance"
  )

  expect_s3_class(result, "pmsims")
  expect_equal(result$outcome, "binary")
  expect_true(is.numeric(result$min_n))
  expect_gt(result$min_n, 0)
  expect_equal(result$target_performance, 0.9)
})

test_that("simulate_continuous returns a pmsims object", {
  local_mocked_bindings(
    continuous_tuning = function(...) 0.25,
    simulate_custom = mock_simulate_custom,
    .package = "pmsims"
  )

  result <- simulate_continuous(
    signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "continuous",
    maximum_achievable_rsquared = 0.5,
    metric = "calibration_slope",
    target_performance = 0.9,
    n_reps_total = 40,
    mean_or_assurance = "assurance"
  )

  expect_s3_class(result, "pmsims")
  expect_equal(result$outcome, "continuous")
  expect_true(is.numeric(result$min_n))
  expect_gt(result$min_n, 0)
  expect_equal(result$target_performance, 0.9)
})

test_that("simulate_survival returns a pmsims object", {
  local_mocked_bindings(
    survival_tuning = function(...) {
      c(lambda_opt = 0.1, sigma_sq = 0.2, beta_signal = 0.3)
    },
    simulate_custom = mock_simulate_custom,
    .package = "pmsims"
  )

  result <- simulate_survival(
    signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "continuous",
    maximum_achievable_cindex = 0.75,
    baseline_hazard = 0.01,
    censoring_rate = 0.3,
    metric = "calibration_slope",
    target_performance = 0.9,
    n_reps_total = 40,
    mean_or_assurance = "assurance"
  )

  expect_s3_class(result, "pmsims")
  expect_equal(result$outcome, "survival")
  expect_true(is.numeric(result$min_n))
  expect_gt(result$min_n, 0)
  expect_equal(result$target_performance, 0.9)
})

test_that("wrapper calibration slope bounds are enforced", {
  expect_error(
    simulate_binary(
      signal_parameters = 10,
      noise_parameters = 0,
      predictor_type = "continuous",
      outcome_prevalence = 0.2,
      maximum_achievable_cstatistic = 0.75,
      metric = "calibration_slope",
      target_performance = 0.7,
      n_reps_total = 40,
      mean_or_assurance = "assurance"
    ),
    "Requested target calibration slope is too low; check and try again.",
    fixed = TRUE
  )

  expect_error(
    simulate_continuous(
      signal_parameters = 10,
      noise_parameters = 0,
      predictor_type = "continuous",
      maximum_achievable_rsquared = 0.5,
      metric = "calibration_slope",
      target_performance = 1.3,
      n_reps_total = 40,
      mean_or_assurance = "assurance"
    ),
    "Requested target calibration slope is too high; check and try again.",
    fixed = TRUE
  )

  expect_error(
    simulate_survival(
      signal_parameters = 10,
      noise_parameters = 0,
      predictor_type = "continuous",
      maximum_achievable_cindex = 0.75,
      baseline_hazard = 0.01,
      censoring_rate = 0.3,
      metric = "calibration_slope",
      target_performance = 0.7,
      n_reps_total = 40,
      mean_or_assurance = "assurance"
    ),
    "Requested target calibration slope is too low; check and try again.",
    fixed = TRUE
  )
})

test_that("simulate_binary requires achievable AUC targets", {
  expect_error(
    simulate_binary(
      signal_parameters = 10,
      noise_parameters = 0,
      predictor_type = "continuous",
      outcome_prevalence = 0.2,
      maximum_achievable_cstatistic = 0.80,
      metric = "auc",
      target_performance = 0.9,
      n_reps_total = 40,
      mean_or_assurance = "assurance"
    ),
    "Requested target AUC must be less than the maximum achievable AUC because both are specified on the same metric scale.",
    fixed = TRUE
  )
})

test_that("simulate_binary errors when maximum achievable AUC equals target", {
  expect_error(
    simulate_binary(
      signal_parameters = 10,
      noise_parameters = 0,
      predictor_type = "continuous",
      outcome_prevalence = 0.2,
      maximum_achievable_cstatistic = 0.8,
      metric = "auc",
      target_performance = 0.8,
      n_reps_total = 40,
      mean_or_assurance = "assurance"
    ),
    "Requested target AUC must be less than the maximum achievable AUC because both are specified on the same metric scale.",
    fixed = TRUE
  )
})
