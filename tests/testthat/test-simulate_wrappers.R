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
  expect_identical(result$signal_parameters, 10)
  expect_null(result$parameters)
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
  expect_identical(result$signal_parameters, 10)
  expect_null(result$parameters)
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
  expect_identical(result$signal_parameters, 10)
  expect_null(result$parameters)
})

test_that("wrapper dots are forwarded to simulate_custom", {
  binary_args <- NULL
  continuous_args <- NULL
  survival_args <- NULL

  local_mocked_bindings(
    binary_tuning = function(...) c(mu_lp = 0, sigma_sq = 1, beta_signal = 0.3),
    continuous_tuning = function(...) 0.25,
    survival_tuning = function(...) {
      c(lambda_opt = 0.1, sigma_sq = 0.2, beta_signal = 0.3)
    },
    simulate_custom = function(...) {
      args <- list(...)

      outcome <- attr(args$data_function, "outcome")
      if (identical(outcome, "binary")) {
        binary_args <<- args
      } else if (identical(outcome, "continuous")) {
        continuous_args <<- args
      } else if (identical(outcome, "survival")) {
        survival_args <<- args
      }

      mock_simulate_custom(...)
    },
    .package = "pmsims"
  )

  simulate_binary(
    signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "continuous",
    outcome_prevalence = 0.2,
    maximum_achievable_cstatistic = 0.75,
    metric = "calibration_slope",
    target_performance = 0.9,
    n_reps_total = 40,
    mean_or_assurance = "assurance",
    progress = FALSE,
    min_sample_size = 50
  )

  simulate_continuous(
    signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "continuous",
    maximum_achievable_rsquared = 0.5,
    metric = "calibration_slope",
    target_performance = 0.9,
    n_reps_total = 40,
    mean_or_assurance = "assurance",
    progress = FALSE,
    min_sample_size = 60
  )

  simulate_survival(
    signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "continuous",
    maximum_achievable_cindex = 0.75,
    baseline_hazard = 0.01,
    censoring_rate = 0.3,
    metric = "calibration_slope",
    target_performance = 0.9,
    n_reps_total = 40,
    mean_or_assurance = "assurance",
    progress = FALSE,
    min_sample_size = 70
  )

  expect_false(binary_args$progress)
  expect_false(continuous_args$progress)
  expect_false(survival_args$progress)

  expect_identical(binary_args$min_sample_size, 50)
  expect_identical(continuous_args$min_sample_size, 60)
  expect_identical(survival_args$min_sample_size, 70)
})

test_that("wrappers store binary predictor prevalence on output", {
  local_mocked_bindings(
    binary_tuning = function(...) c(mu_lp = 0, sigma_sq = 1, beta_signal = 0.3),
    continuous_tuning = function(...) 0.25,
    survival_tuning = function(...) {
      c(lambda_opt = 0.1, sigma_sq = 0.2, beta_signal = 0.3)
    },
    simulate_custom = mock_simulate_custom,
    .package = "pmsims"
  )

  binary_result <- simulate_binary(
    signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "binary",
    binary_predictor_prevalence = 0.3,
    outcome_prevalence = 0.2,
    maximum_achievable_cstatistic = 0.75,
    metric = "calibration_slope",
    target_performance = 0.9,
    n_reps_total = 40,
    mean_or_assurance = "assurance"
  )

  continuous_result <- simulate_continuous(
    signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "binary",
    binary_predictor_prevalence = 0.4,
    maximum_achievable_rsquared = 0.5,
    metric = "calibration_slope",
    target_performance = 0.9,
    n_reps_total = 40,
    mean_or_assurance = "assurance"
  )

  survival_result <- simulate_survival(
    signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "binary",
    binary_predictor_prevalence = 0.25,
    maximum_achievable_cindex = 0.75,
    baseline_hazard = 0.01,
    censoring_rate = 0.3,
    metric = "calibration_slope",
    target_performance = 0.9,
    n_reps_total = 40,
    mean_or_assurance = "assurance"
  )

  expect_identical(binary_result$binary_predictor_prevalence, 0.3)
  expect_identical(continuous_result$binary_predictor_prevalence, 0.4)
  expect_identical(survival_result$binary_predictor_prevalence, 0.25)
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
