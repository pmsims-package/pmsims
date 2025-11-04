test_that("simulate_binary returns a pmsims object", {
  skip_on_cran()
  set.seed(2024)

  result <- simulate_binary(
    signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "continuous",
    outcome_prevalence = 0.2,
    large_sample_cstatistic = 0.75,
    metric = "calibration_slope",
    minimum_acceptable_performance = 0.9,
    n_reps_total = 1000,
    mean_or_assurance = "assurance"
  )

  expect_s3_class(result, "pmsims")
  expect_equal(result$outcome, "binary")
  expect_true(is.numeric(result$min_n))
  expect_gt(result$min_n, 0)
  expect_equal(result$target_performance, 0.9)
})

test_that("simulate_continuous returns a pmsims object", {
  skip_on_cran()
  set.seed(1111)

  result <- simulate_continuous(
    signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "continuous",
    large_sample_rsquared = 0.5,
    metric = "calibration_slope",
    minimum_acceptable_performance = 0.9,
    n_reps_total = 1000,
    mean_or_assurance = "assurance"
  )

  expect_s3_class(result, "pmsims")
  expect_equal(result$outcome, "continuous")
  expect_true(is.numeric(result$min_n))
  expect_gt(result$min_n, 0)
  expect_equal(result$target_performance, 0.9)
})

test_that("simulate_survival returns a pmsims object", {
  skip_on_cran()
  set.seed(765)

  result <- simulate_survival(
    signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "continuous",
    large_sample_cindex = 0.75,
    baseline_hazard = 0.01,
    censoring_rate = 0.3,
    metric = "calibration_slope",
    minimum_acceptable_performance = 0.9,
    n_reps_total = 1000,
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
      large_sample_cstatistic = 0.75,
      metric = "calibration_slope",
      minimum_acceptable_performance = 0.7,
      n_reps_total = 1000,
      mean_or_assurance = "assurance"
    ),
    "Suggested calibration slope is too low",
    fixed = TRUE
  )

  expect_error(
    simulate_continuous(
      signal_parameters = 10,
      noise_parameters = 0,
      predictor_type = "continuous",
      large_sample_rsquared = 0.5,
      metric = "calibration_slope",
      minimum_acceptable_performance = 1.3,
      n_reps_total = 1000,
      mean_or_assurance = "assurance"
    ),
    "Suggested calibration slope is too high",
    fixed = TRUE
  )

  expect_error(
    simulate_survival(
      signal_parameters = 10,
      noise_parameters = 0,
      predictor_type = "continuous",
      large_sample_cindex = 0.75,
      baseline_hazard = 0.01,
      censoring_rate = 0.3,
      metric = "calibration_slope",
      minimum_acceptable_performance = 0.7,
      n_reps_total = 1000,
      mean_or_assurance = "assurance"
    ),
    "Suggested calibration slope is too low",
    fixed = TRUE
  )
})

test_that("simulate_binary requires achievable AUC targets", {
  expect_error(
    simulate_binary(
      signal_parameters = 10,
      noise_parameters = 0
      predictor_type = "continuous",
      outcome_prevalence = 0.2,
      large_sample_cstatistic = 0.80,
      metric = "auc",
      minimum_acceptable_performance = 0.9,
      n_reps_total = 1000,
      mean_or_assurance = "assurance"
    ),
    "Requested minimum acceptable AUC exceeds the expected large-sample performance",
    fixed = TRUE
  )
})
