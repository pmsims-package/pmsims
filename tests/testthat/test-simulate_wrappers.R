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
  expect_identical(result$metric, "calibration_slope")
  expect_identical(result$metric_2, "auc")
  expect_identical(result$outcome_prevalence, 0.2)
  expect_identical(result$maximum_achievable_cstatistic, 0.75)
  expect_null(result$prevalence)
  expect_null(result$cstatistic)
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
  expect_identical(result$maximum_achievable_rsquared, 0.5)
  expect_null(result$r2)
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
  expect_identical(result$maximum_achievable_cindex, 0.75)
  expect_null(result$cstatistic)
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
    data_control = list(
      predictor_distribution = "binary",
      binary_predictor_prevalence = 0.3
    ),
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
    data_control = list(
      predictor_distribution = "binary",
      binary_predictor_prevalence = 0.4
    ),
    maximum_achievable_rsquared = 0.5,
    metric = "calibration_slope",
    target_performance = 0.9,
    n_reps_total = 40,
    mean_or_assurance = "assurance"
  )

  survival_result <- simulate_survival(
    signal_parameters = 10,
    noise_parameters = 0,
    data_control = list(
      predictor_distribution = "binary",
      binary_predictor_prevalence = 0.25
    ),
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

test_that("machine learning wrappers search on the CSSE scale internally", {
  skip_if_not_installed("glmnet")

  captured <- NULL

  local_mocked_bindings(
    binary_tuning = function(...) c(mu_lp = 0, sigma_sq = 1, beta_signal = 0.3),
    simulate_custom = function(...) {
      captured <<- list(...)
      mock_simulate_custom(...)
    },
    .package = "pmsims"
  )

  result <- simulate_binary(
    signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "continuous",
    outcome_prevalence = 0.2,
    maximum_achievable_cstatistic = 0.75,
    model = "ridge",
    metric = "calibration_slope",
    target_performance = 0.9,
    n_reps_total = 40,
    mean_or_assurance = "assurance"
  )

  # The search ran against CSSE with a converted target ...
  expect_identical(attr(captured$metric_function, "metric"), "csse")
  expect_equal(captured$target_performance, -0.01)

  # ... but the user sees the calibration slope throughout.
  expect_identical(result$metric, "calibration_slope")
  expect_equal(result$target_performance, 0.9)
  expect_equal(result$perf_n, 0.9)
  expect_true(result$internal_csse)
  expect_equal(result$csse_direction, "below")
  expect_identical(result$metric_2, "auc")
})

test_that("non-machine-learning wrappers use the calibration slope directly", {
  captured <- NULL

  local_mocked_bindings(
    binary_tuning = function(...) c(mu_lp = 0, sigma_sq = 1, beta_signal = 0.3),
    simulate_custom = function(...) {
      captured <<- list(...)
      mock_simulate_custom(...)
    },
    .package = "pmsims"
  )

  result <- simulate_binary(
    signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "continuous",
    outcome_prevalence = 0.2,
    maximum_achievable_cstatistic = 0.75,
    model = "glm",
    metric = "calibration_slope",
    target_performance = 0.9,
    n_reps_total = 40,
    mean_or_assurance = "assurance"
  )

  expect_identical(
    attr(captured$metric_function, "metric"),
    "calibration_slope"
  )
  expect_equal(captured$target_performance, 0.9)
  expect_null(result$internal_csse)
})

test_that("an explicit CSSE request is passed through unchanged", {
  skip_if_not_installed("glmnet")

  captured <- NULL

  local_mocked_bindings(
    binary_tuning = function(...) c(mu_lp = 0, sigma_sq = 1, beta_signal = 0.3),
    simulate_custom = function(...) {
      captured <<- list(...)
      mock_simulate_custom(...)
    },
    .package = "pmsims"
  )

  result <- simulate_binary(
    signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "continuous",
    outcome_prevalence = 0.2,
    maximum_achievable_cstatistic = 0.75,
    model = "ridge",
    metric = "csse",
    target_performance = -0.01,
    n_reps_total = 40,
    mean_or_assurance = "assurance"
  )

  # The user's target is used as supplied, with no adjustment.
  expect_identical(attr(captured$metric_function, "metric"), "csse")
  expect_equal(captured$target_performance, -0.01)
  expect_identical(result$metric, "csse")
  expect_equal(result$target_performance, -0.01)
  expect_equal(result$perf_n, -0.01)
  expect_null(result$internal_csse)
})

test_that("simulate_continuous converts the calibration slope for ML models", {
  skip_if_not_installed("glmnet")

  captured <- NULL

  local_mocked_bindings(
    continuous_tuning = function(...) 0.25,
    simulate_custom = function(...) {
      captured <<- list(...)
      mock_simulate_custom(...)
    },
    .package = "pmsims"
  )

  result <- simulate_continuous(
    signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "continuous",
    maximum_achievable_rsquared = 0.5,
    model = "ridge",
    metric = "calibration_slope",
    target_performance = 0.9,
    n_reps_total = 40,
    mean_or_assurance = "assurance"
  )

  expect_identical(attr(captured$metric_function, "metric"), "csse")
  expect_equal(captured$target_performance, -0.01)
  expect_identical(result$metric, "calibration_slope")
  expect_equal(result$target_performance, 0.9)
  expect_equal(result$perf_n, 0.9)
  expect_true(result$internal_csse)
  expect_identical(result$metric_2, "r2")
})

test_that("simulate_survival converts the calibration slope for ML models", {
  skip_if_not_installed("glmnet")

  captured <- NULL

  local_mocked_bindings(
    survival_tuning = function(...) {
      c(lambda_opt = 0.1, sigma_sq = 0.2, beta_signal = 0.3)
    },
    simulate_custom = function(...) {
      captured <<- list(...)
      mock_simulate_custom(...)
    },
    .package = "pmsims"
  )

  # The unmocked second-metric evaluation fits a Cox ridge, which emits glmnet
  # tie-handling deprecation notices unrelated to this test.
  result <- suppressWarnings(simulate_survival(
    signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "continuous",
    maximum_achievable_cindex = 0.7,
    baseline_hazard = 0.01,
    censoring_rate = 0.3,
    model = "ridge",
    metric = "calibration_slope",
    target_performance = 0.9,
    n_reps_total = 40,
    mean_or_assurance = "assurance"
  ))

  expect_identical(attr(captured$metric_function, "metric"), "csse")
  expect_equal(captured$target_performance, -0.01)
  expect_identical(result$metric, "calibration_slope")
  expect_equal(result$target_performance, 0.9)
  expect_equal(result$perf_n, 0.9)
  expect_true(result$internal_csse)
  expect_identical(result$metric_2, "cindex")
})

test_that("a calibration slope target above 1 is restored above 1", {
  skip_if_not_installed("glmnet")

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
    model = "ridge",
    metric = "calibration_slope",
    target_performance = 1.1,
    n_reps_total = 40,
    mean_or_assurance = "assurance"
  )

  expect_equal(result$csse_direction, "above")
  expect_equal(result$target_performance, 1.1)
  expect_equal(result$perf_n, 1.1)
})

test_that("resolve_data_control reports the configuration the generator uses", {
  # Requested and effective values coincide when nothing is substituted.
  plain <- resolve_data_control(list(predictor_distribution = "uniform"), 1)
  expect_identical(plain$effective_distribution, "uniform")
  expect_identical(plain$effective_nonlinear_strength, 0)

  # Complexity 4 swaps a left-at-default "normal" for the Friedman-canonical
  # "uniform", so the requested value would misdescribe the simulated data.
  friedman <- resolve_data_control(NULL, 4)
  expect_identical(friedman$predictor_distribution, "normal")
  expect_identical(friedman$effective_distribution, "uniform")

  # An explicit choice at complexity 4 is left alone.
  explicit <- resolve_data_control(list(predictor_distribution = "t"), 4)
  expect_identical(explicit$effective_distribution, "t")

  # An unset nonlinear_strength picks up the per-complexity default.
  expect_null(resolve_data_control(NULL, 2)$nonlinear_strength)
  expect_identical(
    resolve_data_control(NULL, 2)$effective_nonlinear_strength,
    0.2
  )
  expect_identical(
    resolve_data_control(NULL, 3)$effective_nonlinear_strength,
    0.3
  )

  # An explicit nonlinear_strength is passed through.
  explicit_ns <- resolve_data_control(list(nonlinear_strength = 0.45), 2)
  expect_identical(explicit_ns$effective_nonlinear_strength, 0.45)
})

test_that("wrappers record the effective data-generating configuration", {
  local_mocked_bindings(
    binary_tuning = function(...) c(mu_lp = 0, sigma_sq = 1, beta_signal = 0.3),
    simulate_custom = mock_simulate_custom,
    .package = "pmsims"
  )

  # The mocked tuner returns a fixed beta_signal rather than one tuned for the
  # Friedman signal, which makes the incidental metric_2 glm fit separate. That
  # is an artefact of the mock, not of the configuration being asserted here.
  result <- suppressWarnings(simulate_binary(
    signal_parameters = 10,
    noise_parameters = 0,
    complexity = 4,
    outcome_prevalence = 0.2,
    maximum_achievable_cstatistic = 0.75,
    metric = "calibration_slope",
    target_performance = 0.9,
    n_reps_total = 40,
    mean_or_assurance = "assurance"
  ))

  expect_identical(result$complexity, 4)
  # Not the requested "normal": complexity 4 draws uniform predictors.
  expect_identical(result$predictor_distribution, "uniform")
  expect_identical(result$correlation, 0.3)

  quadratic <- suppressWarnings(simulate_binary(
    signal_parameters = 10,
    noise_parameters = 0,
    complexity = 2,
    outcome_prevalence = 0.2,
    maximum_achievable_cstatistic = 0.75,
    metric = "calibration_slope",
    target_performance = 0.9,
    n_reps_total = 40,
    mean_or_assurance = "assurance"
  ))

  # Resolved from the complexity-level default rather than left NULL.
  expect_identical(quadratic$nonlinear_strength, 0.2)
})
