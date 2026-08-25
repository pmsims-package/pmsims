test_that("calculate_metrics_perf returns metric values and uses fallback", {
  functions <- get_binary_data_model_metric()

  metric_value <- calculate_metrics_perf(
    n = 50,
    data_function = functions$data_function,
    model_function = functions$model_function,
    metric_function = functions$metric_function,
    value_on_error = -1
  )

  expect_true(is.numeric(metric_value))
  expect_gt(metric_value, 0)

  bad_model_function <- function(data) {
    stop("model fitting failed")
  }
  attr(bad_model_function, "model") <- attr(functions$model_function, "model")

  fallback <- calculate_metrics_perf(
    n = 50,
    data_function = functions$data_function,
    model_function = bad_model_function,
    metric_function = functions$metric_function,
    value_on_error = -1
  )

  expect_identical(fallback, -1)
})

test_that("validate_metric_constraints enforces calibration slope limits", {
  expect_error(
    validate_metric_constraints(
      metric = "calibration_slope",
      target_performance = 0.7
    ),
    "Requested target calibration slope is too low; check and try again.",
    fixed = TRUE
  )

  expect_error(
    validate_metric_constraints(
      metric = "calibration_slope",
      target_performance = 1.3
    ),
    "Requested target calibration slope is too high; check and try again.",
    fixed = TRUE
  )

  expect_error(
    validate_metric_constraints(
      metric = "auc",
      target_performance = 0.8,
      maximum_achievable_performance = 0.75
    ),
    "Requested target AUC must be less than the maximum achievable AUC because both are specified on the same metric scale.",
    fixed = TRUE
  )

  expect_silent(
    validate_metric_constraints(
      metric = "auc",
      target_performance = 0.75,
      maximum_achievable_performance = 0.8
    )
  )
})

test_that("validate_metric_constraints errors when maximum achievable equals target", {
  expect_error(
    validate_metric_constraints(
      metric = "auc",
      target_performance = 0.8,
      maximum_achievable_performance = 0.8
    ),
    "Requested target AUC must be less than the maximum achievable AUC because both are specified on the same metric scale.",
    fixed = TRUE
  )
})

test_that("validate_metric_constraints errors for equal or higher target on r2 and cindex", {
  expect_error(
    validate_metric_constraints(
      metric = "r2",
      target_performance = 0.6,
      maximum_achievable_performance = 0.6
    ),
    "Requested target R2 must be less than the maximum achievable R2 because both are specified on the same metric scale.",
    fixed = TRUE
  )

  expect_error(
    validate_metric_constraints(
      metric = "cindex",
      target_performance = 0.76,
      maximum_achievable_performance = 0.75
    ),
    "Requested target C-index must be less than the maximum achievable C-index because both are specified on the same metric scale.",
    fixed = TRUE
  )
})

test_that("validate_metric_constraints does not compare non-comparable metrics", {
  expect_silent(
    validate_metric_constraints(
      metric = "calibration_slope",
      target_performance = 0.9,
      maximum_achievable_performance = 0.9
    )
  )
})

test_that("validate_outcome_prevalence aborts on missing values and warns on low prevalence", {
  expect_error(
    validate_outcome_prevalence(NULL),
    "outcome_prevalence",
    fixed = FALSE
  )

  expect_warning(
    validate_outcome_prevalence(0.04),
    "Outcome prevalence is very low",
    fixed = FALSE
  )

  expect_silent(validate_outcome_prevalence(0.2))
})

test_that("check_pmsims_args matches arguments and validates edge cases", {
  expect_identical(check_pmsims_args(NULL, c("mean", "assurance")), "mean")
  expect_identical(
    check_pmsims_args("ass", c("mean", "assurance")),
    "assurance"
  )
  expect_identical(
    check_pmsims_args(
      c("mea", "ass"),
      c("mean", "assurance"),
      several.ok = TRUE
    ),
    c("mean", "assurance")
  )

  expect_error(
    check_pmsims_args(1, c("mean", "assurance")),
    "must be NULL or a character vector",
    fixed = FALSE
  )
  expect_error(
    check_pmsims_args(c("mean", "ass"), c("mean", "assurance")),
    "must be of length 1",
    fixed = FALSE
  )
  expect_error(
    check_pmsims_args(character(0), c("mean", "assurance"), several.ok = TRUE),
    "must be of length >= 1",
    fixed = FALSE
  )
  expect_error(
    check_pmsims_args("other", c("mean", "assurance")),
    "should be one of",
    fixed = FALSE
  )
})

test_that("get_min_sample_size applies EPV and outcome-specific rules", {
  binary_n <- get_min_sample_size(
    npar = 5,
    prevalence = 0.2,
    c_stat = 0.8,
    calibration_slope = NULL,
    epv_value = 10,
    outcome_type = "binary"
  )
  binary_n_higher_cstat <- get_min_sample_size(
    npar = 5,
    prevalence = 0.2,
    c_stat = 0.9,
    calibration_slope = NULL,
    epv_value = 10,
    outcome_type = "binary"
  )
  expect_type(binary_n, "integer")
  expect_gte(binary_n, 250L)
  expect_gte(binary_n, 3L * 5L)
  expect_lte(binary_n_higher_cstat, binary_n)

  continuous_n <- get_min_sample_size(
    npar = 4,
    prevalence = NULL,
    c_stat = 0.6,
    calibration_slope = 0.9,
    outcome_type = "continuous"
  )
  expect_true(continuous_n >= 12)
})

test_that("adaptive_startvalues summarises bisection history", {
  set.seed(99)
  track_bisection <- list(
    list(x = 100, y = runif(5, 0.68, 0.72)),
    list(x = 150, y = runif(5, 0.74, 0.78)),
    list(x = 120, y = runif(5, 0.7, 0.74))
  )

  output <- list(track_bisection = track_bisection)

  adaptive <- adaptive_startvalues(
    output = output,
    aggregate_fun = function(x) mean(x, na.rm = TRUE),
    var_bootstrap = function(x) stats::var(x) / max(1, length(x)),
    target = 0.73,
    ci_q = 0.975
  )

  expect_named(adaptive, c("summary", "min_value", "max_value"))
  expect_true(is.matrix(adaptive$summary))
  expect_true(is.numeric(adaptive$min_value))
  expect_true(is.numeric(adaptive$max_value))
})
