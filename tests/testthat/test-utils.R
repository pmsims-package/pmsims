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
      minimum_acceptable_performance = 0.7
    ),
    "Suggested calibration slope is too low; check and try again.",
    fixed = TRUE
  )

  expect_error(
    validate_metric_constraints(
      metric = "calibration_slope",
      minimum_acceptable_performance = 1.3
    ),
    "Suggested calibration slope is too high; check and try again.",
    fixed = TRUE
  )

  expect_error(
    validate_metric_constraints(
      metric = "auc",
      minimum_acceptable_performance = 0.8,
      expected_performance = 0.75
    ),
    "Requested minimum acceptable AUC exceeds the expected large-sample performance; adjust inputs and try again.",
    fixed = TRUE
  )

  expect_silent(
    validate_metric_constraints(
      metric = "auc",
      minimum_acceptable_performance = 0.75,
      expected_performance = 0.8
    )
  )
})

test_that("get_min_sample_size applies EPV and outcome-specific rules", {
  binary_n <- get_min_sample_size(
    npar = 5,
    prevalence = 0.2,
    c_stat = 0.8,
    calib_slope = NULL,
    epv_value = 10,
    outcome_type = "binary"
  )
  expect_equal(binary_n, 312L)

  continuous_n <- get_min_sample_size(
    npar = 4,
    prevalence = NULL,
    c_stat = 0.6,
    calib_slope = 0.9,
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
