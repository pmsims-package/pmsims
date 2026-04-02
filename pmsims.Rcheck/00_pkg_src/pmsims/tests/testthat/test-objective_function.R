test_that("objective_function returns negative infinity below the minimum sample size", {
  expect_identical(
    objective_function(
      n = 50,
      penalty_weight = 0.1,
      target_performance = 0.8,
      min_sample_size = 100,
      max_sample_size = 1000,
      value_on_error = 999
    ),
    -Inf
  )
})

test_that("objective_function applies the penalty-adjusted objective formula", {
  local_mocked_bindings(
    calculate_metrics_perf = function(...) 0.9,
    .package = "pmsims"
  )

  objective <- objective_function(
    n = 200,
    penalty_weight = 0.1,
    target_performance = 0.8,
    min_sample_size = 100,
    max_sample_size = 1000,
    value_on_error = 999
  )

  expect_equal(objective, -abs(0.9 - 0.8 - 0.02))
})

test_that("objective_function returns the fallback value on calculation errors", {
  local_mocked_bindings(
    calculate_metrics_perf = function(...) stop("boom"),
    .package = "pmsims"
  )

  expect_identical(
    objective_function(
      n = 200,
      penalty_weight = 0.1,
      target_performance = 0.8,
      min_sample_size = 100,
      max_sample_size = 1000,
      value_on_error = 123
    ),
    123
  )
})
