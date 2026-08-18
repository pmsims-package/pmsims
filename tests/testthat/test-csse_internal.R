test_that("calibration slope and CSSE convert back and forth", {
  expect_equal(calibration_slope_to_csse(0.9), -0.01)
  expect_equal(calibration_slope_to_csse(1.1), -0.010000000000000018)
  expect_equal(calibration_slope_to_csse(1), 0)

  expect_equal(csse_to_calibration_slope(-0.01, "below"), 0.9)
  expect_equal(csse_to_calibration_slope(-0.01, "above"), 1.1)
  expect_equal(csse_to_calibration_slope(0, "below"), 1)
})

test_that("csse_to_calibration_slope handles unusable input", {
  expect_true(is.na(csse_to_calibration_slope(NA_real_)))
  expect_true(is.na(csse_to_calibration_slope(NaN)))
  expect_true(is.na(csse_to_calibration_slope("Not possible")))
  # Small positive values can arise from GP interpolation; treat as perfect.
  expect_equal(csse_to_calibration_slope(1e-8, "below"), 1)
})

test_that("internal CSSE conversion applies to machine learning models only", {
  for (model in c("lasso", "ridge", "rf", "xgboost")) {
    plan <- plan_internal_csse("calibration_slope", model, 0.9)
    expect_true(plan$active, info = model)
    expect_equal(plan$metric, "csse", info = model)
    expect_equal(plan$target_performance, -0.01, info = model)
    expect_equal(plan$user_target_performance, 0.9, info = model)
  }

  for (model in c("glm", "lm", "coxph")) {
    plan <- plan_internal_csse("calibration_slope", model, 0.9)
    expect_false(plan$active, info = model)
    expect_equal(plan$metric, "calibration_slope", info = model)
    expect_equal(plan$target_performance, 0.9, info = model)
  }
})

test_that("internal CSSE conversion leaves other metrics alone", {
  plan <- plan_internal_csse("auc", "rf", 0.75)
  expect_false(plan$active)
  expect_equal(plan$metric, "auc")
  expect_equal(plan$target_performance, 0.75)

  # An explicitly requested CSSE target is passed through untouched.
  plan <- plan_internal_csse("csse", "rf", -0.01)
  expect_false(plan$active)
  expect_equal(plan$metric, "csse")
  expect_equal(plan$target_performance, -0.01)
})

test_that("the direction of the target relative to 1 is recorded", {
  expect_equal(
    plan_internal_csse("calibration_slope", "rf", 0.9)$direction,
    "below"
  )
  expect_equal(
    plan_internal_csse("calibration_slope", "rf", 1.1)$direction,
    "above"
  )
  # An exact target of 1 is treated as "below": models overfit in practice.
  expect_equal(
    plan_internal_csse("calibration_slope", "rf", 1)$direction,
    "below"
  )
})

test_that("results are translated back onto the calibration slope scale", {
  plan <- plan_internal_csse("calibration_slope", "rf", 0.9)
  output <- list(
    perf_n = -0.04,
    target_performance = plan$target_performance,
    metric = "csse"
  )

  restored <- restore_calibration_slope_scale(output, plan)

  expect_equal(restored$perf_n, 0.8)
  expect_equal(restored$target_performance, 0.9)
  expect_equal(restored$metric, "calibration_slope")
  expect_true(restored$internal_csse)
  expect_equal(restored$csse_direction, "below")
  # The values the search actually used are retained for diagnostics.
  expect_equal(restored$csse_perf_n, -0.04)
  expect_equal(restored$csse_target_performance, -0.01)
})

test_that("back-transformation respects a target above 1", {
  plan <- plan_internal_csse("calibration_slope", "ridge", 1.1)
  output <- list(perf_n = -0.04, target_performance = plan$target_performance)

  restored <- restore_calibration_slope_scale(output, plan)

  expect_equal(restored$perf_n, 1.2)
  expect_equal(restored$target_performance, 1.1)
})

test_that("back-transformation tolerates a failed search", {
  plan <- plan_internal_csse("calibration_slope", "rf", 0.9)
  output <- list(
    perf_n = "Not possible. Increase sample or lower performance",
    target_performance = plan$target_performance
  )

  restored <- restore_calibration_slope_scale(output, plan)

  expect_equal(
    restored$perf_n,
    "Not possible. Increase sample or lower performance"
  )
  expect_equal(restored$target_performance, 0.9)
})

test_that("results are untouched when no conversion was applied", {
  plan <- plan_internal_csse("calibration_slope", "glm", 0.9)
  output <- list(
    perf_n = 0.88,
    target_performance = 0.9,
    metric = "calibration_slope"
  )

  restored <- restore_calibration_slope_scale(output, plan)

  expect_equal(restored, output)
  expect_null(restored$internal_csse)
})

test_that("print.pmsims footnotes an internally converted calibration slope", {
  object <- make_minimal_pmsims_object(
    metric = "calibration_slope",
    target_performance = 0.9
  )
  object$model <- "rf"
  object$internal_csse <- TRUE

  output <- paste(capture.output(print(object)), collapse = "\n")

  expect_match(output, "\u2020 derived from calibration slope squared error")
  expect_match(output, "Calibration slope \u2020", fixed = TRUE)
})

test_that("print.pmsims omits the footnote when no conversion was applied", {
  object <- make_minimal_pmsims_object(
    metric = "calibration_slope",
    target_performance = 0.9
  )

  output <- paste(capture.output(print(object)), collapse = "\n")

  expect_false(grepl("\u2020", output, fixed = TRUE))
})
