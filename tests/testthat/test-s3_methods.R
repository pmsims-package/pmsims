test_that("print.pmsims rejects non-pmsims objects", {
  expect_error(
    pmsims:::print.pmsims(list()),
    "Object is not of class 'pmsims'",
    fixed = TRUE
  )
})

test_that("print.pmsims renders key fields and tolerates missing optional values", {
  object <- make_minimal_pmsims_object()
  object$metric_2 <- NULL
  object$metric_2_at_n <- NULL
  object$simulation_time <- NA

  output <- paste(capture.output(print(object)), collapse = "\n")

  expect_match(output, "pmsims: Sample size simulation summary", fixed = TRUE)
  expect_match(output, "Final minimum sample size", fixed = TRUE)
  expect_match(output, "Target for chosen performance metric", fixed = TRUE)
  expect_match(output, "Signal predictors", fixed = TRUE)
  expect_false(grepl("Number of predictors", output, fixed = TRUE))
})

test_that("print.pmsims supports the legacy parameters field", {
  object <- make_minimal_pmsims_object()
  object$parameters <- object$signal_parameters
  object$signal_parameters <- NULL

  output <- paste(capture.output(print(object)), collapse = "\n")

  expect_match(output, "Signal predictors", fixed = TRUE)
  expect_false(grepl("Number of predictors", output, fixed = TRUE))
})

test_that("print.pmsims supports legacy performance input fields", {
  object <- make_minimal_pmsims_object()
  object$prevalence <- object$outcome_prevalence
  object$outcome_prevalence <- NULL
  object$cstatistic <- object$maximum_achievable_cstatistic
  object$maximum_achievable_cstatistic <- NULL

  output <- paste(capture.output(print(object)), collapse = "\n")

  expect_match(output, "Prevalence", fixed = TRUE)
  expect_match(output, "C-statistic", fixed = TRUE)
})

test_that("summary.pmsims prints a compact summary", {
  object <- make_minimal_pmsims_object()

  output <- paste(capture.output(summary(object)), collapse = "\n")

  expect_match(output, "Target performance", fixed = TRUE)
  expect_match(output, "Minimum sample size", fixed = TRUE)
})

test_that("plot.pmsims returns plot data when plot is false", {
  object <- make_minimal_pmsims_object(metric = "auc")

  local_mocked_bindings(
    todataframe = function(...) {
      data.frame(V1 = c(50, 100), y = c(0.71, 0.82))
    },
    .package = "mlpwr"
  )

  output <- pmsims:::plot.pmsims(object, plot = FALSE)

  expect_named(output, c("observed_data", "predicted_data"))
  expect_equal(names(output$observed_data), c("n", "auc"))
  expect_equal(names(output$predicted_data), c("n", "auc"))
  expect_equal(output$observed_data$n, c(50, 100))
  expect_true(all(output$predicted_data$n >= 50))
})

test_that("plot.pmsims draws without error for valid objects", {
  object <- make_minimal_pmsims_object(metric = "auc")
  tmp_plot <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp_plot)
  on.exit(grDevices::dev.off(), add = TRUE)

  local_mocked_bindings(
    todataframe = function(...) {
      data.frame(V1 = c(50, 100), y = c(0.71, 0.82))
    },
    .package = "mlpwr"
  )

  expect_no_error(pmsims:::plot.pmsims(object, plot = TRUE))
})

test_that("plot.pmsims converts a CSSE-scale curve back to calibration slope", {
  # A calibration slope target with an ML model is searched on the CSSE scale,
  # so the stored curve is CSSE while perf_n and the target have already been
  # translated back. The plotted curve has to be translated too, or the target
  # line and min_n marker float around 0.9 above the data.
  object <- make_minimal_pmsims_object(metric = "calibration_slope")
  object$internal_csse <- TRUE
  object$csse_direction <- "below"
  object$target_performance <- 0.9
  object$perf_n <- 1 - sqrt(0.01)
  object$mlpwr_ds$data <- list(
    list(x = 50, y = c(-0.04, -0.02)),
    list(x = 100, y = c(-0.02, -0.01))
  )
  object$mlpwr_ds$fit$fitfun <- function(n) -0.03

  output <- pmsims:::plot.pmsims(object, plot = FALSE)

  # Aggregation happens on the CSSE scale first: mean(-0.04, -0.02) = -0.03.
  expect_equal(
    output$observed_data$calibration_slope,
    c(1 - sqrt(0.03), 1 - sqrt(0.015))
  )
  expect_true(all(output$predicted_data$calibration_slope == 1 - sqrt(0.03)))

  # The curve now lives on the same scale as the annotations drawn over it.
  expect_true(all(output$observed_data$calibration_slope > 0.5))
})

test_that("plot.pmsims honours the CSSE direction", {
  object <- make_minimal_pmsims_object(metric = "calibration_slope")
  object$internal_csse <- TRUE
  object$csse_direction <- "above"
  object$mlpwr_ds$data <- list(
    list(x = 50, y = -0.04),
    list(x = 100, y = -0.01)
  )
  object$mlpwr_ds$fit$fitfun <- function(n) -0.04

  output <- pmsims:::plot.pmsims(object, plot = FALSE)

  expect_equal(
    output$observed_data$calibration_slope,
    c(1 + sqrt(0.04), 1 + sqrt(0.01))
  )
})

test_that("plot.pmsims leaves a directly-requested metric untouched", {
  # metric = "csse" asked for explicitly is reported on the CSSE scale, and
  # regression models never route through CSSE at all.
  object <- make_minimal_pmsims_object(metric = "csse")
  object$mlpwr_ds$data <- list(
    list(x = 50, y = -0.04),
    list(x = 100, y = -0.01)
  )
  object$mlpwr_ds$fit$fitfun <- function(n) -0.02

  output <- pmsims:::plot.pmsims(object, plot = FALSE)

  expect_equal(output$observed_data$csse, c(-0.04, -0.01))
  expect_true(all(output$predicted_data$csse == -0.02))
})
