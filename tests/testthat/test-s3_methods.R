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
