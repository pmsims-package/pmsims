test_that("print.pmsims rejects non-pmsims objects", {
  expect_error(
    pmsims:::print.pmsims(list()),
    "Object is not of class 'pmsims'",
    fixed = TRUE
  )
})

test_that("print.pmsims groups the inputs and leads the results with N", {
  object <- make_minimal_pmsims_object()
  object$metric_2 <- NULL
  object$metric_2_at_n <- NULL
  object$simulation_time <- NA

  output <- capture_pmsims_output(print(object))

  expect_match(output, "pmsims: Sample size simulation summary", fixed = TRUE)
  expect_match(output, "Data-generating scenario", fixed = TRUE)
  expect_match(output, "Model and performance", fixed = TRUE)
  expect_match(output, "Sample-size criterion", fixed = TRUE)
  expect_match(output, "Minimum sample size", fixed = TRUE)
  expect_match(output, "Performance at N = 100", fixed = TRUE)
  # An absent value drops its row rather than printing a placeholder.
  expect_false(grepl("Running time", output, fixed = TRUE))
  expect_false(grepl("Final minimum sample size", output, fixed = TRUE))
})

test_that("print.pmsims combines the predictor counts onto one line", {
  object <- make_minimal_pmsims_object()

  output <- capture_pmsims_output(print(object))

  expect_match(output, "Predictors", fixed = TRUE)
  expect_match(output, "5 signal + 2 noise", fixed = TRUE)
  expect_false(grepl("Signal predictors", output, fixed = TRUE))
  expect_false(grepl("Noise predictors", output, fixed = TRUE))
})

test_that("print.pmsims drops a zero predictor count", {
  object <- make_minimal_pmsims_object()
  object$noise_parameters <- 0L

  output <- capture_pmsims_output(print(object))

  expect_match(output, "5 signal", fixed = TRUE)
  expect_false(grepl("0 noise", output, fixed = TRUE))
})

test_that("print.pmsims uses human-readable model names", {
  logistic <- capture_pmsims_output(print(make_minimal_pmsims_object()))
  expect_match(logistic, "Logistic regression", fixed = TRUE)

  forest <- make_minimal_pmsims_object()
  forest$model <- "rf"
  expect_match(
    capture_pmsims_output(print(forest)),
    "Random forest",
    fixed = TRUE
  )

  survival_forest <- make_minimal_pmsims_object()
  survival_forest$model <- "rf"
  survival_forest$outcome <- "survival"
  expect_match(
    capture_pmsims_output(print(survival_forest)),
    "Random survival forest",
    fixed = TRUE
  )
})

test_that("print.pmsims hides internal identifiers unless asked for them", {
  object <- make_minimal_pmsims_object(metric = "calibration_slope")
  object$method <- "mlpwr"

  output <- capture_pmsims_output(print(object))
  expect_false(grepl("'calibration_slope'", output, fixed = TRUE))
  expect_false(grepl("maximum_achievable_cstatistic", output, fixed = TRUE))
  expect_false(grepl("'glm'", output, fixed = TRUE))

  detailed <- capture_pmsims_output(print(object, verbose = TRUE))
  expect_match(detailed, "'calibration_slope'", fixed = TRUE)
  expect_match(detailed, "maximum_achievable_cstatistic", fixed = TRUE)
  expect_match(detailed, "'glm'", fixed = TRUE)
  expect_match(detailed, "Search method", fixed = TRUE)
})

test_that("print.pmsims states the criterion with the direction of improvement", {
  slope_below <- make_minimal_pmsims_object(
    metric = "calibration_slope",
    target_performance = 0.9
  )
  expect_match(
    capture_pmsims_output(print(slope_below)),
    paste("Calibration slope", cli::symbol$geq, "0.900"),
    fixed = TRUE
  )

  # Better means closer to 1, so a target above 1 reverses the operator.
  slope_above <- make_minimal_pmsims_object(
    metric = "calibration_slope",
    target_performance = 1.1
  )
  expect_match(
    capture_pmsims_output(print(slope_above)),
    paste("Calibration slope", cli::symbol$leq, "1.100"),
    fixed = TRUE
  )

  lower_is_better <- make_minimal_pmsims_object(
    metric = "brier_score",
    target_performance = 0.15
  )
  expect_match(
    capture_pmsims_output(print(lower_is_better)),
    paste("Brier score", cli::symbol$leq, "0.150"),
    fixed = TRUE
  )
})

test_that("print.pmsims shows the target alongside the achieved value", {
  object <- make_minimal_pmsims_object(
    metric = "calibration_slope",
    target_performance = 0.9
  )
  object$perf_n <- 0.901

  output <- capture_pmsims_output(print(object))

  expect_match(output, "0.901", fixed = TRUE)
  expect_match(
    output,
    paste0("(target ", cli::symbol$geq, " 0.900)"),
    fixed = TRUE
  )
})

test_that("print.pmsims names AUC and the C-statistic consistently", {
  object <- make_minimal_pmsims_object(metric = "auc")
  object$metric_2 <- NULL
  object$metric_2_at_n <- NULL

  output <- capture_pmsims_output(print(object))

  expect_match(output, "Large-sample C-statistic", fixed = TRUE)
  # The assumption, the criterion and the achieved value all use one name.
  expect_equal(count_matches(output, "C-statistic"), 3L)
  expect_false(grepl("AUC", output, fixed = TRUE))
})

test_that("print.pmsims does not repeat the model or the mode in the results", {
  object <- make_minimal_pmsims_object()

  results <- pmsims_results_section(capture_pmsims_output(print(object)))

  expect_false(any(grepl("Logistic regression", results, fixed = TRUE)))
  expect_false(any(grepl("Mode", results, fixed = TRUE)))
  expect_true(any(grepl("Minimum sample size", results, fixed = TRUE)))
})

test_that("print.pmsims supports the legacy parameters field", {
  object <- make_minimal_pmsims_object()
  object$parameters <- object$signal_parameters
  object$signal_parameters <- NULL

  output <- capture_pmsims_output(print(object))

  expect_match(output, "5 signal + 2 noise", fixed = TRUE)
})

test_that("print.pmsims supports legacy performance input fields", {
  object <- make_minimal_pmsims_object()
  object$prevalence <- object$outcome_prevalence
  object$outcome_prevalence <- NULL
  object$cstatistic <- object$maximum_achievable_cstatistic
  object$maximum_achievable_cstatistic <- NULL

  output <- capture_pmsims_output(print(object))

  expect_match(output, "Prevalence", fixed = TRUE)
  expect_match(output, "Large-sample C-statistic", fixed = TRUE)
})

test_that("print.pmsims reports the data-generating configuration", {
  object <- make_minimal_pmsims_object()
  object$complexity <- 2
  object$nonlinear_strength <- 0.2
  object$predictor_distribution <- "normal"
  object$correlation <- 0.3

  output <- capture_pmsims_output(print(object))

  expect_match(output, "Signal form", fixed = TRUE)
  expect_match(output, "Linear + quadratic", fixed = TRUE)
  expect_match(output, "Nonlinear strength", fixed = TRUE)
  expect_match(output, "Predictor distribution", fixed = TRUE)
  expect_match(output, "Normal", fixed = TRUE)
  expect_match(output, "Predictor correlation", fixed = TRUE)
  # predictor_distribution supersedes the legacy predictor_type row.
  expect_false(grepl("Predictor type", output, fixed = TRUE))
})

test_that("print.pmsims omits nonlinear strength where it has no effect", {
  # Complexity 1 is purely linear and complexity 4 is the canonical Friedman
  # function; nonlinear_strength resolves to 0 and means nothing in both.
  for (k in c(1, 4)) {
    object <- make_minimal_pmsims_object()
    object$complexity <- k
    object$nonlinear_strength <- 0
    object$predictor_distribution <- "normal"
    object$correlation <- 0.3

    output <- capture_pmsims_output(print(object))

    expect_match(output, "Signal form", fixed = TRUE)
    expect_false(grepl("Nonlinear strength", output, fixed = TRUE))
  }
})

test_that("print.pmsims reports predictor prevalence only for binary predictors", {
  object <- make_minimal_pmsims_object()
  object$complexity <- 1
  object$predictor_type <- "binary"
  object$predictor_distribution <- "binary"
  object$binary_predictor_prevalence <- 0.25
  object$correlation <- 0

  output <- capture_pmsims_output(print(object))

  expect_match(output, "Predictor prevalence", fixed = TRUE)
  expect_match(output, "0.25", fixed = TRUE)

  # Continuous predictors carry a placeholder prevalence of 0, which would be
  # meaningless to print.
  continuous <- make_minimal_pmsims_object()
  continuous$complexity <- 1
  continuous$predictor_type <- "continuous"
  continuous$predictor_distribution <- "normal"
  continuous$binary_predictor_prevalence <- 0

  continuous_output <- capture_pmsims_output(print(continuous))

  expect_false(grepl("Predictor prevalence", continuous_output, fixed = TRUE))
})

test_that("print.pmsims falls back to predictor type without a distribution", {
  # simulate_custom() results carry no data-generating configuration at all.
  object <- make_minimal_pmsims_object()

  output <- capture_pmsims_output(print(object))

  expect_match(output, "Predictor type", fixed = TRUE)
  expect_match(output, "Continuous", fixed = TRUE)
  expect_false(grepl("Signal form", output, fixed = TRUE))
  expect_false(grepl("Predictor correlation", output, fixed = TRUE))
})

test_that("print.pmsims footnotes a calibration slope derived from CSSE", {
  object <- make_minimal_pmsims_object(metric = "calibration_slope")
  object$internal_csse <- TRUE
  object$csse_direction <- "below"
  object$csse_perf_n <- -0.0098
  object$csse_target_performance <- -0.01

  output <- capture_pmsims_output(print(object))

  expect_match(
    output,
    paste0("Calibration slope", pmsims_footnote_marker()),
    fixed = TRUE
  )
  expect_match(
    output,
    paste0(pmsims_footnote_marker(), "Derived from the calibration-slope"),
    fixed = TRUE
  )

  detailed <- capture_pmsims_output(print(object, verbose = TRUE))
  expect_match(detailed, "Search scale ('csse')", fixed = TRUE)
  expect_match(detailed, "-0.0098", fixed = TRUE)
})

test_that("print.pmsims closes with a note explaining the mode", {
  assurance <- make_minimal_pmsims_object(mean_or_assurance = "assurance")
  expect_match(
    capture_pmsims_output(print(assurance)),
    "Assurance mode selects N",
    fixed = TRUE
  )

  average <- make_minimal_pmsims_object(mean_or_assurance = "mean")
  expect_match(
    capture_pmsims_output(print(average)),
    "Mean mode selects N",
    fixed = TRUE
  )
})

test_that("print.pmsims reports an unreachable target without a performance block", {
  object <- make_minimal_pmsims_object()
  object$min_n <- "Not possible. Increase sample or lower performance"
  object$perf_n <- object$min_n
  object$metric_2 <- NULL
  object$metric_2_at_n <- NULL

  output <- capture_pmsims_output(print(object))

  expect_match(output, "Not possible", fixed = TRUE)
  expect_false(grepl("Performance at", output, fixed = TRUE))
})

test_that("summary.pmsims prints the detailed display", {
  object <- make_minimal_pmsims_object()

  output <- capture_pmsims_output(summary(object))

  expect_match(output, "Minimum sample size", fixed = TRUE)
  expect_match(output, "Sample-size criterion", fixed = TRUE)
  expect_match(output, "maximum_achievable_cstatistic", fixed = TRUE)
  expect_match(output, "'auc'", fixed = TRUE)

  expect_error(
    pmsims:::summary.pmsims(list()),
    "Object is not of class 'pmsims'",
    fixed = TRUE
  )
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
