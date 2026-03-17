predict.fake_rf <- function(object, newdata, ...) {
  rep(0.4, nrow(newdata))
}

make_continuous_inputs <- function(metric) {
  parse_inputs(
    data_spec = list(
      type = "continuous",
      args = list(
        n_signal_parameters = 5,
        noise_parameters = 5,
        predictor_type = "continuous",
        beta_signal = 0.1
      )
    ),
    metric = metric,
    model = "lm"
  )
}

make_survival_inputs <- function(metric) {
  parse_inputs(
    data_spec = list(
      type = "survival",
      args = list(
        n_signal_parameters = 5,
        noise_parameters = 5,
        predictor_type = "continuous",
        beta_signal = 0.1,
        baseline_hazard = 0.01,
        censoring_rate = 0.2
      )
    ),
    metric = metric,
    model = "coxph"
  )
}

test_that("default_metric_generator dispatches by outcome and validates metrics", {
  binary_fn <- default_metric_generator("brier_score", make_binary_data_function())
  expect_equal(attr(binary_fn, "metric"), "brier_score")

  continuous_fn <- default_metric_generator("r2", make_continuous_data_function())

  survival_fn <- default_metric_generator("cindex", make_survival_data_function())

  probs <- c(0.25, 0.70, 0.62, 0.65, 0.60, 0.40)
  y_hat_continuous <- c(1.05, 1.55, 1.95, 2.55, 2.95, 3.45)
  y_hat_survival <- seq(-0.75, 0.75, length.out = nrow(make_survival_fixture_data()))
  local_mocked_bindings(
    predict_custom = function(x, y, fit, model, type = "response") {
      if (nrow(x) == length(probs)) {
        if (type == "response") {
          return(probs)
        }
        return(stats::qlogis(probs))
      }

      if (nrow(x) == length(y_hat_continuous)) {
        return(y_hat_continuous)
      }

      if (nrow(x) == length(y_hat_survival)) {
        return(y_hat_survival)
      }

      stop("Unexpected input shape in test stub.")
    },
    .package = "pmsims"
  )

  binary_data <- make_binary_fixture_data()
  continuous_data <- make_continuous_fixture_data()
  survival_data <- make_survival_fixture_data()

  expect_equal(
    binary_fn(binary_data, fit = NULL, model = "glm"),
    binary_brier_score(binary_data, fit = NULL, model = "glm")
  )
  expect_equal(
    continuous_fn(continuous_data, fit = NULL, model = "lm"),
    continuous_r2(continuous_data, fit = NULL, model = "lm")
  )
  expect_equal(
    survival_fn(survival_data, fit = NULL, model = "coxph"),
    survival_cindex(survival_data, fit = NULL, model = "coxph")
  )

  expect_error(
    default_metric_generator("does_not_exist", make_binary_data_function()),
    "Default metric does_not_exist for binary outcomes does not exist.",
    fixed = TRUE
  )
})

test_that("predict_custom returns response predictions for lm", {
  inputs <- make_continuous_inputs("r2")
  set.seed(1234)
  data <- inputs$data_function(300)
  fit <- inputs$model_function(data)
  x <- data[, names(data) != "y", drop = FALSE]

  preds <- pmsims:::predict_custom(x, fit = fit, model = "lm", type = "response")

  expect_type(preds, "double")
  expect_length(preds, nrow(x))
  expect_false(anyNA(preds))
})

test_that("predict_custom returns linear predictors for coxph without attaching survival", {
  inputs <- make_survival_inputs("auc")
  set.seed(1234)
  data <- inputs$data_function(300)
  fit <- inputs$model_function(data)
  x <- data[, !(names(data) %in% c("time", "event", "id")), drop = FALSE]

  preds <- pmsims:::predict_custom(x, fit = fit, model = "coxph", type = "lp")

  expect_type(preds, "double")
  expect_length(preds, nrow(x))
  expect_false(anyNA(preds))
})

test_that("predict_custom covers error and fallback branches", {
  x <- data.frame(x1 = c(0, 1, 2))

  local_mocked_bindings(
    predict = function(object, newdata, ...) rep(0.4, nrow(newdata)),
    .package = "stats"
  )
  expect_equal(
    predict_custom(x, fit = structure(list(), class = "fake_rf"), model = "rf"),
    rep(0.4, 3)
  )

  expect_error(
    predict_custom(x, fit = list(), model = "lasso", type = "survival"),
    "Type 'survival' not supported for lasso.",
    fixed = TRUE
  )

  expect_error(
    predict_custom(x, fit = list(), model = "unknown"),
    "predict_custom: unknown model type 'unknown'.",
    fixed = TRUE
  )
})

test_that("binary metric functions return finite values", {
  data <- make_binary_fixture_data()
  probs <- c(0.25, 0.70, 0.62, 0.65, 0.60, 0.40)
  links <- stats::qlogis(probs)
  expected_auc <- as.numeric(pROC::auc(data$y, probs, quiet = TRUE))

  local_mocked_bindings(
    predict_custom = function(x, y, fit, model, type = "response") {
      if (type == "response") {
        probs
      } else {
        links
      }
    },
    .package = "pmsims"
  )

  expect_equal(binary_auc_metric(data, fit = NULL, model = "glm"), expected_auc)
  expect_equal(
    binary_brier_score(data, fit = NULL, model = "glm"),
    mean((data$y - probs)^2)
  )
  expect_equal(
    binary_brier_score_scaled(data, fit = NULL, model = "glm"),
    1 - mean((data$y - probs)^2) / mean((data$y - mean(data$y))^2)
  )
  expect_true(is.finite(binary_calib_slope(data, fit = NULL, model = "glm")))
  expect_true(is.finite(binary_calib_itl(data, fit = NULL, model = "glm")))
})

test_that("continuous metrics use the current metric API", {
  for (metric_name in c("r2", "calib_slope", "calib_itl")) {
    inputs <- make_continuous_inputs(metric_name)
    set.seed(1234)
    data <- inputs$data_function(300)
    test_data <- inputs$data_function(300)
    fit <- inputs$model_function(data)

    metric <- inputs$metric_function(test_data, fit, "lm")

    expect_type(metric, "double")
    expect_false(is.na(metric))
  }
})

test_that("survival metrics cover finite and fallback paths", {
  data <- make_survival_fixture_data()
  y_hat <- seq(-0.75, 0.75, length.out = nrow(data))

  local_mocked_bindings(
    predict_custom = function(x, y, fit, model, type = "response") {
      y_hat
    },
    .package = "pmsims"
  )

  expect_true(is.finite(survival_cindex(data, fit = NULL, model = "coxph")))
  expect_true(is.finite(survival_calib_slope(data, fit = NULL, model = "coxph")))

  concordance <- survival::concordancefit(survival::Surv(data$time, data$event), y_hat)$concordance

  local_mocked_bindings(
    timeROC = function(...) stop("boom"),
    .package = "timeROC"
  )
  expect_equal(
    survival_auc(data, fit = NULL, model = "coxph"),
    as.numeric(concordance),
    tolerance = 1e-8
  )
})

test_that("survival_auc returns a finite probability-scale value", {
  inputs <- make_survival_inputs("auc")
  set.seed(1234)
  data <- inputs$data_function(400)
  test_data <- inputs$data_function(400)
  fit <- inputs$model_function(data)

  metric <- inputs$metric_function(test_data, fit, "coxph")

  expect_type(metric, "double")
  expect_false(is.na(metric))
  expect_gte(metric, 0)
  expect_lte(metric, 1)
})

test_that("survival metric helpers return NaN on unsupported probability paths", {
  data <- make_survival_fixture_data()

  local_mocked_bindings(
    predict_custom = function(...) NULL,
    .package = "pmsims"
  )

  expect_warning(
    metric <- survival_calib_slope_free(data, fit = NULL, model = "xgboost"),
    "predicted survival probabilities not available",
    fixed = FALSE
  )
  expect_true(is.nan(metric))
})

test_that("survival_cindex returns NaN when prediction fails", {
  data <- make_survival_fixture_data()

  local_mocked_bindings(
    predict_custom = function(...) stop("boom"),
    .package = "pmsims"
  )

  expect_true(is.nan(survival_cindex(data, fit = NULL, model = "coxph")))
})
