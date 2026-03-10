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
