test_that("default_model_generators returns functions with model metadata", {
  expect_equal(attr(default_model_generators("binary", "glm"), "model"), "glm")
  expect_equal(attr(default_model_generators("continuous", "lm"), "model"), "lm")
  expect_equal(attr(default_model_generators("survival", "coxph"), "model"), "coxph")
  expect_equal(attr(default_model_generators("binary", "lasso"), "model"), "lasso")
  expect_equal(attr(default_model_generators("binary", "rf"), "model"), "rf")
  expect_equal(attr(default_model_generators("binary", "xgboost"), "model"), "xgboost")
})

test_that("default_model_generators validates outcome and model names", {
  expect_error(
    default_model_generators("invalid", "glm"),
    "Outcome \"invalid\" not found.",
    fixed = TRUE
  )
  expect_error(
    default_model_generators("binary", "invalid"),
    "Model \"invalid\" not found for outcome \"binary\"",
    fixed = TRUE
  )
})

test_that("cheap model generators fit expected model classes", {
  binary_fit <- default_model_generators("binary", "glm")(make_binary_fixture_data())
  expect_s3_class(binary_fit, "glm")

  continuous_fit <- default_model_generators("continuous", "lm")(make_continuous_fixture_data())
  expect_s3_class(continuous_fit, "glm")

  survival_fit <- default_model_generators("survival", "coxph")(make_survival_fixture_data())
  expect_s3_class(survival_fit, "coxph")
})

test_that("cv.ranger_tune reports missing optional dependencies clearly", {
  required <- c("tuneRanger", "mlr", "ranger")
  installed <- rownames(utils::installed.packages())

  if (all(required %in% installed)) {
    skip("All optional dependencies are installed; skipping missing-dependency branch.")
  }

  expect_error(
    cv.ranger_tune(
      data = make_binary_fixture_data(),
      formula = y ~ x1 + x2,
      type = "classification"
    ),
    "Please install required packages:",
    fixed = TRUE
  )
})

test_that("cv.ranger_tune validates survival formulas when dependencies are installed", {
  skip_if_not_installed("tuneRanger")
  skip_if_not_installed("mlr")
  skip_if_not_installed("ranger")

  expect_error(
    cv.ranger_tune(
      data = make_survival_fixture_data(),
      formula = y ~ x1 + x2,
      type = "survival",
      iters = 1,
      iters.warmup = 1,
      build.final.model = FALSE,
      show.info = FALSE
    ),
    "For survival, specify Surv(time, status) on LHS of formula.",
    fixed = TRUE
  )
})

test_that("print.cv.ranger_tune prints a compact summary", {
  object <- structure(
    list(
      recommended.pars = list(mtry = 2),
      results = data.frame(mtry = 2, metric = 0.1),
      measure = "mse"
    ),
    class = "cv.ranger_tune"
  )

  output <- paste(capture.output(print(object)), collapse = "\n")

  expect_match(output, "tuneRanger results", fixed = TRUE)
  expect_match(output, "Recommended parameters", fixed = TRUE)
})
