test_that("default_model_generators returns functions with model metadata", {
  expect_equal(attr(default_model_generators("binary", "glm"), "model"), "glm")
  expect_equal(
    attr(default_model_generators("continuous", "lm"), "model"),
    "lm"
  )
  expect_equal(
    attr(default_model_generators("survival", "coxph"), "model"),
    "coxph"
  )
  expect_equal(
    attr(default_model_generators("binary", "lasso"), "model"),
    "lasso"
  )
  expect_equal(attr(default_model_generators("binary", "rf"), "model"), "rf")
  expect_equal(
    attr(default_model_generators("binary", "xgboost"), "model"),
    "xgboost"
  )
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
  binary_fit <- default_model_generators(
    "binary",
    "glm"
  )(make_binary_fixture_data())
  expect_s3_class(binary_fit, "glm")

  continuous_fit <- default_model_generators(
    "continuous",
    "lm"
  )(make_continuous_fixture_data())
  expect_s3_class(continuous_fit, "glm")

  survival_fit <- default_model_generators(
    "survival",
    "coxph"
  )(make_survival_fixture_data())
  expect_s3_class(survival_fit, "coxph")
})

test_that("cv.ranger_tune reports missing optional dependencies clearly", {
  required <- c("tuneRanger", "mlr", "ranger")
  missing <- required[
    !vapply(required, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing) == 0) {
    skip(
      "All optional dependencies are installed; skipping missing-dependency branch."
    )
  }

  expected <- paste0(
    "The ranger tuning feature requires the following optional package",
    if (length(missing) > 1) "s" else "",
    ":"
  )

  expect_error(
    cv.ranger_tune(
      data = make_binary_fixture_data(),
      formula = y ~ x1 + x2,
      type = "classification"
    ),
    expected,
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

test_that("resolve_mlr_measures maps exported mlr measures by id", {
  skip_if_not_installed("mlr")

  measures <- resolve_mlr_measures(c("auc", "acc"), task_type = "classif")

  expect_length(measures, 2)
  expect_true(all(vapply(measures, inherits, logical(1), what = "Measure")))
  expect_identical(vapply(measures, `[[`, character(1), "id"), c("auc", "acc"))
})

test_that("resolve_mlr_measures rejects unknown ids", {
  skip_if_not_installed("mlr")

  expect_error(
    resolve_mlr_measures(
      "definitely_not_a_real_measure",
      task_type = "classif"
    ),
    "Unknown mlr measure: definitely_not_a_real_measure",
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

ridge_floor_data <- function(n, seed, intercept = -2.8) {
  set.seed(seed)
  x <- matrix(stats::rnorm(n * 5), n, 5)
  y <- stats::rbinom(n, 1, stats::plogis(intercept + x %*% rep(0.5, 5)))
  list(x = x, y = as.numeric(y))
}

test_that("cv_glmnet_ridge matches cv.glmnet when lambda.min is interior", {
  skip_if_not_installed("glmnet")
  d <- ridge_floor_data(60, 1, intercept = 0)
  set.seed(2)
  ref <- glmnet::cv.glmnet(d$x, d$y, alpha = 0, family = "binomial")
  skip_if(ref$lambda.min == min(ref$lambda), "fixture floors the default path")
  set.seed(2)
  fit <- cv_glmnet_ridge(d$x, d$y, family = "binomial")
  expect_identical(fit$lambda, ref$lambda)
  expect_identical(fit$lambda.min, ref$lambda.min)
  expect_equal(stats::coef(fit), stats::coef(ref))
})

test_that("cv_glmnet_ridge extends the path when CV picks its smallest lambda", {
  skip_if_not_installed("glmnet")
  d <- ridge_floor_data(5000, 3)
  set.seed(4)
  ref <- glmnet::cv.glmnet(d$x, d$y, alpha = 0, family = "binomial")
  expect_equal(ref$lambda.min, min(ref$lambda))
  set.seed(4)
  fit <- cv_glmnet_ridge(d$x, d$y, family = "binomial")
  expect_lt(fit$lambda.min, ref$lambda.min)
  expect_gt(fit$lambda.min, min(fit$lambda))
  expect_equal(max(fit$lambda), max(ref$lambda))
  expect_lt(min(fit$lambda), 1e-8 * max(fit$lambda))
  # Less shrinkage: the ridge coefficients move towards the unpenalised fit.
  b_glm <- stats::coef(stats::glm.fit(cbind(1, d$x), d$y, family = binomial()))
  b_ref <- as.numeric(stats::coef(ref, s = "lambda.min"))
  b_fit <- as.numeric(stats::coef(fit, s = "lambda.min"))
  expect_lt(sum((b_fit - b_glm)[-1]^2), sum((b_ref - b_glm)[-1]^2))
})

test_that("ridge generators fit for every outcome type", {
  skip_if_not_installed("glmnet")
  d <- ridge_floor_data(400, 5)
  binary <- data.frame(y = d$y, d$x)
  expect_s3_class(
    default_model_generators("binary", "ridge")(binary),
    "cv.glmnet"
  )
  continuous <- data.frame(y = drop(d$x %*% rep(0.3, 5)) + stats::rnorm(400), d$x)
  expect_s3_class(
    default_model_generators("continuous", "ridge")(continuous),
    "cv.glmnet"
  )
  survival <- data.frame(
    time = stats::rexp(400, exp(drop(d$x %*% rep(0.3, 5)))),
    event = stats::rbinom(400, 1, 0.7),
    d$x
  )
  expect_s3_class(
    default_model_generators("survival", "ridge")(survival),
    "cv.glmnet"
  )
})
