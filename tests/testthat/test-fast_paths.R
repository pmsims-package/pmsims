# The metrics use direct computations in place of glm(), lm(), coxph(),
# basehaz(), survfit(), pROC and predict(). These tests check each against the
# function it replaces, including the cases where it must fall back.

glm_slope <- function(y, x, ...) {
  unname(stats::coef(suppressWarnings(stats::glm(y ~ x, ...)))[2])
}

test_that("calibration_glm() matches glm() for logit and weighted cloglog", {
  set.seed(1)
  n <- 3000
  x <- rnorm(n, -1, 1.2)
  y <- rbinom(n, 1, plogis(0.3 + 0.8 * x))
  expect_equal(
    calibration_glm(y, x),
    unname(stats::coef(stats::glm(y ~ x, family = stats::binomial()))),
    tolerance = 1e-10
  )

  w <- rexp(n) * rbinom(n, 1, 0.7)
  fam <- stats::binomial("cloglog")
  expect_equal(
    calibration_glm(y, x, family = fam, weights = w),
    unname(stats::coef(suppressWarnings(
      stats::glm(y ~ x, weights = w, family = fam)
    ))),
    tolerance = 1e-10
  )

  itl <- stats::glm(y ~ 1, offset = x, family = stats::binomial())
  expect_equal(
    calibration_glm(y, offset = x),
    unname(stats::coef(itl)),
    tolerance = 1e-10
  )
})

test_that("binary calibration slope falls back to glm() where needed", {
  set.seed(2)
  x <- rnorm(200)
  y <- rbinom(200, 1, plogis(x))
  binomial <- stats::binomial()

  # Separation: glm() warns about fitted probabilities of 0 or 1.
  ys <- as.numeric(x > 0)
  expect_null(calibration_glm(ys, x))
  expect_equal(
    suppressWarnings(binary_calibration_slope(ys, x)),
    glm_slope(ys, x, family = binomial)
  )
  # Constant score: glm() cannot estimate the slope.
  expect_true(is.na(binary_calibration_slope(y, rep(0.3, 200))))
  # All events or none.
  y0 <- rep(0, 200)
  expect_equal(
    suppressWarnings(binary_calibration_slope(y0, x)),
    glm_slope(y0, x, family = binomial)
  )
  # Factor and missing outcomes.
  expect_equal(
    binary_calibration_slope(factor(y), x),
    glm_slope(factor(y), x, family = binomial)
  )
  y_na <- y
  y_na[3] <- NA
  expect_null(calibration_glm(y_na, x))
  expect_equal(
    binary_calibration_slope(y_na, x),
    glm_slope(y_na, x, family = binomial)
  )
})

test_that("continuous calibration slope matches lm()", {
  set.seed(3)
  x <- rnorm(500)
  y <- rnorm(500, 0.2 + 0.7 * x)
  lm_slope <- function(y, x) unname(stats::coef(stats::lm(y ~ x))[2])
  expect_equal(continuous_calibration_slope(y, x), lm_slope(y, x))
  expect_true(is.na(continuous_calibration_slope(y, rep(1, 500))))
  y[4] <- NA
  expect_equal(continuous_calibration_slope(y, x), lm_slope(y, x))
})

make_surv <- function(n, seed, round_to = NULL, near_equal = FALSE) {
  set.seed(seed)
  lp <- rnorm(n, 0.3, 0.8)
  t_event <- rexp(n, 0.5 * exp(lp))
  cut <- stats::quantile(t_event, 0.6)
  time <- pmin(t_event, cut)
  if (!is.null(round_to)) {
    time <- round(time, round_to)
  }
  if (near_equal) {
    # Pairs of times closer than aeqSurv()'s tolerance.
    time[seq(2, n, by = 10)] <- time[seq(1, n, by = 10)] * (1 + 1e-10)
  }
  list(time = time, event = as.numeric(t_event <= cut), lp = lp)
}

test_that("offset_baseline_hazard() matches basehaz()", {
  for (d in list(
    make_surv(2000, 1),
    make_surv(2000, 2, round_to = 1), # many tied event times (Efron)
    make_surv(2000, 3, near_equal = TRUE)
  )) {
    bh <- survival::basehaz(
      survival::coxph(survival::Surv(d$time, d$event) ~ offset(d$lp)),
      centered = FALSE
    )
    fast <- offset_baseline_hazard(d$time, d$event, d$lp)
    expect_equal(fast$time, bh$time)
    expect_equal(unname(fast$hazard), bh$hazard, tolerance = 1e-12)
  }
  # A single event.
  d <- make_surv(300, 4)
  d$event[] <- 0
  d$event[10] <- 1
  bh <- survival::basehaz(
    survival::coxph(survival::Surv(d$time, d$event) ~ offset(d$lp)),
    centered = FALSE
  )
  fast <- offset_baseline_hazard(d$time, d$event, d$lp)
  expect_equal(unname(fast$hazard), bh$hazard, tolerance = 1e-12)
  # No events, or missing events: left to survival.
  expect_null(offset_baseline_hazard(d$time, d$event * 0, d$lp))
  d$event[5] <- NA
  expect_null(offset_baseline_hazard(d$time, d$event, d$lp))
})

test_that("censoring_km() matches survfit()", {
  for (d in list(
    make_surv(2000, 5),
    make_surv(2000, 6, round_to = 1),
    make_surv(2000, 7, near_equal = TRUE)
  )) {
    sf <- survival::survfit(survival::Surv(d$time, 1 - d$event) ~ 1)
    km <- censoring_km(d$time, d$event)
    expect_equal(km$time, sf$time)
    expect_equal(km$surv, sf$surv, tolerance = 1e-12)
  }
  d <- make_surv(300, 8)
  sf <- survival::survfit(survival::Surv(d$time, rep(1, 300)) ~ 1)
  km <- censoring_km(d$time, rep(0, 300)) # everyone censored
  expect_equal(km$surv, sf$surv, tolerance = 1e-12)
})

test_that("cox_calibration_slope() is identical to coxph()", {
  for (d in list(make_surv(2000, 9), make_surv(2000, 10, round_to = 1))) {
    x <- d$lp + rnorm(length(d$lp), 0, 0.5)
    expect_identical(
      cox_calibration_slope(d$time, d$event, x),
      unname(stats::coef(survival::coxph(survival::Surv(d$time, d$event) ~ x)))
    )
  }
})

test_that("rank_auc() matches pROC::auc()", {
  set.seed(11)
  y <- rbinom(1000, 1, 0.3)
  s <- rnorm(1000) + y
  pauc <- function(y, s) pROC::auc(y, s, quiet = TRUE)[1]
  expect_equal(rank_auc(y, s), pauc(y, s), tolerance = 1e-14)
  expect_equal(rank_auc(y, round(s)), pauc(y, round(s)), tolerance = 1e-14)
  expect_equal(rank_auc(y, -s), pauc(y, -s), tolerance = 1e-14) # direction
  expect_null(rank_auc(rep(1, 10), rnorm(10))) # one class
  y[2] <- NA
  expect_null(rank_auc(y, s))
})

test_that("plain linear predictions equal predict()", {
  set.seed(12)
  d <- data.frame(y = rbinom(300, 1, 0.4), x1 = rnorm(300), x2 = rnorm(300))
  x <- d[, -1]
  fit <- stats::glm(y ~ ., data = d, family = stats::binomial())
  for (type in c("link", "response")) {
    expect_identical(
      predict_custom(x, NULL, fit, "glm", type),
      unname(stats::predict(fit, newdata = x, type = type))
    )
  }
  # Reordered columns, offset terms and offset arguments use predict().
  X <- cbind(1, as.matrix(x[, 2:1]))
  expect_null(plain_linear_predictor(fit, X, intercept = TRUE))
  expect_equal(
    unname(predict_custom(x[, 2:1], NULL, fit, "glm", "link")),
    unname(stats::predict(fit, newdata = x, type = "link"))
  )
  d$o <- rnorm(300)
  fit_term <- stats::glm(
    y ~ x1 + x2 + offset(o),
    data = d,
    family = stats::binomial()
  )
  fit_arg <- stats::glm(
    y ~ x1 + x2,
    offset = o,
    data = d,
    family = stats::binomial()
  )
  X <- cbind(1, as.matrix(x))
  expect_null(plain_linear_predictor(fit_term, X, intercept = TRUE))
  expect_null(plain_linear_predictor(fit_arg, X, intercept = TRUE))

  ds <- data.frame(time = rexp(300), event = rbinom(300, 1, 0.7), x)
  cfit <- default_model_generators("survival", "coxph")(ds)
  lp <- predict_custom(x, NULL, cfit, "coxph", "lp")
  cfit$formula <- stats::formula(cfit)
  environment(cfit$formula) <- list2env(list(Surv = survival::Surv))
  attr(cfit$terms, ".Environment") <- environment(cfit$formula)
  expect_identical(lp, unname(stats::predict(cfit, newdata = x, type = "lp")))
})

test_that("survival forests: terminal-node predictions equal predict()", {
  skip_if_not_installed("ranger")
  set.seed(13)
  n <- 400
  d <- data.frame(
    time = rexp(n),
    event = rbinom(n, 1, 0.7),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  new <- data.frame(x1 = rnorm(500), x2 = rnorm(500))
  for (rule in c("logrank", "extratrees", "maxstat", "C")) {
    fit <- ranger::ranger(
      survival::Surv(time, event) ~ .,
      d,
      num.trees = 30,
      splitrule = rule,
      seed = 1,
      num.threads = 1
    )
    pr <- stats::predict(fit, new, num.threads = 1)
    j <- 7
    expect_identical(rsf_tree_average(fit, new, function(v) v[j]), pr$chf[, j])
    expect_equal(rsf_tree_average(fit, new, sum), rowSums(pr$chf))
  }
})

test_that("forest and xgboost predictions do not depend on threads", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("xgboost")
  set.seed(14)
  x <- matrix(rnorm(600), 300, dimnames = list(NULL, c("x1", "x2")))
  y <- rbinom(300, 1, plogis(x[, 1]))
  rf <- lapply(1:2, function(k) {
    set.seed(3)
    f <- ranger::ranger(x = x, y = factor(y), num.trees = 30, num.threads = k)
    stats::predict(f, x, num.threads = k)$predictions
  })
  expect_identical(rf[[1]], rf[[2]])
  xgb <- lapply(1:2, function(k) {
    old <- options(pmsims.threads = k)
    on.exit(options(old))
    set.seed(3)
    f <- default_models$binary$xgboost(data.frame(y = y, x))
    predict_custom(data.frame(x), NULL, f, "xgboost")
  })
  expect_identical(xgb[[1]], xgb[[2]])
})

test_that("running-sum copula equals the matrix product", {
  for (p in c(5, 40)) {
    R <- matrix(0.3, p, p)
    diag(R) <- 1
    set.seed(15)
    fast <- draw_correlated_predictors(500, p, "normal", 0.3)
    set.seed(15)
    slow <- matrix(rnorm(500 * p), 500, p) %*% chol(R)
    expect_equal(fast, slow, tolerance = 1e-13)
  }
})

test_that("cox_calibration_slope() matches coxph() with no events or a missing event", {
  set.seed(4)
  n <- 200
  x <- rnorm(n)
  time <- rexp(n, exp(0.5 * x))
  reference <- function(time, event, x) {
    cf <- suppressWarnings(stats::coef(survival::coxph(
      survival::Surv(time, event) ~ x
    )))
    if (is.null(cf)) NaN else as.numeric(cf)
  }
  no_events <- rep(0, n)
  expect_identical(
    suppressWarnings(cox_calibration_slope(time, no_events, x)),
    reference(time, no_events, x)
  )
  event <- rbinom(n, 1, 0.6)
  event[3] <- NA
  expect_equal(
    suppressWarnings(cox_calibration_slope(time, event, x)),
    reference(time, event, x)
  )
})
