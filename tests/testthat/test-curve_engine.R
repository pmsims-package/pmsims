# Synthetic learning curve: replicate value = ceiling - scale / sqrt(n) plus
# training noise that shrinks with n and test-set noise. Fast: no model fits.
synthetic_curve <- function(
  ceiling = 0.95,
  scale = 2,
  train_sd = 0.05,
  test_sd = 0.01
) {
  data_function <- function(n) data.frame(n = n, z = stats::rnorm(1))
  model_function <- function(d) d
  attr(model_function, "model") <- "glm"
  metric_function <- function(test_data, fit, model) {
    ceiling -
      scale / sqrt(fit$n) +
      train_sd * sqrt(100 / fit$n) * fit$z +
      test_sd * test_data$z
  }
  attr(metric_function, "metric") <- "auc"
  list(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function
  )
}

# True answer for the 20th percentile.
synthetic_answer <- function(
  target,
  ceiling = 0.95,
  scale = 2,
  train_sd = 0.05,
  test_sd = 0.01
) {
  f <- function(n) {
    ceiling -
      scale / sqrt(n) +
      stats::qnorm(0.2) * sqrt(train_sd^2 * 100 / n + test_sd^2) -
      target
  }
  stats::uniroot(f, c(5, 1e8))$root
}

run_curve <- function(sc, target, seed = 1, ...) {
  set.seed(seed)
  suppressWarnings(suppressMessages(simulate_custom(
    sc$data_function,
    sc$model_function,
    sc$metric_function,
    target_performance = target,
    mean_or_assurance = "assurance",
    test_n = 1,
    n_reps_total = 400,
    n_reps_per = 20,
    progress = FALSE,
    verify_reps = 50,
    method = "curve",
    ...
  )))
}

test_that("fit_learning_curve recovers a known curve", {
  n <- c(50, 100, 200, 400, 800, 1600)
  est <- 0.9 - 3 * n^(-0.7)
  f <- fit_learning_curve(n, est, rep(1e6, length(n)))
  expect_equal(f$a, 0.9, tolerance = 0.01)
  expect_equal(f$c, 0.7, tolerance = 0.05)
  expect_equal(curve_crossing(f, 0.85), (3 / 0.05)^(1 / 0.7), tolerance = 0.05)
  expect_identical(curve_crossing(f, 0.95), Inf)
  expect_null(fit_learning_curve(c(10, 10), c(0.5, 0.6), c(100, 100)))
  # The ceiling is capped at the metric's maximum.
  capped <- fit_learning_curve(
    n,
    -0.002 + 0.01 * log(n) / 10,
    rep(1, length(n)),
    a_max = 0
  )
  expect_lte(capped$a, 0)
})

test_that("the curve engine finds the answer from a start that is far too low or too high", {
  sc <- synthetic_curve()
  truth <- synthetic_answer(0.85)
  for (mult in c(1 / 16, 16)) {
    res <- run_curve(sc, 0.85, min_n_floor = 10, start_n = round(truth * mult))
    # "not_verified" is possible here by chance: the check flags about 2-5%
    # of answers that sit at or just below the target.
    expect_true(res$status %in% c("ok", "not_verified"))
    expect_lt(abs(log(res$min_n / truth)), log(1.25))
  }
})

test_that("a slowly converging but reachable target is not declared unreachable", {
  # The case that made the adaptive stage's plateau rule fire falsely: strict
  # target, large replicate noise, slow convergence.
  sc <- synthetic_curve(
    ceiling = 1,
    scale = 1.5,
    train_sd = 0.15,
    test_sd = 0.02
  )
  truth <- synthetic_answer(
    0.95,
    ceiling = 1,
    scale = 1.5,
    train_sd = 0.15,
    test_sd = 0.02
  )
  statuses <- vapply(1:5, function(s) run_curve(sc, 0.95, seed = s)$status, "")
  expect_false(any(statuses %in% c("unreachable", "not_bracketed")))
})

test_that("an unreachable target is reported, not answered", {
  sc <- synthetic_curve(ceiling = 0.85)
  res <- run_curve(sc, 0.9)
  expect_true(res$status %in% c("unreachable", "not_bracketed"))
  expect_true(is.na(res$min_n))
})

test_that("the curve engine respects user bounds and is reproducible", {
  sc <- synthetic_curve()
  a <- run_curve(
    sc,
    0.85,
    seed = 3,
    min_sample_size = 300,
    max_sample_size = 900
  )
  expect_lte(max(as.numeric(names(a$summaries$mean_performance))), 900)
  expect_gte(min(as.numeric(names(a$summaries$mean_performance))), 300)
  expect_lte(a$min_n, 900)
  b <- run_curve(
    sc,
    0.85,
    seed = 3,
    min_sample_size = 300,
    max_sample_size = 900
  )
  expect_identical(a$min_n, b$min_n)
})

# CSSE-scale curve (as used for ML models): slope ~ N(1 + bias, s / sqrt(n)),
# CSSE = -(1 - slope)^2. With bias 0 the 20th percentile reaches -0.01 at
# n = (qnorm(0.9) * s / 0.1)^2, about 1,200 (|1 - slope| is two-sided); with bias 0.12 its ceiling is
# -0.0144, so -0.01 is unreachable.
csse_curve <- function(bias = 0, s = 2.7) {
  data_function <- function(n) data.frame(n = n, z = stats::rnorm(1))
  model_function <- function(d) d
  attr(model_function, "model") <- "ridge"
  metric_function <- function(test_data, fit, model) {
    -(1 - (1 + bias + s / sqrt(fit$n) * fit$z + 0.003 * test_data$z))^2
  }
  attr(metric_function, "metric") <- "csse"
  list(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function
  )
}

test_that("a CSSE-scale target is found from far below or above", {
  sc <- csse_curve()
  truth <- (stats::qnorm(0.9) * 2.7 / 0.1)^2
  for (mult in c(1 / 8, 8)) {
    res <- run_curve(sc, -0.01, seed = 2, start_n = round(truth * mult))
    expect_true(res$status %in% c("ok", "not_verified"))
    expect_lt(abs(log(res$min_n / truth)), log(1.3))
  }
})

test_that("an unreachable CSSE target stops without running to huge n", {
  sc <- csse_curve(bias = 0.12)
  res <- run_curve(sc, -0.01, seed = 2, start_n = 500)
  expect_true(res$status %in% c("unreachable", "not_bracketed"))
  expect_lte(res$diagnostics$bounds[2], 1e6)
})

test_that("an answer beyond the user's upper bound is reported, not clamped", {
  sc <- synthetic_curve()
  truth <- synthetic_answer(0.85)
  res <- run_curve(
    sc,
    0.85,
    seed = 1,
    min_sample_size = 100,
    max_sample_size = round(truth / 2)
  )
  expect_identical(res$status, "not_bracketed")
  expect_true(is.na(res$min_n))
})

test_that("a target just above the ceiling stops by max_n", {
  # Ceiling -(0.105)^2 = -0.011 against a target of -0.01. From the data such
  # a curve cannot be told apart from one that creeps up to the target far
  # out (ridge p5 reaches -0.01 only near n = 40,000), so the search may go
  # as far as max_n; it must then stop with a status rather than an answer.
  sc <- csse_curve(bias = 0.105)
  res <- run_curve(sc, -0.01, seed = 1, start_n = 500, max_n = 2e5)
  expect_true(res$status %in% c("unreachable", "not_bracketed"))
  expect_lte(res$diagnostics$bounds[2], 2e5)
})

test_that("failed replicates are told apart from legitimate fallback-valued ones", {
  data_function <- function(n) data.frame(n = n, z = stats::rnorm(1))
  model_function <- function(d) d
  attr(model_function, "model") <- "glm"
  metric_function <- function(test_data, fit, model) {
    if (fit$z < -1.5) {
      stop("did not converge")
    }
    0.95 - 2 / sqrt(fit$n) + 0.02 * fit$z
  }
  attr(metric_function, "metric") <- "auc"
  ev <- new_evaluator(
    data_function,
    model_function,
    metric_function,
    1,
    0.5,
    streams = new_simulation_streams(1L)
  )
  v <- ev$batch(100, 50, "search")
  expect_identical(sum(attr(v, "failed")), sum(v == 0.5))
  expect_gt(sum(attr(v, "failed")), 0)
})

test_that("a target near a slowly approached ceiling is not declared unreachable early", {
  # AUC-like curve with ceiling 0.80 and target 0.799 (reachable at ~20,000):
  # with few points the fitted ceiling collapsed onto the largest value seen.
  data_function <- function(n) data.frame(n = n, z = stats::rnorm(1))
  model_function <- function(d) d
  attr(model_function, "model") <- "glm"
  metric_function <- function(test_data, fit, model) {
    0.80 - 2 / fit$n^0.75 + 0.03 * sqrt(100 / fit$n) * fit$z
  }
  attr(metric_function, "metric") <- "auc"
  sc <- list(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function
  )
  statuses <- vapply(
    1:3,
    function(s) run_curve(sc, 0.799, seed = s, start_n = 200)$status,
    ""
  )
  expect_false(any(statuses == "unreachable"))
})

test_that("a target near the ceiling is flagged; an ordinary one is not", {
  data_function <- function(n) data.frame(n = n, z = stats::rnorm(1))
  model_function <- function(d) d
  attr(model_function, "model") <- "glm"
  metric_function <- function(test_data, fit, model) {
    0.80 - 2 / fit$n^0.75 + 0.03 * sqrt(100 / fit$n) * fit$z
  }
  attr(metric_function, "metric") <- "auc"
  near <- run_curve(
    list(
      data_function = data_function,
      model_function = model_function,
      metric_function = metric_function
    ),
    0.799,
    seed = 2,
    start_n = 2000
  )
  # Target 0.001 below the ceiling: the answer is flagged as near the
  # ceiling or as poorly determined (its interval spans more than 2x).
  if (identical(near$status, "ok")) {
    expect_true(
      near$diagnostics$near_ceiling || near$diagnostics$poorly_determined
    )
  }
  plain <- run_curve(synthetic_curve(), 0.85, seed = 1)
  expect_false(plain$diagnostics$near_ceiling)
})

test_that("max_n defaults to 200,000 for random forests and xgboost", {
  expect_identical(default_max_n("rf"), 2e5)
  expect_identical(default_max_n("xgboost"), 2e5)
  expect_identical(default_max_n("glm"), 1e6)
  expect_identical(default_max_n(NULL), 1e6)
})

test_that("the quantile SE factor is estimated from the data", {
  normal <- run_curve(synthetic_curve(), 0.85, seed = 1)
  skewed <- run_curve(csse_curve(), -0.01, seed = 1, start_n = 300)
  # About 1.43 for a normal 20th percentile; larger for skewed CSSE values.
  expect_lt(abs(normal$diagnostics$curve$se_factor - 1.43), 0.3)
  expect_gt(
    skewed$diagnostics$curve$se_factor,
    normal$diagnostics$curve$se_factor
  )
})

test_that("an ordinary answer passes the shape-free cross-check", {
  res <- run_curve(synthetic_curve(), 0.85, seed = 2)
  expect_true(is.finite(res$diagnostics$crosscheck_n))
  expect_false(res$diagnostics$crosscheck_disagrees)
  expect_false(res$diagnostics$poorly_determined)
  expect_gt(res$diagnostics$gain_per_doubling, 0)
})

test_that("weighted_isotonic and isotonic_crossing behave", {
  expect_equal(
    weighted_isotonic(c(1, 3, 2, 4), c(1, 3, 1, 1)),
    c(1, 2.75, 2.75, 4)
  )
  expect_equal(
    isotonic_crossing(c(100, 200, 400), c(0.7, 0.8, 0.9), c(1, 1, 1), 0.85),
    exp(mean(log(c(200, 400))))
  )
  expect_identical(
    isotonic_crossing(c(100, 200), c(0.5, 0.6), c(1, 1), 0.9),
    Inf
  )
})

test_that("curve results plot, including stopped searches", {
  ok <- run_curve(synthetic_curve(), 0.85, seed = 1)
  d <- plot(ok, plot = FALSE)
  expect_s3_class(d$observed_data, "data.frame")
  expect_true(all(c("n", "y", "lo", "hi", "reps") %in% names(d$observed_data)))
  expect_s3_class(d$fitted_curve, "data.frame")
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_s3_class(plot(ok), "ggplot")
  stopped <- run_curve(csse_curve(bias = 0.12), -0.01, seed = 2, start_n = 500)
  expect_true(stopped$status %in% c("unreachable", "not_bracketed"))
  expect_s3_class(plot(stopped), "ggplot")
})

test_that("messages for CSSE targets use the calibration slope scale", {
  stopped <- run_curve(csse_curve(bias = 0.12), -0.01, seed = 2, start_n = 500)
  expect_match(stopped$status_message, "calibration slope within")
  expect_false(grepl("-0.01", stopped$status_message, fixed = TRUE))
})

test_that("results are identical on one core and several", {
  skip_on_os("windows")
  sc <- synthetic_curve()
  one <- run_curve(sc, 0.85, seed = 4)
  two <- run_curve(sc, 0.85, seed = 4, cores = 2)
  expect_identical(one$min_n, two$min_n)
  expect_identical(one$verification$performance, two$verification$performance)
})

test_that("the live plot redraws after each batch and does not change results", {
  draws <- 0L
  local_mocked_bindings(plot_learning_curve = function(x, ...) {
    draws <<- draws + 1L
    expect_true(length(x$data) >= 1L)
    invisible(NULL)
  })
  withr_opts <- options(pmsims.live_plot_force = TRUE)
  on.exit(options(withr_opts), add = TRUE)
  live <- run_curve(synthetic_curve(), 0.85, seed = 5, live_plot = TRUE)
  options(pmsims.live_plot_force = NULL)
  quiet <- run_curve(synthetic_curve(), 0.85, seed = 5)
  expect_gt(draws, 5L)
  expect_identical(live$min_n, quiet$min_n)
})

test_that("the live plot draws for real on a graphics device", {
  old <- options(pmsims.live_plot_force = TRUE)
  on.exit(options(old), add = TRUE)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(run_curve(
    csse_curve(),
    -0.01,
    seed = 3,
    start_n = 300,
    live_plot = TRUE
  ))
})
