# Fast fake components: a "learning curve" whose replicate values are
# ceiling - scale / sqrt(n) plus noise, with no model fitting.
fake_curve_components <- function(ceiling = 0.95, scale = 2, noise = 0.01) {
  data_function <- function(n) data.frame(n = n, e = stats::rnorm(1))
  model_function <- function(d) d
  attr(model_function, "model") <- "glm"
  metric_function <- function(test_data, fit, model) {
    ceiling - scale / sqrt(fit$n) + noise * (fit$e + test_data$e)
  }
  attr(metric_function, "metric") <- "auc"
  list(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function
  )
}

test_that("a replicate's stream does not depend on earlier RNG use", {
  set.seed(1)
  s1 <- new_simulation_streams()
  set.seed(1)
  s2 <- new_simulation_streams()
  stats::runif(10) # extra draws, as an added check or a GP refit would make

  x1 <- with_stream(s1, "search", 1000, 3, stats::rnorm(5))
  x2 <- with_stream(s2, "search", 1000, 3, stats::rnorm(5))
  expect_identical(x1, x2)

  expect_false(identical(
    x1,
    with_stream(s1, "search", 1000, 4, stats::rnorm(5))
  ))
  expect_false(identical(
    x1,
    with_stream(s1, "search", 1001, 3, stats::rnorm(5))
  ))
  expect_false(identical(
    x1,
    with_stream(s1, "verify", 1000, 3, stats::rnorm(5))
  ))
})

test_that("streams leave the caller's RNG as found, apart from the base-seed draw", {
  set.seed(42)
  sample.int(.Machine$integer.max, 1L) # the base-seed draw
  expected <- stats::runif(3)

  set.seed(42)
  streams <- new_simulation_streams()
  with_stream(streams, "search", 50, 1, stats::rnorm(100))
  expect_identical(stats::runif(3), expected)
  expect_identical(RNGkind()[1], "Mersenne-Twister")
})

test_that("replicate values do not depend on order or on parallel execution", {
  fc <- fake_curve_components()
  make <- function(...) {
    new_evaluator(
      fc$data_function,
      fc$model_function,
      fc$metric_function,
      test_n = 10,
      value_on_error = 0,
      streams = new_simulation_streams(7L),
      ...
    )
  }
  a <- make()
  v_a <- a$batch(500, 6)

  b <- make()
  b$batch(80, 3) # different work first
  v_b <- b$batch(500, 6)
  expect_identical(v_a, v_b)

  skip_on_os("windows")
  p <- make(parallel = TRUE, cores = 2L)
  expect_identical(p$batch(500, 6), v_a)
})

test_that("the evaluator counts failures and warnings instead of hiding them", {
  data_function <- function(n) data.frame(x = stats::rnorm(n))
  model_function <- function(d) {
    warning("did not converge")
    d
  }
  attr(model_function, "model") <- "glm"
  calls <- 0L
  metric_function <- function(test_data, fit, model) {
    calls <<- calls + 1L
    if (calls %% 3L == 0L) {
      stop("fit failed")
    }
    if (calls %% 3L == 1L) {
      return(NA_real_)
    }
    0.7
  }
  ev <- new_evaluator(
    data_function,
    model_function,
    metric_function,
    test_n = 5,
    value_on_error = -1,
    streams = new_simulation_streams(1L)
  )
  expect_no_warning(vals <- ev$batch(20, 6))
  expect_identical(as.numeric(vals), c(-1, 0.7, -1, -1, 0.7, -1))
  expect_identical(attr(vals, "failed"), c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE))
  f <- ev$failures()
  expect_identical(f$failed, 4L)
  expect_identical(f$warnings, 6L)
  expect_identical(f$first_error, "fit failed")
})

test_that("metrics where smaller is better are rejected", {
  expect_error(
    check_metric_direction("calibration_in_the_large"),
    "better when smaller"
  )
  expect_error(check_metric_direction("brier_score"), "better when smaller")
  expect_silent(check_metric_direction("brier_score_scaled"))
  expect_error(
    validate_metric_constraints("calibration_in_the_large", 0.02),
    "better when smaller"
  )
})

test_that("pmsims_threads is at least one and honours the option", {
  old <- options(pmsims.threads = NULL)
  on.exit(options(old), add = TRUE)
  expect_gte(pmsims_threads(), 1L)
  options(pmsims.threads = 0)
  expect_identical(pmsims_threads(), 1L)
  options(pmsims.threads = 3)
  expect_identical(pmsims_threads(), 3L)
})

test_that("adaptive_status stops the search only at max_n, clearly below target", {
  step <- function(n, perf, call, se = 0.005) {
    list(n = n, performance = perf, se = se, call = call)
  }
  # A plateau is not evidence of an unreachable target: the main search runs.
  plateau <- list(
    stop_reason = "plateau",
    track = list(step(100, 0.80, "below"), step(200, 0.81, "below"))
  )
  expect_null(adaptive_status(plateau, 0.9))

  at_max <- list(
    stop_reason = "max_n_reached",
    track = list(step(100, 0.70, "below"), step(200, 0.75, "below"))
  )
  expect_identical(adaptive_status(at_max, 0.9)$status, "not_bracketed")

  close_at_max <- list(
    stop_reason = "max_n_reached",
    track = list(step(100, 0.70, "below"), step(200, 0.895, "uncertain"))
  )
  expect_null(adaptive_status(close_at_max, 0.9))

  exhausted <- list(
    stop_reason = "budget_exhausted",
    track = list(step(100, 0.70, "below"), step(200, 0.75, "below"))
  )
  expect_null(adaptive_status(exhausted, 0.9))
})

test_that("simulate_custom stops at max_n when the target is out of reach", {
  fc <- fake_curve_components(ceiling = 0.85)
  set.seed(3)
  expect_warning(
    res <- suppressMessages(simulate_custom(
      fc$data_function,
      fc$model_function,
      fc$metric_function,
      target_performance = 0.9,
      mean_or_assurance = "mean",
      test_n = 10,
      n_reps_total = 200,
      n_reps_per = 10,
      progress = FALSE,
      method = "mlpwr",
      max_n = 5000
    )),
    "No sample size up to"
  )
  expect_identical(res$status, "not_bracketed")
  expect_true(is.na(res$min_n))
  expect_type(res$min_n, "double")
  expect_output(print(res), "Not found")
  expect_error(plot(res), "search stopped")
})

test_that("an unreachable target without max_n is flagged by the check", {
  fc <- fake_curve_components(ceiling = 0.85)
  set.seed(3)
  expect_warning(
    res <- suppressMessages(simulate_custom(
      fc$data_function,
      fc$model_function,
      fc$metric_function,
      target_performance = 0.9,
      mean_or_assurance = "mean",
      test_n = 10,
      n_reps_total = 200,
      n_reps_per = 10,
      progress = FALSE,
      method = "mlpwr"
    )),
    "clearly below the target"
  )
  expect_identical(res$status, "not_verified")
})

test_that("simulate_custom flags an answer that fails the check", {
  fc <- fake_curve_components(ceiling = 0.85)
  set.seed(3)
  # Bounds supplied, so no adaptive stage: mlpwr can only return the edge.
  expect_warning(
    res <- suppressMessages(simulate_custom(
      fc$data_function,
      fc$model_function,
      fc$metric_function,
      target_performance = 0.9,
      mean_or_assurance = "mean",
      test_n = 10,
      min_sample_size = 100,
      max_sample_size = 2000,
      n_reps_total = 200,
      n_reps_per = 10,
      progress = FALSE,
      method = "mlpwr"
    )),
    "clearly below the target|did not return"
  )
  expect_true(res$status %in% c("not_verified", "not_bracketed"))
})

test_that("simulate_custom verifies a reachable target and is reproducible", {
  fc <- fake_curve_components(ceiling = 0.95, scale = 2)
  run <- function() {
    set.seed(11)
    suppressMessages(simulate_custom(
      fc$data_function,
      fc$model_function,
      fc$metric_function,
      target_performance = 0.85,
      mean_or_assurance = "assurance",
      test_n = 10,
      n_reps_total = 200,
      n_reps_per = 10,
      progress = FALSE,
      method = "mlpwr",
      verify_reps = 50
    ))
  }
  res <- run()
  expect_identical(res$status, "ok")
  expect_true(res$verification$verified)
  # True answer: 0.95 - 2 / sqrt(n) = 0.85 at n = 400, plus noise.
  expect_gt(res$min_n, 250)
  expect_lt(res$min_n, 1000)
  expect_identical(run()$min_n, res$min_n)
})

test_that("the secondary metric is skipped when there is no sample size", {
  expect_identical(
    secondary_metric_at(list(min_n = NA_real_), function(...) 1),
    NA_real_
  )
})

test_that("max_n defaults by model", {
  expect_identical(default_max_n("rf"), 2e5)
  expect_identical(default_max_n("xgboost"), 2e5)
  expect_identical(default_max_n("glm"), 1e6)
  expect_identical(default_max_n(NULL), 1e6)
})

test_that("the last rung of the ladder is max_n", {
  fc <- fake_curve_components(ceiling = 0.85)
  set.seed(3)
  b <- calculate_adaptive_bounds(
    fc$data_function,
    fc$model_function,
    fc$metric_function,
    value_on_error = 0.5,
    start_n = 100,
    test_n = 10,
    n_reps_per = 10,
    n_reps_total = 500,
    target_performance = 0.9,
    mean_or_assurance = "mean",
    max_n = 5000
  )
  expect_identical(b$stop_reason, "max_n_reached")
  expect_identical(max(vapply(b$track, `[[`, numeric(1), "n")), 5000)
  expect_lte(b$max_sample_size, 5000)
})

test_that("mostly failing replicates give status replicates_failed", {
  fc <- fake_curve_components()
  broken <- function(d) stop("model did not converge")
  attr(broken, "model") <- "glm"
  set.seed(1)
  expect_warning(
    res <- suppressMessages(simulate_custom(
      fc$data_function,
      broken,
      fc$metric_function,
      target_performance = 0.7,
      mean_or_assurance = "mean",
      test_n = 10,
      n_reps_total = 200,
      n_reps_per = 10,
      progress = FALSE
    )),
    "failed to fit or score.*model did not converge"
  )
  expect_identical(res$status, "replicates_failed")
  expect_true(is.na(res$min_n))
})

test_that("CSSE values are described as calibration slopes in messages", {
  plan <- plan_internal_csse("calibration_slope", "lasso", 0.9)
  f <- describe_as_calibration_slope(function(...) 0, plan)
  expect_identical(describe_value(f)(-0.01), "a calibration slope of 0.9")
  expect_identical(describe_value(function(...) 0)(0.123456), "0.1235")
})
