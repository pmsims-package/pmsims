test_that("get_perf computes means and quantiles and validates inputs", {
  results <- matrix(
    c(0.6, 0.7, 0.8, 0.7, 0.8, 0.9),
    nrow = 2,
    byrow = TRUE
  )

  expect_equal(get_perf(results, mean = TRUE), c(0.7, 0.8))
  expect_equal(get_perf(results, p = 0.5), c(0.7, 0.8))
  expect_error(
    get_perf(results),
    "Either p or mean must be specified",
    fixed = TRUE
  )
})

test_that("get_summaries returns named summary vectors", {
  results <- matrix(
    c(0.6, 0.7, 0.8, 0.7, 0.8, 0.9),
    nrow = 2,
    byrow = TRUE
  )

  summaries <- get_summaries(results)

  expect_named(
    summaries,
    c(
      "mean_performance",
      "median_performance",
      "quant20_performance",
      "quant5_performance",
      "quant95_performance"
    )
  )
  expect_equal(length(summaries$mean_performance), 2)
})

test_that("adaptive_startvalues falls back when no interval crosses the target", {
  low_output <- list(
    track_bisection = list(
      list(x = 100, y = c(0.50, 0.52, 0.53)),
      list(x = 150, y = c(0.54, 0.55, 0.56))
    )
  )

  adaptive <- adaptive_startvalues(
    output = low_output,
    aggregate_fun = function(x) mean(x, na.rm = TRUE),
    var_bootstrap = function(x) stats::var(x) / max(1, length(x)),
    target = 0.8
  )

  expect_equal(adaptive$min_value, 150)
  expect_equal(adaptive$max_value, 180)
})

test_that("calculate_adaptive_bounds finds bounds in mean mode", {
  data_function <- function(n) {
    data.frame(y = seq_len(n))
  }
  model_function <- function(data) {
    list(n = nrow(data))
  }
  attr(model_function, "model") <- "glm"
  metric_function <- function(data, fit, model) {
    fit$n / 100
  }

  output <- calculate_adaptive_bounds(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    value_on_error = -1,
    start_n = 20,
    test_n = 10,
    n_reps_per = 3,
    n_reps_total = 12,
    target_performance = 0.6,
    threshold = 0,
    mean_or_assurance = "mean",
    verbose = FALSE
  )

  expect_equal(output$min_sample_size, 40)
  expect_equal(output$max_sample_size, 80)
  expect_length(output$track, 3)
})

test_that("calculate_adaptive_bounds uses the fallback value on repeated errors", {
  data_function <- function(n) {
    data.frame(y = seq_len(n))
  }
  model_function <- function(data) {
    stop("boom")
  }
  attr(model_function, "model") <- "glm"
  metric_function <- function(data, fit, model) {
    0.5
  }

  output <- calculate_adaptive_bounds(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    value_on_error = 0.1,
    start_n = 10,
    test_n = 10,
    n_reps_per = 2,
    n_reps_total = 4,
    target_performance = 0.5,
    threshold = 0,
    mean_or_assurance = "assurance",
    verbose = FALSE
  )

  expect_equal(output$track[[1]]$performance, 0.1)
})

test_that("compute_start_sample_sizes covers binary continuous and survival branches", {
  auc_metric <- make_metric_stub("auc")
  slope_metric <- make_metric_stub("calib_slope")
  cindex_metric <- make_metric_stub("cindex")

  binary <- compute_start_sample_sizes(
    data_function = make_binary_data_function(baseline_prob = 0.2),
    metric_function = slope_metric,
    target_performance = 0.9,
    c_statistic = 0.65,
    mean_or_assurance = "assurance"
  )
  expect_equal(binary$start_max_sample_size, 5 * binary$start_min_sample_size)

  binary_auc <- compute_start_sample_sizes(
    data_function = make_binary_data_function(baseline_prob = 0.3),
    metric_function = auc_metric,
    target_performance = 0.75,
    c_statistic = 0.75,
    mean_or_assurance = "mean"
  )
  expect_equal(binary_auc$start_max_sample_size, 100 * binary_auc$npar)

  continuous <- compute_start_sample_sizes(
    data_function = make_continuous_data_function(),
    metric_function = slope_metric,
    target_performance = 0.9,
    mean_or_assurance = "mean"
  )
  expect_equal(continuous$start_max_sample_size, 100 * continuous$npar)

  continuous_r2 <- compute_start_sample_sizes(
    data_function = make_continuous_data_function(),
    metric_function = make_metric_stub("r2"),
    target_performance = 0.5,
    mean_or_assurance = "mean"
  )
  expect_equal(continuous_r2$start_max_sample_size, 200 * continuous_r2$npar)

  survival <- compute_start_sample_sizes(
    data_function = make_survival_data_function(censoring_rate = 0.3),
    metric_function = cindex_metric,
    target_performance = 0.75,
    mean_or_assurance = "mean"
  )
  expect_equal(survival$start_max_sample_size, 100 * survival$npar)
})

test_that("compute_start_sample_sizes validates data function metadata", {
  bad_data_function <- function(n, baseline_prob = 2) {
    data.frame(y = seq_len(n), x = seq_len(n))
  }
  attr(bad_data_function, "outcome") <- "binary"

  expect_error(
    compute_start_sample_sizes(
      data_function = bad_data_function,
      metric_function = make_metric_stub("auc"),
      target_performance = 0.75
    ),
    "baseline_prob must be between 0 and 1",
    fixed = TRUE
  )
})

test_that("get_min_sample_size warns on invalid prevalence inputs", {
  expect_warning(
    binary_n <- get_min_sample_size(
      npar = 4,
      prevalence = NULL,
      c_stat = 0.7,
      epv_value = 10,
      outcome_type = "binary"
    ),
    "Prevalence not provided or invalid; assuming 50% events.",
    fixed = TRUE
  )
  expect_true(binary_n > 0)

  expect_warning(
    survival_n <- get_min_sample_size(
      npar = 4,
      prevalence = NULL,
      c_stat = 0.7,
      epv_value = 10,
      outcome_type = "survival"
    ),
    "Event proportion not provided; assuming 50% events.",
    fixed = TRUE
  )
  expect_true(survival_n > 0)
})
