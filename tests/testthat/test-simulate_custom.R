test_that("parse_inputs", {
  # Binary
  inputs <- parse_inputs(
    data_spec = list(
      type = "binary",
      args = list(
        n_signal_parameters = 5,
        noise_parameters = 5,
        predictor_type = "continuous",
        mu_lp = stats::qlogis(0.1),
        beta_signal = 0.1,
        baseline_prob = 0.1
      )
    ),
    metric = "auc",
    model = "glm"
  )
  expect_equal(length(inputs), 3)

  # Continuous

  inputs <- parse_inputs(
    data_spec = list(
      type = "continuous",
      args = list(
        n_signal_parameters = 5,
        noise_parameters = 5,
        predictor_type = "continuous",
        beta_signal = 0.1
      )
    ),
    metric = "r2",
    model = "lm"
  )
  expect_equal(length(inputs), 3)

  # Survival
  inputs <- parse_inputs(
    data_spec = list(
      type = "survival",
      args = list(
        n_signal_parameters = 5,
        noise_parameters = 5,
        predictor_type = "continuous",
        beta_signal = 0.1,
        baseline_hazard = 0.01,
        censoring_rate = 0.3
      )
    ),
    metric = "auc",
    model = "coxph"
  )
  expect_equal(length(inputs), 3)
})

test_that("simulate_custom", {
  set.seed(1234)
  data_opts <- list(
    type = "binary",
    args = list(
      n_signal_parameters = 5,
      noise_parameters = 0,
      predictor_type = "continuous",
      baseline_prob = 0.2,
      mu_lp = stats::qlogis(0.2),
      beta_signal = 0.5
    )
  )
  data_function <- default_data_generators(data_opts)
  outcome_type <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome_type, model = "glm")

  metric_function <- default_metric_generator(
    "auc",
    data_function
  )

  sim_results_mlpwr <- suppressWarnings(simulate_custom(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    target_performance = 0.73,
    c_statistic = 0.8,
    test_n = 2000,
    min_sample_size = 75,
    max_sample_size = 200,
    n_reps_total = 40,
    n_reps_per = 10,
    method = "mlpwr",
    verbose = FALSE
  ))

  expect_s3_class(sim_results_mlpwr, "pmsims")
  expect_true(is.numeric(sim_results_mlpwr$min_n))
  expect_gt(sim_results_mlpwr$min_n, 0)
  expect_true(is.list(sim_results_mlpwr$summaries))
  expect_equal(sim_results_mlpwr$mean_or_assurance, "assurance")
  expect_identical(sim_results_mlpwr$n_reps_total, 40)
  expect_identical(sim_results_mlpwr$n_reps_per, 10)
  expect_identical(sim_results_mlpwr$test_n, 2000)
  expect_identical(sim_results_mlpwr$method, "mlpwr")
  expect_identical(sim_results_mlpwr$min_sample_size, 75)
  expect_identical(sim_results_mlpwr$max_sample_size, 200)
  expect_identical(sim_results_mlpwr$c_statistic, 0.8)

  sim_results_mlpwr_bs <- suppressWarnings(simulate_custom(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    target_performance = 0.73,
    c_statistic = 0.8,
    test_n = 2000,
    min_sample_size = 75,
    max_sample_size = 200,
    n_reps_total = 40,
    n_reps_per = 10,
    method = "mlpwr-bs",
    verbose = FALSE
  ))

  expect_s3_class(sim_results_mlpwr_bs, "pmsims")
  expect_true(is.numeric(sim_results_mlpwr_bs$min_n))
  expect_gt(sim_results_mlpwr_bs$min_n, 0)
})

test_that("simulate_custom returns bisection history when verbose is TRUE", {
  set.seed(1234)
  data_opts <- list(
    type = "binary",
    args = list(
      n_signal_parameters = 5,
      noise_parameters = 0,
      predictor_type = "continuous",
      baseline_prob = 0.2,
      mu_lp = stats::qlogis(0.2),
      beta_signal = 0.5
    )
  )
  data_function <- default_data_generators(data_opts)
  outcome_type <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome_type, model = "glm")
  metric_function <- default_metric_generator("auc", data_function)

  sim_results_bisection <- suppressWarnings(simulate_custom(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    target_performance = 0.73,
    c_statistic = 0.8,
    test_n = 2000,
    min_sample_size = 75,
    max_sample_size = 200,
    n_reps_total = 40,
    n_reps_per = 10,
    method = "bisection",
    verbose = TRUE
  ))

  expect_true(is.list(sim_results_bisection$history))
  expect_true(length(sim_results_bisection$history) > 0)
})

test_that("simulate_custom can suppress the mlpwr progress bar", {
  set.seed(1234)
  data_opts <- list(
    type = "binary",
    args = list(
      n_signal_parameters = 5,
      noise_parameters = 0,
      predictor_type = "continuous",
      baseline_prob = 0.2,
      mu_lp = stats::qlogis(0.2),
      beta_signal = 0.5
    )
  )
  data_function <- default_data_generators(data_opts)
  outcome_type <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome_type, model = "glm")
  metric_function <- default_metric_generator("auc", data_function)

  capture_path <- tempfile(fileext = ".txt")
  old_sink <- sink.number(type = "output")
  capture <- file(capture_path, open = "wt")
  sink(capture, type = "output")
  on.exit(
    {
      if (sink.number(type = "output") > old_sink) {
        sink(type = "output")
      }
      if (inherits(try(close(capture), silent = TRUE), "try-error")) {
        invisible(NULL)
      }
      if (file.exists(capture_path)) {
        unlink(capture_path)
      }
    },
    add = TRUE
  )

  suppressWarnings(
    simulate_custom(
      data_function = data_function,
      model_function = model_function,
      metric_function = metric_function,
      target_performance = 0.73,
      c_statistic = 0.8,
      test_n = 2000,
      min_sample_size = 75,
      max_sample_size = 200,
      n_reps_total = 40,
      n_reps_per = 10,
      method = "mlpwr",
      progress = FALSE,
      verbose = FALSE
    )
  )

  if (sink.number(type = "output") > old_sink) {
    sink(type = "output")
  }
  close(capture)
  output <- readLines(capture_path, warn = FALSE)

  # min_sample_size and max_sample_size define the search space, so the
  # adaptive first stage is skipped (see test-engines.R).
  expect_false(any(grepl("Estimating first stage", output)))
  expect_true(any(grepl("Estimating second stage", output)))
  expect_false(any(grepl("sims \\(", output)))
})

test_that("simulate_custom forwards options to every engine", {
  captured <- list()
  engine_output <- function() {
    list(
      min_n = 50,
      perf_n = 0.8,
      mlpwr_ds = NULL,
      summaries = list(),
      results = matrix(0.8, nrow = 1, dimnames = list("50", NULL))
    )
  }

  local_mocked_bindings(
    calculate_mlpwr = function(...) {
      captured$mlpwr <<- list(...)
      engine_output()
    },
    calculate_bisection = function(...) {
      captured$bisection <<- list(...)
      engine_output()
    },
    calculate_mlpwr_bs = function(...) {
      captured$mlpwr_bs <<- list(...)
      engine_output()
    },
    .package = "pmsims"
  )

  data_function <- function(n) data.frame(y = rep(0, n), x1 = rep(0, n))
  model_function <- function(data) NULL
  metric_function <- function(test_data, fit, model) 0.8
  attr(data_function, "outcome") <- "binary"
  attr(model_function, "model") <- "glm"
  attr(metric_function, "metric") <- "auc"

  common <- list(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    target_performance = 0.8,
    n_reps_total = 20,
    n_reps_per = 5
  )

  do.call(
    simulate_custom,
    c(common, list(method = "mlpwr", ci_perc = 0.9))
  )
  do.call(
    simulate_custom,
    c(common, list(method = "bisection", tol = 0.25))
  )
  do.call(
    simulate_custom,
    c(common, list(method = "mlpwr-bs", ci_perc = 0.85))
  )

  expect_identical(captured$mlpwr$ci_perc, 0.9)
  expect_identical(captured$bisection$tol, 0.25)
  expect_identical(captured$mlpwr_bs$ci_perc, 0.85)
})

test_that("resolve_value_on_error preserves current defaults and supports custom overrides", {
  builtin_metric <- function(test_data, fitted_model, model_name) 0.8
  attr(builtin_metric, "metric") <- "auc"

  custom_metric <- function(test_data, fitted_model, model_name) 0.8
  attr(custom_metric, "metric") <- "my_custom_metric"
  attr(custom_metric, "value_on_error") <- -Inf

  unknown_metric <- function(test_data, fitted_model, model_name) 0.8
  attr(unknown_metric, "metric") <- "my_other_metric"

  invalid_metric <- function(test_data, fitted_model, model_name) 0.8
  attr(invalid_metric, "value_on_error") <- c(0.1, 0.2)

  expect_identical(resolve_value_on_error(builtin_metric), 0.5)
  expect_identical(resolve_value_on_error(custom_metric), -Inf)
  expect_identical(resolve_value_on_error(unknown_metric), 0.5)
  expect_error(
    resolve_value_on_error(invalid_metric),
    'attr(metric_function, "value_on_error") must be a single non-missing numeric value.',
    fixed = TRUE
  )
})
