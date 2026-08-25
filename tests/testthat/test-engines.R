test_that("calculate_mlpwr", {
  set.seed(1234)
  functions <- get_binary_data_model_metric()

  output <- suppressWarnings(calculate_mlpwr(
    test_n = 1000,
    n_reps_total = 20,
    n_reps_per = 5,
    se_final = NULL,
    min_sample_size = 50,
    max_sample_size = 200,
    target_performance = 0.75,
    c_statistic = 0.8,
    mean_or_assurance = "mean",
    n_init = 4,
    data_function = functions$data_function,
    model_function = functions$model_function,
    metric_function = functions$metric_function,
    value_on_error = 0.5
  ))

  expect_true(is.numeric(output$min_n))
  expect_true(is.list(output$summaries))

  output_assurance <- suppressWarnings(calculate_mlpwr(
    test_n = 1000,
    n_reps_total = 20,
    n_reps_per = 5,
    se_final = NULL,
    min_sample_size = 50,
    max_sample_size = 200,
    target_performance = 0.75,
    c_statistic = 0.8,
    mean_or_assurance = "assurance",
    n_init = 4,
    data_function = functions$data_function,
    model_function = functions$model_function,
    metric_function = functions$metric_function,
    value_on_error = 0.5
  ))

  expect_true(is.numeric(output_assurance$min_n))
})

test_that("calculate_mlpwr calls mlpwr::find.design once", {
  calls <- 0L
  find_design_args <- NULL

  local_mocked_bindings(
    compute_start_sample_sizes = function(...) {
      list(start_min_sample_size = 50)
    },
    calculate_adaptive_bounds = function(...) {
      list(min_sample_size = 50, max_sample_size = 100)
    },
    get_summaries = function(...) {
      list(mean = 0.8)
    },
    .package = "pmsims"
  )

  local_mocked_bindings(
    find.design = function(...) {
      calls <<- calls + 1L
      find_design_args <<- list(...)
      list(
        dat = list(list(x = 50, y = c(0.79, 0.81))),
        fit = list(),
        boundaries = c(50, 100),
        final = list(design = 50, power = 0.8),
        aggregate_fun = function(x) mean(x, na.rm = TRUE)
      )
    },
    .package = "mlpwr"
  )

  data_function <- function(n) data.frame(y = rep(0, n), x1 = rep(0, n))
  model_function <- function(data) NULL
  metric_function <- function(test_data, fit, model) 0.8
  attr(model_function, "model") <- "glm"
  attr(metric_function, "metric") <- "auc"

  output <- calculate_mlpwr(
    test_n = 100,
    n_reps_total = 20,
    n_reps_per = 5,
    se_final = NULL,
    min_sample_size = 50,
    max_sample_size = 100,
    target_performance = 0.75,
    c_statistic = 0.8,
    mean_or_assurance = "mean",
    n_init = 4,
    progress = FALSE,
    verbose = FALSE,
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    value_on_error = 0.5,
    ci_perc = 0.9
  )

  expect_identical(calls, 1L)
  expect_identical(output$min_n, 50)
  expect_identical(find_design_args$ci_perc, 0.9)
})

test_that("calculate_mlpwr_bs", {
  set.seed(1234)
  functions <- get_binary_data_model_metric()

  output <- suppressWarnings(calculate_mlpwr_bs(
    test_n = 1000,
    n_reps_total = 40,
    n_reps_per = 5,
    se_final = NULL,
    min_sample_size = 50,
    max_sample_size = 200,
    target_performance = 0.75,
    c_statistic = 0.8,
    mean_or_assurance = "mean",
    verbose = FALSE,
    data_function = functions$data_function,
    model_function = functions$model_function,
    metric_function = functions$metric_function,
    value_on_error = 0.5
  ))

  expect_true(is.numeric(output$min_n))
  expect_true(is.list(output$summaries))
})

test_that("calculate_bisection", {
  set.seed(1234)
  functions <- get_binary_data_model_metric()

  output <- suppressWarnings(calculate_bisection(
    data_function = functions$data_function,
    model_function = functions$model_function,
    metric_function = functions$metric_function,
    value_on_error = 0.5,
    min_sample_size = 50,
    max_sample_size = 200,
    test_n = 1000,
    n_reps_total = 40,
    n_reps_per = 10,
    target_performance = 0.75,
    c_statistic = 0.8,
    mean_or_assurance = "mean",
    tol = 1e-3,
    parallel = FALSE,
    cores = 2,
    verbose = FALSE
  ))

  expect_true(is.numeric(output$min_n))

  output_assurance <- suppressWarnings(calculate_bisection(
    data_function = functions$data_function,
    model_function = functions$model_function,
    metric_function = functions$metric_function,
    value_on_error = 0.5,
    min_sample_size = 50,
    max_sample_size = 200,
    test_n = 1000,
    n_reps_total = 40,
    n_reps_per = 10,
    target_performance = 0.75,
    c_statistic = 0.8,
    mean_or_assurance = "assurance",
    tol = 1e-3,
    parallel = FALSE,
    cores = 2,
    verbose = FALSE
  ))

  expect_true(is.numeric(output_assurance$min_n))
})

# Supplying both bounds defines the search space, so the adaptive starting
# value search would only produce bounds that are immediately discarded.

fake_find_design <- function(boundaries_store) {
  function(...) {
    args <- list(...)
    boundaries_store$boundaries <- args$boundaries
    list(
      dat = list(list(x = 50, y = c(0.79, 0.81))),
      fit = list(),
      boundaries = args$boundaries,
      final = list(design = 50, power = 0.8),
      aggregate_fun = function(x) mean(x, na.rm = TRUE)
    )
  }
}

test_that("calculate_mlpwr skips the adaptive stage when bounds are supplied", {
  adaptive_calls <- 0L
  start_calls <- 0L
  store <- new.env()

  local_mocked_bindings(
    compute_start_sample_sizes = function(...) {
      start_calls <<- start_calls + 1L
      list(start_min_sample_size = 50)
    },
    calculate_adaptive_bounds = function(...) {
      adaptive_calls <<- adaptive_calls + 1L
      list(min_sample_size = 200, max_sample_size = 400)
    },
    get_summaries = function(...) list(mean = 0.8),
    .package = "pmsims"
  )
  local_mocked_bindings(
    find.design = fake_find_design(store),
    .package = "mlpwr"
  )

  data_function <- function(n) data.frame(y = rep(0, n), x1 = rep(0, n))
  model_function <- function(data) NULL
  metric_function <- function(test_data, fit, model) 0.8
  attr(model_function, "model") <- "glm"
  attr(metric_function, "metric") <- "auc"

  printed <- capture.output(
    calculate_mlpwr(
      test_n = 100,
      n_reps_total = 20,
      n_reps_per = 5,
      se_final = NULL,
      min_sample_size = 100,
      max_sample_size = 500,
      target_performance = 0.75,
      c_statistic = 0.8,
      mean_or_assurance = "mean",
      n_init = 4,
      progress = FALSE,
      verbose = FALSE,
      data_function = data_function,
      model_function = model_function,
      metric_function = metric_function,
      value_on_error = 0.5
    )
  )

  expect_identical(adaptive_calls, 0L)
  expect_identical(start_calls, 0L)
  # The user's bounds reach mlpwr untouched.
  expect_identical(store$boundaries, c(100, 500))
  # And nothing claims the adaptive stage ran.
  expect_false(any(grepl("Adaptive starting value search", printed)))
  expect_false(any(grepl("Starting values determined", printed)))
})

test_that("calculate_mlpwr still runs the adaptive stage without bounds", {
  adaptive_calls <- 0L
  store <- new.env()

  local_mocked_bindings(
    compute_start_sample_sizes = function(...) list(start_min_sample_size = 50),
    calculate_adaptive_bounds = function(...) {
      adaptive_calls <<- adaptive_calls + 1L
      list(min_sample_size = 200, max_sample_size = 400)
    },
    get_summaries = function(...) list(mean = 0.8),
    .package = "pmsims"
  )
  local_mocked_bindings(
    find.design = fake_find_design(store),
    .package = "mlpwr"
  )

  data_function <- function(n) data.frame(y = rep(0, n), x1 = rep(0, n))
  model_function <- function(data) NULL
  metric_function <- function(test_data, fit, model) 0.8
  attr(model_function, "model") <- "glm"
  attr(metric_function, "metric") <- "auc"

  printed <- capture.output(
    calculate_mlpwr(
      test_n = 100,
      n_reps_total = 20,
      n_reps_per = 5,
      se_final = NULL,
      min_sample_size = NULL,
      max_sample_size = NULL,
      target_performance = 0.75,
      c_statistic = 0.8,
      mean_or_assurance = "mean",
      n_init = 4,
      progress = FALSE,
      verbose = FALSE,
      data_function = data_function,
      model_function = model_function,
      metric_function = metric_function,
      value_on_error = 0.5
    )
  )

  expect_identical(adaptive_calls, 1L)
  expect_identical(store$boundaries, c(200, 400))
  expect_true(any(grepl("Adaptive starting value search", printed)))
})

test_that("calculate_bisection skips the adaptive stage when bounds are supplied", {
  set.seed(1234)
  functions <- get_binary_data_model_metric()
  adaptive_calls <- 0L

  local_mocked_bindings(
    calculate_adaptive_bounds = function(...) {
      adaptive_calls <<- adaptive_calls + 1L
      list(min_sample_size = 200, max_sample_size = 400)
    },
    .package = "pmsims"
  )

  output <- suppressWarnings(calculate_bisection(
    data_function = functions$data_function,
    model_function = functions$model_function,
    metric_function = functions$metric_function,
    value_on_error = 0.5,
    min_sample_size = 50,
    max_sample_size = 200,
    test_n = 200,
    n_reps_total = 20,
    n_reps_per = 10,
    target_performance = 0.75,
    c_statistic = 0.8,
    mean_or_assurance = "mean",
    tol = 1e-3,
    parallel = FALSE,
    cores = 2,
    verbose = FALSE
  ))

  expect_identical(adaptive_calls, 0L)
  # The search stayed inside the user's bounds.
  expect_gte(output$min_sample_size_bound, 50)
  expect_lte(output$max_sample_size_bound, 200)
})

test_that("calculate_mlpwr_bs skips the adaptive stage but keeps its bisection", {
  set.seed(1234)
  functions <- get_binary_data_model_metric()
  adaptive_calls <- 0L
  store <- new.env()

  local_mocked_bindings(
    calculate_adaptive_bounds = function(...) {
      adaptive_calls <<- adaptive_calls + 1L
      list(min_sample_size = 200, max_sample_size = 400)
    },
    .package = "pmsims"
  )
  local_mocked_bindings(
    find.design = fake_find_design(store),
    .package = "mlpwr"
  )

  suppressWarnings(calculate_mlpwr_bs(
    test_n = 200,
    n_reps_total = 20,
    n_reps_per = 5,
    se_final = NULL,
    min_sample_size = 50,
    max_sample_size = 200,
    target_performance = 0.75,
    c_statistic = 0.8,
    mean_or_assurance = "mean",
    progress = FALSE,
    verbose = FALSE,
    data_function = functions$data_function,
    model_function = functions$model_function,
    metric_function = functions$metric_function,
    value_on_error = 0.5
  ))

  expect_identical(adaptive_calls, 0L)
  expect_identical(store$boundaries, c(50, 200))
})
