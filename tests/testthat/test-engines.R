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
    value_on_error = 0.5
  )

  expect_identical(calls, 1L)
  expect_identical(output$min_n, 50)
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
