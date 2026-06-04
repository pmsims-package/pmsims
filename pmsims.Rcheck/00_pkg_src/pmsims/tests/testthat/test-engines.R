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
