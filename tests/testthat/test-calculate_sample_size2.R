test_that("calculate_sample_size2", {
  opts <- list(type = "binary",
               args = list(n_params = 10))
  
  output <- pmsims::calculate_sample_size2(
    data = opts,
    target_performance = 0.70,
    test_n = 1e4,
    tune_param = 0.7,
    min_sample_size = 100,
    max_sample_size = 3000,
    n_reps = 10,
    n_sample_sizes = 10
  )
  expect_equal(length(output), 5)
})

test_that("tune error message", {
  opts <- list(type = "binary",
               args = list(n_params = 10))
  
  expect_error(pmsims::calculate_sample_size2(
    data = opts,
    target_performance = 0.70,
    test_n = 1e4,
    min_sample_size = 100,
    max_sample_size = 3000,
    n_reps = 10,
    n_sample_sizes = 10
  ), regexp = "one of tune_param or large_sample_performance must be specified")
})

test_that("calculate_sample_size2 with tuning", {
  opts <- list(type = "binary",
               args = list(n_params = 10))
  
  output <- pmsims::calculate_sample_size2(
    data = opts,
    target_performance = 0.70,
    test_n = 1e4,
   large_sample_performance = 0.75,
    min_sample_size = 100,
    max_sample_size = 3000,
    n_reps = 10,
    n_sample_sizes = 10
  )
  expect_equal(length(output), 5)
})