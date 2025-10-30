test_that("simulate_binary", {
  set.seed(1234)

  output <- simulate_binary(
    n_signal_parameters = 5,
    noise_parameters = 5,
    predictor_type = "continuous",
    baseline_prob = 0.1,
    min_sample_size = 100,
    max_sample_size = 3000,
    minimum_threshold = 0.05,
    tune_param = 0.60,
    large_sample_performance = 0.7,
  )
  expect_equal(length(output), 8)
})

# test_that("simulate_continuous", {
#   set.seed(4321)
#   output <- simulate_continuous(
#     n_signal_parameters = 10,
#     noise_parameters = 10,
#     predictor_type = "continuous",
#     min_sample_size = 100,
#     max_sample_size = 3000,
#     n_reps_total = 10,
#     minimum_threshold = 0.05,
#     tune_param = 0.6353973
#   )
#   expect_equal(length(output), 8)
# })

# test_that("simulate_survival", {
#   set.seed(1234)
#
#   output <- simulate_survival(
#     signal_parameters = 10,
#     noise_parameters = 10,
#     predictor_type = "continuous",
#     min_sample_size = 100,
#     max_sample_size = 3000,
#     n_reps_total = 10,
#     minimum_threshold = 0.05,
#     large_sample_performance = 0.7
#   )
#   expect_equal(length(output), 8)
# })
