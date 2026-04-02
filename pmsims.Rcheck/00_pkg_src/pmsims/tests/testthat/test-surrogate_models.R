test_that("sm_linear_extrapolation interpolates minimum sample size", {
  results <- matrix(
    c(0.50, 0.50, 0.50, 0.70, 0.70, 0.70, 0.90, 0.90, 0.90),
    nrow = 3,
    byrow = TRUE
  )
  simulation_parameters <- list(train_size = c(50, 100, 150))

  output <- sm_linear_extrapolation(
    results = results,
    simulation_parameters = simulation_parameters,
    target_performance = 0.70
  )

  expect_named(output, c("min_n", "target", "summaries", "data", "train_size"))
  expect_equal(output$min_n, 100)
  expect_equal(output$target, 0.70)
  expect_equal(output$train_size, simulation_parameters$train_size)
  expect_equal(
    colnames(output$summaries),
    c(
      "median_performance",
      "quant20_performance",
      "quant5_performance",
      "quant95_performance"
    )
  )
})
