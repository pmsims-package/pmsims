test_that("binary_tuning aligns with requested prevalence and performance", {
  set.seed(2024)
  tuning <- binary_tuning(
    target_prevalence = 0.2,
    target_performance = 0.75,
    candidate_features = 10,
    proportion_noise_features = 0
  )

  expect_length(tuning, 5)
  expect_equal(tuning[4], 0.2, tolerance = 0.02)
  expect_equal(tuning[5], 0.75, tolerance = 0.05)
})

test_that("continuous_tuning returns sensible beta_signal", {
  beta_signal <- continuous_tuning(
    r2 = 0.5,
    proportion_noise_features = 0.2,
    candidate_features = 20
  )

  expect_true(is.numeric(beta_signal))
  expect_gt(beta_signal, 0)
})

test_that("survival_tuning achieves target prevalence and c-index", {
  skip_on_cran()
  skip_if_not_installed("survival")
  set.seed(123)

  tuning <- survival_tuning(
    target_prevalence = 0.3,
    target_performance = 0.75,
    proportion_noise_features = 0,
    candidate_features = 6,
    N_sim_optim = 1000,
    N_sim_final = 2000
  )

  expect_length(tuning, 5)
  expect_named(
    tuning,
    c("lambda_opt", "sigma_sq", "beta_signal", "event_rate", "cindex")
  )
  event_rate <- tuning[["event_rate"]]
  cindex <- tuning[["cindex"]]
  expect_false(is.null(event_rate))
  expect_false(is.null(cindex))
  expect_false(is.na(event_rate))
  expect_false(is.na(cindex))
  expect_true(is.finite(event_rate))
  expect_true(is.finite(cindex))
  expect_true(event_rate > 0 && event_rate < 1)
  expect_equal(event_rate, 0.3, tolerance = 0.05)
  expect_equal(cindex, 0.75, tolerance = 0.05)
})
