test_that("binary_tuning aligns with requested prevalence and performance", {
  set.seed(2024)
  tuning <- binary_tuning(
    target_prevalence = 0.2,
    target_performance = 0.75,
    candidate_features = 10,
    proportion_noise_features = 0,
    n_sim = 10000,
    n_validate = 20000,
    tolerance = 0.05
  )

  expect_length(tuning, 5)
  expect_named(
    tuning,
    c(
      "mu_lp",
      "beta_signal",
      "prevalence_achieved",
      "auc_achieved",
      "var_lp_unit"
    )
  )
  expect_gt(tuning[["beta_signal"]], 0)
  expect_gt(tuning[["var_lp_unit"]], 0)
  expect_equal(tuning[["prevalence_achieved"]], 0.2, tolerance = 0.03)
  expect_equal(tuning[["auc_achieved"]], 0.75, tolerance = 0.05)
})

test_that("continuous_tuning returns sensible beta_signal", {
  set.seed(2024)
  tuning <- continuous_tuning(
    r2 = 0.5,
    proportion_noise_features = 0.2,
    candidate_features = 20,
    n_sim = 20000
  )

  expect_length(tuning, 3)
  expect_named(tuning, c("beta_signal", "r2_achieved", "var_lp_unit"))
  expect_true(is.numeric(tuning))
  expect_gt(tuning[["beta_signal"]], 0)
  expect_gt(tuning[["var_lp_unit"]], 0)
  expect_equal(tuning[["r2_achieved"]], 0.5, tolerance = 0.03)
})

test_that("survival_tuning achieves target prevalence and c-index", {
  skip_if_not_installed("survival")
  set.seed(123)

  tuning <- survival_tuning(
    target_prevalence = 0.3,
    target_performance = 0.75,
    proportion_noise_features = 0,
    candidate_features = 6,
    n_sim = 5000,
    n_validate = 10000,
    tolerance = 0.08
  )

  expect_length(tuning, 5)
  expect_named(
    tuning,
    c("lambda_opt", "beta_signal", "event_rate", "cindex", "var_lp_unit")
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
  expect_gt(tuning[["lambda_opt"]], 0)
  expect_gt(tuning[["var_lp_unit"]], 0)
  expect_gte(tuning[["beta_signal"]], 0)
  expect_equal(event_rate, 0.3, tolerance = 0.05)
  expect_equal(cindex, 0.75, tolerance = 0.08)
})
