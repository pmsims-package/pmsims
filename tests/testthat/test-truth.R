# Statistical "truth" checks. The rest of the suite checks that functions run
# and take the right branch; these check that the numbers mean what they claim.
# Each would have caught a bug that the shape-only tests let through: the
# transposed copula, the reversed survival AUC, and searching metrics where
# smaller is better.

binary_dgp <- function(
  prevalence = 0.2,
  cstat = 0.8,
  p = 5,
  correlation = 0.3
) {
  tp <- binary_tuning(
    target_prevalence = prevalence,
    target_performance = cstat,
    candidate_features = p,
    proportion_noise_features = 0,
    correlation = correlation,
    n_sim = 50000
  )
  default_data_generators(list(
    type = "binary",
    args = list(
      n_signal_parameters = p,
      noise_parameters = 0,
      beta_signal = tp[["beta_signal"]],
      mu_lp = tp[["mu_lp"]],
      correlation = correlation,
      baseline_prob = prevalence
    )
  ))
}

mean_metric <- function(
  data_function,
  model,
  metric,
  n,
  reps = 10,
  test_n = 20000
) {
  outcome <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome, model)
  metric_function <- default_metric_generator(metric, data_function)
  test <- data_function(test_n)
  mean(replicate(
    reps,
    metric_function(test, model_function(data_function(n)), model)
  ))
}

test_that("binary data have the requested prevalence and discrimination", {
  skip_on_cran()
  set.seed(1)
  df <- binary_dgp(prevalence = 0.2, cstat = 0.8)
  d <- df(100000)
  expect_lt(abs(mean(d$y) - 0.2), 0.005)
  # A model fitted on a very large sample approaches the true model.
  big <- mean_metric(df, "glm", "auc", n = 50000, reps = 1)
  expect_lt(abs(big - 0.8), 0.01)
  slope <- mean_metric(df, "glm", "calibration_slope", n = 50000, reps = 1)
  expect_lt(abs(slope - 1), 0.05)
})

test_that("predictors have the requested correlation", {
  skip_on_cran()
  set.seed(2)
  X <- generate_predictors(50000, 6, 0, correlation = 0.3)
  r <- stats::cor(X)
  expect_lt(abs(mean(r[upper.tri(r)]) - 0.3), 0.01)
  expect_lt(max(abs(r[upper.tri(r)] - 0.3)), 0.03)
})

test_that("binary metrics improve with sample size", {
  skip_on_cran()
  set.seed(3)
  df <- binary_dgp(prevalence = 0.3, cstat = 0.75)
  for (metric in c("auc", "calibration_slope", "brier_score_scaled")) {
    small <- mean_metric(df, "glm", metric, n = 150)
    large <- mean_metric(df, "glm", metric, n = 3000)
    expect_gt(large, small, label = paste(metric, "at n = 3000"))
  }
})

test_that("survival discrimination metrics agree and improve with sample size", {
  skip_on_cran()
  set.seed(4)
  df <- default_data_generators(list(
    type = "survival",
    args = list(
      n_signal_parameters = 5,
      noise_parameters = 0,
      beta_signal = 0.4,
      baseline_hazard = 1,
      censoring_rate = 0.5
    )
  ))
  fit <- default_model_generators("survival", "coxph")(df(3000))
  test <- df(20000)
  cindex <- survival_cindex(test, fit, "coxph")
  expect_gt(cindex, 0.6)
  # survival_auc is a discrimination measure too: it must not be 1 - C.
  expect_gt(survival_auc(test, fit, "coxph"), 0.6)

  small <- mean_metric(df, "coxph", "cindex", n = 100)
  large <- mean_metric(df, "coxph", "cindex", n = 3000)
  expect_gt(large, small)
})

test_that("continuous R-squared improves with sample size and reaches its target", {
  skip_on_cran()
  set.seed(5)
  tp <- continuous_tuning(
    r2 = 0.3,
    candidate_features = 5,
    proportion_noise_features = 0,
    correlation = 0.3
  )
  df <- default_data_generators(list(
    type = "continuous",
    args = list(
      n_signal_parameters = 5,
      noise_parameters = 0,
      beta_signal = tp[["beta_signal"]],
      correlation = 0.3
    )
  ))
  # Absolute tolerance: one 20,000-row test set gives R-squared an SE of ~0.006.
  expect_lt(abs(mean_metric(df, "lm", "r2", n = 50000, reps = 1) - 0.3), 0.02)
  expect_gt(
    mean_metric(df, "lm", "r2", n = 3000),
    mean_metric(df, "lm", "r2", n = 60)
  )
})
