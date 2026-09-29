test_that("generate_predictors", {
  data <- generate_predictors(
    n = 100,
    n_signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "continuous"
  )
  expect_equal(nrow(data), 100)
  expect_equal(ncol(data), 10)

  data2 <- generate_predictors(
    n = 100,
    n_signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "binary",
    binary_prevalence = 0.3
  )
  expect_equal(nrow(data2), 100)
  expect_equal(ncol(data2), 10)
  expect_equal(colnames(data2), paste0("x", 1:10))
})

test_that("generate_linear_predictor", {
  X <- generate_predictors(
    n = 100,
    n_signal_parameters = 10,
    noise_parameters = 0,
    predictor_type = "continuous"
  )
  lp <- generate_linear_predictor(
    X,
    n_signal_parameters = 5,
    noise_parameters = 5,
    intercept = 1,
    beta_signal = 0.5,
    complexity = 1
  )
  expect_equal(length(lp), 100)
})


test_that("generate_continuous_data", {
  signal_parameters <- 5
  noise_parameters <- 5
  data <- generate_continuous_data(
    n = 100,
    n_signal_parameters = signal_parameters,
    noise_parameters = noise_parameters,
    predictor_type = "binary",
    binary_prevalence = 0.1,
    beta_signal = 0.1
  )

  expect_equal(nrow(data), 100)
  expect_equal(ncol(data), 11)
  expect_equal(
    colnames(data),
    c("y", paste0("x", 1:(signal_parameters + noise_parameters)))
  )
})

test_that("generate_binary_data", {
  signal_parameters <- 5
  noise_parameters <- 5
  baseline_prob <- 0.1
  mu_lp <- stats::qlogis(baseline_prob)
  data <- generate_binary_data(
    n = 100,
    mu_lp = mu_lp,
    n_signal_parameters = signal_parameters,
    noise_parameters = noise_parameters,
    predictor_type = "continuous",
    beta_signal = 0.1,
    baseline_prob = baseline_prob
  )

  expect_equal(nrow(data), 100)
  expect_equal(ncol(data), 11)
  expect_equal(
    colnames(data),
    c("y", paste0("x", 1:(signal_parameters + noise_parameters)))
  )
})

test_that("generate_survival_data", {
  signal_parameters <- 5
  noise_parameters <- 5
  data <- generate_survival_data(
    n = 100,
    n_signal_parameters = signal_parameters,
    noise_parameters = noise_parameters,
    predictor_type = "continuous",
    beta_signal = 0.1,
    baseline_hazard = 0.02,
    censoring_rate = 0.3
  )

  expect_equal(nrow(data), 100)
  expect_equal(ncol(data), signal_parameters + noise_parameters + 2)
  expect_equal(
    colnames(data),
    c("time", "event", paste0("x", 1:(signal_parameters + noise_parameters)))
  )
})


test_that("update_arguments", {
  signal_parameters <- 5
  noise_parameters <- 5
  opts <- list(
    args = list(
      n_signal_parameters = signal_parameters,
      noise_parameters = noise_parameters,
      predictor_type = "binary",
      binary_prevalence = 0.1,
      beta_signal = 0.1
    )
  )
  f <- update_arguments(generate_continuous_data, opts)

  expect_type(f, "closure")
  data <- f(100)

  expect_equal(nrow(data), 100)
  expect_equal(ncol(data), 11)
})


test_that("default_data_generators", {
  signal_parameters <- 5
  noise_parameters <- 5
  opts <- list(
    type = "continuous",
    args = list(
      n_signal_parameters = signal_parameters,
      noise_parameters = noise_parameters,
      predictor_type = "binary",
      binary_prevalence = 0.1,
      beta_signal = 0.1
    )
  )

  f <- default_data_generators(opts)

  expect_type(f, "closure")
  data <- f(100)

  expect_equal(nrow(data), 100)
  expect_equal(ncol(data), 11)
  expect_equal(attr(f, "outcome"), "continuous")
})

test_that("generate_predictors induces a common pairwise correlation", {
  set.seed(1)
  families <- c("normal", "uniform", "exponential", "lognormal", "t", "laplace")
  for (dist in families) {
    X <- generate_predictors(
      20000,
      n_signal_parameters = 20,
      noise_parameters = 0,
      correlation = 0.3,
      distribution = dist
    )
    expect_true(all(is.finite(X)))
    # Rank correlation of a Gaussian copula does not depend on the margins:
    # (6 / pi) * asin(rho / 2).
    off_diag <- stats::cor(X, method = "spearman")[upper.tri(diag(20))]
    expect_equal(mean(off_diag), (6 / pi) * asin(0.3 / 2), tolerance = 0.03)
    # Every pair, not just the average, should sit near the target.
    expect_lt(max(abs(off_diag - mean(off_diag))), 0.05)
  }

  X <- generate_predictors(20000, 20, 0, correlation = 0.3)
  off_diag <- stats::cor(X)[upper.tri(diag(20))]
  expect_equal(mean(off_diag), 0.3, tolerance = 0.02)
})

test_that("correlated predictors keep their marginal distributions", {
  set.seed(1)
  draw <- function(...) {
    generate_predictors(20000, 10, 0, correlation = 0.3, ...)
  }
  expect_equal(mean(draw(distribution = "exponential")), 1, tolerance = 0.03)
  expect_equal(mean(draw(distribution = "uniform")), 0.5, tolerance = 0.02)
  expect_equal(stats::sd(as.vector(draw())), 1, tolerance = 0.02)
})

test_that("correlated binary predictors match the Gaussian copula", {
  set.seed(1)
  rho <- 0.3
  # Phi coefficient implied by a latent correlation rho at prevalence p.
  phi_theory <- function(p) {
    c <- stats::qnorm(1 - p)
    p11 <- stats::integrate(
      function(z) stats::dnorm(z) * stats::pnorm((rho * z - c) / sqrt(1 - rho^2)),
      c,
      Inf
    )$value
    (p11 - p^2) / (p * (1 - p))
  }
  for (prev in c(0.1, 0.3, 0.5)) {
    X <- generate_predictors(
      30000,
      n_signal_parameters = 10,
      noise_parameters = 0,
      predictor_type = "binary",
      binary_prevalence = prev,
      correlation = rho
    )
    expect_setequal(unique(as.vector(X)), c(0, 1))
    expect_equal(mean(X), prev, tolerance = 0.03)
    off_diag <- stats::cor(X)[upper.tri(diag(10))]
    expect_equal(mean(off_diag), phi_theory(prev), tolerance = 0.03)
  }
})
