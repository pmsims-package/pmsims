# Quick checks of the HTE generators, models and metrics. Run from the repository
# root; stops at the first failed data check and lists any failing combinations.
source("HTE/hte_data_generators.R")
source("HTE/hte_models.R")
source("HTE/hte_metrics.R")

set.seed(2026)
generators <- list(
  hte_gen1 = hte_gen1,
  hte_gen2 = hte_gen2,
  hte_gen3 = make_hte_gen3_sun_scen4(benchtm::generate_X_syn(30000))
)
models <- list(
  ols = fit_linear_regression,
  lasso = fit_penalized_regression,
  forest = fit_causal_forest
)
metrics <- list(
  evalhte = metric_test_heterogeneity_evalhte,
  blp = metric_calibration_slope_blp,
  rmse = metric_rmse_tau,
  spearman = metric_spearman_tau,
  regret = metric_regret,
  qini = metric_qini,
  autoc = metric_autoc
)

# Data: layout, randomisation, and true effects that match the data
for (g in names(generators)) {
  d <- generators[[g]](20000)
  tau <- attr(d, "tau")
  w <- d[[2]]
  stopifnot(
    names(d)[1] == "Y",
    setequal(unique(w), 0:1),
    length(tau) == nrow(d),
    abs(mean(w) - 0.5) < 0.02,
    all(abs(vapply(d[-(1:2)], function(v) cor(as.numeric(factor(v)), w), 0)) < 0.05)
  )
  ate <- mean(d[[1]][w == 1]) - mean(d[[1]][w == 0])
  cat(sprintf("%s: ATE %.2f, mean tau %.2f, SD tau %.2f\n", g, ate, mean(tau), sd(tau)))
  stopifnot(abs(ate - mean(tau)) < 0.15)
}

# Every combination returns one finite number, without using value_on_error
failed <- character(0)
for (g in names(generators)) {
  train <- generators[[g]](1000)
  test <- generators[[g]](3000)
  for (m in names(models)) {
    fit <- models[[m]](train)
    for (k in names(metrics)) {
      value <- tryCatch(metrics[[k]](test, fit, m), error = function(e) conditionMessage(e))
      if (!is.numeric(value) || length(value) != 1 || !is.finite(value)) {
        failed <- c(failed, sprintf("%s / %s / %s: %s", g, m, k, format(value)))
      }
    }
  }
}
if (length(failed) > 0) stop("Failed:\n", paste(failed, collapse = "\n"))
cat("All", length(generators) * length(models) * length(metrics), "combinations returned a number\n")

# OLS on a correctly specified generator: RMSE near 0 and rank near 1 in a large sample
fit <- fit_linear_regression(hte_gen1(100000))
test <- hte_gen1(5000)
cat(sprintf("Large n: RMSE %.3f, Spearman %.3f\n",
            -metric_rmse_tau(test, fit, ""), metric_spearman_tau(test, fit, "")))
stopifnot(metric_rmse_tau(test, fit, "") > -0.1, metric_spearman_tau(test, fit, "") > 0.98)
