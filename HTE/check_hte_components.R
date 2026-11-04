# Quick checks of the HTE generators, models and metrics. Run from the repository
# root; stops at the first failed data check and lists any failing combinations.
source("HTE/hte_data_generators.R")
source("HTE/hte_models.R")
source("HTE/hte_metrics.R")

set.seed(2026)

# Stand-in for a synthetic data model fitted to real data, with a rare category.
# Sampling from a large synthetic pool avoids refitting for every simulated dataset.
real <- hte_gen2(2000)
real$site <- sample(c("A", "B", "rare"), nrow(real), TRUE, prob = c(0.6, 0.39, 0.01))
pool <- synthpop::syn(real, k = 50000, print.flag = FALSE)$syn
pool$site <- as.character(pool$site)
synthetic_gen <- function(n) pool[sample(nrow(pool), n, replace = TRUE), ]

generators <- list(
  hte_gen1 = hte_gen1,
  hte_gen2 = hte_gen2,
  hte_gen3 = make_hte_gen3_sun_scen4(benchtm::generate_X_syn(30000)),
  synthetic = synthetic_gen
)
models <- list(
  ols = fit_linear_regression,
  lasso = fit_penalized_regression,
  forest = fit_causal_forest
)
metrics <- list(
  evalhte = metric_test_heterogeneity_evalhte,
  blp = metric_calibration_slope_blp,
  qini = metric_qini,
  autoc = metric_autoc,
  calib_grouped = metric_calibration_slope_grouped,
  calib_large = metric_calibration_in_the_large,
  calib_error = metric_calibration_error_debiased,
  test_blp = metric_test_blp,
  test_gates = metric_test_gates,
  test_qini = metric_test_rate_qini,
  test_autoc = metric_test_rate_autoc
)
# Tests tied to one model: fitted_model is the only input they use
model_specific <- list(
  ols = list(test_lr = metric_lr_test_ols),
  forest = list(test_forest = metric_test_forest_calibration)
)

# Data: layout and randomisation
for (g in names(generators)) {
  check_hte_generator(generators[[g]])
  d <- generators[[g]](20000)
  w <- d[[2]]
  balance <- vapply(d[-(1:2)], function(v) cor(as.numeric(factor(v)), w), 0)
  cat(sprintf("%s: %.1f%% treated, largest covariate correlation %.3f\n",
              g, 100 * mean(w), max(abs(balance))))
  stopifnot(abs(mean(w) - 0.5) < 0.03, all(abs(balance) < 0.05))
}

# A category missing from the training data must not break prediction
train <- synthetic_gen(1000)
train <- train[train$site != "rare", ]
test <- synthetic_gen(1000)
stopifnot(any(test$site == "rare"))
for (m in names(models)) stopifnot(length(models[[m]](train)$predict(test)) == nrow(test))

# Every combination returns one finite number, without using value_on_error
failed <- character(0)
for (g in names(generators)) {
  train <- generators[[g]](1000)
  test <- generators[[g]](3000)
  for (m in names(models)) {
    fit <- models[[m]](train)
    to_run <- c(metrics, model_specific[[m]])
    for (k in names(to_run)) {
      value <- tryCatch(to_run[[k]](test, fit, m), error = function(e) conditionMessage(e))
      if (!is.numeric(value) || length(value) != 1 || !is.finite(value)) {
        failed <- c(failed, sprintf("%s / %s / %s: %s", g, m, k, format(value)))
      }
    }
  }
}
if (length(failed) > 0) stop("Failed:\n", paste(failed, collapse = "\n"))
cat("All combinations returned a number\n")

# OLS on a correctly specified generator is calibrated in a large sample
fit <- fit_linear_regression(hte_gen1(100000))
slope_error <- metric_calibration_slope_blp(hte_gen1(30000), fit, "")
cat(sprintf("Large n: squared calibration slope error %.4f\n", -slope_error))
stopifnot(slope_error > -0.04)
