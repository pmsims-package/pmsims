
metric_lr_test_ols <- function(test_data, fitted_model, model_name = "") {
  lm_fit <- if (inherits(fitted_model, "hte_linear")) fitted_model$model else fitted_model
  if (!inherits(lm_fit, "lm") || inherits(lm_fit, "glm")) {
    stop("`fitted_model` must be the output of fit_linear_regression() or an lm() fit.")
  }
  
  tt <- stats::terms(lm_fit)
  term_order <- attr(tt, "order")
  if (!any(term_order > 1)) stop("The model has no interaction terms.")
  
  # Keep the intercept (assign = 0) and all main-effect columns
  X <- stats::model.matrix(lm_fit)
  keep <- attr(X, "assign") %in% c(0, which(term_order == 1))
  X0 <- X[, keep, drop = FALSE]
  
  y <- stats::model.response(stats::model.frame(lm_fit))
  w <- lm_fit$weights
  
  reduced <- if (is.null(w)) stats::lm.fit(X0, y) else stats::lm.wfit(X0, y, w)
  
  rss <- function(r) if (is.null(w)) sum(r^2) else sum(w * r^2)
  n <- if (is.null(w)) length(y) else sum(w != 0)
  
  lr <- n * log(rss(reduced$residuals) / rss(lm_fit$residuals))
  df <- lm_fit$rank - reduced$rank
  
  p <- stats::pchisq(lr, df, lower.tail = FALSE)
  ifelse(p<0.05,1,0)
}
attr(metric_lr_test_ols, "value_on_error") <- 0

# 1. GRF Heterogeneity Test (Robust matrix extraction)
metric_test_heterogeneity_grf <- function(test_data, fitted_model, model_name) {
  y <- test_data[[1]]
  w <- test_data[[2]]
  tau_hat <- fitted_model$predict(test_data)
  
  eval_forest <- grf::causal_forest(
    X = matrix(tau_hat, ncol = 1),
    Y = y,
    W = w,
    W.hat = mean(w),
    num.trees = 500
  )
  
  calib_test <- grf::test_calibration(eval_forest)
  
  # Row 2 (or 'differential.prediction') contains the slope/test of heterogeneity
  row_idx <- if ("differential.prediction" %in% rownames(calib_test)) "differential.prediction" else min(2, nrow(calib_test))
  
  # Find p-value column position regardless of column naming ("p.value", "p-value", or col 4)
  col_idx <- grep("p.*val", colnames(calib_test), ignore.case = TRUE)
  if (length(col_idx) == 0) col_idx <- 4
  
  p_val <- calib_test[row_idx, col_idx]
  sig = ifelse(p_val < 0.05, 1, 0)
  return(as.numeric(sig))
}
attr(metric_test_heterogeneity_grf, "value_on_error") <- 0


# 2. evalHTE Subgroup Test (Safe quantile ANOVA omnibus test)
metric_test_heterogeneity_evalhte <- function(test_data, fitted_model, model_name,
                                              num_groups = 10, alpha = 0.05) {
  y <- test_data[[1]]
  w <- as.numeric(as.character(test_data[[2]]))
  tau_hat <- as.numeric(fitted_model$predict(test_data))
  
  # A constant prediction (e.g. a one-node tree) can't sort anyone into groups
  if (length(unique(tau_hat)) < 2) return(0)
  
  res <- evalHTE::het.test(D = w, tau = tau_hat, Y = y, ngates = num_groups)
  as.numeric(res$pval < alpha)
}
attr(metric_test_heterogeneity_evalhte, "value_on_error") <- 0

# 3. Calibration Slope for Benefit
metric_calibration_for_benefit_grf <- function(test_data, fitted_model, model_name) {
  y <- test_data[[1]]
  w <- test_data[[2]]
  tau_hat <- fitted_model$predict(test_data)
  
  eval_forest <- grf::causal_forest(
    X = matrix(tau_hat, ncol = 1),
    Y = y,
    W = w,
    W.hat = mean(w),
    num.trees = 500
  )
  
  blp <- grf::best_linear_projection(eval_forest, A = matrix(tau_hat, ncol = 1))
  
  # Row 1 corresponds to predictor matrix A
  col_idx <- grep("estimate", colnames(blp), ignore.case = TRUE)
  if (length(col_idx) == 0) col_idx <- 1
  
  slope <- blp[1, col_idx]
  return(as.numeric(slope))
}
attr(metric_calibration_for_benefit_grf, "value_on_error") <- 0.5


metric_calibration_slope_blp <- function(test_data, fitted_model, model_name) {
  test_df <- as.data.frame(test_data)
  y <- test_df[[1]]
  w <- as.numeric(as.character(test_df[[2]]))
  tau_hat <- as.numeric(fitted_model$predict(test_df))
  
  if (stats::var(tau_hat) < 1e-12) {
    stop("Predicted benefit is constant, so the calibration slope is undefined.")
  }
  
  w_c <- w - mean(w)
  s_c <- tau_hat - mean(tau_hat)
  
  # Covariates adjust for prognosis: this reduces noise but isn't needed for validity
  X_df <- test_df[, 3:ncol(test_df), drop = FALSE]
  X_df[] <- lapply(X_df, function(v) if (is.character(v)) factor(v) else v)
  X_df <- X_df[, vapply(X_df, function(v) length(unique(v)) > 1, logical(1)), drop = FALSE]
  X <- stats::model.matrix(~ ., data = X_df)
  
  design <- cbind(X, w_c = w_c, calib = w_c * s_c)
  slope <- stats::lm.fit(design, y)$coefficients[["calib"]]
  
  if (is.na(slope)) stop("Calibration slope could not be estimated.")
  -(1 - slope)^2
}
attr(metric_calibration_slope_blp, "value_on_error") <- -1

# =============================================================================
# Ranking metrics: work on observed randomised data
# =============================================================================

# AIPW scores from a linear outcome model with treatment interactions, fitted on
# the test data. Treatment probability is the observed share, so this assumes
# randomisation.
aipw_scores <- function(test_data) {
  test_df <- as.data.frame(test_data)
  y <- test_df[[1]]
  w <- as.numeric(as.character(test_df[[2]]))
  
  X_df <- test_df[, 3:ncol(test_df), drop = FALSE]
  X_df <- X_df[, vapply(X_df, function(v) length(unique(v)) > 1, logical(1)), drop = FALSE]
  X <- stats::model.matrix(~ ., data = X_df)
  X_main <- X[, -1, drop = FALSE]
  
  beta <- stats::lm.fit(cbind(X, w, w * X_main), y)$coefficients
  beta[is.na(beta)] <- 0
  mu_0 <- as.vector(cbind(X, 0, 0 * X_main) %*% beta)
  mu_1 <- as.vector(cbind(X, 1, X_main) %*% beta)
  
  p <- mean(w)
  mu_1 - mu_0 + w * (y - mu_1) / p - (1 - w) * (y - mu_0) / (1 - p)
}

# Point estimate only: R = 2 is the fewest bootstrap reps grf allows, and the SE
# isn't used. Depends on the amount of heterogeneity, so check its large-sample
# value before setting a target.
rate_estimate <- function(test_data, fitted_model, target) {
  tau_hat <- as.numeric(fitted_model$predict(test_data))
  if (stats::var(tau_hat) < 1e-12) return(0) # a constant prediction ranks nobody
  grf::rank_average_treatment_effect.fit(
    aipw_scores(test_data), tau_hat, target = target, R = 2
  )$estimate
}

metric_qini <- function(test_data, fitted_model, model_name) {
  rate_estimate(test_data, fitted_model, "QINI")
}
attr(metric_qini, "value_on_error") <- 0

metric_autoc <- function(test_data, fitted_model, model_name) {
  rate_estimate(test_data, fitted_model, "AUTOC")
}
attr(metric_autoc, "value_on_error") <- 0

# =============================================================================
# Calibration for benefit: work on observed randomised data. Observed benefit in
# each group is the mean AIPW score, which is the arm difference adjusted for
# covariates. See also metric_calibration_slope_blp above, which needs no groups.
# Slopes and offsets are returned as -(error)^2 so higher = better on both sides
# of the ideal value: a target of -0.01 means within 0.1.
# =============================================================================

benefit_groups <- function(tau_hat, n_groups) {
  cut(rank(tau_hat, ties.method = "first"), n_groups, labels = FALSE)
}

# Slope of observed on predicted benefit across groups of predicted benefit
metric_calibration_slope_grouped <- function(test_data, fitted_model, model_name,
                                             n_groups = 5) {
  tau_hat <- as.numeric(fitted_model$predict(test_data))
  if (stats::var(tau_hat) < 1e-12) {
    stop("Predicted benefit is constant, so the calibration slope is undefined.")
  }
  group <- benefit_groups(tau_hat, n_groups)
  observed <- tapply(aipw_scores(test_data), group, mean)
  predicted <- tapply(tau_hat, group, mean)
  slope <- stats::coef(stats::lm(observed ~ predicted))[["predicted"]]
  -(1 - slope)^2
}
attr(metric_calibration_slope_grouped, "value_on_error") <- -1

# Calibration in the large: mean predicted benefit against the estimated ATE
metric_calibration_in_the_large <- function(test_data, fitted_model, model_name) {
  tau_hat <- as.numeric(fitted_model$predict(test_data))
  -(mean(tau_hat) - mean(aipw_scores(test_data)))^2
}
attr(metric_calibration_in_the_large, "value_on_error") <- -1

# Root mean squared gap between predicted and observed benefit across groups,
# negated. Subtracts each group's sampling variance: without this the error
# stays well above 0 at any training n, because of noise in the test set. Same
# idea as ECETH (Xu & Yadlowsky), not their exact estimator.
metric_calibration_error_debiased <- function(test_data, fitted_model, model_name,
                                              n_groups = 10) {
  tau_hat <- as.numeric(fitted_model$predict(test_data))
  gap <- aipw_scores(test_data) - tau_hat
  group <- benefit_groups(tau_hat, n_groups)
  sq_error <- tapply(gap, group, function(x) mean(x)^2 - stats::var(x) / length(x))
  -sqrt(max(mean(sq_error), 0))
}
attr(metric_calibration_error_debiased, "value_on_error") <- -10

# =============================================================================
# Tests for heterogeneity: each returns 1 if significant, so use the mean
# criterion and read the target as power. All are one-sided, as is
# grf::test_calibration: only a positive link with benefit counts.
#
# Trial data (the model's own training data; test_n doesn't matter):
#   metric_lr_test_ols, metric_test_forest_calibration
# Separate validation data (power depends on test_n):
#   metric_test_blp, metric_test_gates, metric_test_rate_*,
#   metric_test_heterogeneity_evalhte, metric_test_heterogeneity_grf
# =============================================================================

# Causal forest's own calibration test: is the differential forest prediction
# coefficient > 0? Uses out-of-bag predictions, so only works for causal forests.
metric_test_forest_calibration <- function(test_data, fitted_model, model_name,
                                           alpha = 0.05) {
  if (!inherits(fitted_model, "hte_causal_forest")) {
    stop("`fitted_model` must be the output of fit_causal_forest().")
  }
  calib <- grf::test_calibration(fitted_model$model)
  as.numeric(calib["differential.forest.prediction", "Pr(>t)"] < alpha)
}
attr(metric_test_forest_calibration, "value_on_error") <- 0

# BLP test: is the slope on the centred predicted benefit > 0?
metric_test_blp <- function(test_data, fitted_model, model_name, alpha = 0.05) {
  test_df <- as.data.frame(test_data)
  y <- test_df[[1]]
  w <- as.numeric(as.character(test_df[[2]]))
  tau_hat <- as.numeric(fitted_model$predict(test_df))
  if (stats::var(tau_hat) < 1e-12) return(0) # constant prediction: no test possible
  
  X_df <- test_df[, 3:ncol(test_df), drop = FALSE]
  X_df <- X_df[, vapply(X_df, function(v) length(unique(v)) > 1, logical(1)), drop = FALSE]
  w_c <- w - mean(w)
  calib <- w_c * (tau_hat - mean(tau_hat))
  fit <- stats::lm(y ~ ., data = cbind(X_df, w_c = w_c, calib = calib))
  
  t_value <- summary(fit)$coefficients["calib", "t value"]
  as.numeric(stats::pt(t_value, fit$df.residual, lower.tail = FALSE) < alpha)
}
attr(metric_test_blp, "value_on_error") <- 0

# GATES test: is observed benefit higher in the top group than the bottom group?
metric_test_gates <- function(test_data, fitted_model, model_name,
                              n_groups = 5, alpha = 0.05) {
  tau_hat <- as.numeric(fitted_model$predict(test_data))
  if (stats::var(tau_hat) < 1e-12) return(0) # constant prediction: no test possible
  group <- benefit_groups(tau_hat, n_groups)
  scores <- aipw_scores(test_data)
  test <- stats::t.test(scores[group == n_groups], scores[group == 1], alternative = "greater")
  as.numeric(test$p.value < alpha)
}
attr(metric_test_gates, "value_on_error") <- 0

# RATE test: is the RATE > 0? Bootstraps the SE, so slower than the estimate alone.
rate_test <- function(test_data, fitted_model, target, alpha, n_boot) {
  tau_hat <- as.numeric(fitted_model$predict(test_data))
  if (stats::var(tau_hat) < 1e-12) return(0) # constant prediction: no test possible
  rate <- grf::rank_average_treatment_effect.fit(
    aipw_scores(test_data), tau_hat, target = target, R = n_boot
  )
  as.numeric(rate$estimate / rate$std.err > stats::qnorm(1 - alpha))
}

metric_test_rate_qini <- function(test_data, fitted_model, model_name,
                                  alpha = 0.05, n_boot = 200) {
  rate_test(test_data, fitted_model, "QINI", alpha, n_boot)
}
attr(metric_test_rate_qini, "value_on_error") <- 0

metric_test_rate_autoc <- function(test_data, fitted_model, model_name,
                                   alpha = 0.05, n_boot = 200) {
  rate_test(test_data, fitted_model, "AUTOC", alpha, n_boot)
}
attr(metric_test_rate_autoc, "value_on_error") <- 0
