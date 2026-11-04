
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
# Metrics using the true effects. Simulation only: these need the generator to
# set attr(data, "tau"). simulate_custom() swaps errors for value_on_error, so
# a missing tau shows up as a flat metric: run HTE/check_hte_components.R first.
# =============================================================================

true_effect <- function(test_data) {
  tau <- attr(test_data, "tau", exact = TRUE)
  if (is.null(tau)) stop("test_data has no \"tau\" attribute.")
  tau
}

# RMSE of the predicted effect (root PEHE), negated as pmsims needs higher = better
metric_rmse_tau <- function(test_data, fitted_model, model_name) {
  tau_hat <- as.numeric(fitted_model$predict(test_data))
  -sqrt(mean((true_effect(test_data) - tau_hat)^2))
}
attr(metric_rmse_tau, "value_on_error") <- -10

# Rank correlation with the true effects. Undefined if true effects are constant.
# Ties in the true effects cap this below 1.
metric_spearman_tau <- function(test_data, fitted_model, model_name) {
  tau <- true_effect(test_data)
  tau_hat <- as.numeric(fitted_model$predict(test_data))
  if (stats::var(tau) < 1e-12) stop("True effects are constant.")
  if (stats::var(tau_hat) < 1e-12) return(0) # a constant prediction ranks nobody
  stats::cor(tau, tau_hat, method = "spearman")
}
attr(metric_spearman_tau, "value_on_error") <- 0

# Regret of treating when predicted effect > threshold, compared with treating
# by the true effect. Negated, so 0 is best. Assumes a higher Y is better.
metric_regret <- function(test_data, fitted_model, model_name, threshold = 0) {
  net <- true_effect(test_data) - threshold
  tau_hat <- as.numeric(fitted_model$predict(test_data))
  -(mean(pmax(net, 0)) - mean(net * (tau_hat > threshold)))
}
attr(metric_regret, "value_on_error") <- -10

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
