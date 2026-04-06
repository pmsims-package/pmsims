default_metric_generator <- function(metric, data_function) {
  outcome <- attr(data_function, "outcome")
  if (outcome == "binary") {
    if (metric == "auc") {
      metric_function <- binary_auc_metric
    } else if (metric == "calib_slope") {
      metric_function <- binary_calib_slope
    } else if (metric == "calib_itl") {
      metric_function <- binary_calib_itl
    } else if (metric == "brier_score") {
      metric_function <- binary_brier_score
    } else if (metric == "brier_score_scaled") {
      metric_function <- binary_brier_score_scaled
    } else {
      stop(paste(
        "Default metric",
        metric,
        "for",
        outcome,
        "outcomes does not exist."
      ))
    }
  }
  if (outcome == "survival") {
    if (metric == "cindex") {
      metric_function <- survival_cindex
    } else if (metric == "auc") {
      metric_function <- survival_auc
    } else if (metric == "calib_slope") {
      metric_function <- survival_calib_slope
    } else if (metric == "calib_slope_free") {
      metric_function <- survival_calib_slope_free
    } else if (metric == "IBS") {
      # Integrated Brier Score
      metric_function <- NULL # survival_ibs; TODO: Implement survival IBS
    } else {
      stop(paste(
        "Default metric",
        metric,
        "for",
        outcome,
        "outcomes does not exist."
      ))
    }
  }
  if (outcome == "continuous") {
    if (metric == "r2") {
      metric_function <- continuous_r2
    } else if (metric == "calib_slope") {
      metric_function <- continuous_calib_slope
    } else if (metric == "calib_itl") {
      metric_function <- continuous_calib_itl
    } else {
      stop(paste(
        "Default metric",
        metric,
        "for",
        outcome,
        "outcomes does not exist."
      ))
    }
  }
  attr(metric_function, "metric") <- metric
  return(metric_function)
}

#' @keywords internal
#' @export
predict_custom <- function(x, y = NULL, fit, model, type = "response") {
  # x: data.frame or matrix of predictors (no outcome column)
  # y: optional (not used here, kept for API compatibility)
  # fit: fitted model object
  # model: string identifying model type: "lm", "glm", "lasso", "rf", "xgboost", "coxph" etc.
  # type: "response", "link", "lp", "survival" (if supported)
  # return: numeric vector (or matrix for survival probabilities when appropriate)
  
  # Ensure x is data.frame or matrix for predict functions
  if (is.data.frame(x)) {
    x_df <- x
    x_mat <- as.matrix(x)
  } else {
    x_df <- as.data.frame(x)
    x_mat <- as.matrix(x)
  }
  
  # GLM (base R)
  if (model %in% c("lm","glm")) {
    return(stats::predict(fit, newdata = x_df, type = type))
  }
  
  # LASSO / Ridge (glmnet::cv.glmnet)
  # Both models share the same glmnet predict interface; only alpha differs at fit time.
  if (model %in% c("lasso", "ridge")) {
    if (!("glmnet" %in% rownames(utils::installed.packages()))) {
      warning("glmnet not installed; predict for ", model, " will fail.")
    }
    s_val <- "lambda.min"
    glmnet_type <- switch(type,
                          response = "response",
                          link     = "link",
                          lp       = "link",
                          stop("Type '", type, "' not supported for ", model, "."))
    preds <- as.numeric(predict(fit, newx = x_mat, s = s_val, type = glmnet_type))
    # For binary family: type="response" returns probabilities; type="link" returns log-odds.
    # For Cox family: type="link" returns the linear predictor (log-hazard ratio).
    return(preds)
  }
  
  # Random forest via ranger
  if (model == "rf" || model == "ranger") {
    # Expect fit is a ranger object
    if (!inherits(fit, "ranger")) {
      # try calling base predict if it's not a ranger object
      pr <- try(stats::predict(fit, newdata = x_df), silent = TRUE)
      if (!inherits(pr, "try-error")) return(pr)
      stop("rf: model object not of class 'ranger' and generic predict failed.")
    }
    
    ncores <- parallel::detectCores(logical = FALSE)
    nthreads <- ncores - 2
    
    pr <- predict(fit, data = x_df, num.threads = nthreads)
    preds <- pr$predictions
    
    # Classification (probabilities) => matrix with columns per class
    if (is.matrix(preds) && ncol(preds) >= 2) {
      # assume second column corresponds to "1" (if factor levels present, check)
      # If type = "response", return probability of positive class (second column)
      if (type == "response") {
        return(as.numeric(preds[, ncol(preds)]))
      }
      # If type = "link", return logit of probability
      if (type == "link") {
        p <- as.numeric(preds[, ncol(preds)])
        # avoid division by zero
        p <- pmin(pmax(p, .Machine$double.eps), 1 - .Machine$double.eps)
        return(stats::qlogis(p))
      }
    }
    
    # Regression or single numeric prediction
    if (is.numeric(preds) && is.vector(preds)) {
      return(as.numeric(preds))
    }
    
    # Survival: ranger returns a matrix of survival probabilities by timepoint
    if (is.matrix(pr$survival) && inherits(fit, "ranger") && fit$treetype == "Survival") {
      # If user asks for survival probabilities, return the survival matrix
      if (type == "survival") {
        
        times <- pr$unique.death.times
        surv_matrix <- pr$survival
        
        return(surv_matrix)
      }
      # For linear predictor / risk score: use log of the integrated cumulative
      # hazard across all event times, i.e. lp = log(H_inf).  This is a
      # monotone transformation of the cumulative hazard and serves as a
      # well-defined rank-preserving risk score for discrimination metrics and
      # Cox-recalibration (survival_calib_slope).
      if (type == "lp") {
        return(log(rowSums(pr$chf) + .Machine$double.eps))
      }
    }
    
    stop("rf (ranger) prediction type not supported or unknown prediction structure.")
  }
  
  # xgboost
  if (model == "xgboost" || inherits(fit, "xgb.Booster")) {
    if (!("xgboost" %in% rownames(utils::installed.packages()))) {
      warning("xgboost not installed; predict for xgboost will fail.")
    }
    # xgboost predict expects a matrix or xgb.DMatrix
    dmat <- xgboost::xgb.DMatrix(data = x_mat)
    preds <- stats::predict(fit, dmat)
    
    # For binary: preds are probabilities (objective = binary:logistic)
    if (type == "response") {
      return(as.numeric(preds))
    }
    # For link: return logit
    if (type == "link") {
      p <- pmin(pmax(as.numeric(preds), .Machine$double.eps), 1 - .Machine$double.eps)
      return(stats::qlogis(p))
    }
    # For linear predictor / risk score (survival objective gives risk)
    if (type == "lp") {
      return(as.numeric(preds))
    }
    # For survival probabilities, not directly available from xgboost cox objective
    if (type == "survival") {
      stop("xgboost: direct survival probability matrix is not available from xgboost predictions. Consider using type = 'lp' and mapping to survival via a baseline if needed.")
    }
    
    stop("xgboost: unsupported 'type' requested.")
  }
  
  # Cox models or other types that might use survival:::predict.coxph
  if (model == "coxph") {
    if (type %in% c("lp", "link")) {
      return(survival:::predict.coxph(fit, x_df, type = "lp"))
    } else if (type == "survival") {
      return(survival:::predict.coxph(fit, x_df, type = "survival"))
    } else {
      stop("coxph predict_custom: only 'lp' or 'survival' supported.")
    }
  }
  
  stop("predict_custom: unknown model type '", model, "'.")
}

#### Binary metric functions (unchanged except using predict_custom where used) ####

binary_auc_metric <- function(data, fit, model) {
  y <- data[, "y"]
  x <- data[, names(data) != "y", drop = FALSE]
  y_hat <- predict_custom(x, y, fit, model, type = "response")
  auc <- pROC::auc(y, as.numeric(y_hat), quiet = TRUE)
  return(auc[1])
}

binary_calib_slope <- function(data, fit, model) {
  y <- data[, "y"]
  x <- data[, names(data) != "y", drop = FALSE]
  
  # Parametric models (glm, lasso, ridge) expose a well-defined log-odds linear
  # predictor, so the standard calibration-slope regression (Austin & Steyerberg
  # 2014) is appropriate directly on the link scale.
  #
  # Non-parametric / tree-based models (rf, xgboost) do not have a native
  # log-odds scale.  The standard recalibration approach (Platt scaling) fits a
  # logistic regression of the observed outcome on the logit-transformed
  # predicted probability.  This is mathematically equivalent for well-calibrated
  # parametric models but avoids treating an rf/xgboost output as a structural
  # log-odds.
  if (model %in% c("rf", "ranger", "xgboost")) {
    p_hat <- predict_custom(x, y, fit, model, type = "response")
    p_hat <- pmin(pmax(as.numeric(p_hat), .Machine$double.eps),
                  1 - .Machine$double.eps)
    logit_p <- stats::qlogis(p_hat)
    recal <- try(
      stats::glm(y ~ logit_p, family = stats::binomial()),
      silent = TRUE
    )
  } else {
    y_link <- predict_custom(x, y, fit, model, type = "link")
    recal <- try(
      stats::glm(y ~ y_link, family = stats::binomial()),
      silent = TRUE
    )
  }
  
  if (inherits(recal, "try-error")) {
    return(NaN)
  } else {
    return(as.numeric(stats::coef(recal)[2]))
  }
}

binary_calib_itl <- function(data, fit, model) {
  y <- data[, "y"]
  x <- data[, names(data) != "y", drop = FALSE]
  
  # For non-parametric / tree-based models apply Platt-scaling recalibration:
  # fix the slope at 1 by using the logit of the predicted probability as an
  # offset, then estimate only the intercept.  The intercept quantifies
  # calibration-in-the-large (mean predicted vs. observed event rate).
  if (model %in% c("rf", "ranger", "xgboost")) {
    p_hat <- predict_custom(x, y, fit, model, type = "response")
    p_hat <- pmin(pmax(as.numeric(p_hat), .Machine$double.eps),
                  1 - .Machine$double.eps)
    logit_p <- stats::qlogis(p_hat)
    recal_itl <- try(
      stats::glm(y ~ 1, offset = logit_p, family = stats::binomial()),
      silent = TRUE
    )
  } else {
    y_link <- predict_custom(x, y, fit, model, type = "link")
    recal_itl <- try(
      stats::glm(y ~ 1, offset = y_link, data = data, family = stats::binomial()),
      silent = TRUE
    )
  }
  
  if (inherits(recal_itl, "try-error")) {
    return(NaN)
  } else {
    return(abs(as.numeric(stats::coef(recal_itl)[1])))
  }
}

binary_brier_score <- function(data, fit, model) {
  y <- data[, "y"]
  x <- data[, names(data) != "y", drop = FALSE]
  y_hat <- predict_custom(x, y, fit, model, type = "response")
  bs <- mean((y - y_hat) ^ 2)
  return(bs)
}

binary_brier_score_scaled <- function(data, fit, model) {
  y <- data[, "y"]
  x <- data[, names(data) != "y", drop = FALSE]
  y_hat <- predict_custom(x, y, fit, model, type = "response")
  return(1 - mean((y - y_hat) ^ 2) / mean((y - mean(y)) ^ 2))
}

#### Continuous metrics: use predict_custom so lasso/xgboost/ranger work ####

continuous_r2 <- function(data, fit, model) {
  y <- data[, "y"]
  x <- data[, names(data) != "y", drop = FALSE]
  y_hat <- predict_custom(x, y, fit, model, type = "response")
  n <- length(y)
  mse <- sum((y_hat - y) ^ 2) / n
  mst <- stats::var(y) * (n + 1) / n
  r2 <- 1 - (mse / mst)
  return(r2)
}

# Calibration slope for continuous outcomes: OLS regression of observed y on
# predicted y_hat.  A slope of 1 indicates perfect mean calibration.
#
# NOTE — expected behaviour by model class:
#
#   lm / lasso / ridge:
#     Slope clusters near 1 at moderate n.  Regularised models may show
#     slopes slightly above 1 at very small n (shrinkage compresses y_hat).
#
#   Random Forest (rf):
#     Slopes are systematically > 1, even at large n.  This is correct and
#     expected: RF predictions are averages of terminal-node means, which
#     compresses the prediction range relative to y.  OLS compensates by
#     estimating a slope > 1.  This is a structural property of averaging
#     estimators (the "regression-to-the-mean" of bagged trees), not a bug
#     in the metric.  The slope worsens at small n (more shrinkage) and
#     attenuates as n grows.  Post-hoc recalibration would change what is
#     being measured; the metric is reporting truthfully.
#
#   XGBoost:
#     Without round-count regularisation, XGBoost over-fits the training
#     scale at small n, producing over-dispersed predictions and slopes << 1.
#     The model generator now uses xgb.cv early stopping to select nrounds,
#     which substantially stabilises the slope across sample sizes.
continuous_calib_slope <- function(data, fit, model) {
  y <- data[, "y"]
  x <- data[, names(data) != "y", drop = FALSE]
  y_hat <- predict_custom(x, y, fit, model, type = "response")
  slope <- try(stats::lm(y ~ y_hat), silent = TRUE)
  if (inherits(slope, "try-error")) {
    return(NaN)
  } else {
    return(as.numeric(stats::coef(slope)[2]))
  }
}

continuous_calib_itl <- function(data, fit, model) {
  y <- data[, "y"]
  x <- data[, names(data) != "y", drop = FALSE]
  y_hat <- predict_custom(x, y, fit, model, type = "response")
  slope <- try(stats::lm(y ~ 1, offset = y_hat), silent = TRUE)
  if (inherits(slope, "try-error")) {
    return(NaN)
  } else {
    return(as.numeric(stats::coef(slope)[1]))
  }
}

#### Survival metrics: use predict_custom(type="lp") for linear predictors where possible ####

survival_cindex <- function(data, fit, model) {
  y_surv <- survival::Surv(data$time, data$event)
  
  x <- data[, !(names(data) %in% c("time", "event", "id")), drop = FALSE]
  
  # request linear predictor / risk score
  y_hat <- try(predict_custom(x, NULL, fit, model, type = "lp"), silent = TRUE)
  if (inherits(y_hat, "try-error")) {
    return(NaN)
  }
  cf <- try(survival::concordancefit(y_surv, -1 * as.numeric(y_hat)), silent = TRUE)
  if (inherits(cf, "try-error") || is.null(cf)) {
    return(NaN)
  }
  return(cf$concordance)
}

# Cox-recalibration slope
# For all supported models we fit a univariate Cox regression of the observed
# survival outcome on the model-derived risk score (linear predictor or log-CHF
# for rf).  A slope of 1 indicates perfect Cox calibration.
#
# For rf (ranger):  the LP is log(sum CHF), a monotone risk score — Cox
#   regression on it is a standard external calibration approach.
# For xgboost Cox:  the model already outputs a log-hazard ratio (LP); the
#   recalibration slope detects any systematic shrinkage or over-fitting.
# For lasso / ridge Cox:  same as xgboost above.
# For coxph:        classical calibration slope as in van Houwelingen (2000).
survival_calib_slope <- function(data, fit, model) {
  y_surv <- survival::Surv(data$time, data$event)
  x     <- data[, !(names(data) %in% c("time", "event", "id")), drop = FALSE]
  
  y_hat <- try(predict_custom(x, NULL, fit, model, type = "lp"), silent = TRUE)
  if (inherits(y_hat, "try-error") || all(is.na(y_hat))) {
    return(NaN)
  }
  y_hat <- as.numeric(y_hat)
  
  # Guard against degenerate predictions (zero variance => unidentifiable slope)
  if (stats::var(y_hat, na.rm = TRUE) < .Machine$double.eps) {
    warning("survival_calib_slope: predicted risk score has zero variance; returning NaN.")
    return(NaN)
  }
  
  cf <- try(stats::coef(survival::coxph(y_surv ~ y_hat)), silent = TRUE)
  if (inherits(cf, "try-error") || is.null(cf)) {
    return(NaN)
  }
  return(as.numeric(cf))
}

# Model-free IPCW calibration slope
survival_calib_slope_free <- function(data, fit, model, eval_time = NULL) {
  # This function currently prefers model types that can return survival probabilities.
  # For coxph and ranger (survival), we attempt to extract predicted survival probabilities.
  data <- data[base::order(data$time), ]
  eval_time = NULL
  
  # data must have time, event, and predictors
  y_surv <- survival::Surv(data$time, data$event)
  x <- data[, !(names(data) %in% c("time", "event", "id")), drop = FALSE]
  
  pred_surv <- NULL
  # Try to get survival probabilities via predict_custom(type = "survival")
  try({
    pred_surv <- predict_custom(x, NULL, fit, model, type = "survival")
  }, silent = TRUE)
  
  # If predict_custom did not yield survival probs, but can get lp, try to approximate
  if (is.null(pred_surv) || (is.atomic(pred_surv) && all(is.na(pred_surv)))) {
    # If cox-like linear predictor available, try to obtain predicted survival via baseline from a coxph fit
    if (model == "coxph") {
      pred_surv <- try(survival:::predict.coxph(fit, data, type = "survival"), silent = TRUE)
      if (inherits(pred_surv, "try-error")) pred_surv <- NULL
    } else if (model %in% c("rf", "ranger")) {
      # ranger survival returns matrix of survival probabilities by time index in predict()
      pr <- try(predict(fit, data = x), silent = TRUE)
      if (!inherits(pr, "try-error") && !is.null(pr$predictions) && is.matrix(pr$predictions)) {
        pred_surv <- pr$predictions
      }
    } else {
      # For lasso (glmnet with family='cox') or xgboost survival: we generally obtain lp not direct survival probs
      pred_surv <- NULL
    }
  }
  
  if (is.null(pred_surv)) {
    # can't compute model-free calibration if survival probabilities not available
    warning("survival_calib_slope_free: predicted survival probabilities not available for model '", model, "'. Returning NaN.")
    return(NaN)
  }
  
  # If pred_surv is matrix: columns correspond to time grid. We choose eval_time
  if (is.matrix(pred_surv)) {
    # choose eval_time if not given: last event time
    if (is.null(eval_time)) {
      eval_time <- max(data$time[data$event == 1]) * 0.9999
    }
    # need an associated vector of times for the survival matrix columns - ranger ties columns to fit$unique.event.times
    # Attempt to obtain times from model object (only supported for ranger). Otherwise use last column.
    surv_times <- NULL
    if (inherits(fit, "ranger") && !is.null(fit$unique.death.times)) {
      surv_times <- fit$unique.death.times
    }
    if (!is.null(surv_times)) {
      # find closest column index
      idx <- which.min(abs(surv_times - eval_time))
    } else {
      # fallback to last column
      idx <- ncol(pred_surv)
    }
    pred_surv_at_time <- as.numeric(pred_surv[, idx])
  } else {
    # pred_surv not matrix: maybe a numeric vector of survival probabilities already at eval_time
    pred_surv_at_time <- as.numeric(pred_surv)
    if (is.null(eval_time)) eval_time <- max(data$time[data$event == 1]) * 0.9999
  }
  
  # Get predicted model free yhat from logit: y_hat = log(S(t)/1-S(t))
  # Bound probabilities to avoid Inf
  pred_surv_at_time <- pmin(pmax(pred_surv_at_time, .Machine$double.eps), 1 - .Machine$double.eps)
  y_hat <- stats::qlogis(pred_surv_at_time)
  
  # Observed binary outcome: event before eval_time
  y_obs <- as.numeric(data$time <= eval_time & data$event == 1)
  
  # Compute IPCW weights for censoring at eval_time
  ipcw_obj <- try(
    pec::ipcw(
      survival::Surv(time, event) ~ 1,
      data = data,
      method = "marginal", # for Kaplan-meier
      times = eval_time,
      subjectTimes = data$time
    ),
    silent = TRUE
  )
  
  if (inherits(ipcw_obj, "try-error") || is.null(ipcw_obj)) {
    warning("survival_calib_slope_free: ipcw computation failed.")
    return(NaN)
  }
  
  w <- ipcw_obj$IPCW.subjectTimes
  
  fit_slope <- try(suppressWarnings(stats::glm(
    y_obs ~ y_hat,
    weights = w,
    family = stats::binomial()
  )), silent = TRUE)
  
  if (inherits(fit_slope, "try-error") || is.null(fit_slope)) {
    return(NaN)
  } else {
    return(as.numeric(stats::coef(fit_slope)[2]))
  }
}

survival_auc <- function(data, fit, model) {
  y_surv <- survival::Surv(data$time, data$event)
  x <- data[, !(names(data) %in% c("time", "event", "id")), drop = FALSE]
  
  # get linear predictor / risk score where possible
  y_hat <- try(predict_custom(x, NULL, fit, model, type = "lp"), silent = TRUE)
  if (inherits(y_hat, "try-error") || is.null(y_hat)) {
    return(NaN)
  }
  
  # time-dependent AUC at last event time
  if (class(try(survival::concordancefit(y_surv, y_hat), silent = TRUE))[1] == "try-error") {
    auc_survival <- NaN
  } else {
    t_max <- max(data[data$event == 1, "time"])
    auc_survival <- timeROC::timeROC(
      T = data$time,
      delta = data$event,
      marker = as.numeric(y_hat),
      times = t_max * 0.9999999,
      cause = 1,
      weighting = "marginal"
    )$AUC[2]
  }
  return(as.numeric(auc_survival))
}