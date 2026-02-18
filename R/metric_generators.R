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

predict_custom <- function(x, y, fit, model, type = "response") {
  if (model == "glm") {
    stats::predict(fit, newdata = x, type = type)
  } else if (model == "lasso") {
    x <- as.matrix(x)
    stats::predict(fit, newx = x, s = fit$lambda.1se, type = type)[, 1]
  } else if (model == "rf") {
    response <- stats::predict(fit, x, type = type)$predictions[, 1]
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
  y_link <- predict_custom(x, y, fit, model, type = "link")
  slope <- try(
    stats::glm(y ~ y_link, family = stats::binomial()),
    silent = TRUE
  )
  if (inherits(slope, "try-error")) {
    calib_slope <- NaN
  } else {
    calib_slope <- as.numeric(stats::coef(slope)[2])
  }
  return(calib_slope)
}

binary_calib_itl <- function(data, fit, model) {
  y <- data[, "y"]
  x <- data[, names(data) != "y", drop = FALSE]
  y_link <- predict_custom(x, y, fit, model, type = "link")
  slope_itl <- try(
    stats::glm(y ~ 1, offset = y_link, data = data, family = stats::binomial()),
    silent = TRUE
  )
  if (inherits(slope_itl, "try-error")) {
    return(NaN)
  } else {
    return(abs(as.numeric(stats::coef(slope_itl)[1])))
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

# Cox-like calibration slope (uses linear predictor)
survival_calib_slope <- function(data, fit, model) {
  y_surv <- survival::Surv(data$time, data$event)
  x <- data[, !(names(data) %in% c("time", "event", "id")), drop = FALSE]
  y_hat <- try(predict_custom(x, NULL, fit, model, type = "lp"), silent = TRUE)
  if (inherits(y_hat, "try-error")) {
    return(NaN)
  }
  cf <- try(stats::coef(survival::coxph(y_surv ~ as.numeric(y_hat))), silent = TRUE)
  if (inherits(cf, "try-error") || is.null(cf)) {
    slope <- NaN
  } else {
    slope <- as.numeric(cf)
  }
  return(slope)
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
