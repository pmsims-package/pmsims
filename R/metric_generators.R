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
predict_custom <- function(x, y, fit, model, type = "response") {
  if (model == "glm") {
    predict(fit, newdata = x, type = type)
  } else if (model == "lasso") {
    x <- as.matrix(x)
    predict(fit, newx = x, s = fit$lambda.1se, type = type)[, 1]
  } else if (model == "rf") {
    response <- predict(fit, x, type = type)$predictions[, 1]
    if (type == "response") {
      return(response)
    } else {
      # TODO: calibration for random forest
    }
  }
}

binary_auc_metric <- function(data, fit, model) {
  y <- data[, "y"]
  x <- data[, names(data) != "y"]
  #x = model.matrix(y~., data)[,-1]
  y_hat <- predict_custom(x, y, fit, model, type = "response")
  auc <- pROC::auc(y, as.numeric(y_hat), quiet = TRUE)
  return(auc[1])
}

binary_calib_slope <- function(data, fit, model) {
  # Computes calibration slope for logistic regression
  y <- data[, "y"]
  x <- data[, names(data) != "y"]
  #x = model.matrix(y~., data)[,-1]
  y_link <- predict_custom(x, y, fit, model, type = "link")
  slope <- try(
    glm(y ~ y_link, family = binomial()),
    silent = TRUE
  )
  if (class(slope)[1] == "try-error") {
    calib_slope <- NaN
  } else {
    calib_slope <- as.numeric(coef(slope)[2])
  }
  return(calib_slope)
}

binary_calib_itl <- function(data, fit, model) {
  # Calibration slope for logistic regression returns the absolute value of
  # calibration in the large as positve or negative is bad
  y <- data[, "y"]
  x <- data[, names(data) != "y"]
  y_link <- predict_custom(x, y, fit, model, type = "link")
  slope_itl <- try(
    glm(y ~ 1, offset = y_link, data = data, family = "binomial"),
    silent = TRUE
  )
  if (class(slope_itl)[1] == "try-error") {
    return(NaN)
  } else {
    return(abs(as.numeric(coef(slope_itl)[1])))
  }
}

binary_brier_score <- function(data, fit, model) {
  y <- data[, "y"]
  x <- data[, names(data) != "y"]
  y_hat <- predict_custom(x, y, fit, model, type = "response")
  bs <- mean((y - y_hat)**2)
  return(bs)
}

binary_brier_score_scaled <- function(data, fit, model) {
  # This works for models being glm(family = binomial) models
  # or those with predict(type = "response") giving probability outcomes
  y <- data[, "y"]
  x <- data[, names(data) != "y"]
  y_hat <- predict_custom(x, y, fit, model, type = "response")
  return(1 - mean((y - y_hat)**2) / mean((y - mean(y))**2))
  # Note: same as r2, or cor(y, y_hat, "pearson")**2
  # with y_hat being a probability of y=1 (not 1/0 prediction)
}

continuous_r2 <- function(data, fit, model) {
  # Formula for out of sample r-squared taken from this:
  # https://www.tandfonline.com/doi/full/10.1080/00031305.2023.2216252
  y <- data[, "y"]
  x <- data[, names(data) != "y"]
  n <- length(y)
  y_hat <- predict(fit, x, type = "response")
  mse <- sum((y_hat - y)^2) / n
  mst <- var(y) * (n + 1) / n
  r2 <- 1 - (mse / mst)
  return(r2)
}

continuous_calib_slope <- function(data, fit, model) {
  # Computes calibration slope for logistic regression
  y <- data[, "y"]
  x <- data[, names(data) != "y"]
  y_hat <- predict(fit, x, type = "response")
  slope <- try(lm(y ~ y_hat), silent = TRUE)
  if (class(slope)[1] == "try-error") {
    return(NaN)
  } else {
    return(as.numeric(coef(slope)[2]))
  }
}

continuous_calib_itl <- function(data, fit, model) {
  # Computes calibration slope for logistic regression
  y <- data[, "y"]
  x <- data[, names(data) != "y"]
  y_hat <- predict(fit, x, type = "response")
  slope <- try(lm(y ~ 1, offset = y_hat), silent = TRUE)
  if (class(slope)[1] == "try-error") {
    return(NaN)
  } else {
    return(as.numeric(coef(slope)[1]))
  }
}

survival_cindex <- function(data, fit, model) {
  # This works for models being the Cox models or those with predict(type =
  # "lp") giving some sort of a risk score, or linear predictor
  y_surv <- survival::Surv(data$time, data$event)
  x <- data[,
    names(data) != "time" & names(data) != "event" & names(data) != "id"
  ]
  y_hat <- predict(fit, x, type = "lp")
  cf <- try(survival::concordancefit(y_surv, -1 * y_hat), silent = TRUE)
  if (class(cf)[1] == "try-error") {
    cindex <- NaN
  } else {
    if (is.null(cf)) {
      cindex <- NaN
    } else {
      cindex <- cf$concordance
    }
  }
  return(cindex)
}

# 1) Cox-like calibration slope (uses linear predictor)
survival_calib_slope <- function(data, fit, model) {
  y_surv <- survival::Surv(data$time, data$event)
  x <- data[,
    names(data) != "time" & names(data) != "event" & names(data) != "id"
  ]
  y_hat <- survival:::predict.coxph(fit, x, type = "lp")
  cf <- try(coef(survival::coxph(y_surv ~ y_hat)), silent = TRUE)
  if (class(cf)[1] == "try-error" || is.null(cf)) {
    slope <- NaN
  } else {
    slope <- as.numeric(cf)
  }
  return(slope)
}

# 2) Model-free IPCW calibration slope
survival_calib_slope_free <- function(data, fit, model, eval_time = NULL) {
  # the data set should be ordered, order(time,-status) in order to get the
  # values IPCW.subjectTimes in the right order
  data <- data[base::order(data$time), ]
  eval_time = NULL

  # data must have time, event, and predictors
  y_surv <- survival::Surv(data$time, data$event)
  x <- data[,
    names(data) != "time" & names(data) != "event" & names(data) != "id"
  ]

  # Get predicted survival (higher = higher risk)
  pred_surv <- survival:::predict.coxph(fit, data, type = "survival")

  # Get predicted model free yhat from logit: y_hat = log(S(t)/1-S(t))
  y_hat <- stats::qlogis(pred_surv)

  # Choose evaluation time if not given: last follow-up time
  if (is.null(eval_time)) {
    eval_time <- max(data$time[data$event == 1]) * 0.9999
  }

  # Compute IPCW weights for censoring at eval_time
  ipcw_obj <- try(
    pec::ipcw(
      Surv(time, event) ~ 1,
      data = data,
      method = "marginal", # for Kaplan-meier
      # times = sort(unique(data$time)),
      times = eval_time,
      subjectTimes = data$time
    ),
    silent = TRUE
  )

  if (class(ipcw_obj)[1] == "try-error" || is.null(ipcw_obj)) {
    slope <- NaN
  } else {
    # Observed binary outcome: event before eval_time
    y_obs <- as.numeric(data$time <= eval_time & data$event == 1)

    # IPCW weights
    w <- ipcw_obj$IPCW.subjectTimes

    # Weighted logistic regression of observed on predicted LP
    fit_slope <- try(suppressWarnings(glm(
      y_obs ~ y_hat,
      weights = w,
      family = binomial
    )))

    if (class(fit_slope)[1] == "try-error" || is.null(fit_slope)) {
      slope <- NaN
    } else {
      slope <- as.numeric(coef(fit_slope)[2])
    }
  }

  return(slope)
}

survival_auc <- function(data, fit, model) {
  # For models being the Cox models or those with predict( type="lp") giving
  # some sort of a risk score, or linear predictor AUC is time-dependent for
  # survival outcomes, this function computes AUC for the latest event time
  # available in the data
  y_surv <- survival::Surv(data$time, data$event)
  x <- data[, names(data) != "time" & names(data) != "event"]
  y_hat <- predict(fit, x, type = "lp")
  if (
    class(try(
      survival::concordancefit(
        y_surv,
        y_hat
      ),
      silent = TRUE
    ))[1] ==
      "try-error"
  ) {
    auc_survival <- NaN
  } else {
    t_max <- max(data[data$event == 1, "time"])
    auc_survival <- timeROC::timeROC(
      T = data$time,
      delta = data$event,
      marker = y_hat,
      times = t_max * 0.9999999,
      # 0.99x stabilizes if t_max is final study time
      cause = 1,
      weighting = "marginal"
    )$AUC[2]
  }
  return(as.numeric(auc_survival))
}
