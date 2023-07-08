default_metric_generator <- function(data_function, metric) {
  outcome <- attr(data_function, "outcome")
  if (outcome == "binary") {
    if (metric == "auc") {
      metric_function <- binary_auc_metric
    } else if (metric == "calib_slope") {
      metric_function <- binary_calib_slope
    } else if (metric == "brier_score") {
      metric_function <- binary_brier_score
    } else if (metric == "brier_score_scaled") {
      metric_function <- binary_brier_score_scaled
    } else {
      stop(paste(
        "Default metric", metric, "for", outcome,
        "outcomes does not exist."
      ))
    }
  }
  if (outcome == "survival") {
    if (metric == "cindex") {
      metric_function <- survival_cindex
    } else if (metric == "auc") {
      metric_function <- survival_auc
    } else if (metric == "IBS") { # integrated Brier Score
      metric_function <- NULL # survival_ibs; not ready, placeholder
    } else {
      stop(paste(
        "Default metric", metric, "for", outcome,
        "outcomes does not exist."
      ))
    }
  }
  if (outcome == "continuous") {
    if (metric == "r2") {
      metric_function <- continuous_r2
    } else if (metric == "something_else") { 
      metric_function <- NULL # placeholder
    } else {
      stop(paste(
        "Default metric", metric, "for", outcome,
        "outcomes does not exist."
      ))
    } 
  }
  attr(metric_function, "metric") = metric
  return(metric_function)
}

binary_auc_metric <- function(data, model) {
  y <- data[, "y"]
  x <- data[, names(data) != "y"]
  y_hat <- predict(model, x, type = "response")
  auc <- pROC::auc(y, as.numeric(y_hat), quiet = TRUE)
  return(auc[1])
}

binary_calib_slope <- function(data, model) {
  # computes calibration slope for logistic regression
  y <- data[, "y"]
  x <- data[, names(data) != "y"]
  y_link <- predict(model, x, type = "link")
  # checks if regression converges first
  if (class(try(glm(y ~ y_link, data = data, family = "binomial"),
    silent = TRUE
  ))[1] == "try-error") {
    calib_slope <- NaN
  } else {
    calib_model <- glm(y ~ y_link, data = data, family = "binomial")
    calib_slope <- as.numeric(coef(calib_model)[2])
  }
  return(calib_slope)
}

binary_brier_score <- function(data, model) {
  y <- data[, "y"]
  x <- data[, names(data) != "y"]
  y_hat <- predict(model, x, type = "response")
  bs <- mean((y - y_hat)**2)
  return(bs)
}

binary_brier_score_scaled <- function(data, model) {
  # this works for models being glm(family="binomial") models
  # or those with predict(type = "response") giving probability outcomes
  y <- data[, "y"]
  x <- data[, names(data) != "y"]
  y_hat <- predict(model, x, type = "response")
  bss <- 1 - mean((y - y_hat)**2) / mean((y - mean(y))**2)
  # note: same as r2, or cor(y, y_hat, "pearson")**2
  # with y_hat being a probability of y=1 (not 1/0 prediction)
  return(bss)
}

continuous_r2 <- function(data, model) {
  # this works for models being lm() models
  # or those with predict(type = "response") giving continuous y_hat
  y <- data[, "y"]
  x <- data[, names(data) != "y"]
  y_hat <- predict(model, x, type = "response")
  r2 <- cor(y, y_hat, method = "pearson")**2
  return(r2)
}

survival_cindex <- function(data, model) {
  # this works for models being the Cox models
  # or those with predict(type = "lp") giving some sort of a risk score,
  # or linear predictor
  y_surv <- survival::Surv(data$time, data$event)
  x <- data[, names(data) != "time" & names(data) != "event"]
  y_hat <- predict(model, x, type = "lp")

  cf <- try(concordancefit(y_surv, -1 * y_hat), silent = TRUE)
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

survival_auc <- function(data, model) {
  # this works for models being the Cox models
  # or those with predict( type="lp") giving some sort of a risk score,
  # or linear predictor
  # AUC is time-dependent for survival outcomes,
  # this function computes AUC for the latest event time available in the data

  y_surv <- survival::Surv(data$time, data$event)
  x <- data[, names(data) != "time" & names(data) != "event"]
  y_hat <- predict(model, x, type = "lp")

  if (class(try(survival::concordancefit(y_surv, y_hat), silent = TRUE))[1] == "try-error") {
    auc_survival <- NaN
  } else {
    t_max <- max(data[data$event == 1, "time"])
    auc_survival <- timeROC::timeROC(
      T = data$time,
      delta = data$event,
      marker = y_hat,
      times = t_max * 0.9999999,
      # 0.99... stabilizes in case t_max is final study time
      cause = 1,
      weighting = "marginal"
    )$AUC[2]
  }
  return(as.numeric(auc_survival))
}