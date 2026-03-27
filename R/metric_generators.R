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
  if (model %in% c("lm", "glm")) {
    return(stats::predict(fit, newdata = x_df, type = type))
  }

  # LASSO (glmnet::cv.glmnet)
  if (model == "lasso") {
    # Expect fit is cv.glmnet (or glmnet object) and x_mat is numeric matrix
    if (!("glmnet" %in% rownames(utils::installed.packages()))) {
      warning("glmnet not installed; predict for lasso will fail.")
    }
    #s_val <- if (!is.null(fit$lambda.1se)) fit$lambda.1se else if (!is.null(fit$lambda.min)) fit$lambda.min else NULL
    #if (is.null(s_val)) s_val <- NULL

    s_val = "lambda.min"

    # Choose glmnet type mapping
    glmnet_type <- switch(
      type,
      response = "response",
      link = "link",
      lp = "link",
      stop("Type '", type, "' not supported for lasso.")
    )
    preds <- as.numeric(predict(
      fit,
      newx = x_mat,
      s = s_val,
      type = glmnet_type
    ))
    # for binary response, glmnet::predict(..., type="response") returns probabilities
    # for cox (survival) family, glmnet::predict(..., type="link") returns linear predictor
    return(preds)
  }

  # Random forest via ranger
  if (model == "rf" || model == "ranger") {
    # Expect fit is a ranger object
    if (!inherits(fit, "ranger")) {
      # try calling base predict if it's not a ranger object
      pr <- try(stats::predict(fit, newdata = x_df), silent = TRUE)
      if (!inherits(pr, "try-error")) {
        return(pr)
      }
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
    if (
      is.matrix(pr$survival) &&
        inherits(fit, "ranger") &&
        fit$treetype == "Survival"
    ) {
      # If user asks for survival probabilities, return the survival matrix
      if (type == "survival") {
        times <- pr$unique.death.times
        surv_matrix <- pr$survival

        return(surv_matrix)
      }
      # For linear predictor / risk score, convert survival to linear predictor using
      # logit = (S(t) / 1- S(t)) or
      # lp = log(H(t)) where H(t) = sum(h(t))
      if (type == "lp") {
        #times <- pr$unique.death.times
        #surv_matrix <- pr$survival

        # obtain the survival at an ith observation unique time
        #surv_vec <- sapply(1:nrow(x_df), function(i) {
        #   t_i <- x_df$time[i]
        #   idx <- which.min(abs(times - t_i))
        #  surv_matrix[i, idx]
        # })

        # get lp from survival vector + adjust S(t) = 1 by subtracting 1e-8
        # surv_vec_to_lp <- qlogis(surv_vec - 1e-8)
        # chf n x times matrix: summing over times gives cumulative hazard
        return(log(rowSums(pr$chf)))
      }
    }

    stop(
      "rf (ranger) prediction type not supported or unknown prediction structure."
    )
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
      p <- pmin(
        pmax(as.numeric(preds), .Machine$double.eps),
        1 - .Machine$double.eps
      )
      return(stats::qlogis(p))
    }
    # For linear predictor / risk score (survival objective gives risk)
    if (type == "lp") {
      return(as.numeric(preds))
    }
    # For survival probabilities, not directly available from xgboost cox objective
    if (type == "survival") {
      stop(
        "xgboost: direct survival probability matrix is not available from xgboost predictions. Consider using type = 'lp' and mapping to survival via a baseline if needed."
      )
    }

    stop("xgboost: unsupported 'type' requested.")
  }

  # Cox models or other types that might use survival:::predict.coxph
  if (model == "coxph") {
    fit_for_prediction <- fit
    formula_env <- new.env(
      parent = environment(stats::formula(fit_for_prediction))
    )
    formula_env$Surv <- survival::Surv
    environment(fit_for_prediction$formula) <- formula_env
    attr(fit_for_prediction$terms, ".Environment") <- formula_env

    if (type %in% c("lp", "link")) {
      return(survival:::predict.coxph(fit_for_prediction, x_df, type = "lp"))
    } else if (type == "survival") {
      return(survival:::predict.coxph(
        fit_for_prediction,
        x_df,
        type = "survival"
      ))
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
  bs <- mean((y - y_hat)^2)
  return(bs)
}

binary_brier_score_scaled <- function(data, fit, model) {
  y <- data[, "y"]
  x <- data[, names(data) != "y", drop = FALSE]
  y_hat <- predict_custom(x, y, fit, model, type = "response")
  return(1 - mean((y - y_hat)^2) / mean((y - mean(y))^2))
}

#### Continuous metrics: use predict_custom so lasso/xgboost/ranger work ####

continuous_r2 <- function(data, fit, model) {
  y <- data[, "y"]
  x <- data[, names(data) != "y", drop = FALSE]
  y_hat <- predict_custom(x, y, fit, model, type = "response")
  n <- length(y)
  mse <- sum((y_hat - y)^2) / n
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
  cf <- try(
    survival::concordancefit(y_surv, -1 * as.numeric(y_hat)),
    silent = TRUE
  )
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
  cf <- try(
    stats::coef(survival::coxph(y_surv ~ as.numeric(y_hat))),
    silent = TRUE
  )
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
  try(
    {
      pred_surv <- predict_custom(x, NULL, fit, model, type = "survival")
    },
    silent = TRUE
  )

  # If predict_custom did not yield survival probs, but can get lp, try to approximate
  if (is.null(pred_surv) || (is.atomic(pred_surv) && all(is.na(pred_surv)))) {
    # If cox-like linear predictor available, try to obtain predicted survival via baseline from a coxph fit
    if (model == "coxph") {
      pred_surv <- try(
        survival:::predict.coxph(fit, data, type = "survival"),
        silent = TRUE
      )
      if (inherits(pred_surv, "try-error")) pred_surv <- NULL
    } else if (model %in% c("rf", "ranger")) {
      # ranger survival returns matrix of survival probabilities by time index in predict()
      pr <- try(predict(fit, data = x), silent = TRUE)
      if (
        !inherits(pr, "try-error") &&
          !is.null(pr$predictions) &&
          is.matrix(pr$predictions)
      ) {
        pred_surv <- pr$predictions
      }
    } else {
      # For lasso (glmnet with family='cox') or xgboost survival: we generally obtain lp not direct survival probs
      pred_surv <- NULL
    }
  }

  if (is.null(pred_surv)) {
    # can't compute model-free calibration if survival probabilities not available
    warning(
      "survival_calib_slope_free: predicted survival probabilities not available for model '",
      model,
      "'. Returning NaN."
    )
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
    if (is.null(eval_time)) {
      eval_time <- max(data$time[data$event == 1]) * 0.9999
    }
  }

  # Get predicted model free yhat from logit: y_hat = log(S(t)/1-S(t))
  # Bound probabilities to avoid Inf
  pred_surv_at_time <- pmin(
    pmax(pred_surv_at_time, .Machine$double.eps),
    1 - .Machine$double.eps
  )
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

  fit_slope <- try(
    suppressWarnings(stats::glm(
      y_obs ~ y_hat,
      weights = w,
      family = stats::binomial()
    )),
    silent = TRUE
  )

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

  concordance <- try(survival::concordancefit(y_surv, y_hat), silent = TRUE)
  if (inherits(concordance, "try-error") || is.null(concordance)) {
    return(NaN)
  }

  t_max <- max(data[data$event == 1, "time"])
  auc_survival <- try(
    timeROC::timeROC(
      T = data$time,
      delta = data$event,
      marker = as.numeric(y_hat),
      times = t_max * 0.9999999,
      cause = 1,
      weighting = "marginal"
    )$AUC,
    silent = TRUE
  )

  if (inherits(auc_survival, "try-error") || length(auc_survival) == 0) {
    return(as.numeric(concordance$concordance))
  }

  auc_survival <- auc_survival[!is.na(auc_survival)]
  if (length(auc_survival) == 0) {
    return(as.numeric(concordance$concordance))
  }

  return(as.numeric(utils::tail(auc_survival, 1)))
}
