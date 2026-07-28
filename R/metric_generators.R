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
    } else if (metric == "csse") {
      metric_function <- binary_csse
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
    } else if (metric == "csse") {
      metric_function <- survival_csse
    } else if (metric == "IBS") {
      # Reserved for future Integrated Brier Score support.
      metric_function <- NULL
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
    } else if (metric == "csse") {
      metric_function <- continuous_csse
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

#' Predict from supported pmsims model objects
#'
#' @param x Predictor data supplied as a data frame or matrix.
#' @param y Optional outcome data retained for API compatibility.
#' @param fit Fitted model object returned by one of the supported model
#'   generators.
#' @param model Character string identifying the model family.
#' @param type Prediction scale requested from the fitted model.
#'
#' @return Numeric predictions, or a survival-probability matrix when the
#'   requested model and `type` support it.
#' @keywords internal
#' @noRd
predict_custom <- function(x, y = NULL, fit, model, type = "response") {
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

  # LASSO / RIDGE (glmnet::cv.glmnet)
  if (model %in% c("lasso", "ridge")) {
    # glmnet prediction requires a numeric matrix.
    require_optional_packages("glmnet", "lasso predictions")

    s_val <- "lambda.min"

    # Choose glmnet type mapping
    glmnet_type <- switch(
      type,
      response = "response",
      link = "link",
      lp = "link",
      stop("Type '", type, "' not supported for lasso.")
    )
    preds <- as.numeric(stats::predict(
      fit,
      newx = x_mat,
      s = s_val,
      type = glmnet_type
    ))
    # For binary responses, type = "response" returns probabilities.
    # For Cox models, type = "link" returns the linear predictor.
    return(preds)
  }

  # Random forest via ranger or randomForestSRC (rfsrc)
  if (model %in% c("rf", "ranger", "rfsrc")) {
    is_ranger <- inherits(fit, "ranger")
    is_rfsrc <- inherits(fit, "rfsrc")

    # Unknown object: fall back to a generic predict and extract sensibly
    if (!is_ranger && !is_rfsrc) {
      pr <- try(stats::predict(fit, newdata = x_df), silent = TRUE)
      if (!inherits(pr, "try-error")) {
        if (is.list(pr) && !is.null(pr$predictions)) {
          return(as.numeric(pr$predictions))
        }
        return(as.numeric(pr))
      }
      stop(
        "rf: model object is neither 'ranger' nor 'rfsrc', and generic predict() failed."
      )
    }

    # ----------------------------------------------------------------- ranger
    if (is_ranger) {
      require_optional_packages("ranger", "random forest (ranger) predictions")

      ncores <- parallel::detectCores(logical = FALSE)
      nthreads <- max(1L, ifelse(is.na(ncores), 1L, ncores - 2L))

      pr <- stats::predict(fit, data = x_df, num.threads = nthreads)

      # Survival forest
      if (identical(fit$treetype, "Survival")) {
        if (type == "survival") {
          # n x length(unique.death.times) matrix of survival probabilities
          return(pr$survival)
        }
        if (type %in% c("lp", "link")) {
          # Risk score from cumulative hazard summed over the time grid.
          # log() puts it on a Cox-lp-like scale: log H(t) = log H0(t) + eta.
          chf_sum <- pmax(rowSums(pr$chf), .Machine$double.eps)
          return(log(chf_sum))
        }
        stop(
          "rf (ranger survival): type '",
          type,
          "' not supported (use 'survival' or 'lp')."
        )
      }

      preds <- pr$predictions

      # Classification probabilities => matrix (cols per class)
      if (is.matrix(preds) && ncol(preds) >= 2) {
        if (type == "response") {
          return(as.numeric(preds[, ncol(preds)]))
        } # P(positive class)
        if (type == "link") {
          p <- pmin(
            pmax(as.numeric(preds[, ncol(preds)]), .Machine$double.eps),
            1 - .Machine$double.eps
          )
          return(stats::qlogis(p))
        }
      }

      # Regression / single numeric prediction
      if (is.numeric(preds) && is.vector(preds)) {
        return(as.numeric(preds))
      }

      stop("rf (ranger): unsupported prediction structure.")
    }

    # ------------------------------------------------------- randomForestSRC
    if (is_rfsrc) {
      require_optional_packages(
        "randomForestSRC",
        "random survival forest (rfsrc) predictions"
      )

      pr <- stats::predict(fit, newdata = x_df)

      # Survival forest
      if (identical(fit$family, "surv")) {
        if (type == "survival") {
          # n x length(time.interest) matrix of survival probabilities
          return(pr$survival)
        }
        if (type %in% c("lp", "link")) {
          # Ensemble mortality is rfsrc's native risk score (higher = higher risk)
          risk <- pmax(as.numeric(pr$predicted), .Machine$double.eps)
          return(log(risk))
        }
        stop(
          "rf (rfsrc survival): type '",
          type,
          "' not supported (use 'survival' or 'lp')."
        )
      }

      preds <- pr$predicted

      # Classification probabilities => matrix (cols per class)
      if (is.matrix(preds) && ncol(preds) >= 2) {
        if (type == "response") {
          return(as.numeric(preds[, ncol(preds)]))
        } # P(positive class)
        if (type == "link") {
          p <- pmin(
            pmax(as.numeric(preds[, ncol(preds)]), .Machine$double.eps),
            1 - .Machine$double.eps
          )
          return(stats::qlogis(p))
        }
      }

      # Regression / single numeric prediction
      if (is.numeric(preds) && is.null(dim(preds))) {
        return(as.numeric(preds))
      }

      stop("rf (rfsrc): unsupported prediction structure.")
    }
  }

  # xgboost
  if (model == "xgboost" || inherits(fit, "xgb.Booster")) {
    require_optional_packages("xgboost", "xgboost predictions")
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
    # For linear predictor / risk score.
    # NOTE: with objective = "survival:cox", xgboost returns predictions on the
    # HAZARD-RATIO scale, i.e. preds = exp(lp). The log-hazard linear predictor
    # required for a valid Cox calibration slope is therefore log(preds).
    # Ranking metrics (C-index, AUC) are unaffected because log is monotone.
    if (type == "lp") {
      return(log(pmax(as.numeric(preds), .Machine$double.eps)))
    }
    # For survival probabilities, not directly available from xgboost cox objective
    if (type == "survival") {
      stop(
        "xgboost: direct survival probability matrix is not available from xgboost predictions. Consider using type = 'lp' and mapping to survival via a baseline if needed."
      )
    }

    stop("xgboost: unsupported 'type' requested.")
  }

  # Cox models or other types that might use survival predictions
  if (model == "coxph") {
    fit_for_prediction <- fit
    formula_env <- new.env(
      parent = environment(stats::formula(fit_for_prediction))
    )
    formula_env$Surv <- survival::Surv
    environment(fit_for_prediction$formula) <- formula_env
    attr(fit_for_prediction$terms, ".Environment") <- formula_env

    if (type %in% c("lp", "link")) {
      return(stats::predict(fit_for_prediction, newdata = x_df, type = "lp"))
    } else if (type == "survival") {
      return(stats::predict(
        fit_for_prediction,
        newdata = x_df,
        type = "survival"
      ))
    } else {
      stop("coxph predict_custom: only 'lp' or 'survival' supported.")
    }
  }

  stop("predict_custom: unknown model type '", model, "'.")
}

# Binary metrics

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

binary_csse <- function(data, fit, model) {
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
  return(-(1 - calib_slope)^2)
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

# Continuous metrics

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

continuous_csse <- function(data, fit, model) {
  y <- data[, "y"]
  x <- data[, names(data) != "y", drop = FALSE]
  y_hat <- predict_custom(x, y, fit, model, type = "response")
  slope <- try(stats::lm(y ~ y_hat), silent = TRUE)
  calib_slope <- as.numeric(stats::coef(slope)[2])
  return(-(1 - calib_slope)^2)
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

# Survival metrics

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

# Horizon-based calibration slope for survival models.
# Random survival forests (ranger) do NOT produce a proportional-hazards linear
# predictor. Their ensemble-mortality score log(sum_t H(t)) is a fine RANKING
# statistic (used by C-index / AUC) but is not on a log-hazard scale where the
# calibrated slope is 1: under non-proportional hazards it is an arbitrary,
# time-grid-weighted aggregate. rf is therefore routed to a horizon-based
# calibration slope using the predicted log cumulative hazard, log H_i(t*).
survival_calib_slope <- function(data, fit, model, eval_time = NULL) {
  data <- data[base::order(data$time), ]

  # ONE fixed horizon for all models. In a simulation, set this from the DGP
  # (a known survival quantile / clinical horizon) and pass it in. Do NOT default
  # to the per-replicate observed median: it drifts across replicates and shrinks
  # with n, so it couples the metric to sample size -- the very thing you study.
  if (is.null(eval_time)) {
    ev <- data$time[data$event == 1]
    eval_time <- if (length(ev)) stats::median(ev) else stats::median(data$time)
  }

  # Predicted survival at t* on a common footing (rf: survival matrix;
  # coxph/lasso/ridge/xgboost: Breslow baseline applied to the lp).
  S <- predicted_survival_at_time(data, fit, model, eval_time)
  if (is.null(S) || all(is.na(S))) {
    return(NaN)
  }

  eps <- .Machine$double.eps
  S <- pmin(pmax(S, eps), 1 - eps)
  eta <- log(-log(S)) # cloglog = log H(t*); log-hazard scale

  # Graf/IPCW binary outcome at t*, identical machinery for every model.
  iw <- ipcw_binary_at_time(data, eval_time)

  fit_slope <- try(
    suppressWarnings(stats::glm(
      iw$y ~ eta,
      weights = iw$w,
      family = stats::binomial(link = "cloglog")
    )),
    silent = TRUE
  )
  if (inherits(fit_slope, "try-error") || is.null(fit_slope)) {
    return(NaN)
  }
  as.numeric(stats::coef(fit_slope)[2])
}

# Alternative calibration slope using PH linear predictors when available.
survival_calib_slope_PH <- function(data, fit, model, eval_time = NULL) {
  y_surv <- survival::Surv(data$time, data$event)
  x <- data[, !(names(data) %in% c("time", "event", "id")), drop = FALSE]

  if (model %in% c("coxph", "lasso", "ridge", "xgboost")) {
    # Genuine PH log-hazard linear predictor (xgboost after log() in predict_custom).
    # Horizon-invariant => classic Cox calibration slope; identical to old behaviour.
    eta <- try(predict_custom(x, NULL, fit, model, type = "lp"), silent = TRUE)
    if (inherits(eta, "try-error")) {
      return(NaN)
    }
    eta <- as.numeric(eta)
  } else if (model %in% c("rf", "ranger")) {
    # No PH lp. Use predicted log cumulative hazard at a fixed horizon:
    #   eta = log(-log S(t*)) = log H(t*)  -> same log-hazard scale as above.
    if (is.null(eval_time)) {
      ev <- data$time[data$event == 1]
      eval_time <- if (length(ev)) {
        stats::median(ev)
      } else {
        stats::median(data$time)
      }
    }
    S <- predicted_survival_at_time(data, fit, model, eval_time)
    if (is.null(S) || all(is.na(S))) {
      return(NaN)
    }
    S <- pmin(pmax(S, .Machine$double.eps), 1 - .Machine$double.eps)
    eta <- log(-log(S))
  } else {
    return(NaN)
  }

  cf <- try(stats::coef(survival::coxph(y_surv ~ eta)), silent = TRUE)
  if (inherits(cf, "try-error") || is.null(cf)) {
    return(NaN)
  }
  as.numeric(cf)
}


# Calibration-slope squared error using the horizon-based metric for every model.
survival_csse <- function(data, fit, model) {
  slope <- survival_calib_slope(data, fit, model)
  if (!is.finite(slope)) {
    return(NaN)
  }
  return(-(1 - slope)^2)
}

# Predicted survival probability S_i(t*) at a horizon, for any supported model.
#   rf/ranger : taken directly from the predicted survival matrix.
#   coxph/lasso/ridge/xgboost : these are proportional-hazards models that yield
#     a log-hazard linear predictor (xgboost only after predict_custom() applies
#     log() to its hazard-ratio output). We estimate a Breslow baseline with the
#     lp held as an offset, then S_i(t*) = exp(-H0(t*) * exp(lp_i)).
predicted_survival_at_time <- function(data, fit, model, eval_time) {
  x <- data[, !(names(data) %in% c("time", "event", "id")), drop = FALSE]

  if (model %in% c("rf", "ranger")) {
    ncores <- parallel::detectCores(logical = FALSE)
    nthreads <- max(1L, ifelse(is.na(ncores), 1L, ncores - 2L))

    pr <- try(
      stats::predict(fit, data = as.data.frame(x), num.threads = nthreads),
      silent = TRUE
    )
    if (inherits(pr, "try-error") || !is.list(pr) || is.null(pr$survival)) {
      return(NULL)
    }
    times <- pr$unique.death.times
    if (is.null(times)) {
      times <- fit$unique.death.times
    }
    if (is.null(times) || length(times) != ncol(pr$survival)) {
      return(NULL)
    }
    idx <- which.min(abs(times - eval_time))
    return(as.numeric(pr$survival[, idx]))
  }

  # PH-lp models: baseline via Breslow with lp as offset
  lp <- try(predict_custom(x, NULL, fit, model, type = "lp"), silent = TRUE)
  if (inherits(lp, "try-error") || is.null(lp)) {
    return(NULL)
  }
  lp <- as.numeric(lp)
  bh <- try(
    survival::basehaz(
      survival::coxph(survival::Surv(data$time, data$event) ~ offset(lp)),
      centered = FALSE
    ),
    silent = TRUE
  )
  if (inherits(bh, "try-error") || is.null(bh)) {
    return(NULL)
  }
  H0 <- stats::approx(bh$time, bh$hazard, xout = eval_time, rule = 2)$y
  exp(-H0 * exp(lp))
}

# IPCW (Graf) weights and binarised outcome for calibration at a fixed horizon.
# Weight 1/G(T_i-) for events before t*, 1/G(t*) for those still at risk at t*,
# and 0 for subjects censored before t* (their t*-status is unknown). G is the
# Kaplan-Meier estimate of the censoring-time distribution.
ipcw_binary_at_time <- function(data, eval_time) {
  cens_fit <- survival::survfit(
    survival::Surv(data$time, 1 - data$event) ~ 1
  )
  Gfun <- stats::stepfun(cens_fit$time, c(1, cens_fit$surv))
  eps <- .Machine$double.eps

  y_obs <- as.numeric(data$time <= eval_time & data$event == 1)
  w <- numeric(nrow(data))
  ev_before <- data$time <= eval_time & data$event == 1
  at_risk <- data$time > eval_time
  w[ev_before] <- 1 / pmax(Gfun(data$time[ev_before] - 1e-10), eps)
  w[at_risk] <- 1 / pmax(Gfun(eval_time), eps)
  # censored before t* keep weight 0
  list(y = y_obs, w = w)
}

# Previous model-free IPCW calibration-slope method.
#
# This logit-risk formulation has been superseded by
# survival_calib_slope() above, which uses the complementary log-log scale to
# align predicted survival with cumulative hazard. It is retained only for the
# legacy internal "calib_slope_free" metric selector.
#
# Works for any model that yields a predicted survival probability (rf via its
# survival matrix; coxph/lasso/ridge/xgboost via a Breslow baseline applied to
# the linear predictor). The predicted risk F_i(t*) = 1 - S_i(t*) is mapped to
# logit(F_i) and a censoring-weighted logistic regression of the t*-event status
# on logit(F) is fitted. The slope is 1 when the model is calibrated (predicted
# risks have the correct spread); < 1 indicates over-confident predictions,
# > 1 under-confident. Default horizon t* = median observed event time.
survival_calib_slope_free <- function(data, fit, model, eval_time = NULL) {
  data <- data[base::order(data$time), ]

  if (is.null(eval_time)) {
    ev_times <- data$time[data$event == 1]
    eval_time <- if (length(ev_times) > 0) {
      stats::median(ev_times)
    } else {
      stats::median(data$time)
    }
  }

  S <- predicted_survival_at_time(data, fit, model, eval_time)
  if (is.null(S) || all(is.na(S))) {
    warning(
      "survival_calib_slope_free: predicted survival not available for model '",
      model,
      "'. Returning NaN."
    )
    return(NaN)
  }

  eps <- .Machine$double.eps
  Frisk <- pmin(pmax(1 - S, eps), 1 - eps) # predicted risk at t*
  lp_risk <- stats::qlogis(Frisk) # logit risk -> slope ~1 when calibrated

  iw <- ipcw_binary_at_time(data, eval_time)

  fit_slope <- try(
    suppressWarnings(stats::glm(
      iw$y ~ lp_risk,
      weights = iw$w,
      family = stats::binomial()
    )),
    silent = TRUE
  )
  if (inherits(fit_slope, "try-error") || is.null(fit_slope)) {
    return(NaN)
  }
  as.numeric(stats::coef(fit_slope)[2])
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
