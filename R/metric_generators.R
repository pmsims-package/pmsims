default_metric_generator <- function(metric, data_function) {
  outcome <- attr(data_function, "outcome")
  if (outcome == "binary") {
    if (metric == "auc") {
      metric_function <- binary_auc_metric
    } else if (metric == "calibration_slope") {
      metric_function <- binary_calib_slope
    } else if (metric == "calibration_in_the_large") {
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
    } else if (metric == "calibration_slope") {
      metric_function <- survival_calib_slope
    } else if (metric == "calibration_slope_free") {
      metric_function <- survival_calib_slope_free
    } else if (metric == "csse") {
      metric_function <- survival_csse
    } else if (metric == "ibs") {
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
    } else if (metric == "calibration_slope") {
      metric_function <- continuous_calib_slope
    } else if (metric == "calibration_in_the_large") {
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
          eps <- 1 / (2 * 300) # 2 x num.trees
          chf_sum <- pmax(rowSums(pr$chf), eps)
          #chf_sum <- pmax(rowSums(pr$chf), .Machine$double.eps)
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
        # eps is the finest resolution the ensemble can express; derive it from
        # the fitted forest rather than hard-coding 300 trees.
        eps <- rf_prob_eps(fit)
        p <- pmin(pmax(as.numeric(preds[, ncol(preds)]), eps), 1 - eps)
        eta <- stats::qlogis(p)

        # Out-of-bag recalibration (see the recalibration section below). The
        # map is monotone, so AUC / C-index are unchanged; it corrects only the
        # spread of the predicted probabilities.
        rc <- rf_recal_binary(fit)
        if (!is.null(rc)) {
          eta <- rc$a + rc$b * eta
        }

        if (type == "link") {
          return(eta)
        }
        if (type == "response") {
          return(stats::plogis(eta))
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


# Calibration-slope squared error. Dispatches on model family: proportional-
# hazards models go through survival_calib_slope_PH(), the ML learners (xgboost,
# rf/ranger) through the horizon-based survival_calib_slope().
survival_csse <- function(data, fit, model) {
  ph_models <- c("coxph", "lasso", "ridge")
  ml_models <- c("xgboost", "rf", "ranger")

  if (model %in% ph_models) {
    slope <- survival_calib_slope_PH(
      data = data,
      fit = fit,
      model = model
    )
  } else if (model %in% ml_models) {
    slope <- survival_calib_slope(
      data = data,
      fit = fit,
      model = model
    )
  } else {
    stop("Unsupported model: ", model)
  }

  if (!is.finite(slope)) {
    return(NaN)
  }

  -(1 - slope)^2
}

#' Cox calibration slope at a landmark horizon
#'
#' Predicted survival at \code{eval_time} is obtained exactly as in the
#' reference script -- via \code{survfit()} for every proportional-hazards
#' model, and read off the ensemble matrix for the two forests -- then mapped to
#' the complementary log-log scale and used as the sole covariate in a Cox model
#' fitted to the validation outcome. The coefficient is the calibration slope.
#'
#' Baselines come from the TRAINING data (the model's own \code{survfit}
#' baseline for \code{coxph}; \code{x}/\code{y} at \code{lambda.min} for
#' \code{glmnet}; a training-fitted \code{coxph} on the xgboost linear predictor).
#'
#' @param data       Validation data with columns \code{time}, \code{event} and
#'                   the predictors.
#' @param fit        Fitted model object.
#' @param model      Model string, as passed by the engines. Used only for
#'                   messages and for the glmnet lambda; dispatch is on class.
#' @param eval_time  Landmark horizon. Defaults to \code{median(data$time)}, as
#'                   in the script.
#' @param train_data Training data, same columns as \code{data}. REQUIRED for
#'                   ridge, lasso and xgboost, which cannot reconstruct their
#'                   baseline hazard from the fit object alone. Ignored for
#'                   coxph, ranger and rfsrc.
#' @param eps        Clamp applied to predicted survival before the cloglog
#'                   transform.
#'
#' @return A single numeric calibration slope (1 = calibrated), or \code{NaN}.
#' @keywords internal
survival_calib_slope_cox <- function(
  data,
  fit,
  model,
  eval_time = NULL,
  train_data = NULL,
  eps = 1e-6
) {
  if (is.null(eval_time)) {
    eval_time <- stats::median(data$time)
  }
  # if (!is.finite(eval_time) || eval_time <= 0) return(NaN)

  pred_names <- setdiff(names(data), c("time", "event", "id"))
  x_df <- data[, pred_names, drop = FALSE]
  S <- NULL

  # Restore Surv() in the formula environment: pmsims fits its models inside
  # functions whose environment does not carry it, and survfit()/model.frame()
  # need to re-evaluate the response. Same workaround as predict_custom().
  .fix_env <- function(f) {
    e <- new.env(parent = environment(stats::formula(f)))
    e$Surv <- survival::Surv
    environment(f$formula) <- e
    attr(f$terms, ".Environment") <- e
    f
  }

  # Survival probability at eval_time from a survfit object, one value per row
  # of newdata. extend = TRUE so a horizon beyond the last event time returns
  # the final estimate rather than being dropped.
  .surv_at <- function(sf) {
    s <- try(summary(sf, times = eval_time, extend = TRUE)$surv, silent = TRUE)
    if (inherits(s, "try-error")) {
      return(NULL)
    }
    as.numeric(s)
  }

  # --- Cox --------------------------------------------------------------
  if (inherits(fit, "coxph")) {
    sf <- try(survival::survfit(.fix_env(fit), newdata = x_df), silent = TRUE)
    if (inherits(sf, "try-error")) {
      return(NaN)
    }
    S <- .surv_at(sf)

    # --- Ridge / Lasso ----------------------------------------------------
  } else if (inherits(fit, "cv.glmnet") || inherits(fit, "glmnet")) {
    if (is.null(train_data)) {
      warning(
        "survival_calibration_slope_cox: '",
        model,
        "' needs train_data ",
        "(glmnet fits do not retain x/y, so survfit() cannot recover the ",
        "baseline hazard). Returning NaN.",
        call. = FALSE
      )
      return(NaN)
    }
    gfit <- if (inherits(fit, "cv.glmnet")) fit$glmnet.fit else fit
    s_val <- if (inherits(fit, "cv.glmnet")) fit$lambda.min else min(fit$lambda)

    x_train <- as.matrix(train_data[, pred_names, drop = FALSE])
    x_test <- as.matrix(x_df)
    y_train <- survival::Surv(train_data$time, train_data$event)

    sf <- try(
      survival::survfit(
        gfit,
        s = s_val,
        x = x_train,
        y = y_train,
        newx = x_test
      ),
      silent = TRUE
    )
    if (inherits(sf, "try-error")) {
      return(NaN)
    }
    S <- .surv_at(sf)

    # --- XGBoost ----------------------------------------------------------
  } else if (inherits(fit, "xgb.Booster")) {
    if (is.null(train_data)) {
      warning(
        "survival_calibration_slope_cox: 'xgboost' needs train_data to ",
        "fit the baseline hazard on the linear predictor. Returning NaN.",
        call. = FALSE
      )
      return(NaN)
    }
    x_train <- as.matrix(train_data[, pred_names, drop = FALSE])
    x_test <- as.matrix(x_df)

    # survival:cox predictions are exp(lp); outputmargin = TRUE gives the
    # log-hazard linear predictor, which is what coxph expects as a covariate.
    lp_train <- try(
      stats::predict(fit, newdata = x_train, outputmargin = TRUE),
      silent = TRUE
    )
    lp_test <- try(
      stats::predict(fit, newdata = x_test, outputmargin = TRUE),
      silent = TRUE
    )
    if (inherits(lp_train, "try-error") || inherits(lp_test, "try-error")) {
      return(NaN)
    }

    tr_lp <- data.frame(
      time = train_data$time,
      event = train_data$event,
      xgb_lp = as.numeric(lp_train)
    )
    te_lp <- data.frame(xgb_lp = as.numeric(lp_test))

    base_fit <- try(
      survival::coxph(
        survival::Surv(time, event) ~ xgb_lp,
        data = tr_lp,
        x = TRUE,
        y = TRUE
      ),
      silent = TRUE
    )
    if (inherits(base_fit, "try-error")) {
      return(NaN)
    }

    sf <- try(survival::survfit(base_fit, newdata = te_lp), silent = TRUE)
    if (inherits(sf, "try-error")) {
      return(NaN)
    }
    S <- .surv_at(sf)

    # --- ranger -----------------------------------------------------------
  } else if (inherits(fit, "ranger")) {
    pr <- try(stats::predict(fit, data = x_df), silent = TRUE)
    if (inherits(pr, "try-error") || is.null(pr$survival)) {
      return(NaN)
    }
    grid <- pr$unique.death.times
    if (is.null(grid)) {
      grid <- fit$unique.death.times
    }
    if (is.null(grid) || length(grid) != ncol(pr$survival)) {
      return(NaN)
    }
    S <- as.numeric(pr$survival[, which.min(abs(grid - eval_time))])

    # --- rfsrc ------------------------------------------------------------
  } else if (inherits(fit, "rfsrc")) {
    pr <- try(stats::predict(fit, newdata = x_df), silent = TRUE)
    if (inherits(pr, "try-error") || is.null(pr$survival)) {
      return(NaN)
    }
    grid <- pr$time.interest
    if (is.null(grid)) {
      grid <- fit$time.interest
    }
    if (is.null(grid) || length(grid) != ncol(pr$survival)) {
      return(NaN)
    }
    S <- as.numeric(pr$survival[, which.min(abs(grid - eval_time))])
  } else {
    warning(
      "survival_calibration_slope_cox: unsupported fit of class ",
      paste(class(fit), collapse = "/"),
      ". Returning NaN.",
      call. = FALSE
    )
    return(NaN)
  }

  if (is.null(S) || length(S) != nrow(data) || !all(is.finite(S))) {
    return(NaN)
  }

  cll <- log(-log(pmin(pmax(S, eps), 1 - eps)))
  if (!all(is.finite(cll)) || stats::sd(cll) < 1e-10) {
    return(NaN)
  }

  d <- data.frame(time = data$time, event = data$event, cll = cll)
  if (sum(d$event == 1) < 2) {
    return(NaN)
  }

  slope_fit <- try(
    survival::coxph(survival::Surv(time, event) ~ cll, data = d),
    silent = TRUE
  )
  if (inherits(slope_fit, "try-error")) {
    return(NaN)
  }

  slope <- as.numeric(stats::coef(slope_fit)[1])
  if (!is.finite(slope)) {
    return(NaN)
  }
  slope
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
    # STEP 1: point prediction S_i(t*) only (see rsf_survival_at_point).
    S <- rsf_survival_at_point(fit, x, eval_time)
    if (is.null(S)) {
      return(NULL)
    }

    # STEP 2: apply the out-of-bag recalibration map, fitted at this same t*.
    rc <- rf_recal_survival(fit, eval_time)
    if (!is.null(rc)) {
      eps <- .Machine$double.eps
      eta <- log(-log(pmin(pmax(S, eps), 1 - eps)))
      S <- exp(-exp(rc$a + rc$b * eta))
    }
    return(S)
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
# internal "calibration_slope_free" metric selector.
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

#### Random-forest point prediction and out-of-bag recalibration ####
#
# WHY THIS SECTION EXISTS
#
# A calibration-slope sample-size question is well posed only for a learner
# whose calibration slope converges to 1. For coxph / lasso / ridge it does: the
# slope sits below 1 at small n because of overfitting and rises to 1 as n
# grows, so "the smallest n at which the slope reaches 0.9" is a real number.
#
# A random forest is different. ranger probability forests are systematically
# UNDER-dispersed on the logit scale -- bagging plus mtry averaging shrinks
# predictions toward the marginal -- so the recalibration slope converges to a
# model-specific constant strictly greater than 1. Measured on the 10-predictor
# binary DGP at C = 0.8 with min.node.size = 15 (test n = 20,000):
#
#     n =   179   slope 1.081        n = 2,864   slope 1.289
#     n =   716   slope 1.287        n = 5,728   slope 1.272
#
# A target of |slope - 1| <= 0.1 is therefore unreachable at ANY n, and a search
# that keeps doubling n looking for it will never terminate.
#
# The fix is to split the forest into (a) a ranking function and (b) a
# calibration map, and fit the map on OUT-OF-BAG training predictions. ranger
# returns these for free -- fit$predictions for probability forests,
# fit$survival for survival forests -- so this costs no extra model fits and
# uses no test data. The recalibrated forest then behaves like the regression
# models: its slope is below/around 1 at small n (the map is itself estimated
# from finite data) and tightens on 1 as n grows.
#
# REQUIRED HOOK
#
# ranger keeps the OOB predictions but discards the training OUTCOME, and a
# metric function only receives (test_data, fit, model). The training outcome
# must therefore be carried on the fit. Add one line to the model function:
#
#   binary:    fit$pmsims_train <- list(y = d[["y"]])
#   survival:  fit$pmsims_train <- list(time = d$time, event = d$event)
#
# If the hook is absent every function below returns NULL and predictions fall
# back to the raw, uncalibrated forest -- i.e. exactly the current behaviour.

# Finest probability resolution the ensemble can express.
rf_prob_eps <- function(fit, default_trees = 300) {
  nt <- tryCatch(fit$num.trees, error = function(e) NULL)
  if (is.null(nt) || !is.finite(nt) || nt < 1) {
    nt <- default_trees
  }
  1 / (2 * nt)
}

rf_train_outcome <- function(fit) {
  tr <- tryCatch(fit$pmsims_train, error = function(e) NULL)
  if (is.null(tr)) {
    tr <- attr(fit, "pmsims_train", exact = TRUE)
  }
  tr
}

# Point prediction S_i(t*) for a random survival forest.
#
# ranger materialises a full n_test x n_unique_death_times survival matrix AND a
# matching cumulative-hazard matrix, and n_unique_death_times grows linearly
# with the TRAINING size. We only ever need one column. Left unchunked this is
# an O(test_n * n_train) allocation: at test_n = 30,000 and n_train = 45,000
# that is ~15 GB across the two matrices, which is what kills long jobs.
#
# Predicting in blocks and discarding everything but the horizon column as we go
# holds peak memory at O(chunk * udt) instead.
rsf_survival_at_point <- function(
  fit,
  x,
  eval_time,
  chunk = getOption("pmsims.predict_chunk", 2000L)
) {
  x_df <- as.data.frame(x)
  n_test <- nrow(x_df)
  if (n_test == 0L) {
    return(numeric(0))
  }
  chunk <- max(1L, min(n_test, as.integer(chunk)))

  S <- numeric(n_test)
  idx <- NA_integer_
  for (s in seq(1L, n_test, by = chunk)) {
    e <- min(s + chunk - 1L, n_test)
    pr <- try(
      stats::predict(fit, data = x_df[s:e, , drop = FALSE], num.threads = 2),
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
    if (is.na(idx)) {
      idx <- which.min(abs(times - eval_time))
    }
    S[s:e] <- as.numeric(pr$survival[, idx])
    rm(pr)
  }
  S
}

# Logit-scale (Platt) recalibration map from OOB probability predictions.
rf_recal_binary <- function(fit) {
  if (!inherits(fit, "ranger")) {
    return(NULL)
  }
  tr <- rf_train_outcome(fit)
  if (is.null(tr) || is.null(tr$y)) {
    return(NULL)
  }

  oob <- fit$predictions
  if (is.null(oob) || !is.matrix(oob) || ncol(oob) < 2) {
    return(NULL)
  }

  y <- tr$y
  if (is.factor(y)) {
    y <- as.numeric(as.character(y))
  }
  y <- as.numeric(y)
  p <- as.numeric(oob[, ncol(oob)])
  if (length(p) != length(y)) {
    return(NULL)
  }

  ok <- is.finite(p) & is.finite(y)
  if (sum(ok) < 20L || length(unique(y[ok])) < 2L) {
    return(NULL)
  }

  eps <- rf_prob_eps(fit)
  eta <- stats::qlogis(pmin(pmax(p[ok], eps), 1 - eps))
  if (stats::sd(eta) < 1e-10) {
    return(NULL)
  }

  cal <- try(
    suppressWarnings(stats::glm(y[ok] ~ eta, family = stats::binomial())),
    silent = TRUE
  )
  if (inherits(cal, "try-error")) {
    return(NULL)
  }
  cf <- as.numeric(stats::coef(cal))
  if (length(cf) < 2L || !all(is.finite(cf))) {
    return(NULL)
  }

  list(a = cf[1], b = cf[2])
}

# Cloglog recalibration map at t*, from OOB survival predictions.
# Fitted at the SAME horizon the metric evaluates at, so the map is never
# applied at a horizon it was not estimated for.
rf_recal_survival <- function(fit, eval_time) {
  if (!inherits(fit, "ranger")) {
    return(NULL)
  }
  tr <- rf_train_outcome(fit)
  if (is.null(tr) || is.null(tr$time) || is.null(tr$event)) {
    return(NULL)
  }

  S_oob <- fit$survival
  times <- fit$unique.death.times
  if (is.null(S_oob) || is.null(times) || ncol(S_oob) != length(times)) {
    return(NULL)
  }
  if (nrow(S_oob) != length(tr$time)) {
    return(NULL)
  }

  idx <- which.min(abs(times - eval_time))
  eps <- .Machine$double.eps
  S <- pmin(pmax(as.numeric(S_oob[, idx]), eps), 1 - eps)
  eta <- log(-log(S))

  train_df <- data.frame(time = tr$time, event = tr$event)
  iw <- try(ipcw_binary_at_time(train_df, eval_time), silent = TRUE)
  if (inherits(iw, "try-error")) {
    return(NULL)
  }

  keep <- is.finite(eta) & is.finite(iw$w) & iw$w > 0
  if (sum(keep) < 20L || length(unique(iw$y[keep])) < 2L) {
    return(NULL)
  }
  if (stats::sd(eta[keep]) < 1e-10) {
    return(NULL)
  }

  cal <- try(
    suppressWarnings(stats::glm(
      iw$y[keep] ~ eta[keep],
      weights = iw$w[keep],
      family = stats::binomial(link = "cloglog")
    )),
    silent = TRUE
  )
  if (inherits(cal, "try-error")) {
    return(NULL)
  }
  cf <- as.numeric(stats::coef(cal))
  if (length(cf) < 2L || !all(is.finite(cf))) {
    return(NULL)
  }

  list(a = cf[1], b = cf[2])
}
