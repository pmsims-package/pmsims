#' default_model_generators Generate appropriate model based on input arguments
#'
#' @param outcome type of outcome, possible options are: "binary".
#' @return Model function.
#' @keywords internal
default_models <- list(
  binary = list(
    glm = function(d) {
      stats::glm("y ~ .", data = d, family = "binomial")
    },
    lasso = function(d) {
      # expects first column y (0/1) and remaining columns predictors
      d <- as.matrix(d)
      x <- d[, -1, drop = FALSE]
      y <- d[, 1]
      glmnet::cv.glmnet(
        x,
        y,
        family = "binomial"
      )
    },
    rf = function(d) {
      # expects column 1 = y (0/1) and remaining columns predictors
      x <- d[, -1, drop = FALSE]
      y <- d[, 1]
      ranger::ranger(
        x = x,
        y = y,
        probability = TRUE
      )
    },
    xgboost = function(d, nrounds = 100, params = list(objective = "binary:logistic", eval_metric = "logloss")) {
      # expects first column y (0/1), remaining columns predictors
      x <- as.matrix(d[, -1, drop = FALSE])
      y <- as.numeric(d[, 1])
      dtrain <- xgboost::xgb.DMatrix(data = x, label = y)
      xgboost::xgb.train(
        params = params,
        data = dtrain,
        nrounds = nrounds,
        verbose = 0
      )
    }
  ),
  
  continuous = list(
    lm = function(d) {
      stats::glm("y ~ .", data = d, family = "gaussian")
    },
    lasso = function(d) {
      # expects first column y (numeric), remaining columns predictors
      dmat <- as.matrix(d)
      x <- dmat[, -1, drop = FALSE]
      y <- dmat[, 1]
      glmnet::cv.glmnet(
        x,
        y,
        family = "gaussian"
      )
    },
    rf = function(d) {
      # expects first column y (numeric), remaining columns predictors
      x <- d[, -1, drop = FALSE]
      y <- d[, 1]
      ranger::ranger(
        x = x,
        y = y
      )
    },
    xgboost = function(d, nrounds = 100, params = list(objective = "reg:squarederror", eval_metric = "rmse")) {
      # expects first column y (numeric), remaining columns predictors
      x <- as.matrix(d[, -1, drop = FALSE])
      y <- as.numeric(d[, 1])
      dtrain <- xgboost::xgb.DMatrix(data = x, label = y)
      xgboost::xgb.train(
        params = params,
        data = dtrain,
        nrounds = nrounds,
        verbose = 0
      )
    }
  ),
  
  survival = list(
    coxph = function(d) {
      # expects columns named 'time' and 'event' and remaining columns predictors
      formula <- stats::as.formula("survival::Surv(time, event) ~ .")
      survival::coxph(formula, data = d)
    },
    lasso = function(d) {
      # glmnet with 'cox' family: predictors as matrix, response as Surv(time, event)
      # Remove time/event from predictors
      stopifnot(all(c("time", "event") %in% colnames(d)))
      x <- as.matrix(d[, setdiff(colnames(d), c("time", "event")), drop = FALSE])
      y <- survival::Surv(d$time, d$event)
      glmnet::cv.glmnet(x, y, family = "cox")
    },
    rf = function(d) {
      # ranger survival forest: formula interface with Surv()
      stopifnot(all(c("time", "event") %in% colnames(d)))
      formula <- stats::as.formula("survival::Surv(time, event) ~ .")
      ranger::ranger(formula, data = d)
    },
    xgboost = function(d, nrounds = 100, params = list(objective = "survival:cox", eval_metric = "cox-nloglik")) {
      # XGBoost Cox objective: uses times as label but does not directly take a censoring vector.
      # We pass the observed times as the label and include event as a weight (1=event, 0=censored)
      # NOTE: This is a pragmatic/commonly-used approach — consult xgboost docs and consider
      # alternative survival-specific methods if you need strict handling of censoring.
      stopifnot(all(c("time", "event") %in% colnames(d)))
      x <- as.matrix(d[, setdiff(colnames(d), c("time", "event")), drop = FALSE])
      label_time <- as.numeric(d$time)
      event <- as.numeric(d$event)
      # Use event indicator as weight (censored rows get weight 0)
      dtrain <- xgboost::xgb.DMatrix(data = x, label = label_time, weight = event)
      xgboost::xgb.train(
        params = params,
        data = dtrain,
        nrounds = nrounds,
        verbose = 0
      )
    }
  )
)


#' @keywords internal
#' @noRd
default_model_generators <- function(outcome, model) {
  if (model %in% names(default_models[[outcome]])) {
    model_function <- default_models[[outcome]][[model]]
    attr(model_function, "model") <- model
    return(model_function)
  } else {
    stop(paste0(
      "Model \"",
      model,
      "\" not found for outcome \"",
      outcome,
      "\""
    ))
  }
}
