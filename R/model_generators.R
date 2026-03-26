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
      ncores <- parallel::detectCores(logical = FALSE)
      nthreads <- ncores - 2

      x <- d[, -1, drop = FALSE]
      y <- as.factor(d[, 1])

      #### new

      ff <- NULL
      invisible(
        capture.output(
          ff <- randomForest::tuneRF(x, y, trace = FALSE, plot = FALSE)
        )
      )

      bestmtry <- data.frame(ff)
      mtry_best <- bestmtry$mtry[which.min(bestmtry$OOBError)]

      ranger::ranger(
        x = x,
        y = y,
        mtry = mtry_best,
        probability = TRUE,
        num.trees = 300,
        num.threads = nthreads
      )
    },
    xgboost = function(
      d,
      nrounds = 100,
      params = list(objective = "binary:logistic", eval_metric = "logloss")
    ) {
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
      ncores <- parallel::detectCores(logical = FALSE)
      nthreads <- ncores - 2

      x <- d[, -1, drop = FALSE]
      y <- d[, 1]

      # cvranger <- cv.ranger_tune(data = d, formula = y ~ .,
      #                      type = "regression",
      ##                      num.trees = 300,
      #                      iters = 30,
      #                      iters.warmup = 30,
      #                      time.budget = NULL,
      #                      num.threads = nthreads,
      #                       tune.parameters = "mtry",
      #                       measure = NULL,
      #                       build.final.model = TRUE,
      #                       show.info = FALSE)

      #maxd <- cvranger$max.depth[which.max(cvranger$mean_metric)]

      # best model

      # cvranger$model$learner.model

      ff <- NULL
      invisible(
        capture.output(
          ff <- randomForest::tuneRF(x, y, trace = FALSE, plot = FALSE)
        )
      )

      bestmtry <- data.frame(ff)
      mtry_best <- bestmtry$mtry[which.min(bestmtry$OOBError)]

      ranger::ranger(
        x = x,
        y = y,
        mtry = mtry_best,
        num.trees = 300,
        num.threads = nthreads
      )
    },
    xgboost = function(
      d,
      nrounds = 100,
      params = list(objective = "reg:squarederror", eval_metric = "rmse")
    ) {
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
      x <- as.matrix(d[,
        setdiff(colnames(d), c("time", "event")),
        drop = FALSE
      ])
      y <- survival::Surv(d$time, d$event)
      glmnet::cv.glmnet(x, y, family = "cox")
    },
    rf = function(d) {
      # ranger survival forest: formula interface with Surv()
      ncores <- parallel::detectCores(logical = FALSE)
      nthreads <- ncores - 2

      stopifnot(all(c("time", "event") %in% colnames(d)))
      formula <- stats::as.formula("survival::Surv(time, event) ~ .")
      ranger::ranger(formula, data = d, num.trees = 300, num.threads = nthreads)
    },
    xgboost = function(
      d,
      nrounds = 100,
      params = list(objective = "survival:cox", eval_metric = "cox-nloglik")
    ) {
      # XGBoost Cox objective: uses times as label but does not directly take a censoring vector.
      # We pass the observed times as the label and include event as a weight (1=event, 0=censored)
      # NOTE: This is a pragmatic/commonly-used approach — consult xgboost docs and consider
      # alternative survival-specific methods if you need strict handling of censoring.
      stopifnot(all(c("time", "event") %in% colnames(d)))
      x <- as.matrix(d[,
        setdiff(colnames(d), c("time", "event")),
        drop = FALSE
      ])
      label_time <- as.numeric(d$time)
      event <- as.numeric(d$event)
      # Use event indicator as weight (censored rows get weight 0)
      dtrain <- xgboost::xgb.DMatrix(
        data = x,
        label = label_time,
        weight = event
      )
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
  if (!outcome %in% names(default_models)) {
    stop(paste0("Outcome \"", outcome, "\" not found."))
  }

  if (!model %in% names(default_models[[outcome]])) {
    stop(paste0(
      "Model \"",
      model,
      "\" not found for outcome \"",
      outcome,
      "\""
    ))
  }

  model_function <- default_models[[outcome]][[model]]
  attr(model_function, "model") <- model
  return(model_function)
}

#' @keywords internal
#' @noRd
cv.ranger_tune <- function(
  data,
  formula,
  type = c("regression", "classification", "survival"),
  tune.parameters = c("mtry", "min.node.size"),
  num.trees = 1000,
  iters = 70,
  iters.warmup = 30,
  time.budget = NULL,
  num.threads = parallel::detectCores() - 1,
  measure = NULL,
  build.final.model = TRUE,
  show.info = TRUE,
  seed = 123,
  ...
) {
  # ===== dependencies =====
  required <- c("tuneRanger", "mlr", "ranger")
  missing_pkgs <- setdiff(required, rownames(installed.packages()))
  if (length(missing_pkgs) > 0) {
    stop(
      "Please install required packages: ",
      paste(missing_pkgs, collapse = ", ")
    )
  }
  library(tuneRanger)
  library(mlr)
  library(ranger)

  type <- match.arg(type)
  set.seed(seed)

  # ===== build mlr task =====
  # For survival, expect Surv(time, status) ~ .
  if (type == "survival") {
    # extract Surv var names from formula LHS
    lhs <- formula[[2]]
    surv_vars <- all.vars(lhs)
    if (length(surv_vars) < 2) {
      stop("For survival, specify Surv(time, status) on LHS of formula.")
    }
    time_col <- surv_vars[1]
    status_col <- surv_vars[2]
    task <- makeSurvTask(data = data, target = c(time_col, status_col))
  } else {
    resp <- all.vars(formula)[1]
    if (type == "classification") {
      if (!is.factor(data[[resp]])) {
        warning("Converting response to factor for classification.")
        data[[resp]] <- as.factor(data[[resp]])
      }
      task <- makeClassifTask(data = data, target = resp)
    } else {
      if (!is.numeric(data[[resp]])) {
        warning("Regression target is not numeric.")
      }
      task <- makeRegrTask(data = data, target = resp)
    }
  }

  # ===== prepare measure: tuneRanger expects a LIST of mlr measure objects =====
  # If user provided NULL -> choose sensible defaults
  if (is.null(measure)) {
    if (type == "regression") {
      measure_obj <- list(mlr::mse)
    } else if (type == "classification") {
      # binary -> AUC, multiclass -> mmce (misclassification) or acc
      if (length(task$task.desc$class.levels) == 2) {
        measure_obj <- list(mlr::auc)
      } else {
        measure_obj <- list(mlr::acc)
      }
    } else {
      # survival
      measure_obj <- list(mlr::cindex)
    }
  } else {
    # allow measure to be character, mlr measure object, or list
    if (is.character(measure)) {
      # single name or vector of names
      measure_obj <- lapply(measure, function(mn) {
        mm <- mlr::getMeasure(mn)
        if (is.null(mm)) {
          stop("Unknown mlr measure: ", mn)
        }
        mm
      })
    } else if (inherits(measure, "Measure")) {
      measure_obj <- list(measure)
    } else if (
      is.list(measure) &&
        all(sapply(measure, function(x) inherits(x, "Measure")))
    ) {
      measure_obj <- measure
    } else {
      stop(
        "`measure` must be NULL, a character name, an mlr::Measure, or a list of Measures."
      )
    }
  }

  # ===== call tuneRanger =====
  tune_args <- list(
    task = task,
    measure = measure_obj,
    iters = iters,
    iters.warmup = iters.warmup,
    time.budget = time.budget,
    num.threads = num.threads,
    num.trees = num.trees,
    parameters = list(),
    tune.parameters = tune.parameters,
    save.file.path = NULL,
    build.final.model = build.final.model,
    show.info = show.info
  )

  # merge extra args
  extra_args <- list(...)
  if (length(extra_args) > 0) {
    tune_args <- c(tune_args, extra_args)
  }

  tune_res <- do.call(tuneRanger::tuneRanger, tune_args)

  # ===== tidy output =====
  out <- list(
    recommended.pars = tune_res$recommended.pars,
    results = tune_res$results,
    measure = sapply(measure_obj, function(m) m$id)
  )
  if (build.final.model && !is.null(tune_res$model)) {
    out$model <- tune_res$model
  }

  class(out) <- c("cv.ranger_tune", class(out))
  return(out)
}

# simple print method
#' @keywords internal
#' @noRd
print.cv.ranger_tune <- function(x, ...) {
  cat("tuneRanger results\n")
  cat("-------------------\n")
  cat("Optimized measure(s):", paste(x$measure, collapse = ", "), "\n\n")
  cat("Recommended parameters:\n")
  print(x$recommended.pars)
  cat("\nTop tuning results (first 6 rows):\n")
  print(head(x$results))
  if (!is.null(x$model)) {
    cat("\nFinal model attached as $model\n")
  }
  invisible(x)
}
