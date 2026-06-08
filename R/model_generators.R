#' default_model_generators Generate appropriate model based on input arguments
#'
#' @format A named list of default model generator functions grouped by outcome
#'   type.
#' @return `default_models` is a list containing built-in model generators for
#'   binary, continuous, and survival outcomes.
#' @keywords internal

# ---------------------------------------------------------------------------
# Internal helper: select nrounds for XGBoost via cross-validation.
#
# Uses xgb.cv with early stopping to find the optimal number of boosting
# rounds for the given data and objective.  This is the XGBoost analogue of
# cv.glmnet's lambda selection: complexity adapts to sample size, which is
# critical for calibration stability at small n.
#
# @param dtrain   xgb.DMatrix (training data)
# @param params   list of xgb parameters (must include objective/eval_metric)
# @param nrounds_max  upper bound on rounds to search (default 500)
# @param nfold    number of CV folds (default 5)
# @param early_stopping_rounds  stop if no improvement for this many rounds
# @return integer best nrounds (at least 1)
# @keywords internal
.xgb_cv_nrounds <- function(dtrain, params,
                            nrounds_max          = 500L,
                            nfold                = 5L,
                            early_stopping_rounds = 20L) {
  cv <- xgboost::xgb.cv(
    params               = params,
    data                 = dtrain,
    nrounds              = nrounds_max,
    nfold                = nfold,
    early_stopping_rounds = early_stopping_rounds,
    verbose              = 0,
    showsd               = FALSE
  )
  best <- cv$best_iteration
  if (is.null(best) || is.na(best) || best < 1L) best <- nrounds_max
  as.integer(best)
}
# ---------------------------------------------------------------------------

default_models <- list(
  binary = list(
    glm = function(d) {
      stats::glm("y ~ .", data = d, family = "binomial")
    },
    lasso = function(d) {
      require_optional_packages("glmnet", "lasso models")

      # expects first column y (0/1) and remaining columns predictors
      d <- as.matrix(d)
      x <- d[, -1, drop = FALSE]
      y <- d[, 1]
      glmnet::cv.glmnet(
        x,
        y,
        alpha  = 1,        # L1 penalty (LASSO)
        family = "binomial"
      )
    },
    ridge = function(d) {
      # Ridge logistic regression via glmnet (alpha = 0); lambda selected by CV
      d <- as.matrix(d)
      x <- d[, -1, drop = FALSE]
      y <- d[, 1]
      glmnet::cv.glmnet(
        x,
        y,
        alpha  = 0,        # L2 penalty (ridge)
        family = "binomial"
      )
    },
    rf = function(d) {
      require_optional_packages(
        c("ranger"),
        "random-forest models"
      )

      # expects column 1 = y (0/1) and remaining columns predictors
      ncores <- parallel::detectCores(logical = FALSE)
      nthreads <- ncores - 2

      x <- d[, -1, drop = FALSE]
      y <- as.factor(d[, 1])
      
      #### new

      #ff <- NULL
     # invisible(
      #  capture.output(
      #    ff <- randomForest::tuneRF(x, y, trace = FALSE, plot = FALSE)
      #  )
      #)

      #bestmtry <- data.frame(ff)
      #mtry_best <- bestmtry$mtry[which.min(bestmtry$OOBError)]

      ranger::ranger(
        x = x,
        y = y,
        mtry = max(1, floor(ncol(x) / 3)),
        probability = TRUE,
        num.trees = 300,
        num.threads = nthreads
      )
    },
    xgboost = function(d,
                       params = list(objective  = "binary:logistic",
                                     eval_metric = "logloss",
                                     eta         = 0.05,
                                     max_depth   = 4L,
                                     subsample   = 0.8,
                                     min_child_weight = 5L)) {
      # expects first column y (0/1), remaining columns predictors.
      # nrounds is selected automatically via xgb.cv (early stopping) so that
      # model complexity adapts to sample size — the analogue of cv.glmnet's
      # lambda selection.  Fixed nrounds = 100 caused severe over-fitting at
      # small n, collapsing the calibration slope well below 1.
      x      <- as.matrix(d[, -1, drop = FALSE])
      y      <- as.numeric(d[, 1])
      dtrain <- xgboost::xgb.DMatrix(data = x, label = y)
      best_nrounds <- .xgb_cv_nrounds(dtrain, params)
      xgboost::xgb.train(
        params  = params,
        data    = dtrain,
        nrounds = best_nrounds,
        verbose = 0
      )
    }
  ),

  continuous = list(
    lm = function(d) {
      stats::glm("y ~ .", data = d, family = "gaussian")
    },
    lasso = function(d) {
      require_optional_packages("glmnet", "lasso models")

      # expects first column y (numeric), remaining columns predictors
      dmat <- as.matrix(d)
      x <- dmat[, -1, drop = FALSE]
      y <- dmat[, 1]
      glmnet::cv.glmnet(
        x,
        y,
        alpha  = 1,        # L1 penalty (LASSO)
        family = "gaussian"
      )
    },
    ridge = function(d) {
      # Ridge regression via glmnet (alpha = 0); lambda selected by CV
      dmat <- as.matrix(d)
      x <- dmat[, -1, drop = FALSE]
      y <- dmat[, 1]
      glmnet::cv.glmnet(
        x,
        y,
        alpha  = 0,        # L2 penalty (ridge)
        family = "gaussian"
      )
    },
    rf = function(d) {
      require_optional_packages(
        c("ranger"),
        "random-forest models"
      )

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
      
      #ff <- NULL
      #invisible(
      #  capture.output(
      #    ff <- randomForest::tuneRF(x, y, trace = FALSE, plot = FALSE)
      #  )
      #)
      
      #bestmtry <- data.frame(ff)
      #mtry_best <- bestmtry$mtry[which.min(bestmtry$OOBError)]
      
      ranger::ranger(
        x = x,
        y = y,
        mtry = max(1, floor(ncol(x) / 3)),
        num.trees = 300L,
        replace = FALSE,
        num.threads = nthreads 
      )
    },
    xgboost = function(d,
                       params = list(objective   = "reg:squarederror",
                                     eval_metric  = "rmse",
                                     eta          = 0.05,
                                     max_depth    = 4L,
                                     subsample    = 0.8,
                                     min_child_weight = 5L)) {
      # expects first column y (numeric), remaining columns predictors.
      # nrounds is selected via xgb.cv early stopping (see .xgb_cv_nrounds).
      # This is essential for continuous calibration slope stability: fixed
      # nrounds = 100 led to over-dispersed predictions at small n (slope << 1)
      # because XGBoost memorises training-scale variation without regularisation
      # on the number of rounds.
      x      <- as.matrix(d[, -1, drop = FALSE])
      y      <- as.numeric(d[, 1])
      dtrain <- xgboost::xgb.DMatrix(data = x, label = y)
      best_nrounds <- .xgb_cv_nrounds(dtrain, params)
      xgboost::xgb.train(
        params  = params,
        data    = dtrain,
        nrounds = best_nrounds,
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
      require_optional_packages("glmnet", "lasso models")

      # glmnet with 'cox' family: predictors as matrix, response as Surv(time, event)
      # Remove time/event from predictors
      stopifnot(all(c("time", "event") %in% colnames(d)))
      x <- as.matrix(d[,
        setdiff(colnames(d), c("time", "event")),
        drop = FALSE
      ])
      y <- survival::Surv(d$time, d$event)
      glmnet::cv.glmnet(x, y, alpha = 1, family = "cox")  # L1 (LASSO)
    },
    ridge = function(d) {
      # Ridge Cox regression via glmnet (alpha = 0); lambda selected by CV
      stopifnot(all(c("time", "event") %in% colnames(d)))
      x <- as.matrix(d[, setdiff(colnames(d), c("time", "event")), drop = FALSE])
      y <- survival::Surv(d$time, d$event)
      glmnet::cv.glmnet(x, y, alpha = 0, family = "cox")  # L2 (ridge)
    },
    rf = function(d) {
      require_optional_packages(c("randomForestSRC", "ranger"), "random-forest models")

      # ranger survival forest: formula interface with Surv()
      ncores <- parallel::detectCores(logical = FALSE)
      nthreads <- ncores - 2

      stopifnot(all(c("time", "event") %in% colnames(d)))
      formula <- stats::as.formula("survival::Surv(time, event) ~ .")
      ranger::ranger(formula, data = d, num.trees = 300, num.threads = nthreads)
      #formula <- stats::as.formula("Surv(time, event) ~ .")
      #randomForestSRC::rfsrc(formula, data = d, ntree = 300)
    },
    xgboost = function(d,
                       params = list(objective   = "survival:cox",
                                     eval_metric  = "cox-nloglik",
                                     eta          = 0.05,
                                     max_depth    = 4L,
                                     subsample    = 0.8,
                                     min_child_weight = 5L)) {
      # XGBoost Cox objective: observed times as label, event indicator as
      # sample weight (1 = event, 0 = censored) — a standard pragmatic approach.
      # nrounds is selected via xgb.cv early stopping (see .xgb_cv_nrounds),
      # which prevents over-fitting the training hazard at small sample sizes
      # and stabilises the calibration slope toward 1 at moderate n.
      stopifnot(all(c("time", "event") %in% colnames(d)))
      x          <- as.matrix(d[, setdiff(colnames(d), c("time", "event")), drop = FALSE])
      label_time <- as.numeric(d$time)
      event      <- as.numeric(d$event)
      dtrain     <- xgboost::xgb.DMatrix(data = x, label = label_time, weight = event)
      best_nrounds <- .xgb_cv_nrounds(dtrain, params)
      xgboost::xgb.train(
        params  = params,
        data    = dtrain,
        nrounds = best_nrounds,
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
resolve_mlr_measures <- function(measure_names, task_type = NA_character_) {
  require_optional_packages("mlr", "mlr measure lookup")

  mlr_attached <- "package:mlr" %in% search()
  if (!mlr_attached) {
    base::attachNamespace(asNamespace("mlr"))
    on.exit(
      {
        if ("package:mlr" %in% search()) {
          detach("package:mlr", unload = FALSE, character.only = TRUE)
        }
      },
      add = TRUE
    )
  }

  available_measures <- mlr::listMeasures(task_type, create = TRUE)
  available_ids <- vapply(available_measures, function(x) x$id, character(1))

  lapply(measure_names, function(measure_name) {
    measure_index <- match(measure_name, available_ids)

    if (is.na(measure_index)) {
      stop("Unknown mlr measure: ", measure_name, call. = FALSE)
    }

    available_measures[[measure_index]]
  })
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
  require_optional_packages(
    c("tuneRanger", "mlr", "ranger"),
    "ranger tuning"
  )

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
    task <- mlr::makeSurvTask(data = data, target = c(time_col, status_col))
  } else {
    resp <- all.vars(formula)[1]
    if (type == "classification") {
      if (!is.factor(data[[resp]])) {
        warning("Converting response to factor for classification.")
        data[[resp]] <- as.factor(data[[resp]])
      }
      task <- mlr::makeClassifTask(data = data, target = resp)
    } else {
      if (!is.numeric(data[[resp]])) {
        warning("Regression target is not numeric.")
      }
      task <- mlr::makeRegrTask(data = data, target = resp)
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
      task_type <- switch(
        type,
        regression = "regr",
        classification = "classif",
        survival = "surv"
      )
      measure_obj <- resolve_mlr_measures(measure, task_type = task_type)
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
  print(utils::head(x$results))
  if (!is.null(x$model)) {
    cat("\nFinal model attached as $model\n")
  }
  invisible(x)
}