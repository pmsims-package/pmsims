# =============================================================================
# User-facing labels and formatting for printed pmsims summaries
#
# The printed summary is a human-readable report, not a view of the object.
# Internal identifiers such as "auc", "rf" and "maximum_achievable_cstatistic"
# are translated here and appear verbatim only in the verbose display. Keeping
# every map in one place is what stops the same statistical quantity from
# acquiring two different names in two sections of the same output.
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

pmsims_is_present <- function(v) {
  !is.null(v) &&
    length(v) > 0L &&
    !(length(v) == 1L && !is.list(v) && is.na(v))
}

pmsims_sentence_case <- function(txt) {
  txt <- as.character(txt)
  if (!length(txt) || is.na(txt[1]) || !nzchar(txt[1])) {
    return(txt)
  }
  paste0(toupper(substr(txt, 1, 1)), substr(txt, 2, nchar(txt)))
}

# -----------------------------------------------------------------------------
# Numbers
# -----------------------------------------------------------------------------

pmsims_fmt_num <- function(v, digits = 3) {
  if (!pmsims_is_present(v)) {
    return(NA_character_)
  }
  if (is.numeric(v) && length(v) == 1L) {
    formatC(v, format = "f", digits = digits)
  } else {
    paste(as.character(v), collapse = ", ")
  }
}

pmsims_fmt_int <- function(v) {
  if (!pmsims_is_present(v)) {
    return(NA_character_)
  }
  if (is.numeric(v) && length(v) == 1L) {
    format(round(v), big.mark = ",", scientific = FALSE, trim = TRUE)
  } else {
    paste(as.character(v), collapse = ", ")
  }
}

# Baseline hazards are often far smaller than the other inputs, so fixed
# decimals would round them away.
pmsims_fmt_signif <- function(v, digits = 3) {
  if (!pmsims_is_present(v)) {
    return(NA_character_)
  }
  if (is.numeric(v) && length(v) == 1L) {
    formatC(v, format = "g", digits = digits)
  } else {
    paste(as.character(v), collapse = ", ")
  }
}

pmsims_fmt_duration <- function(simt) {
  secs <- if (inherits(simt, "difftime")) {
    as.numeric(simt, units = "secs")
  } else if (is.numeric(simt)) {
    as.numeric(simt)
  } else {
    return(NA_character_)
  }
  if (length(secs) != 1L || is.na(secs)) {
    return(NA_character_)
  }

  plural <- function(n, unit) {
    sprintf("%d %s", n, if (n == 1) unit else paste0(unit, "s"))
  }

  secs <- round(secs)
  d <- secs %/% 86400
  rem <- secs %% 86400
  h <- rem %/% 3600
  rem <- rem %% 3600
  m <- rem %/% 60
  s <- rem %% 60
  parts <- character(0)
  if (d > 0) {
    parts <- c(parts, plural(d, "day"))
  }
  if (h > 0) {
    parts <- c(parts, plural(h, "hour"))
  }
  if (m > 0) {
    parts <- c(parts, plural(m, "minute"))
  }
  if (s > 0 || length(parts) == 0) {
    parts <- c(parts, plural(s, "second"))
  }
  paste(parts, collapse = " ")
}

# -----------------------------------------------------------------------------
# Metrics
# -----------------------------------------------------------------------------

#' Human-readable name for a performance metric
#'
#' @param metric Internal metric identifier.
#' @param outcome Outcome type, used where the same identifier means different
#'   things for different outcomes.
#' @return A single character label, or `NULL` when `metric` is absent.
#' @keywords internal
#' @noRd
pmsims_metric_label <- function(metric, outcome = NULL) {
  if (!pmsims_is_present(metric)) {
    return(NULL)
  }
  key <- tolower(as.character(metric)[1])
  outcome <- as.character(outcome %||% "")[1]

  # For a binary outcome the AUC and the C-statistic are the same quantity, and
  # printing both names makes them look like two different measures. The
  # survival version is genuinely different (time-dependent), so it keeps its
  # own name.
  if (identical(key, "auc")) {
    return(switch(
      outcome,
      binary = "C-statistic",
      survival = "Time-dependent AUC",
      "AUC"
    ))
  }

  label <- switch(
    key,
    calibration_slope = "Calibration slope",
    calibration_slope_free = "Calibration slope",
    calibration_in_the_large = "Calibration-in-the-large",
    csse = "Calibration-slope squared error",
    cindex = "C-index",
    cstatistic = "C-statistic",
    brier = "Brier score",
    brier_score = "Brier score",
    brier_score_scaled = "Scaled Brier score",
    ibs = "Integrated Brier score",
    r2 = "R\u00B2",
    rsquared = "R\u00B2",
    rmse = "RMSE",
    NULL
  )
  label %||% pmsims_sentence_case(gsub("_", " ", as.character(metric)[1]))
}

#' Comparison operator describing the sample-size criterion
#'
#' The operator reflects the direction in which the metric improves, not the
#' inequality the search engine happens to evaluate. Calibration measures have
#' an ideal value rather than a direction, so the criterion points towards that
#' ideal from whichever side the requested target sits on.
#'
#' @param metric Internal metric identifier.
#' @param target Requested target value, used for the ideal-point metrics.
#' @return `cli::symbol$geq` or `cli::symbol$leq`, which fall back to `">="`
#'   and `"<="` where the console cannot render the mathematical glyphs.
#' @keywords internal
#' @noRd
pmsims_metric_operator <- function(metric, target = NULL) {
  key <- tolower(as.character(metric %||% "")[1])

  if (key %in% c("brier", "brier_score", "ibs", "rmse", "mse", "mae")) {
    return(cli::symbol$leq)
  }

  ideal <- switch(
    key,
    calibration_slope = 1,
    calibration_slope_free = 1,
    calibration_in_the_large = 0,
    NULL
  )
  if (
    !is.null(ideal) &&
      is.numeric(target) &&
      length(target) == 1L &&
      !is.na(target) &&
      target > ideal
  ) {
    return(cli::symbol$leq)
  }

  cli::symbol$geq
}

# -----------------------------------------------------------------------------
# Models, outcomes, predictors
# -----------------------------------------------------------------------------

#' Human-readable name for a modelling algorithm
#'
#' @param model Internal model identifier.
#' @param outcome Outcome type; `"rf"` and `"glm"` name different models for
#'   different outcomes.
#' @return A single character label, or `NULL` when `model` is absent.
#' @keywords internal
#' @noRd
pmsims_model_label <- function(model, outcome = NULL) {
  if (!pmsims_is_present(model)) {
    return(NULL)
  }
  key <- tolower(as.character(model)[1])
  outcome <- as.character(outcome %||% "")[1]

  label <- switch(
    key,
    glm = if (identical(outcome, "binary")) {
      "Logistic regression"
    } else {
      "Generalised linear model"
    },
    lm = "Linear regression",
    coxph = "Cox proportional hazards",
    lasso = "Lasso regression",
    ridge = "Ridge regression",
    rf = if (identical(outcome, "survival")) {
      "Random survival forest"
    } else {
      "Random forest"
    },
    xgboost = "XGBoost",
    NULL
  )
  label %||% as.character(model)[1]
}

pmsims_outcome_label <- function(outcome) {
  if (!pmsims_is_present(outcome)) {
    return(NULL)
  }
  key <- tolower(as.character(outcome)[1])
  label <- switch(
    key,
    binary = "Binary",
    continuous = "Continuous",
    survival = "Time-to-event",
    NULL
  )
  label %||% pmsims_sentence_case(as.character(outcome)[1])
}

pmsims_distribution_label <- function(distribution) {
  if (!pmsims_is_present(distribution)) {
    return(NULL)
  }
  key <- tolower(as.character(distribution)[1])
  label <- switch(
    key,
    normal = "Normal",
    uniform = "Uniform",
    binary = "Binary",
    exponential = "Exponential",
    lognormal = "Log-normal",
    t = "t",
    laplace = "Laplace",
    continuous = "Continuous",
    NULL
  )
  label %||% pmsims_sentence_case(as.character(distribution)[1])
}

pmsims_signal_form_label <- function(complexity) {
  if (!pmsims_is_present(complexity)) {
    return(NULL)
  }
  label <- switch(
    as.character(complexity)[1],
    "1" = "Linear",
    "2" = "Linear + quadratic",
    "3" = "Linear + quadratic + interaction",
    "4" = "Friedman function",
    NULL
  )
  label %||% as.character(complexity)[1]
}

#' Combine the signal and noise predictor counts onto one line
#'
#' @param signal Number of signal predictors.
#' @param noise Number of noise predictors.
#' @return A string such as `"10 signal + 10 noise"`, dropping a component that
#'   is zero or absent, or `NA_character_` when neither count is available.
#' @keywords internal
#' @noRd
pmsims_predictor_counts <- function(signal, noise) {
  has_signal <- pmsims_is_present(signal) && is.numeric(signal)
  has_noise <- pmsims_is_present(noise) && is.numeric(noise)

  parts <- character(0)
  if (has_signal && signal > 0) {
    parts <- c(parts, paste0(pmsims_fmt_int(signal), " signal"))
  }
  if (has_noise && noise > 0) {
    parts <- c(parts, paste0(pmsims_fmt_int(noise), " noise"))
  }

  if (length(parts)) {
    return(paste(parts, collapse = " + "))
  }
  # Both counts are zero, or only a zero count is recorded: still say so rather
  # than dropping the row from a scenario that does describe its predictors.
  if (has_signal) {
    return(paste0(pmsims_fmt_int(signal), " signal"))
  }
  if (has_noise) {
    return(paste0(pmsims_fmt_int(noise), " noise"))
  }
  NA_character_
}
