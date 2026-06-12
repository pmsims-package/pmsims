validate_metric_constraints <- function(
    metric,
    target_performance,
    maximum_achievable_performance = NULL
) {
  metric_lower <- tolower(metric)
  metric_label <- switch(
    metric_lower,
    auc = "AUC",
    r2 = "R2",
    cindex = "C-index",
    metric
  )
  
  if (metric_lower %in% c("calibration_slope", "calib_slope")) {
    if (target_performance < 0.8) {
      stop(
        "Requested target calibration slope is too low; check and try again.",
        call. = FALSE
      )
    }
    if (target_performance > 1.2) {
      stop(
        "Requested target calibration slope is too high; check and try again.",
        call. = FALSE
      )
    }
  }
  
  if (
    !is.null(maximum_achievable_performance) &&
    metric_lower %in% c("auc", "r2", "cindex")
  ) {
    if (target_performance >= maximum_achievable_performance) {
      stop(
        paste(
          "Requested target",
          metric_label,
          "must be less than the maximum achievable",
          metric_label,
          "because both are specified on the same metric scale."
        ),
        call. = FALSE
      )
    }
  }
}

validate_outcome_prevalence <- function(outcome_prevalence) {
  if (is.null(outcome_prevalence)) {
    cli::cli_abort("`outcome_prevalence` must be specified.")
  }
  
  if (outcome_prevalence < 0.05) {
    cli::cli_warn(
      "Outcome prevalence is very low ({.val {outcome_prevalence}}). Recommended > {.val 0.05}; values below this haven't been tested, and simulations may take a long time."
    )
  }
  
  invisible(TRUE)
}

#' check_pmsims_args - a custom version of the base R match.arg function with improved error message
#'
#' @inherit base::match.arg
#' @keywords internal
#' @noRd
#'
check_pmsims_args <- function(arg, choices, several.ok = FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sysP <- sys.parent()))
    choices <- eval(
      formal.args[[as.character(substitute(arg))]],
      envir = sys.frame(sysP)
    )
  }
  arg_name <- as.character(substitute(arg))
  
  if (is.null(arg)) {
    return(choices[1L])
  } else if (!is.character(arg)) {
    stop(paste0(arg_name, " must be NULL or a character vector"))
  }
  if (!several.ok) {
    if (identical(arg, choices)) {
      return(arg[1L])
    }
    if (length(arg) > 1L) {
      stop(paste0(arg_name, " must be of length 1"))
    }
  } else if (length(arg) == 0L) {
    stop(paste0(arg_name, " must be of length >= 1"))
  }
  i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  if (all(i == 0L)) {
    stop(
      sprintf(
        ngettext(
          length(chs <- unique(choices[nzchar(choices)])),
          sprintf("'%s' should be %%s", arg_name),
          sprintf("'%s' should be one of %%s", arg_name)
        ),
        paste(dQuote(chs), collapse = ", ")
      ),
      domain = NA
    )
  }
  i <- i[i > 0L]
  if (!several.ok && length(i) > 1) {
    stop("there is more than one match in 'check_pmsims_args'")
  }
  choices[i]
}

# =============================================================================
# Complexity and data_control validation
#
# Added for the complexity / data_control interface used by simulate_binary(),
# simulate_continuous() and simulate_survival(). `validate_data_control()` is
# the single place where incompatible-combination errors and warnings live, so
# new rules can be added here without touching the wrappers.
# =============================================================================

#' Validate the `complexity` argument
#'
#' @param complexity A single value; must be one of 1, 2, 3, 4.
#' @return Invisibly `TRUE`; otherwise stops with an informative error.
#' @keywords internal
#' @noRd
validate_complexity <- function(complexity) {
  if (length(complexity) != 1L || !is.numeric(complexity) ||
      !(complexity %in% c(1, 2, 3, 4))) {
    stop("`complexity` must be a single value in {1, 2, 3, 4}.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate the `data_control` list against `complexity`
#'
#' Checks field names and value ranges and flags combinations that the
#' data-generating mechanism cannot honour. Errors are raised for impossible
#' combinations; warnings are raised for ignored-but-harmless settings.
#'
#' @param data_control A named list (or `NULL`) with any of `nonlinear_strength`,
#'   `correlation`, `predictor_distribution`, `binary_predictor_prevalence`.
#' @param complexity A single value in 1:4.
#' @return Invisibly `TRUE`.
#' @keywords internal
#' @noRd
validate_data_control <- function(data_control, complexity) {
  allowed_fields <- c(
    "nonlinear_strength", "correlation",
    "predictor_distribution", "binary_predictor_prevalence"
  )
  allowed_dist <- c(
    "normal", "uniform", "binary", "exponential",
    "lognormal", "t", "laplace"
  )
  
  if (!is.null(data_control)) {
    if (!is.list(data_control)) {
      stop("`data_control` must be a list or NULL.", call. = FALSE)
    }
    unknown <- setdiff(names(data_control), allowed_fields)
    if (length(unknown)) {
      stop(
        "Unknown `data_control` field(s): ", paste(unknown, collapse = ", "),
        ". Allowed: ", paste(allowed_fields, collapse = ", "), ".",
        call. = FALSE
      )
    }
  }
  
  ns   <- data_control$nonlinear_strength
  corr <- data_control$correlation
  pd   <- data_control$predictor_distribution
  bpp  <- data_control$binary_predictor_prevalence
  
  # nonlinear_strength: fraction of signal variance carried by the nonlinear,
  # linearly-inaccessible component (C2/C3 only).
  if (!is.null(ns)) {
    if (!is.numeric(ns) || length(ns) != 1L || ns < 0 || ns >= 1) {
      stop("`data_control$nonlinear_strength` must be a single value in [0, 1).",
           call. = FALSE)
    }
    if (complexity %in% c(1, 4)) {
      warning(
        "`nonlinear_strength` is ignored for complexity ", complexity,
        " (C1 is purely linear; C4 is the Friedman function).",
        call. = FALSE
      )
    }
  }
  
  if (!is.null(corr)) {
    if (!is.numeric(corr) || length(corr) != 1L || corr < -1 || corr > 1) {
      stop("`data_control$correlation` must be a single value in [-1, 1].",
           call. = FALSE)
    }
  }
  
  if (!is.null(pd)) {
    if (!is.character(pd) || length(pd) != 1L || !(pd %in% allowed_dist)) {
      stop("`data_control$predictor_distribution` must be one of: ",
           paste(allowed_dist, collapse = ", "), ".", call. = FALSE)
    }
  }
  
  # Binary predictors: require a prevalence, and forbid C2/C3 (x^2 = x).
  if (identical(pd, "binary")) {
    if (is.null(bpp)) {
      stop(
        "`data_control$binary_predictor_prevalence` must be set (in (0, 1)) ",
        "when `predictor_distribution = \"binary\"`.",
        call. = FALSE
      )
    }
    if (!is.numeric(bpp) || length(bpp) != 1L || bpp <= 0 || bpp >= 1) {
      stop("`data_control$binary_predictor_prevalence` must be in (0, 1).",
           call. = FALSE)
    }
    if (complexity %in% c(2, 3)) {
      stop(
        "Binary predictors are incompatible with complexity ", complexity,
        ": squaring a 0/1 predictor returns itself (x^2 = x), which collapses ",
        "the quadratic term. Use continuous predictors for C2/C3.",
        call. = FALSE
      )
    }
  } else if (!is.null(bpp)) {
    warning(
      "`binary_predictor_prevalence` is ignored unless ",
      "`predictor_distribution = \"binary\"`.",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}