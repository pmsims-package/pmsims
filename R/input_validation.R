validate_metric_constraints <- function(metric,
                                        minimum_acceptable_performance,
                                        expected_performance = NULL) {
  metric_lower <- tolower(metric)

  if (metric_lower %in% c("calibration_slope", "calib_slope")) {
    if (minimum_acceptable_performance < 0.8) {
      stop(
        "Suggested calibration slope is too low; check and try again.",
        call. = FALSE
      )
    }
    if (minimum_acceptable_performance > 1.2) {
      stop(
        "Suggested calibration slope is too high; check and try again.",
        call. = FALSE
      )
    }
  }

  if (!is.null(expected_performance) && metric_lower == "auc") {
    if (expected_performance < minimum_acceptable_performance) {
      stop(
        paste(
          "Requested minimum acceptable AUC exceeds the expected",
          "large-sample performance; adjust inputs and try again."
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
    cli::cli_alert_warning(
      "Outcome prevalence is very low ({.val {outcome_prevalence}}). Recommended > {.val 0.05}; values below this haven’t been tested, and simulations may take a long time."
    )
  }

  invisible(TRUE)
}

#' check_pmsims_args - a custom version of the base R match.arg function with improved error message
#'
#'@inherit base::match.arg
#'
check_pmsims_args <- function(arg, choices, several.ok = FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sysP <- sys.parent()))
    choices <- eval(formal.args[[as.character(substitute(arg))]], 
                    envir = sys.frame(sysP))
  }
  arg_name <- as.character(substitute(arg))
  
  if (is.null(arg)) 
    return(choices[1L])
  else if (!is.character(arg)) 
    stop(paste0(arg_name, " must be NULL or a character vector"))
  if (!several.ok) {
    if (identical(arg, choices)) 
      return(arg[1L])
    if (length(arg) > 1L) 
      stop(paste0(arg_name, " must be of length 1"))
  }
  else if (length(arg) == 0L) 
    stop(paste0(arg_name, " must be of length >= 1"))
  i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  if (all(i == 0L)) 
    stop(sprintf(
      ngettext(
        length(chs <- unique(choices[nzchar(choices)])),
        sprintf("'%s' should be %%s", arg_name),
        sprintf("'%s' should be one of %%s", arg_name)
      ),
      paste(dQuote(chs), collapse = ", ")
    ), domain = NA)
  i <- i[i > 0L]
  if (!several.ok && length(i) > 1) 
    stop("there is more than one match in 'check_pmsims_args'")
  choices[i]
}
