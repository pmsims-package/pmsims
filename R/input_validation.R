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
