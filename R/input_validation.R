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
