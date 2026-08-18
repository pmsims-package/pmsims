# =============================================================================
# Internal calibration-slope <-> CSSE conversion
#
# Optimising the calibration slope directly is unreliable for the machine
# learning models: the search rarely settles on a usable sample size. The
# calibration slope squared error (CSSE) behaves much better as a search
# target, so when a user asks for `metric = "calibration_slope"` with an ML
# model we run the search on the CSSE scale internally and translate the result
# back before returning it.
#
# CSSE is defined as -(1 - slope)^2, i.e. negated squared error, so that larger
# is better and the optimum is 0. Squaring discards the sign of (1 - slope), so
# the direction of the user's target relative to 1 is recorded up front and used
# to pick the correct root when converting back.
#
# Users may also pass `metric = "csse"` explicitly. That path is untouched: the
# target is used exactly as supplied and no back-transformation happens.
# =============================================================================

# Model families for which calibration slope is routed through CSSE. GLM / LM /
# Cox are excluded: they optimise the calibration slope directly without issue.
ml_model_families <- c("lasso", "ridge", "rf", "xgboost")

#' Convert a calibration slope to the CSSE scale
#'
#' @param slope Numeric calibration slope.
#' @return Numeric CSSE value, `-(1 - slope)^2`.
#' @keywords internal
#' @noRd
calibration_slope_to_csse <- function(slope) {
  -(1 - slope)^2
}

#' Convert a CSSE value back to the calibration slope scale
#'
#' @param csse Numeric CSSE value (`<= 0`).
#' @param direction Either `"below"` or `"above"`, giving the side of 1 the
#'   user's calibration slope target sat on.
#' @return Numeric calibration slope.
#' @keywords internal
#' @noRd
csse_to_calibration_slope <- function(csse, direction = c("below", "above")) {
  direction <- match.arg(direction)
  if (!is.numeric(csse) || length(csse) != 1L || !is.finite(csse)) {
    return(NA_real_)
  }
  # Guard against small positive values arising from GP interpolation.
  deviation <- sqrt(max(0, -csse))
  if (direction == "above") 1 + deviation else 1 - deviation
}

#' Plan any internal calibration slope to CSSE conversion
#'
#' Decides whether the requested metric and model combination should be run on
#' the CSSE scale internally, and returns the metric name and target to hand to
#' the search engine.
#'
#' @param metric Character metric requested by the user.
#' @param model Character model family requested by the user.
#' @param target_performance Numeric target supplied by the user.
#' @return A list with `active` (logical), the `metric` and `target_performance`
#'   to use internally, and `direction` (`"below"`/`"above"`, or `NA` when
#'   inactive).
#' @keywords internal
#' @noRd
plan_internal_csse <- function(metric, model, target_performance) {
  inactive <- list(
    active = FALSE,
    metric = metric,
    target_performance = target_performance,
    direction = NA_character_
  )

  if (!identical(metric, "calibration_slope")) {
    return(inactive)
  }
  if (!(length(model) == 1L && model %in% ml_model_families)) {
    return(inactive)
  }
  if (
    !is.numeric(target_performance) ||
      length(target_performance) != 1L ||
      !is.finite(target_performance)
  ) {
    return(inactive)
  }

  list(
    active = TRUE,
    metric = "csse",
    target_performance = calibration_slope_to_csse(target_performance),
    # A target of exactly 1 is treated as "below": models overfit, so the
    # achieved slope sits below 1 in practice.
    direction = if (target_performance > 1) "above" else "below",
    # Retained so the user's target can be restored exactly.
    user_target_performance = target_performance
  )
}

#' Translate a result object back onto the calibration slope scale
#'
#' Rewrites the performance and target fields of a `pmsims` result that was
#' searched on the CSSE scale, and flags the object so `print.pmsims()` can
#' footnote the conversion.
#'
#' @param output A result list returned by [simulate_custom()].
#' @param plan The list returned by `plan_internal_csse()`.
#' @return `output`, modified when `plan$active` is `TRUE`.
#' @keywords internal
#' @noRd
restore_calibration_slope_scale <- function(output, plan) {
  if (!isTRUE(plan$active)) {
    return(output)
  }

  # Keep the values the search actually worked with, for diagnostics.
  output$csse_perf_n <- output$perf_n
  output$csse_target_performance <- output$target_performance

  if (is.numeric(output$perf_n) && length(output$perf_n) == 1L) {
    output$perf_n <- csse_to_calibration_slope(
      output$perf_n,
      direction = plan$direction
    )
  }

  output$target_performance <- plan$user_target_performance
  output$metric <- "calibration_slope"
  output$internal_csse <- TRUE
  output$csse_direction <- plan$direction
  output
}
