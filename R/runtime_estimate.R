# =============================================================================
# Runtime estimation
#
# Some model and outcome combinations -- survival random forests in particular
# -- are slow enough that a full run can take many hours. Rather than a blanket
# "this model is slow" warning, the first (adaptive) stage is timed and used to
# extrapolate the cost of the full run. Users are only told when the estimate
# is genuinely long, and the strength of the message scales with it: a note for
# a run measured in tens of minutes, a warning and a confirmation prompt for
# one that will occupy the session for the rest of the afternoon.
#
# The extrapolation assumes fitting cost grows roughly linearly with the
# training sample size, so stage-1 time is converted to a cost per unit of
# (sample size x replication) and applied to the second stage, which draws its
# sample sizes from the bounds stage 1 established.
# =============================================================================

# Runs estimated to exceed this many seconds get a brief note.
long_run_notice_secs <- 30 * 60
# Runs estimated to exceed this many seconds also ask the user to confirm.
long_run_confirm_secs <- 2 * 60 * 60

#' Estimate total runtime from the timed adaptive stage
#'
#' @param stage_1_secs Numeric elapsed seconds for the adaptive stage.
#' @param track The `track` list returned by `calculate_adaptive_bounds()`,
#'   each element carrying the sample size `n` evaluated.
#' @param n_reps_per Integer replications per candidate sample size.
#' @param n_reps_total Integer total replication budget for the second stage.
#' @param min_sample_size,max_sample_size Numeric search bounds for the second
#'   stage.
#' @return Estimated total seconds, or `NA_real_` when the inputs are not
#'   usable for extrapolation.
#' @keywords internal
#' @noRd
estimate_total_runtime <- function(
  stage_1_secs,
  track,
  n_reps_per,
  n_reps_total,
  min_sample_size,
  max_sample_size
) {
  not_estimable <- NA_real_

  if (
    !is.numeric(stage_1_secs) ||
      length(stage_1_secs) != 1L ||
      !is.finite(stage_1_secs) ||
      stage_1_secs <= 0
  ) {
    return(not_estimable)
  }
  if (!is.list(track) || length(track) == 0L) {
    return(not_estimable)
  }

  ns <- suppressWarnings(as.numeric(vapply(
    track,
    function(z) {
      if (is.null(z$n)) NA_real_ else as.numeric(z$n)[1]
    },
    numeric(1)
  )))
  ns <- ns[is.finite(ns) & ns > 0]
  if (length(ns) == 0L) {
    return(not_estimable)
  }

  # Total (sample size x replication) work done in stage 1.
  work_1 <- sum(ns) * n_reps_per
  if (!is.finite(work_1) || work_1 <= 0) {
    return(not_estimable)
  }
  secs_per_unit <- stage_1_secs / work_1

  # Stage 2 draws sample sizes from across the bounds; use their midpoint as
  # the representative training size.
  bounds <- c(min_sample_size, max_sample_size)
  bounds <- suppressWarnings(as.numeric(bounds))
  bounds <- bounds[is.finite(bounds) & bounds > 0]
  mean_n_2 <- if (length(bounds) > 0) mean(bounds) else mean(ns)

  work_2 <- mean_n_2 * n_reps_total
  if (!is.finite(work_2) || work_2 <= 0) {
    return(not_estimable)
  }

  stage_1_secs + secs_per_unit * work_2
}

#' Describe a duration in the largest unit that keeps it readable
#'
#' @param secs Numeric seconds.
#' @return A single string such as `"45 minutes"` or `"3.2 hours"`.
#' @keywords internal
#' @noRd
format_runtime <- function(secs) {
  hours <- secs / 3600
  if (hours < 1) {
    sprintf("%.0f minutes", secs / 60)
  } else if (hours < 48) {
    sprintf("%.1f hours", hours)
  } else {
    sprintf("%.1f days", hours / 24)
  }
}

#' Ask the user whether to continue with a very long run
#'
#' Only asks when there is somebody there to answer. Non-interactive sessions
#' (scripts, CI, vignette builds) and users who have set
#' `pmsims.confirm_long_runs` to `FALSE` continue without being prompted.
#'
#' @return `TRUE` to proceed, `FALSE` to cancel.
#' @keywords internal
#' @noRd
confirm_long_run <- function() {
  if (!interactive()) {
    return(TRUE)
  }
  if (!isTRUE(getOption("pmsims.confirm_long_runs", TRUE))) {
    return(TRUE)
  }

  utils::menu(c("Continue", "Cancel"), title = "Start this run?") == 1L
}

#' Tell the user when the estimated runtime is long
#'
#' Emits an immediate alert (not a deferred `warning()`, which the user would
#' only see once the run they were being warned about had finished). Runs past
#' `long_run_notice_secs` get a note; runs past `long_run_confirm_secs` get a
#' warning and, in an interactive session, a chance to cancel.
#'
#' @inheritParams estimate_total_runtime
#' @param model Optional character model name, used in the message.
#' @return Invisibly, the estimated total seconds.
#' @keywords internal
#' @noRd
warn_if_long_run <- function(
  stage_1_secs,
  track,
  n_reps_per,
  n_reps_total,
  min_sample_size,
  max_sample_size,
  model = NULL
) {
  estimated_secs <- estimate_total_runtime(
    stage_1_secs = stage_1_secs,
    track = track,
    n_reps_per = n_reps_per,
    n_reps_total = n_reps_total,
    min_sample_size = min_sample_size,
    max_sample_size = max_sample_size
  )

  if (!is.finite(estimated_secs) || estimated_secs <= long_run_notice_secs) {
    return(invisible(estimated_secs))
  }

  duration <- format_runtime(estimated_secs)
  model_label <- if (
    is.character(model) && length(model) == 1L && !is.na(model)
  ) {
    sprintf(" (model '%s')", model)
  } else {
    ""
  }

  # Between the two thresholds a note is enough: long enough to be worth
  # knowing about, not long enough to interrupt for.
  if (estimated_secs <= long_run_confirm_secs) {
    cli::cli_alert_info(
      "This run is estimated to take about {duration}{model_label}."
    )
    return(invisible(estimated_secs))
  }

  cli::cli_alert_warning(
    "This run is estimated to take about {duration}{model_label}."
  )
  cli::cli_bullets(c(
    "i" = "Reduce {.arg n_reps_total}, narrow the sample size range, or choose a faster model to shorten it."
  ))

  if (!confirm_long_run()) {
    cli::cli_abort("Run cancelled.", call = NULL)
  }

  invisible(estimated_secs)
}
