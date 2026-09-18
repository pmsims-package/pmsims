# =============================================================================
# Console summary for pmsims results
#
# The output answers, in order: what scenario was specified, what criterion was
# set, how the simulation was run, what sample size was found, and what
# performance is expected there. Hierarchy is carried by grouping, indentation
# and wording first, so the structure survives a terminal without colour;
# colour and weight only reinforce it.
#
# Implementation-level detail (internal metric identifiers, engine settings,
# the CSSE search scale) is reserved for the verbose display, reached through
# `print(x, verbose = TRUE)` or `summary(x)`.
# =============================================================================

#' Print a `pmsims` result
#'
#' @param x A `pmsims` object.
#' @param ... Currently unused.
#' @param max_width Maximum console width used for the summary.
#' @param verbose Logical. If `TRUE`, add implementation-level detail:
#'   internal metric identifiers, the engine settings used for the search, and
#'   any quantities recorded on an internal search scale. [summary.pmsims()]
#'   is the same display with `verbose = TRUE` by default.
#'
#' @return `x`, invisibly.
#' @keywords internal
#' @export
print.pmsims <- function(x, ..., max_width = 80, verbose = FALSE) {
  if (!inherits(x, "pmsims")) {
    stop("Object is not of class 'pmsims'")
  }
  render_pmsims_summary(x, max_width = max_width, verbose = isTRUE(verbose))
  invisible(x)
}

# -----------------------------------------------------------------------------
# Items
#
# The summary is assembled as a flat list of items and only then rendered, so
# that label and value columns can be aligned across sections that are built
# independently of one another.
# -----------------------------------------------------------------------------

# Footnote marker for a value that needs a caveat. A marker sitting flush
# against its label has to read as an annotation rather than as part of the
# word, so it is a genuinely superscript glyph and is dimmed when rendered.
# cli substitutes a plain "1" for the superscript, which would read as part of
# the word when the marker sits flush against a label, so an asterisk stands in
# instead where the console cannot render the superscript. Resolved per call:
# cli::symbol re-evaluates against the current console, and a constant would
# bake in whatever the machine that built the package happened to support.
pmsims_footnote_marker <- function() {
  if (cli::is_utf8_output()) cli::symbol$sup_1 else "*"
}

pmsims_rule <- function(title = NULL) {
  list(kind = "rule", title = title)
}

pmsims_blank <- function() {
  list(kind = "blank")
}

pmsims_heading <- function(text, indent = 0L) {
  list(kind = "heading", text = text, indent = as.integer(indent))
}

pmsims_note <- function(text) {
  list(kind = "note", text = text)
}

# Returns NULL for an absent value, so that optional rows can be dropped by
# filtering the assembled list rather than by branching at every field.
pmsims_field <- function(
  label,
  value,
  indent = 2L,
  note = NULL,
  group = NA_character_,
  emphasis = "normal",
  marker = ""
) {
  if (!pmsims_is_present(value)) {
    return(NULL)
  }
  value <- as.character(value)[1]
  if (is.na(value)) {
    return(NULL)
  }
  list(
    kind = "field",
    label = as.character(label)[1],
    # A footnote marker sits flush against the label, but is held apart from it
    # so that it can be de-emphasised without dimming the label itself.
    marker = marker,
    value = value,
    indent = as.integer(indent),
    note = note,
    group = group,
    emphasis = emphasis
  )
}

pmsims_compact <- function(...) {
  Filter(Negate(is.null), list(...))
}

# A group of fields under a subheading, dropped entirely when no field in it
# has a value.
pmsims_section <- function(title, fields, indent = 0L) {
  fields <- Filter(Negate(is.null), fields)
  if (!length(fields)) {
    return(list())
  }
  c(list(pmsims_heading(title, indent = indent)), fields)
}

# -----------------------------------------------------------------------------
# Content
# -----------------------------------------------------------------------------

build_pmsims_items <- function(x, verbose = FALSE) {
  outcome <- x$outcome
  model <- x$model
  metric <- x$metric
  metric_2 <- x$metric_2
  target <- x$target_performance %||% x$minimum_acceptable_performance
  min_n <- x$min_n
  # min_n and perf_n hold a diagnostic string when the target could not be
  # reached. That message belongs on the sample-size line, not repeated as a
  # performance estimate.
  numeric_only <- function(v) if (is.numeric(v)) v else NULL
  perf_at <- numeric_only(x$perf_n)
  perf2_at <- numeric_only(x$metric_2_at_n)
  complexity <- x$complexity
  moa <- tolower(as.character(x$mean_or_assurance %||% "mean")[1])

  # A calibration slope obtained by searching on the CSSE scale internally is
  # flagged with a footnote marker and explained below the results, rather than
  # announced. The marker is defined once so that it cannot drift apart from the
  # footnote that explains it.
  derived_from_csse <- isTRUE(x$internal_csse)
  csse_mark <- if (derived_from_csse) pmsims_footnote_marker() else ""

  identifier <- function(id) {
    if (verbose && pmsims_is_present(id)) sprintf(" ('%s')", id) else ""
  }
  # Tagging an absent value would turn NA into the string "NA" and resurrect a
  # row that should have been dropped.
  with_identifier <- function(value, id) {
    if (!pmsims_is_present(value) || is.na(value)) {
      return(NA_character_)
    }
    paste0(value, identifier(id))
  }

  # --- Data-generating scenario ---------------------------------------------

  # nonlinear_strength is resolved to 0 for complexity 1 and 4, where it has no
  # effect, so only report it where it actually shapes the signal.
  nonlinear_strength <- if (
    pmsims_is_present(complexity) && complexity %in% c(2, 3)
  ) {
    pmsims_fmt_num(x$nonlinear_strength, 2)
  }
  # Objects from simulate_custom(), and those written before the 1.0 data
  # generators, carry predictor_type but no distribution; fall back to it.
  predictor_distribution <- x$predictor_distribution
  predictor_type <- if (is.null(predictor_distribution)) x$predictor_type
  predictor_prevalence <- if (identical(x$predictor_type, "binary")) {
    pmsims_fmt_num(x$binary_predictor_prevalence, 2)
  }
  signal_form <- pmsims_signal_form_label(complexity)
  if (verbose && !is.null(signal_form) && pmsims_is_present(complexity)) {
    signal_form <- sprintf("%s (complexity %s)", signal_form, complexity)
  }

  scenario <- pmsims_section(
    "Data-generating scenario",
    pmsims_compact(
      pmsims_field("Outcome", pmsims_outcome_label(outcome)),
      pmsims_field(
        "Prevalence",
        pmsims_fmt_num(x$outcome_prevalence %||% x$prevalence, 2)
      ),
      pmsims_field("Baseline hazard", pmsims_fmt_signif(x$baseline_hazard, 3)),
      pmsims_field("Censoring rate", pmsims_fmt_num(x$censoring_rate, 2)),
      pmsims_field(
        "Predictors",
        pmsims_predictor_counts(
          x$signal_parameters %||% x$parameters,
          x$noise_parameters
        )
      ),
      pmsims_field(
        "Predictor distribution",
        pmsims_distribution_label(predictor_distribution)
      ),
      pmsims_field("Predictor type", pmsims_distribution_label(predictor_type)),
      pmsims_field("Predictor prevalence", predictor_prevalence),
      pmsims_field("Predictor correlation", pmsims_fmt_num(x$correlation, 2)),
      pmsims_field("Signal form", signal_form),
      pmsims_field("Nonlinear strength", nonlinear_strength)
    )
  )

  # --- Model and performance -------------------------------------------------

  large_sample <- NULL
  cstatistic <- x$maximum_achievable_cstatistic %||%
    (if (identical(outcome, "binary")) x$cstatistic)
  cindex <- x$maximum_achievable_cindex %||%
    (if (identical(outcome, "survival")) x$cstatistic)
  rsquared <- x$maximum_achievable_rsquared %||% x$r2
  # Identifiers are appended to values rather than to labels: a label carrying
  # "maximum_achievable_cstatistic" would push the value column across the
  # screen for every other row as well.
  if (pmsims_is_present(cstatistic)) {
    large_sample <- pmsims_field(
      "Large-sample C-statistic",
      paste0(
        pmsims_fmt_num(cstatistic, 3),
        identifier("maximum_achievable_cstatistic")
      )
    )
  }
  if (pmsims_is_present(cindex)) {
    large_sample <- pmsims_field(
      "Large-sample C-index",
      paste0(
        pmsims_fmt_num(cindex, 3),
        identifier("maximum_achievable_cindex")
      )
    )
  }
  if (pmsims_is_present(rsquared)) {
    large_sample <- pmsims_field(
      "Large-sample R\u00B2",
      paste0(
        pmsims_fmt_num(rsquared, 3),
        identifier("maximum_achievable_rsquared")
      )
    )
  }

  metric_label <- pmsims_metric_label(metric, outcome)
  criterion <- if (pmsims_is_present(target)) {
    paste0(
      (metric_label %||% "Performance"),
      identifier(metric),
      " ",
      pmsims_metric_operator(metric, target),
      " ",
      pmsims_fmt_num(target, 3)
    )
  }

  model_performance <- pmsims_section(
    "Model and performance",
    pmsims_compact(
      pmsims_field(
        "Model",
        if (verbose) {
          paste0(pmsims_model_label(model, outcome), identifier(model))
        } else {
          pmsims_model_label(model, outcome)
        }
      ),
      large_sample,
      pmsims_field("Sample-size criterion", criterion)
    )
  )

  # --- Simulation ------------------------------------------------------------

  simulation_fields <- pmsims_compact(
    pmsims_field(
      "Mode",
      if (identical(moa, "assurance")) "Assurance" else "Mean"
    ),
    pmsims_field("Replications", pmsims_fmt_int(x$n_reps_total))
  )
  if (verbose) {
    search_range <- if (
      pmsims_is_present(x$min_sample_size) &&
        pmsims_is_present(x$max_sample_size)
    ) {
      paste0(
        pmsims_fmt_int(x$min_sample_size),
        " to ",
        pmsims_fmt_int(x$max_sample_size)
      )
    }
    simulation_fields <- c(
      simulation_fields,
      pmsims_compact(
        pmsims_field(
          "Replications per point",
          pmsims_fmt_int(x$n_reps_per)
        ),
        pmsims_field("Search method", x$method),
        pmsims_field("Search range", search_range),
        pmsims_field("Evaluation sample size", pmsims_fmt_int(x$test_n))
      )
    )
  }
  simulation <- pmsims_section("Simulation", simulation_fields)

  # A blank line between groups, but only between groups that survived.
  inputs <- list()
  for (section in Filter(
    length,
    list(scenario, model_performance, simulation)
  )) {
    if (length(inputs)) {
      inputs <- c(inputs, list(pmsims_blank()))
    }
    inputs <- c(inputs, section)
  }

  # --- Results ---------------------------------------------------------------

  # min_n is a diagnostic string when the target could not be reached, so it is
  # printed as it stands rather than formatted as a count.
  min_n_text <- if (is.numeric(min_n)) pmsims_fmt_int(min_n) else min_n
  minimum <- pmsims_field(
    "Minimum sample size",
    min_n_text,
    emphasis = if (is.numeric(min_n)) "strong" else "normal"
  )

  performance_heading <- if (is.numeric(min_n)) {
    sprintf("Performance at N = %s", pmsims_fmt_int(min_n))
  } else {
    "Performance at the selected sample size"
  }
  target_note <- if (pmsims_is_present(target)) {
    sprintf(
      "(target %s %s)",
      pmsims_metric_operator(metric, target),
      pmsims_fmt_num(target, 3)
    )
  }
  performance_fields <- pmsims_compact(
    pmsims_field(
      metric_label %||% "Performance",
      with_identifier(pmsims_fmt_num(perf_at, 3), metric),
      indent = 4L,
      note = target_note,
      group = "performance",
      marker = csse_mark
    ),
    pmsims_field(
      pmsims_metric_label(metric_2, outcome) %||% "Other metric",
      with_identifier(pmsims_fmt_num(perf2_at, 3), metric_2),
      indent = 4L,
      group = "performance"
    )
  )
  if (verbose && derived_from_csse) {
    performance_fields <- c(
      performance_fields,
      pmsims_compact(pmsims_field(
        "Search scale ('csse')",
        pmsims_fmt_num(x$csse_perf_n, 4),
        indent = 4L,
        note = if (pmsims_is_present(x$csse_target_performance)) {
          sprintf(
            "(target %s %s)",
            cli::symbol$geq,
            pmsims_fmt_num(x$csse_target_performance, 4)
          )
        },
        group = "performance"
      ))
    )
  }
  performance <- pmsims_section(
    performance_heading,
    performance_fields,
    indent = 2L
  )

  results <- pmsims_compact(minimum)
  if (length(performance)) {
    results <- c(results, list(pmsims_blank()), performance)
  }
  running_time <- pmsims_field(
    "Running time",
    pmsims_fmt_duration(x$simulation_time),
    emphasis = "muted"
  )
  if (!is.null(running_time)) {
    results <- c(results, list(pmsims_blank(), running_time))
  }

  # --- Notes -----------------------------------------------------------------

  notes <- list(pmsims_note(
    if (identical(moa, "assurance")) {
      "Assurance mode selects N so that the target is achieved with high probability across repeated datasets."
    } else {
      "Mean mode selects N so that the target is achieved on average across repeated datasets."
    }
  ))
  if (derived_from_csse) {
    notes <- c(
      notes,
      # Flush against its text, as on the row it refers to: a space after a
      # small raised glyph reads as a hole rather than as separation.
      list(pmsims_note(paste0(
        pmsims_footnote_marker(),
        "Derived from the calibration-slope squared error, on which the ",
        "search was run."
      )))
    )
  }

  items <- list()
  if (length(inputs)) {
    items <- c(items, list(pmsims_rule("Inputs"), pmsims_blank()), inputs)
  }
  if (length(results)) {
    items <- c(
      items,
      list(pmsims_blank(), pmsims_rule("Results"), pmsims_blank()),
      results
    )
  }
  if (length(notes)) {
    items <- c(items, list(pmsims_blank(), pmsims_rule()), notes)
  }
  items
}

# -----------------------------------------------------------------------------
# Rendering
# -----------------------------------------------------------------------------

render_pmsims_summary <- function(x, max_width = 80, verbose = FALSE) {
  scr_width <- min(getOption("width", 80L), as.integer(max_width))
  items <- build_pmsims_items(x, verbose = verbose)

  fields <- Filter(function(it) identical(it$kind, "field"), items)

  # One value column across the whole summary, so Inputs and Results line up.
  # An unusually long label overflows into a two-space gap rather than pushing
  # every other value across the screen.
  value_col <- if (length(fields)) {
    ends <- vapply(
      fields,
      function(it) it$indent + nchar(it$label) + nchar(it$marker %||% ""),
      numeric(1)
    )
    min(max(max(ends) + 2, 27), max(30, floor(scr_width / 2)))
  } else {
    27
  }

  # Notes such as "(target >= 0.900)" align on the widest value in their group.
  group_width <- list()
  for (it in fields) {
    if (!is.na(it$group)) {
      current <- group_width[[it$group]] %||% 0
      group_width[[it$group]] <- max(current, nchar(it$value))
    }
  }

  cli::cat_boxx(
    cli::style_bold(" pmsims: Sample size simulation summary "),
    float = "center",
    padding = 0,
    width = scr_width
  )
  cat("\n")

  for (it in items) {
    switch(
      it$kind,
      blank = cat("\n"),
      rule = if (is.null(it$title)) {
        cli::cat_rule(width = scr_width)
      } else {
        cli::cat_rule(center = it$title, width = scr_width)
      },
      heading = cat(
        strrep(" ", it$indent),
        if (it$indent == 0L) {
          cli::style_bold(cli::col_cyan(it$text))
        } else {
          cli::style_bold(it$text)
        },
        "\n",
        sep = ""
      ),
      note = {
        for (line in strwrap(it$text, width = scr_width)) {
          cat(cli::col_silver(cli::style_italic(line)), "\n", sep = "")
        }
      },
      field = cat(
        pmsims_render_field(it, value_col, group_width),
        "\n",
        sep = ""
      )
    )
  }

  invisible(NULL)
}

pmsims_render_field <- function(it, value_col, group_width) {
  label_style <- switch(
    it$emphasis,
    strong = cli::style_bold,
    muted = cli::col_silver,
    identity
  )
  value_style <- switch(
    it$emphasis,
    strong = function(txt) cli::style_bold(cli::col_blue(txt)),
    muted = cli::col_silver,
    identity
  )

  marker <- it$marker %||% ""
  gap <- max(value_col - it$indent - nchar(it$label) - nchar(marker), 2)
  line <- paste0(
    strrep(" ", it$indent),
    label_style(it$label),
    if (nzchar(marker)) cli::col_silver(marker) else "",
    strrep(" ", gap),
    value_style(it$value)
  )

  if (!is.null(it$note)) {
    width <- if (!is.na(it$group)) {
      group_width[[it$group]] %||% nchar(it$value)
    } else {
      nchar(it$value)
    }
    line <- paste0(
      line,
      strrep(" ", max(width - nchar(it$value), 0) + 4),
      cli::col_silver(it$note)
    )
  }

  line
}
