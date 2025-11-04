#' @keywords internal
#' @export
print.pmsims <- function(x, ..., max_width = 80) {
  if (!inherits(x, "pmsims")) stop("Object is not of class 'pmsims'")

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  has_cli    <- requireNamespace("cli", quietly = TRUE)
  has_crayon <- requireNamespace("crayon", quietly = TRUE)
  has_tools  <- requireNamespace("tools", quietly = TRUE)

  bold   <- function(txt) if (has_crayon) crayon::bold(txt)   else txt
  cyan   <- function(txt) if (has_crayon) crayon::cyan(txt)   else txt
  blue   <- function(txt) if (has_crayon) crayon::blue(txt)   else txt
  dimc   <- function(txt) if (has_crayon) crayon::silver(txt) else txt
  italic <- function(txt) if (has_crayon) crayon::italic(txt) else txt

  scr_width <- min(getOption("width", 80L), as.integer(max_width))

  rule <- function(title = NULL) {
    if (has_cli) {
      if (is.null(title)) cli::cat_rule(width = scr_width)
      else cli::cat_rule(center = as.character(title), width = scr_width)
    } else {
      if (!is.null(title)) cat(title, "\n", sep = "")
      cat(strrep("-", scr_width), "\n", sep = "")
    }
  }

  is_present <- function(v) !is.null(v) && !(length(v) == 1L && is.na(v))
  plural <- function(n, unit) sprintf("%d %s", n, if (n == 1) unit else paste0(unit, "s"))

  fmt_duration <- function(simt) {
    secs <- if (inherits(simt, "difftime")) as.numeric(simt, units = "secs")
    else if (is.numeric(simt)) as.numeric(simt) else return("<NA>")
    if (is.na(secs)) return("<NA>")
    secs <- round(secs)
    d  <- secs %/% 86400; rem <- secs %% 86400
    h  <- rem  %/% 3600 ; rem <- rem %% 3600
    m  <- rem  %/% 60   ; s   <- rem %% 60
    parts <- character(0)
    if (d > 0) parts <- c(parts, plural(d, "day"))
    if (h > 0) parts <- c(parts, plural(h, "hour"))
    if (m > 0) parts <- c(parts, plural(m, "minute"))
    if (s > 0 || length(parts) == 0) parts <- c(parts, plural(s, "second"))
    paste(parts, collapse = " ")
  }

  fmt_num <- function(v, digits = 3) {
    if (!is_present(v)) return("<NA>")
    if (is.numeric(v) && length(v) == 1L) formatC(v, format = "f", digits = digits)
    else paste(v, collapse = ", ")
  }
  fmt_int <- function(v) {
    if (!is_present(v)) return("<NA>")
    format(v, big.mark = ",", scientific = FALSE)
  }

  pretty_metric <- function(metric) {
    if (!is_present(metric)) return(NULL)
    m <- tolower(metric)
    nice <- switch(
      m,
      "calib_slope" = "Calibration slope",
      "cstatistic"  = "C-statistic",
      "brier"       = "Brier score",
      "rmse"        = "RMSE",
      {label <- gsub("_", " ", metric)
      if (has_tools) tools::toTitleCase(label) else label}
    )
    paste0(nice, " ", dimc(sprintf("('%s')", metric)))
  }

  moa        <- x$mean_or_assurance %||% "mean"
  model      <- x$model %||% NA_character_
  target     <- x$target_performance %||% x$minimum_acceptable_performance %||% NA_real_
  metric     <- x$metric %||% NA_character_
  min_n      <- x$min_n %||% NA
  perf_at    <- x$perf_n %||% NA
  simtime    <- x$simulation_time %||% NA
  cstatistic <- x$cstatistic
  r2         <- x$r2

  # --- Inputs table (non-null only)
  inputs <- list(
    "Outcome"                      = x$outcome,
    "Predictor type"               = x$predictor_type,
    "Number of predictors"         = x$parameters,
    "Noise predictors"             = x$noise_parameters,
    "Prevalence"                   = x$prevalence,
    "Baseline hazard"              = x$baseline_hazard,
    "Censoring rate"               = x$censoring_rate
  )
  if (is_present(cstatistic))
    inputs[["Expected large-sample performance"]] <- paste0("C-statistic ('cstatistic') = ", fmt_num(cstatistic, 3))
  if (is_present(r2))
    inputs[["Expected large-sample performance"]] <- paste0("R² ('r2') = ", fmt_num(r2, 3))
  if (is_present(metric) || is_present(target))
    inputs[["Target for chosen performance metric"]] <- paste0(pretty_metric(metric), " = ", fmt_num(target, 3))
  inputs[["Model"]]           <- model
  inputs[["Simulation reps"]] <- fmt_int(x$n_reps_total)
  keep_inputs <- vapply(inputs, is_present, logical(1))
  inputs <- inputs[keep_inputs]

  # --- Results table
  results <- list(
    "Final minimum sample size"  = bold(fmt_int(min_n)),
    "Estimated performance at N" = paste0(fmt_num(perf_at, 3),
                                          " (", pretty_metric(metric),
                                          " = ", fmt_num(target, 3), ")"),
    "Model"                      = model,
    "Mode"                       = if (tolower(moa) == "assurance") "Assurance" else "Mean",
    "Running time"               = fmt_duration(simtime)
  )
  keep_results <- vapply(results, is_present, logical(1))
  results <- results[keep_results]

  # --- Header box
  if (has_cli) {
    cli::cat_boxx(
      bold(" pmsims: Sample size simulation summary "),
      float = "center",
      padding = 0,
      width = scr_width
    )
  } else {
    cat("\n", bold("pmsims: Sample size simulation summary"), "\n", sep = "")
  }

  # --- Shared alignment width for BOTH tables
  shared_w <- max(nchar(c(names(inputs), names(results))), 25L)

  # --- Inputs (aligned two-column, cyan labels)
  rule("Inputs")
  for (nm in names(inputs)) {
    label <- format(nm, width = shared_w, justify = "right")
    cat("  ", cyan(label), " : ", inputs[[nm]], "\n", sep = "")
  }

  # --- Results (aligned two-column, blue labels)
  rule("Results")
  for (nm in names(results)) {
    label <- format(nm, width = shared_w, justify = "right")
    cat("  ", blue(label), " : ", results[[nm]], "\n", sep = "")
  }

  cat("    ", dimc(if (tolower(moa) == "assurance")
    italic("Assurance mode ensures the target metric is met with high probability across repeated datasets.")
    else
    italic("Mean mode ensures the target metric is met on average across datasets.")), "\n", sep = "")

  invisible(x)
}
