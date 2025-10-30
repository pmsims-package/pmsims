#' @export
print.pmsims <- function(x, ...) {
  if (!inherits(x, "pmsims")) {
    stop("Object is not of class 'pmsims'")
  }

  ## 1) Input parameters - print a compact list of commonly useful fields if present
  cat("Input parameters:\n")
  fields_to_show <- c("outcome", "predictor_type", "parameters", "noise_parameters",
                      "prevalence", "baseline_hazard", "censoring_rate",
                      "cstatistic", "r2", "target_performance", "model", "metric",
                      "n_reps_total", "mean_or_assurance")

  # show those available plus any small scalar items commonly present
  shown_any <- FALSE
  for (nm in fields_to_show) {
    if (!is.null(x[[nm]])) {
      val <- x[[nm]]
      # format the value nicely depending on its type
      if (length(val) > 5) {
        val_str <- paste(head(val, 5), collapse = ", ")
        val_str <- paste0(val_str, " ...")
      } else if (is.atomic(val)) {
        val_str <- paste(val, collapse = ", ")
      } else if (is.list(val)) {
        val_str <- paste0("<list of length ", length(val), ">")
      } else {
        val_str <- as.character(val)
      }

      cat("  - ", nm, ": ", val_str, "\n", sep = "")
      shown_any <- TRUE
    }
  }
  # if none of the above printed, print top-level names and small values
  if (!shown_any) {
    top_names <- names(x)
    # print only scalar-ish entries
    scalar_names <- top_names[sapply(x, function(el) {
      (is.atomic(el) && length(el) <= 5) || is.null(el)
    })]
    if (length(scalar_names) > 0) {
      for (nm in scalar_names) {
        if (!is.null(x[[nm]])) {
          cat("  - ", nm, ": ", paste0(capture.output(str(x[[nm]])), collapse = " "), "\n", sep = "")
        }
      }
    } else {
      cat("  <no compact scalar input parameters found on object — inspect object manually>\n")
    }
  }
  cat("\n")

  ## 2) Final estimate of minimum sample size
  min_n <- if (!is.null(x$min_n)) x$min_n else NA
  cat("Final estimate of minimum sample size: ", min_n, "\n\n", sep = "")

  ## 3) Estimated performance at that sample size — try to extract mlpwr / mean performance
  perf_val <- if (!is.null(x$perf_n)) x$perf_n else NA
  cat("Estimated performance at sample size: ", round(perf_val,3), "\n\n", sep = "")


  ## 4) Running time
  if (!is.null(x$simulation_time)) {
    simt <- x$simulation_time
    # print nicely if it's difftime
    if (inherits(simt, "difftime")) {
      cat("Running time: ", format(simt), "\n", sep = "")
    } else {
      cat("Running time: ", paste0(capture.output(str(simt)), collapse = " "), "\n", sep = "")
    }
  } else {
    cat("Running time: <not available>\n")
  }

}
