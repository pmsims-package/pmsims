#' @keywords internal
#' @export
summary.pmsims <- function(object, ...) {
  cat(
    "\n",
    "---------------------------------",
    "\n",
    "Target performance:     ",
    object$target_performance,
    "\n",
    # "Number of parameters:   ", result$parameters, "\n",
    "---------------------------------",
    "\n",
    "Minimum sample size:    ",
    object$min_n,
    "\n",
    "---------------------------------",
    "\n"
  )
  invisible(object)
}
