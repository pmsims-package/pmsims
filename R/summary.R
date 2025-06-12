#' @export
summary.pmsims <- function(result) {
  cat(
    "\n",
    "---------------------------------",
    "\n",
    "Target performance:     ",
    result$target_performance,
    "\n",
    # "Number of parameters:   ", result$parameters, "\n",
    "---------------------------------",
    "\n",
    "Minimum sample size:    ",
    result$min_n,
    "\n",
    "---------------------------------",
    "\n"
  )
}
