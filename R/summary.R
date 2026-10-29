#' Detailed summary of a `pmsims` result
#'
#' The same display as [print.pmsims()], with the implementation-level detail
#' that the default print method hides: internal metric identifiers, the engine
#' settings used for the search, and any quantities recorded on an internal
#' search scale.
#'
#' @param object A `pmsims` object.
#' @param ... Currently unused.
#' @param max_width Maximum console width used for the summary.
#' @param verbose Logical. Set to `FALSE` for the default print display.
#'
#' @return `object`, invisibly.
#' @keywords internal
#' @export
summary.pmsims <- function(object, ..., max_width = 80, verbose = TRUE) {
  if (!inherits(object, "pmsims")) {
    stop("Object is not of class 'pmsims'")
  }
  render_pmsims_summary(
    object,
    max_width = max_width,
    verbose = isTRUE(verbose)
  )
  invisible(object)
}
