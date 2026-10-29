#' Optional dependency checks
#'
#' @param packages Character vector of package names.
#' @param feature Short description of the feature that needs them.
#'
#' @return Invisibly returns `TRUE` when all packages are available.
#' @keywords internal
#' @noRd
require_optional_packages <- function(packages, feature) {
  available <- vapply(
    packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )

  if (all(available)) {
    return(invisible(TRUE))
  }

  missing_pkgs <- packages[!available]
  stop(
    "The ",
    feature,
    " feature requires the following optional package",
    if (length(missing_pkgs) > 1) "s" else "",
    ": ",
    paste(missing_pkgs, collapse = ", "),
    ". Please install ",
    if (length(missing_pkgs) > 1) "them" else "it",
    " to use this feature.",
    call. = FALSE
  )
}
