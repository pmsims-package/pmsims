args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2L) {
  stop(
    "Usage: configure_pkgdown_versions.R <_pkgdown.yml> <current-version>",
    call. = FALSE
  )
}

config_path <- args[[1L]]
current_version <- args[[2L]]

if (!file.exists(config_path)) {
  stop("pkgdown configuration not found: ", config_path, call. = FALSE)
}

config <- yaml::read_yaml(config_path)

# Every published version uses the same absolute destinations. The label is
# supplied by the build matrix so a release page never identifies itself as
# the development site (and vice versa).
config$navbar$structure$right <- c(
  "versions", "search", "github", "lightswitch"
)
config$navbar$components$versions <- list(
  text = current_version,
  menu = list(
    list(
      text = "dev",
      href = "https://pmsims-package.github.io/pmsims/dev/"
    ),
    list(
      text = "1.0.0 (stable)",
      href = "https://pmsims-package.github.io/pmsims/"
    ),
    list(
      text = "0.5.0",
      href = "https://pmsims-package.github.io/pmsims/0.5.0/"
    )
  )
)

yaml::write_yaml(config, config_path)
