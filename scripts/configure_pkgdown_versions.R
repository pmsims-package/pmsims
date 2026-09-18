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

# pkgdown renders the package version beside the navbar brand. Keep release
# notes consistent across every published site.
config$navbar$structure$right <- c(
  "search",
  "github",
  "lightswitch"
)
config$navbar$components$versions <- NULL
config$navbar$components$news <- list(
  text = "News",
  menu = list(
    list(
      text = "Releases"
    ),
    list(
      text = "Version 1.0.0",
      href = paste0(
        "https://pmsims-package.github.io/pmsims/",
        "articles/release-1-0-0.html"
      )
    ),
    list(
      text = "Version 0.5.0",
      href = paste0(
        "https://pmsims-package.github.io/pmsims/",
        "articles/release-0-5-0.html"
      )
    ),
    list(
      text = "--------"
    ),
    list(
      text = "Changelog",
      href = "https://pmsims-package.github.io/pmsims/news/index.html"
    )
  )
)

if (identical(current_version, "0.5.0")) {
  if (is.null(config$template$includes)) {
    config$template$includes <- list()
  }

  config$template$includes$before_body <- paste0(
    '<aside class="archive-banner" role="note">',
    '<div class="container">',
    '<span>You are viewing archived documentation for ',
    '<strong>pmsims 0.5.0</strong>.</span>',
    '<a href="https://pmsims-package.github.io/pmsims/">',
    'View current 1.0.0 documentation</a>',
    '</div>',
    '</aside>'
  )
  config$template$includes$before_navbar <- paste0(
    '<small class="nav-text archive-version me-auto" ',
    'data-bs-toggle="tooltip" data-bs-placement="bottom" ',
    'title="Archived documentation">0.5.0 (archived)</small>'
  )
}

yaml::write_yaml(config, config_path)
