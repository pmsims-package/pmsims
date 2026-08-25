args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1L) {
  stop(
    "Usage: configure_pkgdown_versions.R <_pkgdown.yml>",
    call. = FALSE
  )
}

config_path <- args[[1L]]

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
        "https://pmsims-package.github.io/pmsims/0.5.0/",
        "articles/release-0-5-0.html"
      )
    ),
    list(
      text = "--------"
    ),
    list(
      text = "Changelog",
      href = "news/index.html"
    )
  )
)

yaml::write_yaml(config, config_path)
