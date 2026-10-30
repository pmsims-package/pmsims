# Precompute the sample-size searches in clustered_data.Rmd.
#
# Run from the package root:
#
#   Rscript vignettes/articles/clustered_data_precompute.R
#
# The article's code is extracted with knitr::purl() and run in order, so the
# results always come from the functions shown in the article. Chunks marked
# `eval = FALSE` (the searches) are run here; chunks marked `include = FALSE`
# (which load these results) are skipped. Needs pmsims >= 1.1.0 (the curve
# engine) and glmmTMB. The searches take about 12 minutes on 10 cores.

rmd <- "vignettes/articles/clustered_data.Rmd"
out <- "vignettes/articles/clustered_data.rds"

script <- readLines(knitr::purl(
  rmd,
  output = tempfile(fileext = ".R"),
  documentation = 1,
  quiet = TRUE
))

# Split the purled script into chunks at knitr's "## ----label, options----"
# headers.
header <- grepl("^## ----", script)
chunks <- split(script, cumsum(header))
code <- unlist(lapply(chunks, function(chunk) {
  if (!grepl("^## ----", chunk[1])) {
    return(chunk)
  }
  if (grepl("include *= *FALSE", chunk[1])) {
    return(character(0))
  }
  if (grepl("eval *= *FALSE", chunk[1])) {
    # purl comments out the code of chunks that are not evaluated.
    return(sub("^# ?", "", chunk[-1]))
  }
  chunk[-1]
}))

grDevices::pdf(NULL)
env <- new.env()
eval(parse(text = code), envir = env)

# The data, model and metric functions are dropped from the results: their
# environment holds every object the article created, including large
# datasets, and the article only needs the results.
strip <- function(result) {
  result[c("data_function", "model_function", "metric_function")] <- NULL
  result
}

saveRDS(
  list(
    calibration = strip(env$result),
    discrimination = strip(env$result_c),
    scenarios = env$scenarios,
    created = Sys.time(),
    pmsims_version = as.character(utils::packageVersion("pmsims")),
    glmmTMB_version = as.character(utils::packageVersion("glmmTMB"))
  ),
  out
)
