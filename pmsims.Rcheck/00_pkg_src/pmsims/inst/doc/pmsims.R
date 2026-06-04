## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6, fig.height = 4,
  warning = FALSE,
  message = FALSE
)
vignette_cache_dir <- system.file(
  "extdata", "vignette-cache",
  package = "pmsims",
  mustWork = FALSE
)
if (vignette_cache_dir == "") {
  source_candidates <- c(
    file.path("..", "inst", "extdata", "vignette-cache"),
    file.path("inst", "extdata", "vignette-cache")
  )
  vignette_cache_dir <- source_candidates[dir.exists(source_candidates)][1]
}
if (is.na(vignette_cache_dir) || vignette_cache_dir == "") {
  stop("Could not locate vignette cache directory.")
}

## -----------------------------------------------------------------------------
# install.packages("remotes")
# remotes::install_github("pmsims-package/pmsims")
library(pmsims)

## -----------------------------------------------------------------------------
# set.seed(123)
# 
# binary_example <- simulate_binary(
#   signal_parameters = 20,
#   noise_parameters  = 0,
#   predictor_type = "continuous",
#   binary_predictor_prevalence = NULL,
#   outcome_prevalence = 0.30,
#   maximum_achievable_cstatistic = 0.80,
#   model = "glm",
#   metric = "calibration_slope",
#   target_performance = 0.85,
#   n_reps_total = 1000,
#   mean_or_assurance = "assurance"
# )
# 
# binary_example

## ----Run binary---------------------------------------------------------------
binary_example <- readRDS(file.path(vignette_cache_dir, "binary.rds"))
print(binary_example)

## ----fig.alt="Plot showing learning curve for binary outcome"-----------------
plot(binary_example)

## -----------------------------------------------------------------------------
# continuous_example <- simulate_continuous(
#   signal_parameters = 15,
#   noise_parameters = 0,
#   predictor_type = "continuous",
#   maximum_achievable_rsquared = 0.50,
#   model = "lm",
#   metric = "calibration_slope",
#   target_performance = 0.90,
#   n_reps_total = 1000,
#   mean_or_assurance = "assurance"
# )
# 
# continuous_example

## -----------------------------------------------------------------------------
continuous_example <- readRDS(file.path(vignette_cache_dir, "continuous.rds"))
print(continuous_example)

## ----fig.alt="Plot showing learning curve for continuous outcome"-------------
plot(continuous_example)

## -----------------------------------------------------------------------------
sessionInfo()

