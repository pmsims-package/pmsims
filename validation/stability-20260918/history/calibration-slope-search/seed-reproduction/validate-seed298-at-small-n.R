# Independently test seed 298's tuned generator at the N returned by seed 1.
# No GP is run and this fixed validation N is not a new search estimate.
# Run from the repository root.
source("validation/calibration-slope-search/seed-reproduction/reproduce.R")
load_original_pmsims(".")
out_dir <- "validation/calibration-slope-search/seed-reproduction/runs"
n <- read.csv(file.path(out_dir, "seed1-full-summary.csv"))$min_n

capture_functions <- function() {
  functions <- NULL
  engine <- function(data_function, model_function, metric_function, ...) {
    functions <<- list(
      data = data_function,
      fit = model_function,
      metric = metric_function
    )
    stop("original_generator_recorded", call. = FALSE)
  }
  testthat::local_mocked_bindings(calculate_mlpwr = engine, .package = "pmsims")
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(298L)
  tryCatch(
    pmsims::simulate_binary(
      signal_parameters = 5L,
      noise_parameters = 0L,
      complexity = 1L,
      outcome_prevalence = .20,
      maximum_achievable_cstatistic = .60,
      model = "glm",
      metric = "calibration_slope",
      target_performance = .95,
      n_reps_total = 1000L,
      mean_or_assurance = "assurance",
      progress = FALSE
    ),
    error = function(e) {
      if (is.null(functions)) stop(e)
    }
  )
  functions
}
functions <- capture_functions()
workers <- if (.Platform$OS.type == "windows") 1L else 4L
reps <- 1000L
values <- unlist(
  parallel::mclapply(
    seq_len(reps),
    function(i) {
      set.seed(7000000L + 298L * 10000L + i)
      test <- functions$data(30000L)
      train <- functions$data(n)
      fit <- functions$fit(train)
      functions$metric(test, fit, "glm")
    },
    mc.cores = workers,
    mc.set.seed = FALSE
  ),
  use.names = FALSE
)
stopifnot(length(values) == reps, all(is.finite(values)))
ordered <- sort(values)
ci <- ordered[c(qbinom(.025, reps, .2), qbinom(.975, reps, .2) + 1L)]
row <- data.frame(
  seed = 298L,
  n = n,
  reps = reps,
  empirical_q20 = unname(quantile(values, .2, type = 1)),
  ci_low = ci[1L],
  ci_high = ci[2L],
  target = .95
)
prefix <- file.path(out_dir, "seed298-at-seed1-n")
saveRDS(values, paste0(prefix, "-validation.rds"))
write.csv(row, paste0(prefix, "-validation.csv"), row.names = FALSE)
writeLines(
  c(
    paste("Original commit:", original_commit),
    "Validation N comes from seed 1; tuning uses seed 298; no seed-298 GP was run.",
    capture.output(sessionInfo())
  ),
  paste0(prefix, "-session.txt")
)
print(row)
