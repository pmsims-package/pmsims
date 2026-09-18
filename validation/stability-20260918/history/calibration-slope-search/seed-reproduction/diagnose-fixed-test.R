# Run from the repository root. This records the ONE fixed test draw that the
# original preliminary search uses, not a population validation study.
source("validation/calibration-slope-search/seed-reproduction/reproduce.R")
load_original_pmsims(".")

diagnose <- function(seed) {
  row <- NULL
  inspect_engine <- function(
    data_function,
    metric_function,
    target_performance,
    c_statistic,
    mean_or_assurance,
    test_n,
    ...
  ) {
    # Preserve the original operations before stage 1 draws its test dataset.
    pmsims:::compute_start_sample_sizes(
      data_function,
      metric_function,
      target_performance,
      c_statistic,
      mean_or_assurance
    )
    f <- formals(data_function)
    test <- data_function(test_n)
    true_lp <- f$mu_lp + f$beta_signal * rowSums(test[, -1, drop = FALSE])
    slope <- unname(coef(glm(test$y ~ true_lp, family = binomial()))[2L])
    row <<- data.frame(
      seed = seed,
      fixed_test_n = test_n,
      fixed_test_draws = 1L,
      tuned_beta = f$beta_signal,
      tuned_mu = f$mu_lp,
      true_lp_slope_on_fixed_test = slope,
      population_true_lp_slope = 1
    )
    stop("fixed_test_recorded", call. = FALSE)
  }
  testthat::local_mocked_bindings(
    calculate_mlpwr = inspect_engine,
    .package = "pmsims"
  )
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
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
      if (is.null(row)) stop(e)
    }
  )
  row
}
rows <- do.call(rbind, lapply(c(1L, 298L), diagnose))
write.csv(
  rows,
  "validation/calibration-slope-search/seed-reproduction/fixed-test-diagnostic.csv",
  row.names = FALSE
)
print(rows)
