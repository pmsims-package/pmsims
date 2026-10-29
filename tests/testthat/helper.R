get_binary_data_model_metric <- function() {
  data_opts <- list(
    type = "binary",
    args = list(
      n_signal_parameters = 5,
      noise_parameters = 5,
      predictor_type = "continuous",
      mu_lp = stats::qlogis(0.2),
      baseline_prob = 0.2,
      # Hard beta_signal coded for speed from tuning code commented out below
      beta_signal = 0.6124837
    )
  )
  data_function <- default_data_generators(data_opts)
  outcome_type <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome_type, model = "glm")

  metric_function <- default_metric_generator(
    "auc",
    data_function
  )

  # tuning_parameter <- default_tune(tune_param = beta_signal,
  #                                  max_sample_size = 10000,
  #                                  large_sample_performance = 0.8,
  #                                  data_function = data_function,
  #                                  model_function = model_function,
  #                                  metric_function = metric_function)

  return(list(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function
  ))
}

make_binary_fixture_data <- function() {
  data.frame(
    y = c(0, 1, 0, 1, 1, 0),
    x1 = c(-2, -1, 0, 1, 2, 3),
    x2 = c(0, 1, 0, 1, 0, 1)
  )
}

make_continuous_fixture_data <- function() {
  data.frame(
    y = c(1.0, 1.5, 2.0, 2.5, 3.0, 3.5),
    x1 = c(0, 1, 2, 3, 4, 5),
    x2 = c(1, 1, 0, 0, 1, 1)
  )
}

make_survival_fixture_data <- function() {
  data.frame(
    time = c(5, 8, 12, 4, 9, 7, 15, 6, 11, 10, 14, 13, 16, 18, 17, 20),
    event = c(1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0),
    x1 = c(
      0.2,
      -0.5,
      0.4,
      1.1,
      -0.8,
      0.6,
      -0.2,
      0.1,
      0.7,
      -0.4,
      0.3,
      -0.1,
      0.5,
      -0.6,
      0.8,
      -0.3
    ),
    x2 = c(0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0)
  )
}

make_binary_data_function <- function(
  n_signal_parameters = 3,
  noise_parameters = 0,
  baseline_prob = 0.2,
  beta_signal = 0.2
) {
  default_data_generators(
    list(
      type = "binary",
      args = list(
        n_signal_parameters = n_signal_parameters,
        noise_parameters = noise_parameters,
        predictor_type = "continuous",
        mu_lp = stats::qlogis(baseline_prob),
        baseline_prob = baseline_prob,
        beta_signal = beta_signal
      )
    )
  )
}

make_continuous_data_function <- function(
  n_signal_parameters = 3,
  noise_parameters = 0,
  beta_signal = 0.2
) {
  default_data_generators(
    list(
      type = "continuous",
      args = list(
        n_signal_parameters = n_signal_parameters,
        noise_parameters = noise_parameters,
        predictor_type = "continuous",
        beta_signal = beta_signal
      )
    )
  )
}

make_survival_data_function <- function(
  n_signal_parameters = 3,
  noise_parameters = 0,
  beta_signal = 0.2,
  baseline_hazard = 0.01,
  censoring_rate = 0.2
) {
  default_data_generators(
    list(
      type = "survival",
      args = list(
        n_signal_parameters = n_signal_parameters,
        noise_parameters = noise_parameters,
        predictor_type = "continuous",
        beta_signal = beta_signal,
        baseline_hazard = baseline_hazard,
        censoring_rate = censoring_rate
      )
    )
  )
}

make_metric_stub <- function(metric = "auc") {
  fn <- function(data, fit, model) {
    0.8
  }
  attr(fn, "metric") <- metric
  fn
}

# The printed summary is styled with cli, so strip any ANSI escapes before
# matching on its text.
capture_pmsims_output <- function(expr) {
  cli::ansi_strip(paste(capture.output(expr), collapse = "\n"))
}

# The lines between the "Results" rule and the closing rule. cli draws rules
# with "-" rather than a box-drawing character when unicode is unavailable, as
# it is under testthat's reproducible output.
pmsims_results_section <- function(output) {
  lines <- strsplit(output, "\n", fixed = TRUE)[[1]]
  start <- grep("Results", lines, fixed = TRUE)[1]
  closing <- grep("^[-\u2500]+$", lines)
  end <- closing[closing > start][1]
  lines[seq(start + 1L, end - 1L)]
}

count_matches <- function(text, pattern) {
  found <- gregexpr(pattern, text, fixed = TRUE)[[1]]
  if (identical(as.integer(found), -1L)) 0L else length(found)
}

make_minimal_pmsims_object <- function(
  metric = "auc",
  target_performance = 0.8,
  mean_or_assurance = "mean"
) {
  structure(
    list(
      outcome = "binary",
      predictor_type = "continuous",
      signal_parameters = 5L,
      noise_parameters = 2L,
      outcome_prevalence = 0.2,
      maximum_achievable_cstatistic = 0.85,
      model = "glm",
      metric = metric,
      metric_2 = "calibration_slope",
      target_performance = target_performance,
      min_n = 100,
      perf_n = 0.82,
      metric_2_at_n = 0.98,
      n_reps_total = 40L,
      mean_or_assurance = mean_or_assurance,
      simulation_time = 12,
      mlpwr_ds = list(
        data = list(
          list(x = 50, y = c(0.70, 0.72)),
          list(x = 100, y = c(0.81, 0.83))
        ),
        fit = list(fitfun = function(n) 0.6 + (0.002 * n)),
        boundaries = c(50, 100),
        final = list(design = 100, power = 0.82),
        aggregate_fun = function(x) mean(x, na.rm = TRUE)
      )
    ),
    class = "pmsims"
  )
}
