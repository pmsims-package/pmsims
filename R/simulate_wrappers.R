#' Minimum sample size for binary-outcome prediction models
#'
#' Compute the minimum sample size required to develop a prediction model with a
#' binary outcome. The function wraps a simulation-based engine that combines a
#' bisection search with Gaussian-process curve fitting. From user inputs
#' (outcome prevalence, expected large-sample performance, minimum acceptable
#' performance, etc.) it constructs a data-generating function, a model-fitting
#' function, and a metric function, then searches for the smallest \eqn{n} that
#' meets the chosen performance criterion.
#'
#' @section Criteria:
#' Two formulations are supported.
#'
#' - **Mean-based**: find the smallest \eqn{n} such that the expected model
#'   performance exceeds the target \eqn{M^*}, i.e.
#'   \deqn{\min_n \; \mathbb{E}_{D_n}\{ M \mid D_n \} \ge M^*.}
#'
#' - **Assurance-based**: find the smallest \eqn{n} such that the probability
#'   the performance exceeds \eqn{M^*} is at least \eqn{\delta} (e.g. 0.80),
#'   i.e.
#'   \deqn{\min_n \; \mathbb{P}_{D_n}\!\left( M \mid D_n \ge M^* \right) \ge \delta.}
#'
#' Here, \eqn{M} is the chosen performance metric and the probability/expectation
#' is over repeated samples of training data of size \eqn{n}. The assurance
#' criterion explicitly accounts for variability across training sets; models
#' with higher variance typically require larger \eqn{n} to satisfy it.
#'
#' @param signal_parameters Integer. Number of candidate predictors associated
#'   with the outcome (i.e., true signal features).
#' @param noise_parameters Integer. Number of candidate predictors not
#'   associated with the outcome (noise features). Default is 0.
#' @param predictor_type Character string, either `"continuous"` or `"binary"`.
#'   Specifies the type of simulated candidate predictors.
#' @param binary_predictor_prevalence Optional numeric in (0, 1). Prevalence of
#'   the binary predictors when `predictor_type = "binary"`. Ignored otherwise.
#' @param outcome_prevalence Numeric in (0, 1). Target prevalence of the binary
#'   outcome in the intended modelling context.
#' @param large_sample_cstatistic Numeric in (0, 1). Expected C-statistic for a
#'   model developed on a very large sample (used to tune the data-generating
#'   mechanism).
#' @param model Character string specifying the modelling algorithm (e.g.,
#'   `"glm"`). Passed to the internal model generator.
#' @param metric Character string naming the performance metric used to assess
#'   the sample size; defaults to `"calibration_slope"`. (Internally mapped to
#'   the engine's metric identifiers.)
#' @param minimum_acceptable_performance Numeric. The target threshold
#'   \eqn{M^\\*}; the algorithm searches for the smallest \eqn{n} meeting the
#'   chosen criterion with respect to this threshold.
#' @param n_reps_total Integer. Total number of simulation replications used by
#'   the engine across the search.
#' @param mean_or_assurance Character string, either `"mean"` or `"assurance"`.
#'   Controls whether the minimum \eqn{n} is defined by the mean-based criterion
#'   or the assurance-based criterion (with the assurance level \eqn{\delta}
#'   controlled by the engine's defaults or additional arguments in `...`).
#' @param ... Additional options passed to [simulate_custom()] (e.g., assurance
#'   level \eqn{\delta}, per-iteration settings).
#'
#' @return An object of class `"pmsims"` containing the estimated minimum sample
#'   size and simulation diagnostics (inputs, fitted GP curve, intermediate
#'   evaluations, and summary metrics).
#'
#' @seealso [simulate_continuous()], [simulate_survival()], [simulate_custom()]
#'
#' @examples
#' \dontrun{
#' est <- simulate_binary(
#'   signal_parameters = 10,
#'   noise_parameters = 10,
#'   predictor_type = "continuous",
#'   outcome_prevalence = 0.2,
#'   large_sample_cstatistic = 0.75,
#'   metric = "calibration_slope",
#'   minimum_acceptable_performance = 0.9,
#'   n_reps_total = 1000,
#'   mean_or_assurance = "assurance"
#' )
#' est
#' }
#' @export
simulate_binary <- function(
  signal_parameters,                  # Predictors
  noise_parameters = 0,
  predictor_type = "continuous",
  binary_predictor_prevalence = NULL,
  outcome_prevalence,                 # Outcome
  large_sample_cstatistic,
  model = "glm",                      # Model
  metric = "calibration_slope",       # Performance
  minimum_acceptable_performance,
  n_reps_total = 1000,                # Engine control
  mean_or_assurance = "assurance",
  ...
) {
  # Tune for data function
  tune_param <- binary_tuning(
    target_prevalence = outcome_prevalence,
    target_performance = large_sample_cstatistic,
    candidate_features = signal_parameters,
    proportion_noise_features = noise_parameters
  )[c(1, 3)] # extract mean of linear predictor as new intercept and beta_signal scaled by var of lp

  data_spec <- list(
    type = "binary",
    args = list(
      mu_lp = tune_param[1],
      beta_signal = tune_param[2],
      n_signal_parameters = signal_parameters,
      noise_parameters = noise_parameters,
      predictor_type = predictor_type,
      predictor_prop = binary_predictor_prevalence,
      baseline_prob = outcome_prevalence
    )
  )

  data_function <- default_data_generators(data_spec)
  outcome_type <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome_type, model)

  # Redefine metrics to internal syntax lang
  metric = ifelse(metric == "calibration_slope", "calib_slope", metric)

  suppressWarnings(
    output <- simulate_custom(
      metric_function = default_metric_generator(metric, data_function),
      target_performance = minimum_acceptable_performance,
      c_statistic = large_sample_cstatistic,
      data_function = data_function,
      model_function = model_function,
      min_sample_size = NULL,
      max_sample_size = NULL,
      se_final = NULL,
      n_reps_total = n_reps_total,
      n_reps_per = 20,
      method = "mlpwr-bs",
      mean_or_assurance = mean_or_assurance,
      test_n = 30000
    )
  )

  output$parameters <- signal_parameters
  output$noise_parameters <- noise_parameters
  output$predictor_type <- predictor_type
  output$binary_predictor_prevalence <- output$predictor_type
  output$prevalence <- outcome_prevalence
  output$cstatistic <- large_sample_cstatistic
  output$model <- model
  output$metric <- metric
  output$n_reps_total <- n_reps_total
  output$mean_or_assurance <- mean_or_assurance
  est <- output
  class(est) <- "pmsims"
  est
}

#' Minimum sample size for continuous‐outcome prediction models
#'
#' Compute the minimum sample size required to develop a prediction model with a
#' **continuous** outcome. This wraps the same simulation engine as
#' [simulate_binary()], combining bisection search with Gaussian-process
#' learning-curve modelling. From user inputs (expected large-sample
#' performance, minimum acceptable performance, etc.) it constructs a
#' data-generating function, model-fitting function, and metric function, then
#' searches for the smallest \eqn{n} meeting the chosen criterion.
#'
#' @inheritSection simulate_binary Criteria
#'
#' @inheritParams simulate_binary
#' @param large_sample_rsquared Numeric in (0, 1). Expected large-sample
#'   \eqn{R^2} for the model (used to tune the data-generating mechanism so that
#'   the model attains this performance for very large \eqn{n}).
#'
#' @return An object of class `"pmsims"` containing the estimated minimum sample
#'   size and simulation diagnostics (inputs, fitted GP curve, intermediate
#'   evaluations, and summary metrics).
#'
#' @seealso [simulate_binary()], [simulate_survival()], [simulate_custom()]
#'
#' @examples
#' \dontrun{
#' est <- simulate_continuous(
#'   signal_parameters = 8,
#'   noise_parameters = 8,
#'   predictor_type = "continuous",
#'   large_sample_rsquared = 0.50,
#'   metric = "calibration_slope",
#'   minimum_acceptable_performance = 0.9,
#'   n_reps_total = 1000,
#'   mean_or_assurance = "assurance"
#' )
#' est
#' }
#' @export
simulate_continuous <- function(
  signal_parameters,
  noise_parameters = 0,
  predictor_type = "continuous",
  binary_predictor_prevalence = NULL,
  large_sample_rsquared,
  model = "lm",
  metric = "calibration_slope",
  minimum_acceptable_performance,
  n_reps_total = 1000,
  mean_or_assurance = "assurance",
  ...
) {
  # Tuning the data-generating function
  tune_param <- continuous_tuning(
    r2 = large_sample_rsquared,
    candidate_features = signal_parameters,
    proportion_noise_features = noise_parameters
  )

  data_spec <- list(
    type = "continuous",
    args = list(
      beta_signal = tune_param,
      n_signal_parameters = signal_parameters,
      noise_parameters = noise_parameters,
      predictor_type = predictor_type,
      predictor_prop = binary_predictor_prevalence
    )
  )

  data_function <- default_data_generators(data_spec)
  outcome_type <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome_type, model)

  metric <- ifelse(metric == "calibration_slope", "calib_slope", metric)

  suppressWarnings(
    output <- simulate_custom(
      metric_function = default_metric_generator(metric, data_function),
      target_performance = minimum_acceptable_performance,
      c_statistic = large_sample_rsquared,
      data_function = data_function,
      model_function = model_function,
      min_sample_size = NULL,
      max_sample_size = NULL,
      se_final = NULL,
      n_reps_total = n_reps_total,
      n_reps_per = 20,
      method = "mlpwr-bs",
      mean_or_assurance = mean_or_assurance,
      test_n = 30000
    )
  )

  output$parameters <- signal_parameters
  output$noise_parameters <- noise_parameters
  output$predictor_type <- predictor_type
  output$binary_predictor_prevalence <- output$predictor_type
  output$r2 <- large_sample_rsquared
  output$model <- model
  output$metric <- metric
  output$n_reps_total <- n_reps_total
  output$mean_or_assurance <- mean_or_assurance
  class(output) <- "pmsims"
  output
}

#' Minimum sample size for survival‐outcome prediction models
#'
#' Compute the minimum sample size required to develop a prediction model with a
#' **time-to-event (survival)** outcome. As with the other wrappers, this uses a
#' simulation-based learning-curve approach with Gaussian-process surrogate
#' modelling to locate the smallest \eqn{n} meeting the chosen performance
#' criterion.
#'
#' @inheritSection simulate_binary Criteria
#'
#' @inheritParams simulate_binary
#' @param large_sample_cindex Numeric in (0, 1). Expected large-sample
#'   C-index for the survival model (used to tune the data-generating mechanism
#'   so that the model attains this performance for very large \eqn{n}).
#' @param baseline_hazard Numeric > 0. Baseline hazard level used by the
#'   data-generating mechanism (e.g., the constant hazard in an exponential
#'   baseline). Larger values imply shorter event times, all else equal.
#' @param censoring_rate Numeric in [0, 1). Proportion of individuals expected
#'   to be censored in the simulated datasets (administrative or random
#'   censoring). Higher values imply fewer observed events for a fixed \eqn{n}.
#' @param model Character string; currently `"coxph"` (Cox proportional hazards).
#'
#' @return An object of class `"pmsims"` containing the estimated minimum sample
#'   size and simulation diagnostics (inputs, fitted GP curve, intermediate
#'   evaluations, and summary metrics).
#'
#' @seealso [simulate_binary()], [simulate_continuous()], [simulate_custom()]
#'
#' @examples
#' \dontrun{
#' est <- simulate_survival(
#'   signal_parameters = 10,
#'   noise_parameters = 10,
#'   predictor_type = "continuous",
#'   large_sample_cindex = 0.70,
#'   baseline_hazard = 0.01,
#'   censoring_rate = 0.30,
#'   metric = "calibration_slope",
#'   minimum_acceptable_performance = 0.9,
#'   n_reps_total = 1000,
#'   mean_or_assurance = "assurance"
#' )
#' est
#' }
#' @export
simulate_survival <- function(
  signal_parameters,
  noise_parameters = 0,
  predictor_type = "continuous",
  binary_predictor_prevalence = NULL,
  large_sample_cindex,
  baseline_hazard = 1,
  censoring_rate,
  model = "coxph",
  metric = "calibration_slope",
  minimum_acceptable_performance,
  n_reps_total = 1000,
  mean_or_assurance = "assurance",
  ...
) {
  # Tune the data-generating function
  tune_param <- binary_tuning(
    target_prevalence = 1 - censoring_rate,
    target_performance = large_sample_cindex,
    candidate_features = signal_parameters,
    proportion_noise_features = noise_parameters
  )[c(1, 3)] # extract mean of linear predictor as new intercept and beta_signal scaled by var of lp

  data_spec <- list(
    type = "survival",
    args = list(
      baseline_hazard = baseline_hazard,
      beta_signal = tune_param[2],
      n_signal_parameters = signal_parameters,
      noise_parameters = noise_parameters,
      predictor_type = predictor_type,
      predictor_prop = binary_predictor_prevalence,
      censoring_rate = censoring_rate
    )
  )

  data_function <- default_data_generators(data_spec)
  outcome_type <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome_type, model)

  metric <- ifelse(metric == "calibration_slope", "calib_slope", metric)

  suppressWarnings(
    output <- simulate_custom(
      metric_function = default_metric_generator(metric, data_function),
      target_performance = minimum_acceptable_performance,
      c_statistic = large_sample_cindex,
      data_function = data_function,
      model_function = model_function,
      min_sample_size = NULL,
      max_sample_size = NULL,
      se_final = NULL,
      n_reps_total = n_reps_total,
      n_reps_per = 20,
      method = "mlpwr-bs",
      mean_or_assurance = mean_or_assurance,
      test_n = 30000
    )
  )

  # Append input parameters
  output$parameters <- signal_parameters
  output$noise_parameters <- noise_parameters
  output$predictor_type <- predictor_type
  output$binary_predictor_prevalence <- output$predictor_type
  output$baseline_hazard <- baseline_hazard
  output$censoring_rate <- censoring_rate
  output$cstatistic <- large_sample_cindex
  output$model <- model
  output$metric <- metric
  output$n_reps_total <- n_reps_total
  output$mean_or_assurance <- mean_or_assurance
  class(output) <- "pmsims"
  output
}
