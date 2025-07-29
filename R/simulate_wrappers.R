#' Calculate the minimum sample size required to develop a prediction model
#'
#' Minimum working example using the mlpwr package
#'
#' @param data_generating_function A function of two parameters, n and a tuning
#'   parameter, that returns data for the model function
#' @param model_function A function which takes the object returned by the data
#'   generating function and fits the analysis model of interest.
#' @param metric_function A function which takes a a test dataset and model
#'   object as arguments and returns a performance metricß
#' @param target_performance The desired performance of the prediction model
#' @param test_n The sample size used for testing model performance
#' @param tune_param A tuning parameter to be passed to the data generating
#'   function
#' @param large_sample_performance The desired performance in a large sample
#'   (for the metric defined by tune_metric_function). This may be specified in
#'   place of tune_param. The data generating model is tuned so the desired
#'   performance is obtained when n is equal to the max_sample_size.
#' @param tune_args A named list of arguments to be passed to
#'   tune_generate_data.R. Possible arguments are large_n, min_tune_arg,
#'   max_tune_arg, max_interval_expansion, and tolerance. See
#'   \code{\link{tune_generate_data}} for more details.
#' @param tune_metric_function The metric_function used when tuning the model.
#'   This may differ from the metric function used for determining the sample
#'   size.
#' @param min_sample_size The minimum sample size used in simulations
#' @param max_sample_size The maximum sample size used in simulations
#' @param n_reps_totalThe number of simulation reps
#' @param final_estimate_se The standard error for the estimate of model
#'   performance at the minimum sample size. Either this or nreps should be
#'   specified.
#' @param n_sample_sizes The number of different sample sizes simulations are
#'   carried out at each sample size
#' @param n_init The number of different sample sizes for initialization
#'   (before updates)
#' @return A list of results form the simulation
#' @export
#'
#' @examples
#' # TODO

#' Calculate the minimum sample size required for a binary outcome
#'
#' @inheritParams generate_binary_data
#' @param ... Other options passed to [simulate_custom()]
#'
#' @return
#' @export
#'
#' @examples
simulate_binary <- function(
  n_signal_parameters,
  baseline_prob,
  min_sample_size,
  max_sample_size,
  large_sample_performance,
  minimum_threshold = 0.1,
  noise_parameters = 0,
  predictor_type = "continuous",
  predictor_prop = NULL,
  metric = "auc",
  model = "glm",
  se_final = 0.005,
  n_reps_total = NULL,
  tune_param = NULL,
  ...
) {
  data_spec <- list(
    type = "binary",
    args = list(
      n_signal_parameters = n_signal_parameters,
      noise_parameters = noise_parameters,
      predictor_type = predictor_type,
      predictor_prop = predictor_prop,
      baseline_prob = baseline_prob
    )
  )

  data_function <- default_data_generators(data_spec)
  outcome_type <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome_type, model)

  # Tune data function
  if (is.null(tune_param)) {
    print("Tuning data function...")
    tune_param <- tune_generate_data(
      interval = c(0, 1),
      large_n = set_test_n(max_sample_size),
      tolerance = set_tolerance(large_sample_performance),
      max_interval_expansion = 10,
      data_function = data_function,
      model_function = model_function,
      metric_function = default_metric_generator(
        metric,
        data_function
      ),
      target_performance = large_sample_performance,
      verbose = TRUE
    )
  }

  # TODO: What is this doing?
  se_final <- 0.005

  do.call(
    simulate_custom,
    args = c(
      metric_function = default_metric_generator(metric, data_function),
      target_performance = large_sample_performance - minimum_threshold,
      # Common arguments
      data_function = data_function,
      model_function = model_function,
      min_sample_size = min_sample_size,
      max_sample_size = max_sample_size,
      se_final = se_final,
      n_reps_total = n_reps_total,
      test_n = set_test_n(max_sample_size),
      tune_param = tune_param
    )
  )
}

#' Calculate the minimum sample size required for a continous outcome
#'
#' @inheritParams generate_continuous_data
#' @export
#'
#' @examples
simulate_continuous <- function(
  n_signal_parameters,
  min_sample_size,
  max_sample_size,
  noise_parameters = 0,
  predictor_type = "continuous",
  predictor_prop = NULL,
  model = "lm",
  metric = "r2",
  large_sample_performance = 0.8,
  minimum_threshold = 0.10,
  se_final = 0.005, # To give CIs of +/- 0.01
  n_reps_total = NULL,
  ...
) {
  inputs <- parse_inputs(
    data_spec = list(
      type = "continuous",
      args = list(
        n_signal_parameters = n_signal_parameters,
        noise_parameters = noise_parameters,
        predictor_type = predictor_type,
        predictor_prop = predictor_prop
      )
    ),
    metric,
    model
  )

  if (!(is.null(n_reps_total))) {
    se_final <- NULL
  }

  target_performance <- large_sample_performance - minimum_threshold

  extra_args <- list(...)
  if (!is.null(extra_args$tune_param)) {
    large_sample_performance <- NULL
  }

  do.call(
    simulate_custom,
    args = c(
      inputs,
      target_performance = target_performance,
      large_sample_performance = large_sample_performance,
      min_sample_size = min_sample_size,
      max_sample_size = max_sample_size,
      n_reps_total = n_reps_total,
      se_final = se_final,
      test_n = set_test_n(max_sample_size),
      ...
    )
  )
}

#' Calculate the minimum sample size required for a survival outcome
#'
#' @inheritParams generate_survival_data
#'
#' @return
#' @export
#'
#' @examples
simulate_survival <- function(
  n_signal_parameters,
  min_sample_size,
  max_sample_size,
  noise_parameters = 0,
  predictor_type = "continuous",
  predictor_prop = NULL,
  baseline_hazard = 0.01,
  censoring_rate = 0.2,
  metric = "cindex",
  model = "coxph",
  large_sample_performance = 0.8,
  minimum_threshold = 0.10,
  se_final = 0.005, # To give CIs of +/- 0.01
  n_reps_total = NULL,
  ...
) {
  inputs <- parse_inputs(
    data_spec = list(
      type = "survival",
      args = list(
        n_signal_parameters = n_signal_parameters,
        noise_parameters = noise_parameters,
        predictor_type = predictor_type,
        predictor_prop = predictor_prop,
        baseline_hazard = baseline_hazard,
        censoring_rate = censoring_rate
      )
    ),
    metric,
    model
  )

  if (!(is.null(n_reps_total))) {
    se_final <- NULL
  }

  target_performance <- large_sample_performance - minimum_threshold
  extra_args <- list(...)
  if (!is.null(extra_args$tune_param)) {
    large_sample_performance <- NULL
  }

  do.call(
    simulate_custom,
    args = c(
      inputs,
      target_performance = target_performance,
      large_sample_performance = large_sample_performance,
      min_sample_size = min_sample_size,
      max_sample_size = max_sample_size,
      se_final = se_final,
      n_reps_total = n_reps_total,
      test_n = set_test_n(max_sample_size),
      ...
    )
  )
}
