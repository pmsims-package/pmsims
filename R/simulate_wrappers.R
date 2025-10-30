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
  # Predictors
  signal_parameters,
  noise_parameters = 0,
  predictor_type = "continuous",
  binary_predictor_prevalence = NULL,
  # Outcome
  outcome_prevalence,
  large_sample_cstatistic,
  # Model
  model = "glm",
  # Performance
  metric = "calibration_slope",
  minimum_acceptable_performance,
  # engine control
  n_reps_total = 1000,
  mean_or_assurance = "assurance",
  ...
){


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
  metric = ifelse(metric == "calibration_slope","calib_slope", metric)


  # main pmsims

  suppressWarnings(output <-  simulate_custom(
      metric_function = default_metric_generator(metric, data_function),
      target_performance = minimum_acceptable_performance,
      c_statistic = large_sample_cstatistic,
      # Common arguments
      data_function = data_function,
      model_function = model_function,
      min_sample_size = NULL,
      max_sample_size = NULL,
      se_final = NULL,
      n_reps_total = n_reps_total,
      n_reps_per = 20,
      method = "mlpwr-bs",
      mean_or_assurance = mean_or_assurance,
      test_n = 30000)
  )

  # Predictors
  output$parameters <- signal_parameters
  output$noise_parameters <- noise_parameters
  output$predictor_type <- predictor_type
  output$binary_predictor_prevalence <- output$predictor_type
  # Outcome
  output$prevalence <- outcome_prevalence
  output$cstatistic <- large_sample_cstatistic
  # Model
  output$model <- model
  # Performance
  output$metric <- metric
  # engine control
  output$n_reps_total <- n_reps_total
  output$mean_or_assurance <- mean_or_assurance

 est <- output
 class(est) <- "pmsims"
 est

}


#' Calculate the minimum sample size required for a continous outcome
#'
#' @inheritParams generate_continuous_data
#' @export
#'
#' @examples
simulate_continuous <- function(
    # Predictors
  signal_parameters,
  noise_parameters = 0,
  predictor_type = "continuous",
  binary_predictor_prevalence = NULL,
  # Outcome
  large_sample_rsquared,
  # Model
  model = "lm",
  # Performance
  metric = "calibration_slope",
  minimum_acceptable_performance,
  # engine control
  n_reps_total = 1000,
  mean_or_assurance = "assurance",
  ...
)
{


  # Tune for data function
  tune_param <- continuous_tuning(
    r2 = large_sample_rsquared,
    candidate_features = signal_parameters,
    proportion_noise_features = noise_parameters
  ) # extract beta_signal


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


  # Redefine metrics to internal syntax lang

  metric = ifelse(metric == "calibration_slope","calib_slope", metric)


  # main pmsims

  suppressWarnings(output <-  simulate_custom(
    metric_function = default_metric_generator(metric, data_function),
    target_performance = minimum_acceptable_performance,
    c_statistic = large_sample_rsquared,
    # Common arguments
    data_function = data_function,
    model_function = model_function,
    min_sample_size = NULL,
    max_sample_size = NULL,
    se_final = NULL,
    n_reps_total = n_reps_total,
    n_reps_per = 20,
    method = "mlpwr-bs",
    mean_or_assurance = mean_or_assurance,
    test_n = 30000)
  )


  ## append input parameters


  # Predictors
  output$parameters <- signal_parameters
  output$noise_parameters <- noise_parameters
  output$predictor_type <- predictor_type
  output$binary_predictor_prevalence <- output$predictor_type
  # Outcome
  output$r2 <- large_sample_rsquared
  # Model
  output$model <- model
  # Performance
  output$metric <- metric
  # engine control
  output$n_reps_total <- n_reps_total
  output$mean_or_assurance <- mean_or_assurance

  est <- output
  class(est) <- "pmsims"
  est

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
    # Predictors
  signal_parameters,
  noise_parameters = 0,
  predictor_type = "continuous",
  binary_predictor_prevalence = NULL,
  # Outcome
  large_sample_cindex,
  baseline_hazard = 1,
  censoring_rate,
  # Model
  model = "coxph",
  # Performance
  metric = "calibration_slope",
  minimum_acceptable_performance,
  # engine control
  n_reps_total = 1000,
  mean_or_assurance = "assurance",
  ...
){


  # Tune for data function
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

  # Redefine metrics to internal syntax lang

  metric = ifelse(metric == "calibration_slope","calib_slope", metric)
  # main pmsims

  suppressWarnings(output <-  simulate_custom(
    metric_function = default_metric_generator(metric, data_function),
    target_performance = minimum_acceptable_performance,
    c_statistic = large_sample_cindex,
    # Common arguments
    data_function = data_function,
    model_function = model_function,
    min_sample_size = NULL,
    max_sample_size = NULL,
    se_final = NULL,
    n_reps_total = n_reps_total,
    n_reps_per = 20,
    method = "mlpwr-bs",
    mean_or_assurance = mean_or_assurance,
    test_n = 30000)
  )

  ## append input parameters
  # Predictors
  output$parameters <- signal_parameters
  output$noise_parameters <- noise_parameters
  output$predictor_type <- predictor_type
  output$binary_predictor_prevalence <- output$predictor_type
  output$baseline_hazard <- baseline_hazard
  # Outcome
  output$censoring_rate <- censoring_rate
  output$cstatistic <- large_sample_cindex
  # Model
  output$model <- model
  # Performance
  output$metric <- metric
  # engine control
  output$n_reps_total <- n_reps_total
  output$mean_or_assurance <- mean_or_assurance

  est <- output
  class(est) <- "pmsims"
  est

}
