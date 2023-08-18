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
simulate_custom <- function(data_function = NULL,
                            model_function = NULL,
                            metric_function = NULL,
                            target_performance,
                            test_n = 30000,
                            tune_param = NULL,
                            tune_args = list(),
                            large_sample_performance = NULL,
                            min_sample_size,
                            max_sample_size,
                            n_reps_total = NULL,
                            n_reps_per = 50,
                            se_final = NULL,
                            n_init = 4,
                            method = "mlpwr",
                            verbose = FALSE,
                            ...) {
  if (is.null(data_function)) {
    stop("data_function missing")
  }

  if (sum(c(is.null(tune_param),
            is.null(large_sample_performance))) != 1) {
    stop(paste(
      "Exactly one of 'tune_param' or",
      "'large_sample_performance' must be specified."
    ))
  }

  if (sum(c(is.null(n_reps_total),
            is.null(se_final))) != 1) {
    stop("Exactly one of 'n_reps_total' or 'se_final' must be specified.")
  }

  if (min_sample_size > max_sample_size) {
    stop("min_sample_size must be less than max_sample_size")
  }
  time_0 <- Sys.time()

  # Set default tuning parameters
  if (is.null(tune_param)) {
    default_tuning <- list(
      interval = c(0, 1),
      large_n = set_test_n(max_sample_size),
      tolerance = set_tolerance(large_sample_performance),
      max_interval_expansion = 10
    )
    for (p in names(default_tuning)) {
      if (is.null(tune_args[[p]])) tune_args[[p]] <- default_tuning[[p]]
    }
    tune_args <- c(
      tune_args,
      list(
        data_function = data_function,
        model_function = model_function,
        metric_function = metric_function,
        target_performance = large_sample_performance,
        verbose = verbose
      )
    )
    tune_param <- do.call(tune_generate_data, tune_args)
  }
  # Define a default metric value if calculations fail; 0.5 for default
  metric_name <- attr(metric_function, "metric")
  error_values <- list(
    auc = 0.5,
    cindex = 0.5,
    r2 = 0,
    brier_score_scaled = 0,
    brier_score = 1,
    IBS = 1,
    calib_slope = 0
  )
  value_on_error <- ifelse(metric_name %in% names(error_values),
    error_values[[metric_name]],
    0.5
  )
  time_1 <- Sys.time()

  if (method == "mlpwr") {
    output <- calculate_mlpwr(
      test_n = test_n,
      tune_param = tune_param,
      n_reps_total = n_reps_total,
      n_reps_per = n_reps_per,
      se_final = se_final,
      min_sample_size = min_sample_size,
      max_sample_size = min_sample_size,
      target_performance = target_performance,
      n_init = n_init,
      verbose = verbose,
      data_function = data_function,
      model_function = model_function,
      metric_function = metric_function,
      value_on_error = value_on_error
    )
  } else if (method == "crude") {
    output <- calculate_crude(
      data_function,
      tune_param,
      model_function,
      metric_function,
      value_on_error,
      min_sample_size,
      max_sample_size,
      n_reps_per,
      target_performance,
      ...
    )
  } else {
    stop("Method not found")
  }
  time_2 <- Sys.time()
  results_list <- list(
    outcome = attr(data_function, "outcome"),
    min_n = ifelse(
      is.na(output$min_n),
      "Not possible. Increase sample or lower performance",
      output$min_n
    ),
    target_performance = target_performance,
    summaries = output$summaries,
    data = output$results,
    train_size = rownames(output$results),
    tune_param = tune_param,
    data_function = data_function,
    simulation_time = list(
      "tuning" = difftime(time_1, time_0, units = "secs"),
      "simulating" = difftime(time_2, time_1, units = "secs")
    )
  )
  attr(results_list, "class") <- "pmsims"
  return(results_list)
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
parse_inputs <- function(data_spec,
                         metric,
                         model) {
  if (is.null(metric)) stop("metric is missing")
  if (is.null(data_spec)) stop("data_spec missing")
  # Set data generating function
  data_function <- default_data_generators(data_spec)
  # Set model function, based on outcome type and chosen model
  model_function <- default_model_generators(
    attr(data_function, "outcome"),
    model
  )
  # Set a metric, based on outcome type
  # TODO: handle multiple metrics. Currently selecting first element in
  # 'metric' only.
  metric_function <- default_metric_generator(metric[[1]], data_function)
  return(list(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function
  ))
}

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
  signal_parameters,
  baseline_prob,
  min_sample_size,
  max_sample_size,
  large_sample_discrimination,
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

  # Set model/data functions
  data_spec <- list(
    type = "binary",
    args = list(
      signal_parameters = signal_parameters,
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
    tune_param <- tune_generate_data(
      interval = c(0, 1),
      large_n = set_test_n(max_sample_size),
      tolerance = set_tolerance(large_sample_discrimination),
      max_interval_expansion = 10,
      data_function = data_function,
      model_function = model_function,
      metric_function = default_metric_generator(metric,
                                                 data_function),
      target_performance = large_sample_discrimination,
      verbose = TRUE
    )
  }

  if (!(is.null(n_reps_total))) {
    se_final <- NULL
  }

  # Make a list containing (a) parameters for the discrimination metric
  # (typically AUC); (b) parameters for the calibration metric (slope).
  criteria <- list(
    discrim = c(
      metric = default_metric_generator(metric, data_function),
      target = large_sample_discrimination - minimum_threshold
    ),
    calib = c(
      metric = default_metric_generator("calib_slope", data_function),
      target = 0.9
    )
  )

  lapply(criteria, function(p) {
    do.call(
      simulate_custom,
      args = c(
        metric_function = p$metric,
        target_performance = p$target,
        # Common arguments
        data_function = data_function,
        model_function = model_function,
        min_sample_size = min_sample_size,
        max_sample_size = max_sample_size,
        se_final = se_final,
        n_reps_total = n_reps_total,
        test_n = set_test_n(max_sample_size),
        tune_param = tune_param,
        ...
      )
    )
  }
  )
}

#' Calculate the minimum sample size required for a continous outcome
#'
#' @inheritParams generate_continuous_data
#' @export
#'
#' @examples
simulate_continuous <- function(
    signal_parameters,
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
    ...) {

  inputs <- parse_inputs(
    data_spec = list(
      type = "continuous",
      args = list(
        signal_parameters = signal_parameters,
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
  if (!is.null(extra_args$tune_param)) large_sample_performance <- NULL

  do.call(simulate_custom,
    args = c(inputs,
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
simulate_survival <- function(signal_parameters,
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
                              ...) {
  inputs <- parse_inputs(
    data_spec = list(
      type = "survival",
      args = list(
        signal_parameters = signal_parameters,
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
  if (!is.null(extra_args$tune_param)) large_sample_performance <- NULL

  do.call(simulate_custom,
    args = c(inputs,
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

