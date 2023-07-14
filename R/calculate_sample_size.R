#' Calculate the minimum sample size required to develop a prediction model
#'
#' Minimum working example using the mlpwr package
#'
#' @param data_generating_function A function of two parameters, n and a tuning
#'   parameter, that returns data for the model function
#' @param model_function A function which takes the object returned by the data
#'   generating function and fits the analysis model of interest.
#' @param metric_function A function which takes a a test dataset and model
#'   object as arguments and returns a performance metric

#' @param target_performance The desired performance of the prediction model
#' @param test_n The sample size used for testing model performance
#' @param tune_param A tuning parameter to be passed to the data generating
#'   function
#' @param large_sample_performance The desired model performance in a large
#'   sample. This may be specified in place of tune_param. The data generating
#'   model is tuned so the desired performance is obtained when n is equal to
#'   the max_sample_size.
#' @param tune_args A named list of arguments to be passed to
#'   tune_generate_data.R. Possible arguments are large_n, min_tune_arg,
#'   max_tune_arg, max_interval_expansion, and tolerance. See
#'   \code{\link{tune_generate_data}} for more details.
#' @param min_sample_size The minimum sample size used in simulations
#' @param max_sample_size The maximum sample size used in simulations
#' @param n_reps The number of simulation reps
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
                            n_reps_per = 10,
                            se_final = NULL,
                            n_init = 4,
                            method = "mlpwr",
                            verbose = FALSE) {
  if (is.null(data_function)) {
    stop("data_function missing")
  }

  # Use a default model function if not supplied
  if (is.null(model_function)) {
    model_function <-
      default_model_generators(outcome = attr(data_function, "outcome")
      )
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

  # Set default tuning parameters
  if (is.null(tune_param)) {
    default_tuning <- list(
      min_tune_arg = 0,
      max_tune_arg = 1,
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
        target_large_sample_performance = large_sample_performance,
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

  if (method == "mlpwr") {
    output <- calculate_mlpwr(
      test_n,
      tune_param,
      n_reps_total,
      n_reps_per,
      se_final,
      min_sample_size,
      max_sample_size,
      target_performance,
      n_init,
      verbose,
      data_function,
      model_function,
      metric_function,
      value_on_error
    )
  } else if (method == "crude") {
    n_sample_sizes <- n_reps_total / n_reps_per
    output <- calculate_crude(
      data_function,
      tune_param,
      model_function,
      metric_function,
      value_on_error,
      min_sample_size,
      max_sample_size,
      n_sample_sizes,
      target_performance
    )
  } else {
    stop("Method not found")
  }

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
    data_function = data_function
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
parse_inputs <- function(data_spec, metric) {
  # Set data generating function
  if (is.null(data_spec)) {
    stop("data_spec missing")
  } else {
    data_function <- default_data_generators(data_spec)
  }
  # Set model function, based on outcome type
  model_function <- default_model_generators(
    outcome = attr(data_function, "outcome")
  )
  # Set a metric, based on outcome type
  if (is.null(metric)) stop("metric is missing")
  metric_function <- default_metric_generator(
    data_function,
    metric
  )

  # Return
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
simulate_binary <- function(signal_parameters,
                            noise_parameters = 0,
                            predictor_type = "continuous",
                            predictor_prop = NULL,
                            baseline_prob,
                            metric = "auc",
                            large_sample_performance = 0.8, # e.g. 0.8
                            minimum_threshold = 0.10, # Within 10% of 0.8
                            min_sample_size,
                            max_sample_size,
                            se_final = 0.005,
                            # this will give confidence intervals +/- 0.01
                            n_reps_total = NULL,
                            ...) {
  inputs <- parse_inputs(
    data_spec = list(
      type = "binary",
      args = list(
        signal_parameters = signal_parameters,
        noise_parameters = noise_parameters,
        predictor_type = predictor_type,
        predictor_prop = predictor_prop,
        baseline_prob = baseline_prob
      )
    ),
    metric
  )
  if (!(is.null(n_reps_total))) {
    se_final <- NULL
  }

  # TODO: Decide whether to include these lines
  # if (!is.null(max_sample_size)) {
  #   max_sample_size <- max(max_sample_size,
  #                          min(max(1000, 50 * signal_parameters), 50000)
  #   )
  # }

  do.call(simulate_custom,
    args = c(inputs,
      target_performance = large_sample_performance - (minimum_threshold * large_sample_performance),
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

#' Calculate the minimum sample size required for a continous outcome
#'
#' @inheritParams generate_continuous_data
#' @export
#'
#' @examples
simulate_continuous <- function(
    signal_parameters,
    noise_parameters = 0,
    predictor_type = "continuous",
    predictor_prop = NULL,
    metric = "r2",
    large_sample_performance = 0.8, # e.g. 0.8
    minimum_threshold = 0.10, # Within 10% of 0.8
    min_sample_size,
    max_sample_size,
    se_final = 0.005, # this will give confidence intervals +/- 0.01
    n_reps = NULL,
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
    metric
  )

  if (!(is.null(n_reps))) {
    se_final <- NULL
  }


  do.call(simulate_custom,
    args = c(inputs,
      target_performance = large_sample_performance - (minimum_threshold * large_sample_performance),
      large_sample_performance = large_sample_performance,
      min_sample_size = min_sample_size,
      max_sample_size = max_sample_size,
      n_reps = n_reps,
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
                              noise_parameters = 0,
                              predictor_type = "continuous",
                              predictor_prop = NULL,
                              baseline_hazard = 0.01,
                              censoring_rate = 0.2,
                              metric = "auc",
                              large_sample_performance = 0.8, # e.g. 0.8
                              minimum_threshold = 0.10, # Within 10% of 0.8
                              min_sample_size,
                              max_sample_size,
                              se_final = 0.005, # this will give confidence intervals +/- 0.01
                              n_reps = NULL,
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
    metric
  )

  if (!(is.null(n_reps))) {
    se_final <- NULL
  }

  do.call(simulate_custom,
    args = c(inputs,
      target_performance = large_sample_performance - (minimum_threshold * large_sample_performance),
      large_sample_performance = large_sample_performance,
      min_sample_size = min_sample_size,
      max_sample_size = max_sample_size,
      se_final = se_final,
      n_reps = n_reps,
      test_n = set_test_n(max_sample_size),
      ...
    )
  )
}

#' Calculate the minimum sample size
#'

calculate_crude <- function(
  data_function,
  tune_param,
  model_function,
  metric_function,
  value_on_error,
  min_sample_size,
  max_sample_size,
  n_sample_sizes,
  target_performance) {

  # Make sure n_sample_sizes is 10 or over
  n_sample_sizes <- max(10, n_sample_sizes)

  # Specify grid
  sample_grid <- c(
    round(seq(min_sample_size, max_sample_size, length.out = 25)),
    max(30000, 3 * max_sample_size)
  )
  # Generate data and compute metric for sizes_to_check, n_sample_sizes times
  performance_matrix <-
    matrix(
      nrow = length(sample_grid),
      ncol = n_sample_sizes
    )
  colnames(performance_matrix) <- 1:n_sample_sizes
  rownames(performance_matrix) <- sample_grid

  test_n <- max(3 * max_sample_size, 30000)
  test_data <- data_function(test_n, tune_param)

  metric_calculation <- function(n) {
    tryCatch(
      {
        train_data <- data_function(n, tune_param)
        model <- model_function(train_data)
        metric_function(test_data, model)
      },
      error = function(e) {
        return(value_on_error)
      }
    )
  }

  # Compute performance metrics across sizes and simulations
  for (i in seq_along(sample_grid)) {
    for (j in seq_along(n_sample_sizes)) {
      performance_matrix[i, j] <- metric_calculation(sample_grid[i])
    }
  }

  get_perf <- function(results, p) {
    apply(results, FUN = stats::quantile, MARGIN = 1, probs = p, na.rm = TRUE)
  }

  crude_summaries <- list(
    median_performance = get_perf(performance_matrix, 0.5),
    quant20_performance = get_perf(performance_matrix, 0.2),
    quant5_performance = get_perf(performance_matrix, 0.05),
    quant95_performance = get_perf(performance_matrix, 0.95)
  )

  if (is.na(which(crude_summaries$quant20_performance > target_performance)[1])) {
    crude_min_n <- NA
  } else {
    crude_min_n <-
      sample_grid[
        which(crude_summaries$quant20_performance > target_performance)[1]
      ]
  }
  return(list(results = performance_matrix,
              summaries = crude_summaries,
              min_n = crude_min_n))
}

calculate_mlpwr <- function(
    test_n,
    tune_param,
    n_reps,
    n_reps_per,
    se_final,
    min_sample_size,
    max_sample_size,
    target_performance,
    n_init,
    verbose,
    data_function,
    model_function,
    metric_function,
    value_on_error) {
  mlpwr_simulation_function <- function(n) {
    tryCatch(
      {
        test_data <- data_function(test_n, tune_param)
        train_data <- data_function(n, tune_param)
        model <- model_function(train_data)
        metric_function(test_data, model)
      },
      error = function(e) {
        return(value_on_error)
      }
    )
  }

  aggregate_fun <- function(x) quantile(x, probs = .2)

  # Use a bootstrap to estimate the variance of the estimated quantile
  var_bootstrap <- function(x) {
    var(replicate(20, aggregate_fun(sample(x, length(x), replace = TRUE))))
  }

  # Calculate bootstrapped quantile variance
  noise_fun <- function(x) var_bootstrap(x$y)

  # processing final_estimate_se
  # Auto-stopping or not
  if (!(is.null(se_final))) {
    ci <- se_final * qnorm(0.975) * 2
    n_reps <- 10000 # setting large nreps so ci dominates.
  } else {
    ci <- NULL
  }
  # Perform search using mlpwr
  ds <-
    mlpwr::find.design(
      simfun = mlpwr_simulation_function,
      aggregate_fun = aggregate_fun,
      noise_fun = noise_fun,
      boundaries = c(min_sample_size, max_sample_size),
      power = target_performance,
      surrogate = "gpr",
      setsize = n_reps_per,
      evaluations = n_reps,
      ci = ci,
      n.startsets = n_init,
      silent = !verbose
    )

  # Process results from mlpwr
  perfs <- ds$dat
  perfs <- perfs[order(sapply(perfs, "[[", "x"))]
  max_len <- max(sapply(perfs, \(x) length(x$y)))
  results <- matrix(nrow = length(perfs), ncol = max_len)
  rownames(results) <- sapply(perfs, \(x) x$x)
  for (i in seq_along(perfs)) {
    results[i, seq(1, length(perfs[[i]]$y), 1)] <- perfs[[i]]$y
  }

  get_perf <- function(results, p) {
    apply(results, FUN = stats::quantile, MARGIN = 1, probs = p, na.rm = TRUE)
  }

  mlpwr_summaries <- list(
    median_performance = get_perf(results, 0.5),
    quant20_performance = get_perf(results, 0.2),
    quant5_performance = get_perf(results, 0.05),
    quant95_performance = get_perf(results, 0.95)
  )

  return(list(
    results = perfs,
    summaries = mlpwr_summaries,
    min_n = as.numeric(ds$final$design)
  ))
}
