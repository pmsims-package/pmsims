#' Calculate the minimum sample size required to develop a prediction model
#'
#' Minimum working example using the mlpwr package
#'
#' @param data_generating_function A function of two parameters, n and a tuning parameter, that returns data for the model function
#' @param model_function A function which takes the object returned by the data generating function and fits the analysis model of interest.
#' @param metric_function A function which takes a a test dataset and model object as arguments and returns a performance metric
#' @param target_performance The desired performance of the prediction model
#' @param test_n The sample size used for testing model performance
#' @param tune_param A tuning parameter to be passed to the data generating function
#' @param large_sample_performance The desired model performance in a large sample. This may be specified in place of tune_param. The data generating model is tuned so the desired performance is obtained when n is equal to the max_sample_size.
#' @param tune_args A named list of arguments to be passed to tune_generate_data.R. Possible arguments are large_n, min_tune_arg, max_tune_arg, max_interval_expansion, and tolerance. See \code{\link{tune_generate_data}} for more details.
#' @param min_sample_size The minimum sample size used in simulations
#' @param max_sample_size The maximum sample size used in simulations
#' @param n_reps The number of simulation reps
#' @param final_estimate_se The standard error for the estimate of model performance at the minimum sample size. Either this or nreps should be specified.
#' @param n_sample_sizes The number of different sample sizes simulations are carried out at each sample size
#' @param n_init The number of different sample sizes for initialization (before updates)
#' @return A list of results form the simulation
#' @export
#'
#' @examples
#' # TODO
simulate_custom <- function(data_function = NULL,
                            model_function = NULL,
                            metric_function = NULL,
                            target_performance,
                            test_n = 10000,
                            tune_param = NULL,
                            tune_args = list(),
                            large_sample_performance = NULL,
                            min_sample_size,
                            max_sample_size,
                            n_reps = NULL,
                            se_final = NULL,
                            n_sample_sizes = 10,
                            n_init = 4,
                            verbose = FALSE) {
  # Parse input parameters
  if (is.null(data_function)) {
    stop("data_function missing")
  }
  # Use a default model function if not supplied
  if (is.null(model_function)) {
    model_function <- default_model_generators(outcome = attr(data_function,
                                                              "outcome"))
  }

  if (sum(c(is.null(tune_param), is.null(large_sample_performance))) != 1) {
    stop("Exactly one of 'tune_param' or 'large_sample_performance' must be specified.")
  }
  
  if (sum(c(is.null(n_reps), is.null(se_final))) != 1) {
    stop("Exactly one of 'n_reps' or 'se_final' must be specified.")
  }
  
  if (min_sample_size > max_sample_size) {
    stop("min_sample_size must be less than max_sample_size")
  }
  
  if (!is.null(max_sample_size)) {
    max_sample_size <- max(max_sample_size,
                           min(max(10000, 50 * signal_parameters), 100000))
  }

  # Set tuning arguments
  if (is.null(tune_param)) {
    # Use defaults if tuning parameters not specified
    if (is.null(tune_args$min_tune_arg)) tune_args$min_tune_arg <- 0
    if (is.null(tune_args$max_tune_arg)) tune_args$max_tune_arg <- 1
    if (is.null(tune_args$large_n)) tune_args$large_n <- max_sample_size
    if (is.null(tune_args$tolerance)) tune_args$tolerance <- large_sample_performance / 100
    if (is.null(tune_args$max_interval_expansion)) tune_args$max_interval_expansion <- 10
    default <- list(data_function = data_function,
                    model_function = model_function,
                    metric_function = metric_function,
                    target_large_sample_performance = large_sample_performance,
                    verbose = verbose)
    tune_args <- c(tune_args, default)
    tune_param <- do.call(tune_generate_data, tune_args)
  }

  # Create inputs for mlpwr ----------------------------------------------------
  
  value_on_error <- .5 # can be changed based on the chosen metric
  
  mlpwr_simulation_function <- function(n) {
    tryCatch({
      test_data <- data_function(test_n, tune_param)
      train_data <- data_function(n, tune_param)
      model <- model_function$model(train_data)
      metric_function(test_data, model)
      },
      error = function(e) return(value_on_error)
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
  setsize <- n_reps/n_sample_sizes
  if(!(is.null(se_final))){
    ci = se_final*qnorm(0.975)*2 
    n_reps = 10000 # setting large nreps so ci dominates.
    setsize = 100 # fixing setsize so not driven by nreps
  } else {
    ci = NULL
  }
  
  # Perform search using mlpwr -------------------------------------------------
  ds <-
    mlpwr::find.design(
      simfun = mlpwr_simulation_function,
      aggregate_fun = aggregate_fun,
      noise_fun = noise_fun,
      boundaries = c(min_sample_size, max_sample_size),
      power = target_performance,
      surrogate = "gpr",
      setsize = setsize,
      evaluations = n_reps,
      ci = ci,
      n.startsets = n_init,
      silent = !verbose
    )


  # Process results from mlpwr -------------------------------------------------
  dat <- ds$dat
  dat <- dat[order(sapply(dat, \(x)x$x))]
  maxlen <- max(sapply(dat, \(x) length(x$y)))
  results <- matrix(nrow = length(dat), ncol = maxlen)
  rownames(results) <- sapply(dat, \(x)x$x)
  for (i in seq(length(dat))) {
    results[i, seq(length(dat[[i]]$y))] <- dat[[i]]$y
  }
  
  get_perf <- function(results, p) { 
    apply(results, FUN = stats::quantile, MARGIN = 1, probs = p, na.rm = TRUE)
  }

  median_performance <- get_perf(results, p = 0.5)
  quant20_performance <- get_perf(results, p = 0.2)
  quant5_performance <- get_perf(results, p = 0.05)
  quant95_performance <- get_perf(results, p = 0.95)

  min_n <- as.numeric(ds$final$design)

  results_list <- list(
    outcome = attr(data_function, "outcome"),
    # parameters = data_spec$args$parameters, # TOFIX: This will fail if data_spec isn't provided.
    min_n = ifelse(
      is.na(min_n),
      "Not possible. Increase sample or lower performance",
      min_n
    ),
    target_performance = target_performance,
    summaries = data.frame(
      median_performance = median_performance,
      quant20_performance = quant20_performance,
      quant5_performance = quant5_performance,
      quant95_performance = quant95_performance
    ),
    data = results,
    train_size = rownames(results)
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
  metric_function <- default_metric_generator(data_function,
                                                metric)

  # Return
  return(list(data_function = data_function,
              model_function = model_function,
              metric_function = metric_function))
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
                            large_sample_performance = 0.8,
                            # e.g. 0.8
                            minimum_threshold = 0.10,
                            # Within 10% of 0.8
                            min_sample_size,
                            max_sample_size,
                            se_final = 0.005,
                            # this will give confidence intervals +/- 0.01
                            n_reps = NULL,
                            ...) {
  
  inputs <- parse_inputs(data_spec = list(type = "binary",
                                             args = list(
                                               signal_parameters = signal_parameters,
                                               noise_parameters = noise_parameters,
                                               predictor_type = predictor_type,
                                               predictor_prop = predictor_prop,
                                               baseline_prob = baseline_prob
                                              )
                                          ),
                    metric)
  if (!(is.null(n_reps))) {se_final <- NULL}

  do.call(simulate_custom,
          args = c(inputs,
                   target_performance = large_sample_performance - (minimum_threshold * large_sample_performance),
                   large_sample_performance = large_sample_performance,
                   min_sample_size = min_sample_size,
                   max_sample_size = max_sample_size,
                   se_final = se_final,
                   n_reps = n_reps,
                   test_n = max_sample_size * 2, 
                   ...))
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
  inputs <- parse_inputs(data_spec = list(type = "continuous",
                                          args = list(
                                            signal_parameters = signal_parameters,
                                            noise_parameters = noise_parameters,
                                            predictor_type = predictor_type,
                                            predictor_prop = predictor_prop
                                          )
  ),
  metric)
  do.call(simulate_custom,
          args = c(inputs,
                   target_performance = large_sample_performance - (minimum_threshold * large_sample_performance),
                   large_sample_performance = large_sample_performance,
                   min_sample_size = min_sample_size,
                   max_sample_size = max_sample_size,
                   n_reps = n_reps,
                   se_final = se_final,
                   test_n = max_sample_size * 2,
                   ...))
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
  inputs <- parse_inputs(data_spec = list(type = "survival",
                                          args = list(
                                            signal_parameters = signal_parameters, 
                                            noise_parameters = noise_parameters,
                                            predictor_type = predictor_type,
                                            predictor_prop = predictor_prop,
                                            baseline_hazard = baseline_hazard,
                                            censoring_rate = censoring_rate
                                          )
  ),
  metric)
  do.call(simulate_custom,
          args = c(inputs,
                   target_performance = large_sample_performance - (minimum_threshold * large_sample_performance),
                   large_sample_performance = large_sample_performance,
                   min_sample_size = min_sample_size,
                   max_sample_size = max_sample_size,
                   se_final = se_final,
                   n_reps = n_reps,
                   test_n = max_sample_size * 2,
                   ...))
}

