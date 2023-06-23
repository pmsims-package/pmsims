get_simulation_parameters <- function(min_sample_size,
                                      max_sample_size,
                                      n_reps,
                                      n_sample_sizes) {
  train_size <- seq(
	from = min_sample_size,
	to = max_sample_size,
	length.out = n_sample_sizes
  ) |> as.integer(0)
  n_sims <- rep(n_reps / n_sample_sizes, n_sample_sizes) |> as.integer(0)
  simulation_parameters <- data.frame(
    train_size = train_size,
    n_sims = n_sims
  )
  return(simulation_parameters)
}

get_performance_n <- function(n,
                              test_n,
                              data_function,
                              model_function,
                              tune_param,
                              ...) {
  test_data <- data_function(test_n, tune_param, ...)
  train_data <- data_function(n, tune_param, ...)
  model <- model_function$model(train_data)
  return(model_function$metric(test_data, model))
}

run_simulations <- function(train_size,
                            n_sims,
                            test_n,
                            data_function,
                            model_function,
                            tune_param) {
  results <- expand.grid(rep = 1:max(n_sims), train_size = train_size)
  apply(results, 1, \(r) {
    get_performance_n(n = r[2],
                      test_n = test_n,
                      data_function = data_function,
                      model_function = model_function,
                      tune_param = tune_param)
  })
}

#' Calculate the minimum sample size required to develop a prediction model
#'
#' Minimum working example using the mlpwr package
#'
#' @param data_generating_function A function of two parameters, n and a tuning parameter, that returns data for the model function
#' @param model_function A function which takes the object returned by the data generating function and fits the analysis model of interest.
#' @param performance_function A function which takes a a test dataset and model object as arguments and returns a performance metric
#' @param target_performance The desired performance of the prediction model
#' @param test_n The sample size used for testing model performance
#' @param tune_param A tuning parameter to be passed to the data generating function
#' @param large_sample_performance The desired model performance in a large sample. This may be specified in place of tune_param. The data generating model is tuned so the desired performance is obtained when n is equal to the max_sample_size.
#' @param tune_args A named list of arguments to be passed to tune_generate_data.R. Possible arguments are large_n, min_tune_arg, max_tune_arg, max_interval_expansion, and tolerance. See \code{\link{tune_generate_data}} for more details.
#' @param min_sample_size The minimum sample size used in simulations
#' @param max_sample_size The maximum sample size used in simulations
#' @param n_reps The number of simulation reps
#' @param n_sample_sizes The number of different sample sizes simulations are carried out at
#' @param n_init The number of different sample sizes for initialization (before updates)
#'
#' @return A list of results form the simulation
#' @export
#'
#' @examples
#' # TODO
simulate_custom <- function(data_spec = NULL,
                            data_function = NULL,
                            model_function = NULL,
                            target_performance,
                            test_n = NULL,
                            tune_param = NULL,
                            tune_args = list(),
                            large_sample_performance = NULL,
                            min_sample_size,
                            max_sample_size,
                            n_reps,
                            n_sample_sizes = 10,
                            n_init = 4,
                            verbose = FALSE) {
  # Parse input parameters --------------------------------------------------
  if (is.null(data_spec) & is.null(data_function)) {
    stop("One of 'data_spec' or 'data_function' must be provided.")
  }
  # Use a default data function if not supplied
  if (is.null(data_function)) {
    data_function <- default_data_generators(data_spec)
  }
  # Use a default model function if not supplied
  if (is.null(model_function)) {
    model_function <- default_model_generators(outcome = attr(data_function,
                                                              "outcome"))
  }

  if (sum(c(is.null(tune_param), is.null(large_sample_performance))) != 1) {
    stop("Exactly one of 'tune_param' or 'large_sample_performance' must be specified.")
  }

  # Set tuning arguments -------------------------------------------------------
  if (is.null(tune_param)) {
    # Use defaults if tuning parameters not specified
    if (is.null(tune_args$min_tune_arg)) tune_args$min_tune_arg <- 0
    if (is.null(tune_args$max_tune_arg)) tune_args$max_tune_arg <- 1
    if (is.null(tune_args$large_n)) tune_args$large_n <- max_sample_size
    if (is.null(tune_args$tolerance)) tune_args$tolerance <- large_sample_performance / 100
    if (is.null(tune_args$max_interval_expansion)) tune_args$max_interval_expansion <- 10
    default <- list(data_function = data_function,
                    model_function = model_function,
                    target_large_sample_performance = large_sample_performance,
                    verbose = verbose)
    tune_args <- c(tune_args, default)
    tune_param <- do.call(tune_generate_data, tune_args)
  }

  # Create inputs for mlpwr ----------------------------------------------------
  mlpwr_simulation_function <- function(n) {
    run_simulations(
      train_size = n,
      n_sims = 1,
      test_n = test_n,
      data_function = data_function,
      model_function = model_function,
      tune_param = tune_param
    )
  }

  aggregate_fun <- function(x) quantile(x, probs = .2)
  
  # Use a bootstrap to estimate the variance of the estimated quantile
  var_bootstrap <- function(x) {
    var(replicate(20, aggregate_fun(sample(x, length(x), replace = TRUE))))
  }
  
  # Calculate bootstrapped quantile variance
  noise_fun <- function(x) var_bootstrap(x$y)
  
  # Perform search using mlpwr -------------------------------------------------
  ds <-
    mlpwr::find.design(
      simfun = mlpwr_simulation_function,
      aggregate_fun = aggregate_fun,
      noise_fun = noise_fun,
      boundaries = c(min_sample_size, max_sample_size),
      power = target_performance,
      surrogate = "gpr",
      setsize = n_reps / n_sample_sizes,
      evaluations = n_reps,
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

  median_performance <- apply(results, FUN = stats::quantile, MARGIN = 1, probs = 0.5, na.rm = TRUE)
  quant20_performance <- apply(results, FUN = stats::quantile, MARGIN = 1, probs = 0.2, na.rm = TRUE)
  quant5_performance <- apply(results, FUN = stats::quantile, MARGIN = 1, probs = 0.05, na.rm = TRUE)
  quant95_performance <- apply(results, FUN = stats::quantile, MARGIN = 1, probs = 0.95, na.rm = TRUE)

  min_n <- as.numeric(ds$final$design)

  results_list <- list(
    outcome = attr(data_function, "outcome"),
    parameters = data_spec$args$parameters, # TOFIX: This will fail if data_spec isn't provided.
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

  # simulation_parameters <- get_simulation_parameters(
  #   min_sample_size = min_sample_size,
  #   max_sample_size = max_sample_size,
  #   n_reps = n_reps,
  #   n_sample_sizes = n_sample_sizes
  # )

  attr(results_list, "class") <- "pmsims"
  return(results_list)
}


#' Calculate the minimum sample size required for a binary outcome
#'
#' @param parameters The number of candidate predictor parameters for potential inclusion in the new prediction model.
#' @param prevalence The outcome proportion (for a prognostic model) or overall prevalence (for a diagnostic model) expected within the model development dataset.
#' @param ... Other options passed to [simulate_custom()]
#'
#' @return
#' @export
#'
#' @examples
simulate_binary <- function(parameters,
                            prevalence,
                            cstatistic,
                            min_sample_size,
                            max_sample_size,
                            n_reps = 100,
                            ...) {
  simulate_custom(data_spec = list(outcome = "binary",
                                   args = list(parameters = parameters)),
                  target_performance = cstatistic,
                  large_sample_performance = cstatistic + 0.1,
                  min_sample_size = min_sample_size,
                  max_sample_size = max_sample_size,
                  n_reps = n_reps,
                  test_n = max_sample_size * 2,
                  ...)
}

#' Calculate the minimum sample size required for a linear outcome
#'
#' @return
#' @export
#'
#' @examples
simulate_linear <- function() {
 # TODO
}

#' Calculate the minimum sample size required for a survival outcome
#'
#' @return
#' @export
#'
#' @examples
simulate_survival <- function() {
 # TODO
}

