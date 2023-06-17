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
                              data_generating_function,
                              model_function,
                              performance_function,
                              tune_param,
                              ...) {
  test_data <- data_generating_function(test_n, tune_param, ...)
  train_data <- data_generating_function(n, tune_param, ...)
  model <- model_function(train_data)
  performance <- performance_function(test_data, model)
  return(performance)
}

run_simulations <- function(simulation_parameters,
                            test_n = test_n,
                            data_generating_function = data_generating_function,
                            model_function = model_function,
                            performance_function = performance_function,
                            tune_param = tune_param) {
  results <- matrix(
    nrow = length(simulation_parameters$train_size),
    ncol = max(simulation_parameters$n_sims)
  )
  for (i in 1:length(simulation_parameters$train_size)) {
    for (j in seq(simulation_parameters$n_sims[i])) {
      results[i, j] <- get_performance_n(
        n = simulation_parameters$train_size[i],
        test_n = test_n,
        data_generating_function = data_generating_function,
        model_function = model_function,
        performance_function = performance_function,
        tune_param = tune_param
      ) # function defined below
    }
  }
  rownames(results) <- simulation_parameters$train_size
  return(results)
}

#' Calculate the minimum sample size required to develop a prediction model
#'
#' Minimum working example using the mlpwr package
#'
#' @param data_generating_function A function of two parameters, n and a tuning parameter, that returns data for the model function
#' @param model_function A function which takes the object returned by the data generating function and fits the analysis model of interest.
#' @param performance_function A function which takes a a test dataset and model object as argments and returns a performance metric
#' @param target_performance The desired performance of the prediction model
#' @param test_n The sample size used for testing model performance
#' @param tune_param A tuning parameter to be passed to the data generating function
#' @param large_sample_performance The desired model performance in a large smaple. This may be specified in place of tune_param. The data generating model is tuned so the desired performance is obtained when n is equal to the max_sample_size. 
#' @param tune_args A named list of arguments to be passed to tune_generate_data.R. Possible arguments are large_n, min_tune_arg, max_tune_arg, max_interval_expansion, and tolerance. See \code{\link{tune_generate_data}} for more details.
#' @param min_sample_size The minimum sample size used in simualations
#' @param max_sample_size The maximum sample size used in simulations
#' @param n_reps The number of simualtion reps
#' @param n_sample_sizes The number of different sample sizes simulations are carried out at
#' @param n_init The number of different sample sizes for initialization (before updates)
#'
#' @return A list of results form the simulation
#' @export
#'
#' @examples
#' # TODO
simulate_custom <- function(data,
                            model = NULL,
                            performance_function = NULL,
                            target_performance,
                            test_n,
                            tune_param = NULL,
                            large_sample_performance = NULL,
                            tune_args = list(),
                            min_sample_size,
                            max_sample_size,
                            n_reps,
                            n_sample_sizes = 10,
                            n_init = 4) {
  if (is.function(data)) {
    data_generating_function <- data
  } else if (is.list(data)) {
    data_generating_function <- default_data_generators(data)
  }

  if (is.null(model)) {
    m <- default_model_generators(type = data$type)
    model_function <- m$model
    performance_function <- m$metric
  } else {
    model_function <- model
  }
  
  if (is.null(tune_param) & is.null(large_sample_performance)) {
    stop("one of tune_param or large_sample_performance must be specified")
  }
  if (!is.null(tune_param) & !is.null(large_sample_performance)) {
    stop("only one of tune_param or large_sample_performance must be specified ")
  }
  
  
  # Tuning parameters
  if (is.null(tune_param)) {
    # Managing parameters and defaults
    if (is.null(tune_args$min_tune_arg)) tune_args$min_tune_arg <- 0
    if (is.null(tune_args$max_tune_arg)) tune_args$max_tune_arg <- 1
    if (is.null(tune_args$large_n)) tune_args$large_n <- max_sample_size
    if (is.null(tune_args$tolerance)) tune_args$tolerance <- large_sample_performance/100
    if (is.null(tune_args$max_interval_expansion)) tune_args$max_interval_expansion <-10
    
    tune_args$data_generating_function  <-  data_generating_function
    tune_args$model_function  <-  model_function
    tune_args$performance_function  <-  performance_function
    tune_args$target_large_sample_performance  <-  large_sample_performance
    
    tune_param <- do.call(tune_generate_data, tune_args)
  }

  simfun <- function(n) {
    results <- run_simulations(
      data.frame(
        train_size = n,
        n_sims = 1
      ),
      test_n = test_n,
      data_generating_function = data_generating_function,
      model_function = model_function,
      performance_function = performance_function,
      tune_param = tune_param
    )
    return(as.numeric(results))
  }

  aggregate_fun <- function(x) quantile(x, probs = .2)
  # To estimate the variance of the estimated quantile, we use a bootstrap
  var_bootstrap <- function(x) var(replicate(20, aggregate_fun(sample(x, length(x), replace = T))))
  # noise_fun <- function(x) var_bootstrap(x$y) # This is the bootstrapped quantile variance
  power <- target_performance # This is the goal AUC
  evaluations <- n_reps # Total number of evaluations
  boundaries <- c(min_sample_size, max_sample_size) # Edge Sample Sizes
  surrogate <- "gpr" # Gaussian Process Regression as surrogate Model
  setsize <- n_reps / n_sample_sizes # 5 Evaluations for each sample size
  n.startsets <- n_init

  # perform search
  ds <-
    mlpwr::find.design(
      simfun = simfun,
      aggregate_fun = aggregate_fun,
      # noise_fun = noise_fun,
      boundaries = boundaries,
      power = power,
      surrogate = surrogate,
      setsize = setsize,
      evaluations = evaluations,
      n.startsets = n.startsets
    )

  # extracting results
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
    min_n = ifelse(is.na(min_n),
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

  simulation_parameters <- get_simulation_parameters(
    min_sample_size = min_sample_size,
    max_sample_size = max_sample_size,
    n_reps = n_reps,
    n_sample_sizes = n_sample_sizes
  )

  attr(results_list, "class") <- "pmsims"
  return(results_list)
}


#' Calculate the minimum sample size required for a binary outcome
#'
#' @return
#' @export
#'
#' @examples
simulate_binary <- function() {
 # TODO 
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