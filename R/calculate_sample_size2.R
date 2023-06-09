#' Calculate the minimum sample size required to develop a prediction model
#'
#' Minimum working example using the mlpwr package
#'
#' @param data_generating_function A function of two parameters, n and a tuning parameter, that returns data for the model function
#' @param model_function A function which takes the object returned by the data generating function and fits the analysis model of interest.
#' @param performance_function A function which takes a a test dataset and model object as argments and returns a performance metric
#' @param target_performance The desired performance of the prediction model
#' @param test_n The sample size used for testing model performance
#' @param large_sample_performance The desired model performance in a large smaple. This may be specified in place of tune_param. The data generating model is tuned so the desired performance is obtained when n is equal to the max_sample_size. 
#' @param max_interval_expansion The maximum number of times the interval that the tuning parameter is searched for in can be expanded. The default interval is 0,1 and at each expansion the interval is increased by 1.
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
#'
#' library(pROC)
#'
#' generate_data <- function(n, beta_signal) {
#'   p_signal <- 10 # number of predictors
#'   prob_p <- 0.1 # probability of a predictor to be 1
#'   base_prev <- 0.3 # baseline probability of a positive outcome
#'
#'   alldata <- rbinom(n * p_signal, 1, prob_p)
#'   X <- matrix(alldata, nrow = n, ncol = p_signal)
#'   W_ <- rep(beta_signal, p_signal)
#'   b0 <- log(base_prev / (1 - base_prev))
#'   lp <- X %*% W_ + b0
#'   y_prob <- 1 / (1 + exp(-lp))
#'
#'   # generate outcome
#'   y <- rbinom(n, 1, y_prob)
#'   data <- cbind(y, X) |> data.frame()
#'   x_names <- paste0("x", 1:(ncol(data) - 1))
#'   data_names <- c("y", x_names)
#'   colnames(data) <- data_names
#'   return(data)
#' }
#'
#' fit_model <- function(data) {
#'   logistic_model <- glm("y ~ .", data = data, family = "binomial")
#' }
#'
#' # Get performance must be a function of data and a model object
#' get_performance <- function(data, model) {
#'   y <- data[, 1]
#'   x <- data[, -1]
#'   y_hat <- predict(model, x, type = "response")
#'   auc <- pROC::auc(y, as.numeric(y_hat), quiet = TRUE)
#'   return(auc[1])
#' }
#'
#' calculate_sample_size(
#'   data_generating_function = generate_data,
#'   model_function = fit_model,
#'   performance_function = get_performance,
#'   target_performance = 0.75,
#'   test_n = 10000,
#'   tune_param = 0.7,
#'   min_sample_size = 100,
#'   max_sample_size = 3000,
#'   n_reps = 100,
#'   n_sample_sizes = 10
#' )
get_performance_n <- function(n,
                              test_n,
                              data_generating_function,
                              model_function,
                              performance_function,
                              tune_param,
                              ...) {
  if (is.null(tune_param)) {
    test_data <- data_generating_function(test_n, ...)
    train_data <- data_generating_function(n, ...)
  } else {
    test_data <- data_generating_function(test_n, tune_param, ...)
    train_data <- data_generating_function(n, tune_param, ...)
    
  }
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

calculate_sample_size2 <- function(data,
                                   model = NULL,
                                   performance_function = NULL,
                                   target_performance,
                                   test_n,
                                   large_sample_performance = NULL,
                                   max_interval_expansion = 10,
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

  
  # Tuning parameters
  if (is.null(large_sample_performance)) {
    tune_param = NULL
  } else {
    tune_param <- tune_generate_data(data_generating_function = data_generating_function,
                                     large_n = max_sample_size,
                                     min_tune_arg = 0,
                                     max_tune_arg = 1,
                                     model_function = model_function,
                                     performance_function = performance_function,
                                     target_large_sample_performance = large_sample_performance,
                                     tolerance = large_sample_performance/100,
                                     max_interval_expansion = max_interval_expansion)
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
  noise_fun <- function(x) var_bootstrap(x$y) # This is the bootstrapped quantile variance
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
      noise_fun = noise_fun,
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
