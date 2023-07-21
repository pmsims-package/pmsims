#' Title Create default data generating functions
#'
#' @param opts A list of options to be used with the data generating function. Must include type as either "binary", "continuous", or "survival".
#' Arguments to be passed to the data generating function must be stored in a list item named args.
#' For options that can be passed to the different default generators see \link{generate_continuous_data}, \link{generate_binary_data}, or \link{generate_survival_data}.
#' @return A function with default arguments set to the values passed with opts
#' @export
#'
#' @examples
default_data_generators <- function(opts) {
  type <- opts$type
  if (type == "binary") {
    f <- generate_binary_data
  } else if (type == "continuous") {
    f <- generate_continuous_data
  } else if (type == "survival") {
    f <- generate_survival_data
  } else {
    stop('"opts$type must be one of "continuous", "binary", or "survival""')
  }
  return(update_arguments(f, opts))
}

#' Simulate Continuous data
#'
#' @param n Sample size
#' @param beta_signal Assosiation between signal predictors and the outcome
#' @param signal_parameters Number of predictors that have a non zero assosaion with the outcome
#' @param noise_parameters Number of predictors with no assosiation with outcome
#' @param predictor_type Type of predictor, can be "continuous" or "binary."
#' @param predictor_prop If predictor type is binary, the probability of a predictor taking value 1
#'
#' @return A data frame with one outcome column and signal_parameters + noise_parameters predictor columns
#' @export
#'
#' @examples generate_continuous_data(n = 100, signal_parameters = 10, noise_parameters = 10, predictor_type = "binary", predictor_prop = 0.1, beta_signal = 0.1)
generate_continuous_data <- function(
    n,
    beta_signal,
    signal_parameters,
    noise_parameters,
    predictor_type,
    predictor_prop = NULL) {
  parameters <- signal_parameters + noise_parameters
  intercept <- 0
  X <- generate_predictors(n, parameters, predictor_type, predictor_prop)
  lp <- generate_linear_predictor(
    X,
    signal_parameters,
    noise_parameters,
    intercept,
    beta_signal
  )

  y <- rnorm(n, lp, 1) # error variance is 1
  data <- cbind(y, X)
  return(as.data.frame(data))
}

#' Title Simulate Binary Data
#'
#' @inheritParams generate_continuous_data
#' @param baseline_prob Baseline probability of outcome (ie. probability when all predicors are 0)
#'
#' @return A data frame with one outcome column and signal_parameters + noise_parameters predictor columns
#' @export
#'
#' @examples generate_binary_data(n = 100, signal_parameters = 5, noise_parameters = 5, predictor_type = "continuous", beta_signal = 0.1, baseline_prob = 0.1)
generate_binary_data <- function(
    n,
    beta_signal,
    signal_parameters,
    noise_parameters,
    predictor_type,
    predictor_prop,
    baseline_prob) {
  parameters <- signal_parameters + noise_parameters
  intercept <- log(baseline_prob / (1 - baseline_prob))
  X <- generate_predictors(n, parameters, predictor_type, predictor_prop)
  lp <- generate_linear_predictor(
    X,
    signal_parameters,
    noise_parameters,
    intercept,
    beta_signal
  )
  y_prob <- 1 / (1 + exp(-lp))
  y <- rbinom(n, 1, y_prob)
  data <- cbind(y, X)
  return(as.data.frame(data))
}


#' Title Simulate Survival Data
#'
#' @inheritParams generate_continuous_data
#' @param baseline_hazard Baseline Hazard
#' @param censoring_rate Censoring rate
#'
#' @return A data frame with a column for ID, Time, status (0 = censored, 1 =
#' event), and signal_parameters + noise_parameters predictor columns
#' @export
#'
#' @examples generate_binary_data(n = 100, signal_parameters = 5, noise_parameters = 5, predictor_type = "continuous", beta_signal = 0.1, baseline_prob = 0.1)
generate_survival_data <- function(
    n,
    beta_signal,
    signal_parameters,
    noise_parameters,
    predictor_type,
    predictor_prop,
    baseline_hazard,
    censoring_rate) {
  parameters <- signal_parameters + noise_parameters
  intercept <- 0
  X <- generate_predictors(n, parameters, predictor_type, predictor_prop)
  lp <- generate_linear_predictor(
    X,
    signal_parameters,
    noise_parameters,
    intercept,
    beta_signal
  )

  # Generate survival times
  event_time <- rexp(n, rate = baseline_hazard * exp(lp))

  # Generate censoring indicators
  C <- ifelse(runif(n) < censoring_rate, 1, 0) # 20% censoring rate

  # Return survival data as a data frame
  return(data.frame(
    id = 1:n,
    time = event_time,
    event = 1 - C,
    X
  ))
}

update_arguments <- function(fn, opts) {
  for (key in names(opts$args)) {
    if (key %in% names(formals(fn))) {
      formals(fn)[[key]] <- opts$args[[key]]
    }
  }
  attr(fn, "outcome") <- opts$type
  return(fn)
}

generate_predictors <- function(n, parameters, type, predictor_prop) {
  if (type == "binary") {
    X <- rbinom(n * parameters, 1, predictor_prop)
  } else if (type == "continuous") {
    X <- rnorm(n * parameters)
  } else {
    stop("type must be one of binary or continuous")
  }
  X <- matrix(X, nrow = n, ncol = parameters)
  colnames(X) <- paste0("x", 1:parameters)
  return(X)
}

generate_linear_predictor <- function(
    X,
    signal_parameters,
    noise_parameters,
    intercept,
    beta_signal) {
  W_ <- c(rep(beta_signal, signal_parameters), rep(0, noise_parameters))
  lp <- X %*% W_ + intercept
  return(lp)
}
