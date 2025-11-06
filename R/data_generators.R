#' Title Create default data generating functions
#'
#' @param opts A list of options to be used with the data generating function. Must include type as either "binary", "continuous", or "survival".
#' Arguments to be passed to the data generating function must be stored in a list item named args.
#' For options that can be passed to the different default generators see \link{generate_continuous_data}, \link{generate_binary_data}, or \link{generate_survival_data}.
#' @return A function with default arguments set to the values passed with opts
#' @keywords internal
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

#' Title Simulate continuous data
#'
#' @param n Sample size of simulated dataset
#' @param beta_signal Association between signal predictors and the outcome
#' @param n_signal_parameters Number of predictors that have a non zero association with the outcome
#' @param noise_parameters Number of predictors with no association with outcome
#' @param predictor_type Type of predictor, can be "continuous" or "binary"
#' @param predictor_prop If predictor type is binary, the probability of a predictor taking value 1
#'
#' @return A data frame with one outcome column and n_signal_parameters + noise_parameters predictor columns
#' @keywords internal
generate_continuous_data <- function(
  n,
  beta_signal,
  n_signal_parameters,
  noise_parameters,
  predictor_type,
  predictor_prop = NULL
) {
  parameters <- n_signal_parameters + noise_parameters
  intercept <- 0
  X <- generate_predictors(n, parameters, predictor_type, predictor_prop)
  lp <- generate_linear_predictor(
    X,
    n_signal_parameters,
    noise_parameters,
    intercept,
    beta_signal
  )

  y <- stats::rnorm(n, lp, 1) # error variance is 1
  data <- cbind(y, X)
  return(as.data.frame(data))
}

#' Title Simulate binary data
#'
#' @inheritParams generate_continuous_data
#' @param baseline_prob Baseline probability of outcome (i.e., probability when all predictors are 0)
#'
#' @return A data frame with one outcome column and n_signal_parameters + noise_parameters predictor columns
#' @keywords internal
#' @export
generate_binary_data <- function(
  n,
  mu_lp,
  beta_signal,
  n_signal_parameters,
  noise_parameters,
  predictor_type,
  predictor_prop = NULL,
  baseline_prob
) {
  parameters <- n_signal_parameters + noise_parameters
  intercept <- mu_lp
  X <- generate_predictors(n, parameters, predictor_type, predictor_prop)
  lp <- generate_linear_predictor(
    X,
    n_signal_parameters,
    noise_parameters,
    intercept,
    beta_signal
  )
  y_prob <- stats::plogis(lp)
  y <- stats::rbinom(n, 1, y_prob)
  data <- cbind(y, X)
  return(as.data.frame(data))
}

#' Title Simulate survival data
#'
#' @inheritParams generate_continuous_data
#' @param baseline_hazard Baseline hazard
#' @param censoring_rate Early drop out/censoring rate
#'
#' @return A data frame with a time ("time"), event status ("event") (0 = censored, 1 = event), and n_signal_parameters + noise_parameters predictor columns ("x1", "x2", ... .)
#' @keywords internal
generate_survival_data <- function(
  n,
  beta_signal,
  n_signal_parameters,
  noise_parameters,
  predictor_type,
  predictor_prop,
  baseline_hazard,
  censoring_rate
) {
  parameters <- n_signal_parameters + noise_parameters
  intercept <- 0
  X <- generate_predictors(n, parameters, predictor_type, predictor_prop)
  lp <- generate_linear_predictor(
    X,
    n_signal_parameters,
    noise_parameters,
    intercept,
    beta_signal
  )

  # Generate survival times
  event_time <- stats::rexp(n, rate = baseline_hazard * exp(lp))
  # Introduce right-censoring at a median time
  T_observe <- stats::quantile(event_time, 1 - censoring_rate)
  censor_time <- rep(T_observe, n)
  event <- as.numeric(event_time <= censor_time)
  survival_time <- pmin(event_time, censor_time)

  # Return survival data as a data frame
  return(data.frame(
    time = survival_time,
    event = event,
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
    X <- stats::rbinom(n * parameters, 1, predictor_prop)
  } else if (type == "continuous") {
    X <- stats::rnorm(n * parameters)
  } else {
    stop("type must be one of binary or continuous")
  }
  X <- matrix(X, nrow = n, ncol = parameters)
  colnames(X) <- paste0("x", 1:parameters)
  return(X)
}

generate_linear_predictor <- function(
  X,
  n_signal_parameters,
  noise_parameters,
  intercept,
  beta_signal
) {
  W_ <- c(rep(beta_signal, n_signal_parameters), rep(0, noise_parameters))
  lp <- X %*% W_ + intercept
  return(lp)
}
