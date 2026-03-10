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
#' @param predictor_type Type of predictor, can be "continuous" or "binary" (only for complexity 1)
#' @param predictor_prop If predictor type is binary, the probability of a predictor taking value 1
#' @param complexity Integer (1–4) controlling the complexity of the data-generating mechanism.
#'   \itemize{
#'     \item 1 (Simple): Normal/binary predictors; purely linear predictor.
#'     \item 2 (Quadratic): Normal predictors; linear + quadratic terms.
#'     \item 3 (Interaction): Normal predictors; linear + quadratic + pairwise interaction terms.
#'     \item 4 (Nonlinear): Flexible predictor distributions (see \code{predictor_dist}); linear +
#'           quadratic + absolute value + sine + cosine + pairwise interaction terms.
#'   }
#' @param predictor_dist (complexity 4 only) Named list of distribution specifications. Each element
#'   is a list with \code{dist} (one of \code{"normal"}, \code{"uniform"}, \code{"exponential"},
#'   \code{"lognormal"}, \code{"t"}, \code{"laplace"}) and optional parameter overrides
#'   (\code{mean}, \code{sd}, \code{min}, \code{max}, \code{rate}, \code{meanlog}, \code{sdlog},
#'   \code{df}, \code{location}, \code{scale}).
#'   If \code{NULL} and complexity is 4, all predictors default to \code{normal}.
#'   Example: \code{list(x1 = list(dist = "uniform", min = 0, max = 1),
#'                       x2 = list(dist = "exponential", rate = 2))}.
#'
#' @return A data frame with one outcome column and n_signal_parameters + noise_parameters predictor columns
#' @keywords internal
generate_continuous_data <- function(
    n,
    beta_signal,
    n_signal_parameters,
    noise_parameters,
    predictor_type  = "continuous",
    predictor_prop  = NULL,
    complexity      = 1,
    predictor_dist  = NULL
) {
  parameters <- n_signal_parameters + noise_parameters
  intercept  <- 0
  X <- generate_predictors(n, parameters, predictor_type, predictor_prop,
                           complexity, predictor_dist)
  lp <- generate_linear_predictor(X, n_signal_parameters, noise_parameters,
                                  intercept, beta_signal, complexity)
  
  y    <- stats::rnorm(n, lp, 1)
  data <- cbind(y, X)
  return(as.data.frame(data))
}

#' Title Simulate binary data
#'
#' @inheritParams generate_continuous_data
#' @param mu_lp Intercept / offset for the linear predictor (controls baseline log-odds)
#' @param baseline_prob Baseline probability of outcome (i.e., probability when all predictors are 0)
#'
#' @return A data frame with one outcome column and n_signal_parameters + noise_parameters predictor columns
#' @keywords internal
generate_binary_data <- function(
    n,
    mu_lp,
    beta_signal,
    n_signal_parameters,
    noise_parameters,
    predictor_type  = "continuous",
    predictor_prop  = NULL,
    baseline_prob,
    complexity      = 1,
    predictor_dist  = NULL
) {
  parameters <- n_signal_parameters + noise_parameters
  intercept  <- mu_lp
  X <- generate_predictors(n, parameters, predictor_type, predictor_prop,
                           complexity, predictor_dist)
  lp <- generate_linear_predictor(X, n_signal_parameters, noise_parameters,
                                  intercept, beta_signal, complexity)
  y_prob <- stats::plogis(lp)
  y      <- stats::rbinom(n, 1, y_prob)
  data   <- cbind(y, X)
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
    predictor_type  = "continuous",
    predictor_prop  = NULL,
    baseline_hazard,
    censoring_rate,
    complexity      = 1,
    predictor_dist  = NULL
) {
  parameters <- n_signal_parameters + noise_parameters
  intercept  <- 0
  X <- generate_predictors(n, parameters, predictor_type, predictor_prop,
                           complexity, predictor_dist)
  lp <- generate_linear_predictor(X, n_signal_parameters, noise_parameters,
                                  intercept, beta_signal, complexity)
  
  event_time <- stats::rexp(n, rate = baseline_hazard * exp(lp))
  T_observe  <- stats::quantile(event_time, 1 - censoring_rate)
  censor_time <- rep(T_observe, n)
  event        <- as.numeric(event_time <= censor_time)
  survival_time <- pmin(event_time, censor_time)
  
  return(data.frame(time = survival_time, event = event, X))
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

update_arguments <- function(fn, opts) {
  for (key in names(opts$args)) {
    if (key %in% names(formals(fn))) {
      formals(fn)[[key]] <- opts$args[[key]]
    }
  }
  attr(fn, "outcome") <- opts$type
  return(fn)
}

# ---------------------------------------------------------------------------
#' Generate predictor matrix
#'
#' @param n          Sample size.
#' @param parameters Total number of predictors (signal + noise).
#' @param type       "continuous" or "binary" (used for complexity 1 only).
#' @param predictor_prop Prevalence for binary predictors (complexity 1).
#' @param complexity Integer 1–4 (see outcome-generator documentation).
#' @param predictor_dist Named list of per-predictor distribution specs
#'   (only consulted when complexity == 4). NULL falls back to standard normal.
#'
#' @return An n x parameters numeric matrix with column names x1, x2, ...
#' @keywords internal
generate_predictors <- function(n, parameters, type = "continuous",
                                predictor_prop = NULL,
                                complexity = 1,
                                predictor_dist = NULL) {
  
  ## ---- complexity 1: original behaviour -----------------------------------
  if (complexity == 1) {
    if (type == "binary") {
      if (is.null(predictor_prop))
        stop("predictor_prop must be provided when predictor type is binary")
      if (predictor_prop < 0 || predictor_prop > 1)
        stop("predictor_prop must be between 0 and 1")
      X <- stats::rbinom(n * parameters, 1, predictor_prop)
    } else if (type == "continuous") {
      X <- stats::rnorm(n * parameters)
    } else {
      stop("type must be one of binary or continuous")
    }
    X <- matrix(X, nrow = n, ncol = parameters)
    
    ## ---- complexity 2 & 3: standard normal ----------------------------------
  } else if (complexity %in% c(2, 3)) {
    X <- matrix(stats::rnorm(n * parameters), nrow = n, ncol = parameters)
    
    ## ---- complexity 4: flexible per-predictor distributions -----------------
  } else if (complexity == 4) {
    X <- matrix(NA_real_, nrow = n, ncol = parameters)
    col_names <- paste0("x", seq_len(parameters))
    
    for (j in seq_len(parameters)) {
      cname <- col_names[j]
      spec  <- predictor_dist[[cname]]   # NULL if not specified
      
      dist  <- if (is.null(spec)) "normal" else spec$dist
      
      X[, j] <- switch(
        dist,
        normal = {
          mu  <- if (!is.null(spec$mean)) spec$mean else 0
          sig <- if (!is.null(spec$sd))   spec$sd   else 1
          stats::rnorm(n, mean = mu, sd = sig)
        },
        uniform = {
          lo <- if (!is.null(spec$min)) spec$min else 0
          hi <- if (!is.null(spec$max)) spec$max else 1
          stats::runif(n, min = lo, max = hi)
        },
        exponential = {
          rt <- if (!is.null(spec$rate)) spec$rate else 1
          stats::rexp(n, rate = rt)
        },
        lognormal = {
          ml  <- if (!is.null(spec$meanlog)) spec$meanlog else 0
          sdl <- if (!is.null(spec$sdlog))   spec$sdlog   else 1
          stats::rlnorm(n, meanlog = ml, sdlog = sdl)
        },
        t = {
          df <- if (!is.null(spec$df)) spec$df else 5
          stats::rt(n, df = df)
        },
        laplace = {
          loc <- if (!is.null(spec$location)) spec$location else 0
          scl <- if (!is.null(spec$scale))    spec$scale    else 1
          # Laplace via difference of two exponentials
          u <- stats::runif(n, -0.5, 0.5)
          loc - scl * sign(u) * log(1 - 2 * abs(u))
        },
        stop(sprintf("Unknown distribution '%s' for predictor '%s'. ",
                     dist, cname),
             "Supported: normal, uniform, exponential, lognormal, t, laplace.")
      )
    }
    
  } else {
    stop("complexity must be 1, 2, 3, or 4")
  }
  
  colnames(X) <- paste0("x", seq_len(parameters))
  return(X)
}

# ---------------------------------------------------------------------------
#' Generate linear predictor
#'
#' @param X                  Predictor matrix (n x p).
#' @param n_signal_parameters Number of signal (non-zero-beta) predictors.
#' @param noise_parameters   Number of noise (zero-beta) predictors.
#' @param intercept          Scalar intercept added to the linear predictor.
#' @param beta_signal        Common effect size applied to all signal predictors.
#' @param complexity         Integer 1–4.
#'
#' @details
#' The linear predictor is constructed from the *signal* columns of X only
#' (columns 1 through n_signal_parameters). Noise columns never contribute.
#'
#' \strong{Complexity 1 – Linear:}
#' \deqn{lp = intercept + \sum_{j=1}^{S} \beta x_j}
#'
#' \strong{Complexity 2 – Quadratic:}
#' \deqn{lp = intercept + \sum_{j=1}^{S} \beta (x_j + x_j^2)}
#'
#' \strong{Complexity 3 – Quadratic + Interactions:}
#' \deqn{lp = intercept + \sum_{j=1}^{S} \beta (x_j + x_j^2) +
#'            \sum_{j < k}^{S} \beta x_j x_k}
#' (interaction effect size equals beta_signal)
#'
#' \strong{Complexity 4 – Nonlinear + Interactions:}
#' \deqn{lp = intercept
#'            + \sum_j \beta x_j
#'            + \sum_j \beta x_j^2
#'            + \sum_j \beta |x_j|
#'            + \sum_j \beta \sin(x_j)
#'            + \sum_j \beta \cos(x_j)
#'            + \sum_{j<k} \beta x_j x_k}
#' (all non-linear and interaction terms share beta_signal as effect size)
#'
#' @return Numeric vector of length n.
#' @keywords internal
generate_linear_predictor <- function(X, n_signal_parameters, noise_parameters,
                                      intercept, beta_signal,
                                      complexity = 1) {
  
  n <- nrow(X)
  S <- n_signal_parameters   # number of signal predictors
  
  if (S == 0) return(rep(intercept, n))
  
  Xs <- X[, seq_len(S), drop = FALSE]   # signal columns only
  lp <- rep(intercept, n)
  
  ## ---- complexity 1: purely linear ----------------------------------------
  if (complexity == 1) {
    W  <- rep(beta_signal, S)
    lp <- lp + as.vector(Xs %*% W)
    
    ## ---- complexity 2: linear + quadratic -----------------------------------
  } else if (complexity == 2) {
    W  <- rep(beta_signal, S)
    lp <- lp + as.vector(Xs %*% W)            # linear terms
    lp <- lp + as.vector((Xs^2) %*% W)        # quadratic terms
    
    ## ---- complexity 3: linear + quadratic + pairwise interactions -----------
  } else if (complexity == 3) {
    W  <- rep(beta_signal, S)
    lp <- lp + as.vector(Xs %*% W)
    lp <- lp + as.vector((Xs^2) %*% W)
    
    if (S >= 2) {
      pairs <- utils::combn(S, 2)              # 2 x C(S,2) matrix of column indices
      for (k in seq_len(ncol(pairs))) {
        j1 <- pairs[1, k]; j2 <- pairs[2, k]
        lp <- lp + beta_signal * Xs[, j1] * Xs[, j2]
      }
    }
    
    ## ---- complexity 4: nonlinear + interactions -----------------------------
  } else if (complexity == 4) {
    W  <- rep(beta_signal, S)
    lp <- lp + as.vector(Xs %*% W)            # linear
    lp <- lp + as.vector((Xs^2)    %*% W)     # quadratic
    lp <- lp + as.vector(abs(Xs)   %*% W)     # absolute value
    lp <- lp + as.vector(sin(Xs)   %*% W)     # sine
    lp <- lp + as.vector(cos(Xs)   %*% W)     # cosine
    
    if (S >= 2) {
      pairs <- utils::combn(S, 2)
      for (k in seq_len(ncol(pairs))) {
        j1 <- pairs[1, k]; j2 <- pairs[2, k]
        lp <- lp + beta_signal * Xs[, j1] * Xs[, j2]
      }
    }
    
  } else {
    stop("complexity must be 1, 2, 3, or 4")
  }
  
  return(lp)
}