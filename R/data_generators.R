# =============================================================================
# Data-generating functions for simulation studies
#
# Supports continuous, binary, and survival outcomes across four complexity
# levels. All generators share two internal workhorses:
#   - generate_predictors()       : builds the (possibly correlated) X matrix
#   - generate_linear_predictor() : constructs lp from X
#
# =============================================================================
# INTERFACE SUMMARY
# =============================================================================
#
# Core parameters (all apply uniformly across every complexity setting):
#
#   n_signal          : number of signal predictors
#   n_noise           : number of noise predictors
#   beta_signal       : base effect size for signal predictors
#   correlation       : common pairwise correlation (default = 0.3; 0 = none)
#   binary_prevalence : Bernoulli probability for ALL predictors (default = 0,
#                       i.e. all predictors are continuous)
#   distribution      : single global distribution family for ALL predictors
#                       (default = "normal"; see supported families below).
#                       When binary_prevalence > 0, ALL predictors are binary
#                       and this argument is ignored.
#                       For complexity 4 the Friedman canonical default is
#                       "uniform", applied automatically when distribution is
#                       left at "normal".
#
# Complexity (two dimensions):
#
#   complexity        : integer 1-4, controls the functional form of lp
#                         1 = Linear
#                         2 = Quadratic
#                         3 = Quadratic + Interaction
#                         4 = Friedman (1991)
#
#   predictor_strength: global linear-nonlinear weight applied to ALL signal
#                       predictors. One of:
#                         "strong"   -> w = 1.0
#                         "moderate" -> w = 0.5
#                         "weak"     -> w = 0.3
#                       Default per complexity:
#                         C1 -> "strong"   (w = 1.0)
#                         C2 -> "moderate" (w = 0.5)
#                         C3 -> "moderate" (w = 0.5)
#                         C4 -> "strong"   (w = 1.0)
#                       The user may override the default by supplying this
#                       argument explicitly.
#
# Supported distribution families (continuous predictors):
#   "normal"      : mean = 0, sd = 1
#   "uniform"     : min = 0, max = 1
#   "exponential" : rate = 1
#   "lognormal"   : meanlog = 0, sdlog = 1
#   "t"           : df = 5
#   "laplace"     : location = 0, scale = 1
#
# Outcome-specific arguments:
#   continuous : intercept (default 0)
#   binary     : mu_lp (log-odds intercept, default 0), baseline_prob (doc only)
#   survival   : baseline_hazard, censoring_rate, intercept (default 0)
# =============================================================================

# =============================================================================
# Complexity-level defaults
# =============================================================================

# Default predictor_strength per complexity level
COMPLEXITY_STRENGTH_DEFAULTS <- c(
  "1" = "strong",    # C1 Linear              : w = 1.0
  "2" = "moderate",  # C2 Quadratic           : w = 0.5
  "3" = "moderate",  # C3 Quad + Interaction  : w = 0.5
  "4" = "strong"     # C4 Friedman            : w = 1.0
)

# Default distribution per complexity level (applies when distribution = "normal"
# and binary_prevalence = 0)
COMPLEXITY_DIST_DEFAULTS <- c(
  "1" = "normal",
  "2" = "normal",
  "3" = "normal",
  "4" = "uniform"   # Friedman canonical
)

# Strength-weight lookup
STRENGTH_WEIGHTS <- c(
  strong   = 1.0,
  moderate = 0.5,
  weak     = 0.3
)

# =============================================================================
# Entry point
# =============================================================================

#' Create default data generating functions
#'
#' @param opts A list with two elements:
#'   \describe{
#'     \item{\code{type}}{Outcome type: \code{"continuous"}, \code{"binary"},
#'       or \code{"survival"}.}
#'     \item{\code{args}}{Named list of arguments to pre-set on the
#'       corresponding generator function.}
#'   }
#' @return A partially-applied generator function whose formals have been set
#'   to the values in \code{opts$args}.
#' @keywords internal
default_data_generators <- function(opts) {
  type <- opts$type
  f <- switch(
    type,
    continuous = generate_continuous_data,
    binary     = generate_binary_data,
    survival   = generate_survival_data,
    stop(sprintf(
      'opts$type must be "continuous", "binary", or "survival". Got: "%s".',
      type
    ))
  )
  return(update_arguments(f, opts))
}

# =============================================================================
# Outcome-level generators
# =============================================================================

#' Simulate continuous outcome data
#'
#' @param n                  Sample size.
#' @param n_signal           Number of signal predictors. These occupy the
#'   first \code{n_signal} columns (\code{x1} ... \code{x_S}).
#' @param n_noise            Number of noise predictors (zero coefficient).
#' @param beta_signal        Base effect size. Effective beta per predictor =
#'   \code{beta_signal * w}, where \code{w} is the strength weight.
#' @param complexity         Integer 1-4 specifying the functional form of the
#'   linear predictor and (by default) the predictor strength weight:
#'   \enumerate{
#'     \item \strong{Linear} — \eqn{lp = \alpha + w\beta\sum_j x_j}.
#'       Default strength: \code{"strong"} (w = 1).
#'     \item \strong{Quadratic} — linear + quadratic terms on all signal
#'       predictors. Default strength: \code{"moderate"} (w = 0.5).
#'     \item \strong{Quadratic + Interaction} — C2 terms + pairwise products
#'       across all signal predictors. Default strength: \code{"moderate"}
#'       (w = 0.5).
#'     \item \strong{Friedman} — canonical Friedman (1991) benchmark scaled by
#'       \code{w * beta_signal}, extended beyond 5 signal predictors.
#'       Default strength: \code{"strong"} (w = 1).
#'   }
#' @param predictor_strength Global linear-nonlinear strength weight applied
#'   uniformly to all signal predictors. One of \code{"strong"} (w = 1.0),
#'   \code{"moderate"} (w = 0.5), or \code{"weak"} (w = 0.3).
#'   When \code{NULL} (default), the complexity-level default is used:
#'   C1 = strong, C2 = moderate, C3 = moderate, C4 = strong.
#' @param correlation        Scalar in [-1, 1]. Common pairwise correlation
#'   applied to all predictors via a Gaussian copula (equicorrelation,
#'   rank-based Cholesky). Default = 0.3. Set to 0 for independence. The
#'   copula step preserves each predictor's marginal distribution exactly.
#' @param binary_prevalence  Scalar in [0, 1]. When > 0, \emph{all} predictors
#'   are drawn as Bernoulli(\code{binary_prevalence}) and \code{distribution}
#'   is ignored. When = 0 (default) all predictors are continuous.
#' @param distribution       Single character string naming the distribution
#'   family used for \emph{all} continuous predictors. Default = \code{"normal"}.
#'   For complexity 4, if this is left at \code{"normal"} the framework
#'   automatically uses \code{"uniform"} (Friedman canonical); set explicitly
#'   to \code{"uniform"} to make that choice transparent. Ignored when
#'   \code{binary_prevalence > 0}. Supported families and their default
#'   parameters:
#'   \itemize{
#'     \item \code{"normal"}      — mean = 0, sd = 1
#'     \item \code{"uniform"}     — min = 0, max = 1
#'     \item \code{"exponential"} — rate = 1
#'     \item \code{"lognormal"}   — meanlog = 0, sdlog = 1
#'     \item \code{"t"}           — df = 5
#'     \item \code{"laplace"}     — location = 0, scale = 1
#'   }
#' @param intercept          Scalar intercept added to the linear predictor.
#'   Default = 0.
#'
#' @return A data frame with columns \code{y}, \code{x1}, \code{x2}, ...
#'
#' @references
#' Friedman, J. H. (1991). Multivariate adaptive regression splines.
#'   \emph{The Annals of Statistics}, 19(1), 1-67.
#'   \doi{10.1214/aos/1176347963}
#'
#' Breiman, L. (1996). Bagging predictors.
#'   \emph{Machine Learning}, 24(2), 123-140.
#'
#' @keywords internal
generate_continuous_data <- function(
    n,
    n_signal,
    n_noise,
    beta_signal,
    complexity         = 1,
    predictor_strength = NULL,
    correlation        = 0.3,
    binary_prevalence  = 0,
    distribution       = "normal",
    intercept          = 0
) {
  predictor_strength <- resolve_strength(predictor_strength, complexity)
  X  <- generate_predictors(n, n_signal, n_noise,
                            complexity, correlation,
                            binary_prevalence, distribution)
  lp <- generate_linear_predictor(X, n_signal, n_noise,
                                  intercept, beta_signal,
                                  complexity, predictor_strength)
  y  <- stats::rnorm(n, lp, 1)
  return(as.data.frame(cbind(y, X)))
}

#' Simulate binary outcome data
#'
#' @inheritParams generate_continuous_data
#' @param mu_lp         Intercept on the log-odds scale. Default = 0.
#' @param baseline_prob Nominal baseline event probability (documentation only;
#'   the realised probability is determined by \code{mu_lp}).
#'
#' @return A data frame with columns \code{y} (0/1), \code{x1}, \code{x2}, ...
#' @keywords internal
generate_binary_data <- function(
    n,
    n_signal,
    n_noise,
    beta_signal,
    complexity         = 1,
    predictor_strength = NULL,
    correlation        = 0.3,
    binary_prevalence  = 0,
    distribution       = "normal",
    mu_lp              = 0,
    baseline_prob      = 0.5
) {
  predictor_strength <- resolve_strength(predictor_strength, complexity)
  X  <- generate_predictors(n, n_signal, n_noise,
                            complexity, correlation,
                            binary_prevalence, distribution)
  lp <- generate_linear_predictor(X, n_signal, n_noise,
                                  intercept = mu_lp, beta_signal,
                                  complexity, predictor_strength)
  y  <- stats::rbinom(n, 1, stats::plogis(lp))
  return(as.data.frame(cbind(y, X)))
}

#' Simulate survival outcome data
#'
#' @inheritParams generate_continuous_data
#' @param baseline_hazard Baseline hazard rate (exponential survival model).
#' @param censoring_rate  Administrative censoring proportion in (0, 1).
#'
#' @return A data frame with columns \code{time}, \code{event} (0 = censored,
#'   1 = event), \code{x1}, \code{x2}, ...
#' @keywords internal
generate_survival_data <- function(
    n,
    n_signal,
    n_noise,
    beta_signal,
    baseline_hazard,
    censoring_rate,
    complexity         = 1,
    predictor_strength = NULL,
    correlation        = 0.3,
    binary_prevalence  = 0,
    distribution       = "normal",
    intercept          = 0
) {
  predictor_strength <- resolve_strength(predictor_strength, complexity)
  X  <- generate_predictors(n, n_signal, n_noise,
                            complexity, correlation,
                            binary_prevalence, distribution)
  lp <- generate_linear_predictor(X, n_signal, n_noise,
                                  intercept, beta_signal,
                                  complexity, predictor_strength)
  
  event_time    <- stats::rexp(n, rate = baseline_hazard * exp(lp))
  T_observe     <- stats::quantile(event_time, 1 - censoring_rate)
  censor_time   <- rep(T_observe, n)
  event         <- as.numeric(event_time <= censor_time)
  survival_time <- pmin(event_time, censor_time)
  
  return(data.frame(time = survival_time, event = event, X))
}

# =============================================================================
# Internal helpers
# =============================================================================

# -----------------------------------------------------------------------------
# update_arguments
# Bakes opts$args values into the formals of fn.
# -----------------------------------------------------------------------------
update_arguments <- function(fn, opts) {
  for (key in names(opts$args)) {
    if (key %in% names(formals(fn))) {
      formals(fn)[[key]] <- opts$args[[key]]
    }
  }
  attr(fn, "outcome") <- opts$type
  return(fn)
}

# -----------------------------------------------------------------------------
# resolve_strength
#
# Returns the predictor_strength keyword to use, applying the complexity-level
# default when the user has not supplied an explicit value (NULL).
# Also validates the keyword against STRENGTH_WEIGHTS.
# -----------------------------------------------------------------------------
resolve_strength <- function(predictor_strength, complexity) {
  if (is.null(predictor_strength)) {
    predictor_strength <- COMPLEXITY_STRENGTH_DEFAULTS[[as.character(complexity)]]
  }
  
  if (!predictor_strength %in% names(STRENGTH_WEIGHTS))
    stop(sprintf(
      'predictor_strength must be one of: "%s". Got: "%s".',
      paste(names(STRENGTH_WEIGHTS), collapse = '", "'),
      predictor_strength
    ))
  
  return(predictor_strength)
}

# -----------------------------------------------------------------------------
# draw_predictors
#
# Draws an n x p matrix of i.i.d. observations from the chosen distribution
# family using its canonical default parameters. All p predictors share the
# same family (the distribution argument is global).
#
# Supported families and their canonical defaults:
#   normal      : mean = 0, sd = 1
#   uniform     : min = 0, max = 1
#   exponential : rate = 1
#   lognormal   : meanlog = 0, sdlog = 1
#   t           : df = 5
#   laplace     : location = 0, scale = 1  (via quantile transform)
#   binary      : prop = binary_prevalence  (passed in as argument)
# -----------------------------------------------------------------------------
draw_predictors <- function(n, p, family, binary_prevalence = 0) {
  
  vals <- switch(
    family,
    
    normal = stats::rnorm(n * p),
    
    uniform = stats::runif(n * p),
    
    exponential = stats::rexp(n * p),
    
    lognormal = stats::rlnorm(n * p),
    
    t = stats::rt(n * p, df = 5),
    
    laplace = {
      u <- stats::runif(n * p, -0.5, 0.5)
      -sign(u) * log(1 - 2 * abs(u))   # standard Laplace (loc=0, scale=1)
    },
    
    binary = stats::rbinom(n * p, 1, binary_prevalence),
    
    stop(sprintf(
      paste0('Unknown distribution "%s". ',
             'Supported: "normal", "uniform", "exponential", ',
             '"lognormal", "t", "laplace".'),
      family
    ))
  )
  
  matrix(vals, nrow = n, ncol = p)
}

# -----------------------------------------------------------------------------
# resolve_family
#
# Determines the actual distribution family to draw from. Resolution:
#   1. binary_prevalence > 0  -> "binary" (overrides distribution)
#   2. complexity == 4 AND distribution == "normal"  -> "uniform" (C4 canonical)
#   3. Otherwise -> distribution as supplied
# -----------------------------------------------------------------------------
resolve_family <- function(complexity, distribution, binary_prevalence) {
  if (binary_prevalence > 0) return("binary")
  if (complexity == 4 && distribution == "normal") return("uniform")
  return(distribution)
}

# -----------------------------------------------------------------------------
# apply_correlation
#
# Induces a common pairwise correlation rho across all p columns via a
# Gaussian copula (equicorrelation matrix, rank-based Cholesky). The marginal
# distribution of every column is preserved exactly.
# -----------------------------------------------------------------------------
apply_correlation <- function(X, rho) {
  n <- nrow(X)
  p <- ncol(X)
  
  if (rho == 0) return(X)
  
  min_rho <- if (p > 1) -1 / (p - 1) else -1
  if (rho < min_rho)
    warning(sprintf(
      paste0("correlation = %.3f may produce a non-PSD equicorrelation matrix ",
             "for p = %d predictors (minimum valid rho = %.4f)."),
      rho, p, min_rho
    ))
  
  cor_mat <- matrix(rho, p, p); diag(cor_mat) <- 1
  
  eigs <- eigen(cor_mat, symmetric = TRUE, only.values = TRUE)$values
  if (any(eigs < -1e-8))
    stop(sprintf(
      "Equicorrelation matrix with rho = %.3f is not PSD for p = %d.",
      rho, p
    ))
  
  L      <- chol(cor_mat)
  U      <- apply(X, 2, function(col) rank(col, ties.method = "average") / (n + 1))
  Z_corr <- qnorm(U) %*% t(L)
  
  X_corr <- X
  for (j in seq_len(p)) {
    orig_sorted   <- sort(X[, j])
    new_ranks_int <- pmax(1L, pmin(n, round(rank(Z_corr[, j], ties.method = "average"))))
    X_corr[, j]  <- orig_sorted[new_ranks_int]
  }
  
  colnames(X_corr) <- colnames(X)
  return(X_corr)
}

# =============================================================================
# generate_predictors
# =============================================================================

#' Generate the n x p predictor matrix
#'
#' Draws all predictors from a single global distribution family, then
#' optionally applies an equicorrelation structure via a Gaussian copula.
#'
#' @param n                Sample size.
#' @param n_signal         Number of signal predictors.
#' @param n_noise          Number of noise predictors.
#' @param complexity       Integer 1-4 (used to resolve the C4 distribution
#'   default).
#' @param correlation      Scalar common pairwise correlation; 0 = independent.
#' @param binary_prevalence Scalar Bernoulli prevalence; 0 = all continuous.
#' @param distribution     Global continuous distribution family.
#'
#' @return Named n x p numeric matrix (column names: x1, x2, ...).
#' @keywords internal
generate_predictors <- function(n,
                                n_signal,
                                n_noise,
                                complexity        = 1,
                                correlation       = 0.3,
                                binary_prevalence = 0,
                                distribution      = "normal") {
  
  p         <- n_signal + n_noise
  col_names <- paste0("x", seq_len(p))
  
  # ---- validate inputs -------------------------------------------------------
  if (!is.numeric(n_signal) || n_signal < 1)
    stop("n_signal must be a positive integer.")
  if (!is.numeric(n_noise)  || n_noise  < 0)
    stop("n_noise must be a non-negative integer.")
  if (binary_prevalence < 0 || binary_prevalence > 1)
    stop("binary_prevalence must be in [0, 1].")
  if (!is.numeric(correlation) || length(correlation) != 1 ||
      correlation < -1 || correlation > 1)
    stop("correlation must be a single numeric value in [-1, 1].")
  
  # ---- determine distribution family -----------------------------------------
  family <- resolve_family(complexity, distribution, binary_prevalence)
  
  # ---- draw all predictors from the global family ----------------------------
  X             <- draw_predictors(n, p, family, binary_prevalence)
  colnames(X)   <- col_names
  
  # ---- apply equicorrelation if requested ------------------------------------
  if (correlation != 0)
    X <- apply_correlation(X, correlation)
  
  colnames(X) <- col_names
  return(X)
}

# =============================================================================
# generate_linear_predictor
# =============================================================================

#' Construct the linear predictor
#'
#' @param X                  n x p predictor matrix (colnames: x1, x2, ...).
#' @param n_signal           Signal predictor count.
#' @param n_noise            Noise predictor count.
#' @param intercept          Scalar intercept.
#' @param beta_signal        Base effect size.
#' @param complexity         Integer 1-4.
#' @param predictor_strength Resolved strength keyword ("strong", "moderate",
#'   or "weak"); must not be NULL at this point.
#'
#' @details
#' All signal predictors participate in every term implied by their complexity
#' level. The effective beta is \code{beta_signal * w} where \code{w} is the
#' strength weight. Noise predictors always contribute zero.
#'
#' \strong{Complexity 1 — Linear:}
#' \deqn{lp = \alpha + w\beta \sum_{j=1}^{S} x_j}
#'
#' \strong{Complexity 2 — Quadratic:}
#' \deqn{lp = \alpha + w\beta \sum_{j=1}^{S} (x_j + x_j^2)}
#'
#' \strong{Complexity 3 — Quadratic + Pairwise Interactions:}
#' \deqn{lp = \alpha
#'   + w\beta \sum_{j=1}^{S} (x_j + x_j^2)
#'   + w\beta \sum_{j < k}^{S} x_j x_k}
#'
#' \strong{Complexity 4 — Friedman (1991):}
#' Uses the canonical Friedman #1 function, scaled by \code{w * beta_signal}:
#' \deqn{lp = \alpha
#'   + w\beta \bigl[10\sin(\pi x_1 x_2) + 20(x_3-0.5)^2 + 10x_4 + 5x_5\bigr]}
#' For each signal predictor k \eqn{\ge} 6:
#' \deqn{+ w\beta_k \bigl[\sin(\pi x_k x_{k-1}) + (x_k-0.5)^2\bigr]}
#'
#' @references
#' Friedman, J. H. (1991). Multivariate adaptive regression splines.
#'   \emph{The Annals of Statistics}, 19(1), 1-67.
#'
#' Breiman, L. (1996). Bagging predictors.
#'   \emph{Machine Learning}, 24(2), 123-140.
#'
#' @return Numeric vector of length n.
#' @keywords internal
generate_linear_predictor <- function(X,
                                      n_signal,
                                      n_noise,
                                      intercept,
                                      beta_signal,
                                      complexity,
                                      predictor_strength) {
  
  n  <- nrow(X)
  lp <- rep(intercept, n)
  
  if (n_signal == 0) return(lp)
  
  # Effective beta: one value applies uniformly to all signal predictors
  w          <- STRENGTH_WEIGHTS[[predictor_strength]]
  eff_beta   <- beta_signal * w
  Xs         <- X[, seq_len(n_signal), drop = FALSE]   # signal columns only
  
  # ---- Complexity 1: linear --------------------------------------------------
  if (complexity == 1) {
    lp <- lp + eff_beta * rowSums(Xs)
    
    # ---- Complexity 2: linear + quadratic (all signal predictors) -------------
  } else if (complexity == 2) {
    lp <- lp + eff_beta * rowSums(Xs)         # linear
    lp <- lp + eff_beta * rowSums(Xs^2)       # quadratic
    
    # ---- Complexity 3: linear + quadratic + pairwise interactions -------------
  } else if (complexity == 3) {
    lp <- lp + eff_beta * rowSums(Xs)
    lp <- lp + eff_beta * rowSums(Xs^2)
    
    if (n_signal >= 2) {
      pairs <- utils::combn(n_signal, 2)       # 2 x C(S,2) index matrix
      for (k in seq_len(ncol(pairs))) {
        j1 <- pairs[1, k]; j2 <- pairs[2, k]
        lp <- lp + eff_beta * Xs[, j1] * Xs[, j2]
      }
    }
    
    # ---- Complexity 4: Friedman (1991) benchmark ------------------------------
  } else if (complexity == 4) {
    if (n_signal < 5)
      warning(sprintf(
        paste0("Complexity 4 (Friedman) requires >= 5 signal predictors; ",
               "only %d supplied. Some Friedman terms will be omitted."),
        n_signal
      ))
    
    xcol <- function(k) if (k <= n_signal) Xs[, k] else rep(0, n)
    
    x1 <- xcol(1); x2 <- xcol(2); x3 <- xcol(3)
    x4 <- xcol(4); x5 <- xcol(5)
    
    # Canonical Friedman #1 terms (Friedman 1991, Eq. 4.3; Breiman 1996 p. 126)
    if (n_signal >= 2) lp <- lp + eff_beta * 10 * sin(pi * x1 * x2)
    if (n_signal >= 3) lp <- lp + eff_beta * 20 * (x3 - 0.5)^2
    if (n_signal >= 4) lp <- lp + eff_beta * 10 * x4
    if (n_signal >= 5) lp <- lp + eff_beta *  5 * x5
    
    # Extended terms for signal predictors 6, 7, ...
    if (n_signal >= 6) {
      for (k in 6:n_signal) {
        lp <- lp + eff_beta * (sin(pi * xcol(k) * xcol(k - 1)) +
                                 (xcol(k) - 0.5)^2)
      }
    }
    
  } else {
    stop("complexity must be 1, 2, 3, or 4.")
  }
  
  return(lp)
}
