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
#   n_signal_parameters : number of signal predictors
#   noise_parameters    : number of noise predictors
#   beta_signal         : base effect size for signal predictors (overall scale)
#   predictor_type      : "continuous" (default) or "binary".
#                         When "binary", ALL predictors are Bernoulli and
#                         binary_prevalence must be supplied; distribution is
#                         ignored. When "continuous", distribution governs the
#                         family.
#   binary_prevalence   : Bernoulli probability for ALL predictors.
#                         Required (and used) when predictor_type = "binary".
#                         Ignored when predictor_type = "continuous".
#                         Default = 0 (no binary predictors).
#   correlation         : common pairwise correlation (default = 0.3; 0 = none)
#   distribution        : single global distribution family for ALL continuous
#                         predictors (default = "normal"; ignored when
#                         predictor_type = "binary"). Supported families:
#                           "normal", "uniform", "exponential",
#                           "lognormal", "t", "laplace".
#                         For complexity 4 the Friedman canonical default is
#                         "uniform", applied automatically when distribution is
#                         left at "normal".
#
# Complexity and nonlinear strength:
#
#   complexity          : integer 1-4, controls the functional form of lp
#                           1 = Linear
#                           2 = Quadratic
#                           3 = Quadratic + Interaction
#                           4 = Friedman (1991)
#
#   nonlinear_strength  : for C2/C3 only, the FRACTION of signal variance carried
#                         by the nonlinear (linearly-inaccessible) component, in
#                         [0, 1). Defaults per complexity:
#                           C1 -> 0.0   (pure linear)
#                           C2 -> 0.2   (20% nonlinear)
#                           C3 -> 0.3   (30% nonlinear)
#                           C4 -> 0.0   (canonical Friedman; argument ignored)
#                         Supplied as a variance fraction f and converted
#                         internally to the nonlinear-to-linear SD ratio
#                           kappa = sqrt(f / (1 - f))
#                         so the realised nonlinear variance fraction equals f
#                         exactly (the linear and nonlinear pieces are
#                         orthogonal by construction). Larger f hides more of the
#                         signal from a linear-in-X model, making C2/C3 harder for
#                         AUC / C-index and requiring more sample size. C1 and C4
#                         ignore this argument.
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

# Default distribution per complexity level (applies when distribution = "normal"
# and binary_prevalence = 0)
COMPLEXITY_DIST_DEFAULTS <- c(
  "1" = "normal",
  "2" = "normal",
  "3" = "normal",
  "4" = "uniform"   # Friedman canonical
)

# Nonlinear strength per complexity for C2/C3: the FRACTION of signal variance
# carried by the nonlinear (linearly-inaccessible) component. 0 = pure linear.
# Internally converted to the nonlinear-to-linear SD ratio kappa = sqrt(f/(1-f))
# so the realised nonlinear variance fraction equals f exactly. Larger f hides
# more signal from a linear-in-X model => harder for AUC / C-index and more
# sample size required. C1 and C4 ignore this (C1 is pure linear; C4 uses the
# canonical Friedman form).
COMPLEXITY_NONLINEAR_STRENGTH_DEFAULTS <- c(
  "1" = 0.0,
  "2" = 0.2,
  "3" = 0.3,
  "4" = 0.0
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
#' @param n_signal_parameters Number of signal predictors. These occupy the
#'   first \code{n_signal_parameters} columns (\code{x1} ... \code{x_S}).
#' @param noise_parameters    Number of noise predictors (zero coefficient).
#' @param beta_signal        Base effect size / overall scale of the signal.
#' @param complexity         Integer 1-4 specifying the functional form of the
#'   linear predictor:
#'   \enumerate{
#'     \item \strong{Linear} — \eqn{lp = \alpha + \beta\sum_j x_j}.
#'     \item \strong{Quadratic} — linear + a quadratic nonlinear component.
#'     \item \strong{Quadratic + Interaction} — linear + quadratic and pairwise
#'       interaction nonlinear component.
#'     \item \strong{Friedman} — canonical Friedman (1991) benchmark.
#'   }
#' @param nonlinear_strength Fraction of signal variance carried by the
#'   nonlinear component (C2/C3 only), in [0, 1). When \code{NULL} (default), the
#'   complexity-level default is used: C1 = 0, C2 = 0.2, C3 = 0.3, C4 = 0.
#'   Ignored for C1 (pure linear) and C4 (canonical Friedman).
#' @param predictor_type     Type of predictors: \code{"continuous"} (default)
#'   or \code{"binary"}. When \code{"binary"}, all predictors are drawn as
#'   Bernoulli(\code{binary_prevalence}); \code{binary_prevalence} must be in
#'   (0, 1] and \code{distribution} is ignored.
#' @param binary_prevalence  Scalar in (0, 1]. Bernoulli probability applied to
#'   all predictors when \code{predictor_type = "binary"}. Default = 0.
#' @param correlation        Scalar in [-1, 1]. Common pairwise correlation
#'   applied via a Gaussian copula (equicorrelation, rank-based Cholesky).
#'   Default = 0.3. Set to 0 for independence.
#' @param distribution       Distribution family for \emph{all} continuous
#'   predictors. Default = \code{"normal"}. For complexity 4, if left at
#'   \code{"normal"} the framework uses \code{"uniform"} (Friedman canonical).
#' @param intercept          Scalar intercept added to the linear predictor.
#'   Default = 0.
#'
#' @return A data frame with columns \code{y}, \code{x1}, \code{x2}, ...
#'
#' @references
#' Friedman, J. H. (1991). Multivariate adaptive regression splines.
#'   \emph{The Annals of Statistics}, 19(1), 1-67. \doi{10.1214/aos/1176347963}
#'
#' @keywords internal
generate_continuous_data <- function(
    n,
    n_signal_parameters,
    noise_parameters,
    beta_signal,
    complexity         = 1,
    nonlinear_strength = NULL,
    predictor_type     = "continuous",
    binary_prevalence  = 0,
    correlation        = 0.3,
    distribution       = "normal",
    intercept          = 0
) {
  X  <- generate_predictors(n, n_signal_parameters, noise_parameters,
                            complexity, predictor_type,
                            binary_prevalence, correlation, distribution)
  lp <- generate_linear_predictor(X, n_signal_parameters, noise_parameters,
                                  intercept, beta_signal,
                                  complexity, nonlinear_strength)
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
    n_signal_parameters,
    noise_parameters,
    beta_signal,
    complexity         = 1,
    nonlinear_strength = NULL,
    predictor_type     = "continuous",
    binary_prevalence  = 0,
    correlation        = 0.3,
    distribution       = "normal",
    mu_lp              = 0,
    baseline_prob      = 0.5
) {
  X  <- generate_predictors(n, n_signal_parameters, noise_parameters,
                            complexity, predictor_type,
                            binary_prevalence, correlation, distribution)
  lp <- generate_linear_predictor(X, n_signal_parameters, noise_parameters,
                                  intercept = mu_lp, beta_signal,
                                  complexity, nonlinear_strength)
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
    n_signal_parameters,
    noise_parameters,
    beta_signal,
    baseline_hazard,
    censoring_rate,
    complexity         = 1,
    nonlinear_strength = NULL,
    predictor_type     = "continuous",
    binary_prevalence  = 0,
    correlation        = 0.3,
    distribution       = "normal",
    intercept          = 0
) {
  X  <- generate_predictors(n, n_signal_parameters, noise_parameters,
                            complexity, predictor_type,
                            binary_prevalence, correlation, distribution)
  lp <- generate_linear_predictor(X, n_signal_parameters, noise_parameters,
                                  intercept, beta_signal,
                                  complexity, nonlinear_strength)
  
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
# resolve_nonlinear_strength
#
# Returns the nonlinear-strength fraction to use, applying the complexity-level
# default when the user has not supplied an explicit value (NULL), and
# validating that it lies in [0, 1).
# -----------------------------------------------------------------------------
resolve_nonlinear_strength <- function(nonlinear_strength, complexity) {
  if (is.null(nonlinear_strength)) {
    nonlinear_strength <-
      COMPLEXITY_NONLINEAR_STRENGTH_DEFAULTS[[as.character(complexity)]]
  }
  if (!is.numeric(nonlinear_strength) || length(nonlinear_strength) != 1 ||
      !is.finite(nonlinear_strength) ||
      nonlinear_strength < 0 || nonlinear_strength >= 1) {
    stop("nonlinear_strength must be a single number in [0, 1). Got: ",
         format(nonlinear_strength))
  }
  return(nonlinear_strength)
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
# Determines the actual distribution family to draw from. Resolution order:
#   1. predictor_type == "binary"  -> "binary"  (primary switch; overrides all)
#   2. complexity == 4 AND distribution == "normal"
#                                  -> "uniform" (C4 Friedman canonical default)
#   3. Otherwise                   -> distribution as supplied
# -----------------------------------------------------------------------------
resolve_family <- function(complexity, predictor_type, distribution,
                           binary_prevalence) {
  if (predictor_type == "binary") return("binary")
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
#' @param n_signal_parameters Number of signal predictors.
#' @param noise_parameters  Number of noise predictors.
#' @param complexity       Integer 1-4 (used to resolve the C4 distribution
#'   default).
#' @param predictor_type   \code{"continuous"} (default) or \code{"binary"}.
#' @param binary_prevalence Bernoulli probability; used when
#'   \code{predictor_type = "binary"}.
#' @param correlation      Scalar common pairwise correlation; 0 = independent.
#' @param distribution     Global continuous distribution family; used when
#'   \code{predictor_type = "continuous"}.
#'
#' @return Named n x p numeric matrix (column names: x1, x2, ...).
#' @keywords internal
generate_predictors <- function(n,
                                n_signal_parameters,
                                noise_parameters,
                                complexity        = 1,
                                predictor_type    = "continuous",
                                binary_prevalence = 0,
                                correlation       = 0.3,
                                distribution      = "normal") {
  
  p         <- n_signal_parameters + noise_parameters
  col_names <- paste0("x", seq_len(p))
  
  # ---- validate inputs -------------------------------------------------------
  if (!is.numeric(n_signal_parameters) || n_signal_parameters < 1)
    stop("n_signal_parameters must be a positive integer.")
  if (!is.numeric(noise_parameters)  || noise_parameters  < 0)
    stop("noise_parameters must be a non-negative integer.")
  if (!predictor_type %in% c("continuous", "binary"))
    stop('predictor_type must be "continuous" or "binary".')
  if (!is.numeric(correlation) || length(correlation) != 1 ||
      correlation < -1 || correlation > 1)
    stop("correlation must be a single numeric value in [-1, 1].")
  if (predictor_type == "binary") {
    if (binary_prevalence <= 0 || binary_prevalence > 1)
      stop("binary_prevalence must be in (0, 1] when predictor_type = \"binary\".")
  }
  
  # ---- determine distribution family -----------------------------------------
  family <- resolve_family(complexity, predictor_type, distribution,
                           binary_prevalence)
  
  # ---- draw all predictors from the global family ----------------------------
  X           <- draw_predictors(n, p, family, binary_prevalence)
  colnames(X) <- col_names
  
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
#' @param n_signal_parameters Signal predictor count.
#' @param noise_parameters    Noise predictor count.
#' @param intercept          Scalar intercept (log-odds intercept for binary,
#'   log-hazard intercept for survival, mean intercept for continuous).
#' @param beta_signal        Base effect size (overall scale). The tuning
#'   routines scale this to hit the target oracle metric.
#' @param complexity         Integer 1-4.
#' @param nonlinear_strength Fraction of signal variance carried by the
#'   nonlinear component (C2/C3 only), in [0, 1). \code{NULL} uses the
#'   complexity-level default in \code{COMPLEXITY_NONLINEAR_STRENGTH_DEFAULTS}.
#'
#' @details
#' \strong{Why variance fraction rather than an R^2 split.}
#' For a continuous outcome whose target is R^2, splitting the signal by
#' latent-scale variance is exact: R^2 is a variance ratio, so a correctly
#' specified linear model recovers exactly the linear share. For binary (AUC)
#' and survival (Harrell's C) outcomes that logic is only approximate, because
#' AUC and C are rank/link-mediated. What carries over is the key property: the
#' nonlinear component is built to be \emph{inaccessible to any linear-in-X
#' model}, so the discrimination it carries genuinely requires a nonlinear
#' learner and more sample size.
#'
#' \strong{Construction.} The linear predictor is
#' \deqn{lp = \alpha + \beta\,\Bigl(\,\underbrace{\textstyle\sum_j x_j}_{L}
#'        \; + \; \kappa\,\mathrm{sd}(L)\, \underbrace{\tilde N}_{N\,\mathrm{std}} \Bigr)}
#' where \eqn{L = \sum_j x_j} is the linear score (identical to complexity 1)
#' and \eqn{\tilde N} is the nonlinear aggregate (\eqn{\sum_j x_j^2} for C2;
#' plus \eqn{\sum_{j<k} x_j x_k} for C3) \emph{residualised against the full
#' design} \eqn{[1, x_1, \ldots, x_S]} and standardised to unit SD. Because
#' \eqn{L} and \eqn{\tilde N} are orthogonal, the nonlinear variance fraction is
#' \deqn{f = \frac{\kappa^2}{1 + \kappa^2},\qquad\text{equivalently}\qquad
#'       \kappa = \sqrt{\frac{f}{1 - f}}.}
#' The user supplies \code{nonlinear_strength} = \eqn{f} (e.g. 0.2 = 20% of the
#' signal variance is nonlinear) and \eqn{\kappa} is computed internally so the
#' realised fraction equals \eqn{f} exactly.
#'
#' Because the bracket is fixed given X, \code{lp} is linear in
#' \code{beta_signal}, so the tuning routines (which scale \code{beta_signal} to
#' hit the target oracle AUC / C-index) work unchanged.
#'
#' \strong{Calibrating difficulty.} \eqn{f} sets the difficulty order but not, by
#' itself, a precise AUC/C-index gap: the metric reduction a given \eqn{f}
#' produces also depends on the tuned scale, the prevalence/censoring, and the
#' predictor family. To target a specific linear-accessible metric, calibrate
#' \eqn{f} per complexity by simulation (raise it until a fitted GLM/Cox on the
#' linear terms reaches the desired value while \code{beta_signal} keeps the
#' oracle metric on target).
#'
#' \strong{Complexity 1 — Linear:} \eqn{lp = \alpha + \beta \sum_j x_j}.
#'
#' \strong{Complexity 4 — Friedman (1991)} (canonical, \code{nonlinear_strength}
#' ignored): \eqn{lp = \alpha + \beta[10\sin(\pi x_1 x_2) + 20(x_3-0.5)^2
#' + 10x_4 + 5x_5]}. Friedman contains linear-accessible terms
#' (\eqn{10x_4 + 5x_5}); to place C4 on the same controlled ladder as C2/C3,
#' residualise the Friedman vector against Xs and apply the same kappa split.
#'
#' @references
#' Friedman, J. H. (1991). Multivariate adaptive regression splines.
#'   \emph{The Annals of Statistics}, 19(1), 1-67.
#'
#' @return Numeric vector of length n.
#' @keywords internal
generate_linear_predictor <- function(X,
                                      n_signal_parameters,
                                      noise_parameters,
                                      intercept,
                                      beta_signal,
                                      complexity,
                                      nonlinear_strength = NULL) {
  
  n  <- nrow(X)
  lp <- rep(intercept, n)
  
  if (n_signal_parameters == 0) return(lp)
  
  eff_beta <- beta_signal               # overall signal scale (no strength weight)
  
  Xs  <- X[, seq_len(n_signal_parameters), drop = FALSE]
  S   <- n_signal_parameters
  lin <- rowSums(Xs)
  
  # ---- Complexity 1: pure linear (the f -> 0 limit) -------------------------
  if (complexity == 1) {
    return(lp + eff_beta * lin)
  }
  
  # ---- Complexity 4: canonical Friedman (1991); nonlinear_strength ignored --
  if (complexity == 4) {
    if (n_signal_parameters < 5)
      warning(sprintf(
        "Complexity 4 requires >=5 signal predictors; only %d supplied.",
        n_signal_parameters))
    xcol <- function(k) if (k <= S) Xs[, k] else rep(0, n)
    x1 <- xcol(1); x2 <- xcol(2); x3 <- xcol(3); x4 <- xcol(4); x5 <- xcol(5)
    fr <- 10 * sin(pi * x1 * x2) + 20 * (x3 - 0.5)^2 + 10 * x4 + 5 * x5
    return(lp + eff_beta * fr)
  }
  
  # ---- Complexity 2 / 3: linear + inaccessible nonlinear component ----------
  # Resolve the nonlinear variance fraction f and convert to the SD ratio kappa.
  f     <- resolve_nonlinear_strength(nonlinear_strength, complexity)
  kappa <- sqrt(f / (1 - f))            # f = kappa^2 / (1 + kappa^2)
  
  if (kappa == 0) {                     # f = 0 -> reduces to pure linear
    return(lp + eff_beta * lin)
  }
  
  if (complexity == 2) {
    Nraw <- rowSums(Xs^2)
  } else if (complexity == 3) {
    if (S < 2) {
      warning("Complexity 3 needs >= 2 signal predictors. Falling back to C2.")
      return(generate_linear_predictor(X, n_signal_parameters, noise_parameters,
                                       intercept, beta_signal, 2,
                                       nonlinear_strength))
    }
    inter <- rowSums(vapply(
      seq_len(S - 1),
      function(j) Xs[, j] * rowSums(Xs[, (j + 1):S, drop = FALSE]),
      numeric(n)
    ))
    Nraw <- rowSums(Xs^2) + inter
  } else {
    stop("complexity must be 1, 2, 3 or 4.")
  }
  
  # Residualise the nonlinear aggregate against the FULL design [1, x1..xS] so
  # that no linear-in-X learner can access it, then standardise to unit SD.
  N_ortho <- residuals(stats::lm(Nraw ~ Xs))
  sd_N    <- stats::sd(N_ortho)
  if (!is.finite(sd_N) || sd_N <= 0)
    stop("Nonlinear component is degenerate -- e.g. binary predictors under ",
         "complexity 2/3, where x^2 = x collapses the quadratic term. Use ",
         "continuous predictors for C2/C3 (or complexity 1 for binary ",
         "predictors).")
  N_std <- N_ortho / sd_N
  
  # lp = intercept + eff_beta * ( L + kappa * SD(L) * N_std )
  # Linear part identical to C1; nonlinear variance fraction equals f exactly.
  # Linear in beta_signal, so the oracle-metric tuning scales it unchanged.
  lp + eff_beta * (lin + kappa * stats::sd(lin) * N_std)
}