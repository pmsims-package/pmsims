# =============================================================================
# Data-generating functions for simulation studies
#
# Supports continuous, binary, and survival outcomes across four complexity
# levels.  All generators share two internal workhorses:
#   - generate_predictors()        : builds the (possibly correlated) X matrix
#   - generate_linear_predictor()  : constructs lp from X
#
# Arguments
# ---------
#  complexity         (integer 1-4)   - structural complexity level
#  predictor_dist     (named list)    - per-predictor distribution spec;
#                                       supported at ALL complexity levels.
#                                       Each element is a list with:
#                                         dist  : distribution family (string)
#                                         ...   : family-specific parameters
#  cor_matrix         (matrix | NULL) - inter-predictor correlation (Gaussian
#                                       copula via Cholesky)
#  predictor_roles    (named char)    - "noise", "linear", or "nonlinear"
#                                       (complexities 2-4)
#  predictor_strength (named char)    - "strong" / "moderate" / "weak"
#                                       (complexities 2-4)
#
# Default distributions by complexity
# ------------------------------------
#  C1 continuous : normal(mean=0, sd=1)   | overridable via predictor_dist
#  C1 binary     : binary(prop=predictor_prop) per column;
#                  individual columns may be overridden via predictor_dist
#                  using dist="binary" with a 'prop' parameter
#  C2, C3        : normal(mean=0, sd=1)   | overridable via predictor_dist
#  C4 (Friedman) : uniform(min=0, max=1)  | overridable via predictor_dist
#
# Supported distribution families (all complexities)
# ---------------------------------------------------
#  "normal"      : mean, sd          (defaults: 0, 1)
#  "uniform"     : min, max          (defaults: 0, 1)
#  "binary"      : prop              (default: 0.5)
#  "exponential" : rate              (default: 1)
#  "lognormal"   : meanlog, sdlog    (defaults: 0, 1)
#  "t"           : df                (default: 5)
#  "laplace"     : location, scale   (defaults: 0, 1)
# =============================================================================

#' Create default data generating functions
#'
#' @param opts A list of options. Must include \code{type} ("binary",
#'   "continuous", or "survival") and a sub-list \code{args} whose names
#'   match the formals of the corresponding generator function.
#' @return A function with default arguments pre-set.
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

# =============================================================================
# Outcome-level generators
# =============================================================================

#' Simulate continuous outcome data
#'
#' @param n                  Sample size.
#' @param beta_signal        Effect size for strong-tier signal predictors.
#' @param n_signal_parameters Number of signal predictors (columns x1 to x_S).
#' @param noise_parameters   Number of pure-noise predictors.
#' @param predictor_type     Default predictor type when \code{predictor_dist}
#'   is not specified: \code{"continuous"} (normal) or \code{"binary"}
#'   (Bernoulli). Applies to all complexities as the fallback.
#' @param predictor_prop     Bernoulli probability used when
#'   \code{predictor_type = "binary"} and no per-column override is given.
#' @param complexity         Integer 1-4 (see Details).
#' @param predictor_dist     Named list of per-predictor distribution
#'   specifications. Supported at \strong{all complexity levels}.
#'   Each element is a list with \code{dist} (distribution family) and
#'   optional family-specific parameters. Predictors not listed fall back to
#'   the complexity-level default (see Details). Example:
#'   \preformatted{
#'   list(
#'     x1 = list(dist = "normal",  mean = 2, sd = 0.5),
#'     x2 = list(dist = "binary",  prop = 0.3),
#'     x3 = list(dist = "uniform", min = 0, max = 1)
#'   )}
#' @param cor_matrix         Optional p x p positive semi-definite correlation
#'   matrix (p = n_signal_parameters + noise_parameters). Correlations are
#'   induced via a Gaussian copula (rank-based Cholesky), which preserves
#'   every predictor's marginal distribution exactly. \code{NULL} = independent.
#' @param predictor_roles    Named character vector mapping predictor names to
#'   \code{"noise"}, \code{"linear"}, or \code{"nonlinear"}
#'   (complexities 2-4 only). Defaults: signal columns -> \code{"linear"};
#'   noise columns -> \code{"noise"}.
#' @param predictor_strength Named character vector mapping predictor names to
#'   \code{"strong"} (1.0 x beta_signal), \code{"moderate"}
#'   (0.5 x beta_signal), or \code{"weak"} (0.4 x beta_signal).
#'   Unlisted predictors default to \code{"strong"}.
#'
#' @details
#' \strong{Complexity levels and linear predictor structure:}
#' \itemize{
#'   \item \strong{1 - Linear}: lp = alpha + sum beta_j * x_j.
#'     Roles and strength tiers not applied.
#'   \item \strong{2 - Quadratic}: linear + quadratic terms for nonlinear-role
#'     predictors.
#'   \item \strong{3 - Quadratic + Interactions}: C2 terms + pairwise products
#'     across all active predictors.
#'   \item \strong{4 - Friedman (1991) Nonlinear}: canonical Friedman #1
#'     benchmark, scaled by per-predictor betas, extended for >5 active
#'     predictors.
#' }
#' \strong{Default predictor distributions by complexity:}
#' \itemize{
#'   \item C1 continuous: Normal(0, 1)
#'   \item C1 binary    : Bernoulli(predictor_prop)
#'   \item C2, C3       : Normal(0, 1)
#'   \item C4           : Uniform(0, 1)  [Friedman canonical]
#' }
#' Per-column overrides via \code{predictor_dist} apply at \strong{all} levels.
#'
#' @references
#' Friedman, J. H. (1991). Multivariate adaptive regression splines.
#'   \emph{The Annals of Statistics}, 19(1), 1-67.
#'   \doi{10.1214/aos/1176347963}
#'
#' Breiman, L. (1996). Bagging predictors.
#'   \emph{Machine Learning}, 24(2), 123-140.
#'
#' @return A data frame with columns \code{y}, \code{x1}, \code{x2}, ...
#' @keywords internal
generate_continuous_data <- function(
    n,
    beta_signal,
    n_signal_parameters,
    noise_parameters,
    predictor_type     = "continuous",
    predictor_prop     = NULL,
    complexity         = 1,
    predictor_dist     = NULL,
    cor_matrix         = NULL,
    predictor_roles    = NULL,
    predictor_strength = NULL
) {
  X  <- generate_predictors(n, n_signal_parameters, noise_parameters,
                            predictor_type, predictor_prop,
                            complexity, predictor_dist, cor_matrix)
  lp <- generate_linear_predictor(X, n_signal_parameters, noise_parameters,
                                  intercept = 0, beta_signal,
                                  complexity, predictor_roles,
                                  predictor_strength)
  y  <- stats::rnorm(n, lp, 1)
  return(as.data.frame(cbind(y, X)))
}

#' Simulate binary outcome data
#'
#' @inheritParams generate_continuous_data
#' @param mu_lp         Intercept on the log-odds scale.
#' @param baseline_prob Nominal baseline probability (for documentation;
#'   the realised probability is determined by \code{mu_lp}).
#'
#' @return A data frame with columns \code{y}, \code{x1}, \code{x2}, ...
#' @keywords internal
generate_binary_data <- function(
    n,
    mu_lp,
    beta_signal,
    n_signal_parameters,
    noise_parameters,
    predictor_type     = "continuous",
    predictor_prop     = NULL,
    baseline_prob,
    complexity         = 1,
    predictor_dist     = NULL,
    cor_matrix         = NULL,
    predictor_roles    = NULL,
    predictor_strength = NULL
) {
  X  <- generate_predictors(n, n_signal_parameters, noise_parameters,
                            predictor_type, predictor_prop,
                            complexity, predictor_dist, cor_matrix)
  lp <- generate_linear_predictor(X, n_signal_parameters, noise_parameters,
                                  intercept = mu_lp, beta_signal,
                                  complexity, predictor_roles,
                                  predictor_strength)
  y  <- stats::rbinom(n, 1, stats::plogis(lp))
  return(as.data.frame(cbind(y, X)))
}

#' Simulate survival outcome data
#'
#' @inheritParams generate_continuous_data
#' @param baseline_hazard Baseline hazard rate (exponential model).
#' @param censoring_rate  Administrative censoring proportion (0-1).
#'
#' @return A data frame with columns \code{time}, \code{event},
#'   \code{x1}, \code{x2}, ...
#' @keywords internal
generate_survival_data <- function(
    n,
    beta_signal,
    n_signal_parameters,
    noise_parameters,
    predictor_type     = "continuous",
    predictor_prop     = NULL,
    baseline_hazard,
    censoring_rate,
    complexity         = 1,
    predictor_dist     = NULL,
    cor_matrix         = NULL,
    predictor_roles    = NULL,
    predictor_strength = NULL
) {
  X  <- generate_predictors(n, n_signal_parameters, noise_parameters,
                            predictor_type, predictor_prop,
                            complexity, predictor_dist, cor_matrix)
  lp <- generate_linear_predictor(X, n_signal_parameters, noise_parameters,
                                  intercept = 0, beta_signal,
                                  complexity, predictor_roles,
                                  predictor_strength)
  
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
# draw_one_predictor
#
# Draws n observations from the distribution specified in `spec`.
# `spec` is a list with at minimum a `dist` element (character string).
# `fallback_dist` is used when spec is NULL (i.e., no user override).
#
# Supported families:
#   "normal"      : mean (0), sd (1)
#   "uniform"     : min (0), max (1)
#   "binary"      : prop (0.5)
#   "exponential" : rate (1)
#   "lognormal"   : meanlog (0), sdlog (1)
#   "t"           : df (5)
#   "laplace"     : location (0), scale (1)
# -----------------------------------------------------------------------------
draw_one_predictor <- function(n, spec, fallback_dist, cname) {
  
  # Resolve which distribution to use
  if (is.null(spec)) {
    dist <- fallback_dist
    spec <- list()          # empty: all parameters will use defaults
  } else {
    if (is.null(spec$dist))
      stop(sprintf("predictor_dist[['%s']] must contain a 'dist' element.", cname))
    dist <- spec$dist
  }
  
  switch(
    dist,
    
    normal = {
      mu  <- if (!is.null(spec$mean)) spec$mean else 0
      sig <- if (!is.null(spec$sd))   spec$sd   else 1
      if (sig <= 0)
        stop(sprintf("sd must be positive for predictor '%s'.", cname))
      stats::rnorm(n, mean = mu, sd = sig)
    },
    
    uniform = {
      lo <- if (!is.null(spec$min)) spec$min else 0
      hi <- if (!is.null(spec$max)) spec$max else 1
      if (lo >= hi)
        stop(sprintf("min must be < max for predictor '%s'.", cname))
      stats::runif(n, min = lo, max = hi)
    },
    
    binary = {
      pr <- if (!is.null(spec$prop)) spec$prop else 0.5
      if (pr < 0 || pr > 1)
        stop(sprintf("prop must be in [0, 1] for predictor '%s'.", cname))
      stats::rbinom(n, 1, pr)
    },
    
    exponential = {
      rt <- if (!is.null(spec$rate)) spec$rate else 1
      if (rt <= 0)
        stop(sprintf("rate must be positive for predictor '%s'.", cname))
      stats::rexp(n, rate = rt)
    },
    
    lognormal = {
      ml  <- if (!is.null(spec$meanlog)) spec$meanlog else 0
      sdl <- if (!is.null(spec$sdlog))   spec$sdlog   else 1
      if (sdl <= 0)
        stop(sprintf("sdlog must be positive for predictor '%s'.", cname))
      stats::rlnorm(n, meanlog = ml, sdlog = sdl)
    },
    
    t = {
      df <- if (!is.null(spec$df)) spec$df else 5
      if (df <= 0)
        stop(sprintf("df must be positive for predictor '%s'.", cname))
      stats::rt(n, df = df)
    },
    
    laplace = {
      loc <- if (!is.null(spec$location)) spec$location else 0
      scl <- if (!is.null(spec$scale))    spec$scale    else 1
      if (scl <= 0)
        stop(sprintf("scale must be positive for predictor '%s'.", cname))
      # Quantile-transform method for Laplace distribution
      u <- stats::runif(n, -0.5, 0.5)
      loc - scl * sign(u) * log(1 - 2 * abs(u))
    },
    
    stop(sprintf(
      paste0("Unknown distribution '%s' for predictor '%s'. ",
             "Supported: normal, uniform, binary, exponential, ",
             "lognormal, t, laplace."),
      dist, cname
    ))
  )
}

# -----------------------------------------------------------------------------
# resolve_betas
# Returns numeric vector of per-predictor betas (length = n_signal_parameters)
# -----------------------------------------------------------------------------
resolve_betas <- function(predictor_names, n_signal, predictor_strength,
                          beta_signal) {
  signal_names <- predictor_names[seq_len(n_signal)]
  multipliers  <- rep(1, n_signal)
  names(multipliers) <- signal_names
  
  if (!is.null(predictor_strength)) {
    for (nm in names(predictor_strength)) {
      if (nm %in% signal_names) {
        tier <- predictor_strength[[nm]]
        multipliers[nm] <- switch(
          tier,
          strong   = 1.0,
          moderate = 0.5,
          weak     = 0.4,
          stop(sprintf(
            "Unknown strength tier '%s' for predictor '%s'. Must be: strong, moderate, weak.",
            tier, nm
          ))
        )
      }
    }
  }
  return(beta_signal * multipliers)
}

# -----------------------------------------------------------------------------
# resolve_roles
# Returns named character vector of roles for ALL predictors
# -----------------------------------------------------------------------------
resolve_roles <- function(predictor_names, n_signal, noise_parameters,
                          predictor_roles) {
  p     <- length(predictor_names)
  roles <- character(p)
  names(roles) <- predictor_names
  
  roles[seq_len(n_signal)]     <- "linear"
  roles[seq(n_signal + 1, p)] <- "noise"
  
  if (!is.null(predictor_roles)) {
    for (nm in names(predictor_roles)) {
      if (nm %in% predictor_names) {
        role <- predictor_roles[[nm]]
        if (!role %in% c("noise", "linear", "nonlinear"))
          stop(sprintf(
            "Invalid role '%s' for predictor '%s'. Must be: noise, linear, nonlinear.",
            role, nm
          ))
        roles[nm] <- role
      }
    }
  }
  return(roles)
}

# -----------------------------------------------------------------------------
# apply_correlation
#
# Induces a target correlation structure via a Gaussian copula:
#   1. Map each column to U(0,1) via its empirical CDF.
#   2. Transform to standard normals.
#   3. Apply the Cholesky factor of cor_matrix.
#   4. Rank-assign original marginal values according to the new order.
#
# This approach preserves every predictor's marginal distribution (including
# binary) while achieving the desired pairwise correlations.
# -----------------------------------------------------------------------------
apply_correlation <- function(X, cor_matrix) {
  n <- nrow(X)
  p <- ncol(X)
  
  if (!isTRUE(all.equal(dim(cor_matrix), c(p, p))))
    stop("cor_matrix must be a ", p, " x ", p, " matrix ",
         "(one row/column per predictor).")
  
  diag_vals <- diag(cor_matrix)
  if (any(abs(diag_vals - 1) > 1e-8))
    stop("All diagonal elements of cor_matrix must equal 1.")
  
  if (any(abs(cor_matrix) > 1 + 1e-8))
    stop("All elements of cor_matrix must be in [-1, 1].")
  
  eigs <- eigen(cor_matrix, symmetric = TRUE, only.values = TRUE)$values
  if (any(eigs < -1e-8))
    stop("cor_matrix is not positive semi-definite.")
  
  L <- tryCatch(
    chol(cor_matrix),
    error = function(e)
      stop("Cholesky decomposition of cor_matrix failed: ", conditionMessage(e))
  )
  
  # Map columns to U(0,1) via empirical CDF, then to standard normals
  U      <- apply(X, 2, function(col) rank(col, ties.method = "average") / (n + 1))
  Z_ind  <- qnorm(U)
  Z_corr <- Z_ind %*% t(L)
  
  # Re-assign marginal values according to the new rank order
  X_corr <- X
  for (j in seq_len(p)) {
    orig_sorted   <- sort(X[, j])
    new_ranks_int <- pmax(1L, pmin(n, round(rank(Z_corr[, j], ties.method = "average"))))
    X_corr[, j]  <- orig_sorted[new_ranks_int]
  }
  return(X_corr)
}

# =============================================================================
# generate_predictors
# =============================================================================

#' Generate predictor matrix (with optional per-predictor distributions and
#' optional correlation structure)
#'
#' @param n                   Sample size.
#' @param n_signal_parameters Signal predictor count.
#' @param noise_parameters    Noise predictor count.
#' @param type                Fallback type when no per-column spec is given:
#'   \code{"continuous"} or \code{"binary"}.
#' @param predictor_prop      Bernoulli probability for the binary fallback.
#' @param complexity          Integer 1-4. Determines the \emph{default}
#'   distribution when a column has no entry in \code{predictor_dist}:
#'   \itemize{
#'     \item C1 continuous: Normal(0,1)
#'     \item C1 binary    : Bernoulli(predictor_prop)
#'     \item C2, C3       : Normal(0,1)
#'     \item C4           : Uniform(0,1)
#'   }
#' @param predictor_dist      Named list; each element is a list specifying
#'   the distribution for one predictor column. Applies at \strong{all}
#'   complexity levels. Missing columns use the complexity default.
#' @param cor_matrix          p x p correlation matrix or \code{NULL}.
#'
#' @return Named n x p numeric matrix (column names: x1, x2, ...).
#' @keywords internal
generate_predictors <- function(n,
                                n_signal_parameters,
                                noise_parameters,
                                type           = "continuous",
                                predictor_prop = NULL,
                                complexity     = 1,
                                predictor_dist = NULL,
                                cor_matrix     = NULL) {
  
  parameters <- n_signal_parameters + noise_parameters
  col_names  <- paste0("x", seq_len(parameters))
  
  # ---------------------------------------------------------------------------
  # Determine the complexity-level fallback distribution
  # ---------------------------------------------------------------------------
  # For C1 binary the fallback is handled specially inside the loop.
  fallback <- switch(
    as.character(complexity),
    "1" = if (type == "binary") "binary" else "normal",
    "2" = "normal",
    "3" = "normal",
    "4" = "uniform",
    stop("complexity must be 1, 2, 3, or 4")
  )
  
  # Validate binary fallback parameters upfront
  if (fallback == "binary") {
    if (is.null(predictor_prop))
      stop("predictor_prop must be provided when predictor_type is 'binary'.")
    if (predictor_prop < 0 || predictor_prop > 1)
      stop("predictor_prop must be in [0, 1].")
  }
  
  # ---------------------------------------------------------------------------
  # Draw each column independently using draw_one_predictor()
  # ---------------------------------------------------------------------------
  X <- matrix(NA_real_, nrow = n, ncol = parameters)
  
  for (j in seq_len(parameters)) {
    cname     <- col_names[j]
    user_spec <- predictor_dist[[cname]]   # NULL if user did not specify
    
    # If no user spec AND binary fallback, inject the prop into a synthetic spec
    if (is.null(user_spec) && fallback == "binary") {
      user_spec <- list(dist = "binary", prop = predictor_prop)
    }
    
    X[, j] <- draw_one_predictor(n, user_spec, fallback, cname)
  }
  
  colnames(X) <- col_names
  
  # ---------------------------------------------------------------------------
  # Apply correlation structure if requested (all complexities)
  # ---------------------------------------------------------------------------
  if (!is.null(cor_matrix)) {
    X <- apply_correlation(X, cor_matrix)
    colnames(X) <- col_names
  }
  
  return(X)
}

# =============================================================================
# generate_linear_predictor
# =============================================================================

#' Construct the linear predictor from the predictor matrix
#'
#' @param X                   n x p predictor matrix (colnames: x1, x2, ...).
#' @param n_signal_parameters Signal predictor count.
#' @param noise_parameters    Noise predictor count.
#' @param intercept           Scalar added to every observation's lp.
#' @param beta_signal         Base effect size (strong tier = 1.0 x beta_signal).
#' @param complexity          Integer 1-4.
#' @param predictor_roles     Named character vector; see \code{generate_continuous_data}.
#' @param predictor_strength  Named character vector; see \code{generate_continuous_data}.
#'
#' @details
#' \strong{Role definitions (complexities 2-4):}
#' \itemize{
#'   \item \code{"noise"}     — zero contribution at all complexity levels.
#'   \item \code{"linear"}    — linear term only (plus interactions in C3/C4).
#'   \item \code{"nonlinear"} — linear term AND complexity-specific nonlinear
#'     terms (quadratic in C2/C3; Friedman form in C4); also participates in
#'     interactions.
#' }
#'
#' \strong{Complexity 1 — Linear:}
#' \deqn{lp = \alpha + \sum_{j=1}^{S} \beta \, x_j}
#' (roles and strength tiers not used)
#'
#' \strong{Complexity 2 — Quadratic:}
#' \deqn{lp = \alpha
#'   + \sum_{j:\,\text{linear}} \beta_j x_j
#'   + \sum_{j:\,\text{nonlinear}} \beta_j (x_j + x_j^2)}
#'
#' \strong{Complexity 3 — Quadratic + Interactions:}
#' \deqn{lp = \alpha
#'   + \sum_{j:\,\text{linear}} \beta_j x_j
#'   + \sum_{j:\,\text{nonlinear}} \beta_j (x_j + x_j^2)
#'   + \sum_{\substack{j < k \\ \text{both active}}}
#'       \sqrt{\beta_j \beta_k} \, x_j x_k}
#'
#' \strong{Complexity 4 — Friedman (1991) Nonlinear:}
#' The first five active predictors follow the canonical Friedman #1 function,
#' scaled by per-predictor betas:
#' \deqn{lp = \alpha
#'   + \beta_1 \cdot 10\sin(\pi x_1 x_2)
#'   + \beta_3 \cdot 20(x_3 - 0.5)^2
#'   + \beta_4 \cdot 10 x_4
#'   + \beta_5 \cdot  5 x_5}
#' Each additional active predictor k (k >= 6) contributes:
#' \deqn{\beta_k \bigl[\sin(\pi x_k x_{k-1}) + (x_k - 0.5)^2\bigr]}
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
                                      n_signal_parameters,
                                      noise_parameters,
                                      intercept,
                                      beta_signal,
                                      complexity         = 1,
                                      predictor_roles    = NULL,
                                      predictor_strength = NULL) {
  
  n      <- nrow(X)
  p      <- ncol(X)
  pnames <- colnames(X)
  lp     <- rep(intercept, n)
  
  if (n_signal_parameters == 0) return(lp)
  
  # ---------------------------------------------------------------------------
  # Complexity 1 — purely linear, no roles or strength
  # ---------------------------------------------------------------------------
  if (complexity == 1) {
    Xs <- X[, seq_len(n_signal_parameters), drop = FALSE]
    lp <- lp + as.vector(Xs %*% rep(beta_signal, n_signal_parameters))
    return(lp)
  }
  
  # ---------------------------------------------------------------------------
  # Complexities 2-4 — resolve roles and per-predictor betas
  # ---------------------------------------------------------------------------
  roles <- resolve_roles(pnames, n_signal_parameters, noise_parameters,
                         predictor_roles)
  betas <- resolve_betas(pnames, n_signal_parameters, predictor_strength,
                         beta_signal)
  
  full_betas <- rep(0, p)
  full_betas[seq_len(n_signal_parameters)] <- betas
  
  linear_idx    <- which(roles == "linear")
  nonlinear_idx <- which(roles == "nonlinear")
  active_idx    <- sort(c(linear_idx, nonlinear_idx))
  
  # ---------------------------------------------------------------------------
  # Complexity 2 — linear + quadratic (nonlinear role only)
  # ---------------------------------------------------------------------------
  if (complexity == 2) {
    for (j in active_idx)
      lp <- lp + full_betas[j] * X[, j]
    for (j in nonlinear_idx)
      lp <- lp + full_betas[j] * X[, j]^2
    
    # ---------------------------------------------------------------------------
    # Complexity 3 — linear + quadratic + pairwise interactions
    # ---------------------------------------------------------------------------
  } else if (complexity == 3) {
    for (j in active_idx)
      lp <- lp + full_betas[j] * X[, j]
    for (j in nonlinear_idx)
      lp <- lp + full_betas[j] * X[, j]^2
    
    if (length(active_idx) >= 2) {
      pairs <- utils::combn(active_idx, 2)
      for (k in seq_len(ncol(pairs))) {
        j1    <- pairs[1, k]
        j2    <- pairs[2, k]
        w_int <- sqrt(full_betas[j1] * full_betas[j2])
        lp    <- lp + w_int * X[, j1] * X[, j2]
      }
    }
    
    # ---------------------------------------------------------------------------
    # Complexity 4 — Friedman (1991) benchmark, extended for > 5 active predictors
    # ---------------------------------------------------------------------------
  } else if (complexity == 4) {
    n_active <- length(active_idx)
    
    if (n_active < 5)
      warning("Complexity 4 (Friedman) uses the first 5 active predictors. ",
              "Only ", n_active, " active predictor(s) found; some Friedman ",
              "terms will be omitted.")
    
    xcol <- function(k) if (k <= n_active) X[, active_idx[k]] else rep(0, n)
    bk   <- function(k) if (k <= n_active) full_betas[active_idx[k]] else 0
    
    x1 <- xcol(1); x2 <- xcol(2); x3 <- xcol(3)
    x4 <- xcol(4); x5 <- xcol(5)
    
    # Canonical Friedman #1 terms
    # Friedman (1991, Eq. 4.3); also Breiman (1996, p. 126):
    #   y = 10 sin(pi x1 x2) + 20(x3 - 0.5)^2 + 10 x4 + 5 x5 + eps
    if (n_active >= 2) lp <- lp + bk(1) * 10 * sin(pi * x1 * x2)
    if (n_active >= 3) lp <- lp + bk(3) * 20 * (x3 - 0.5)^2
    if (n_active >= 4) lp <- lp + bk(4) * 10 * x4
    if (n_active >= 5) lp <- lp + bk(5) *  5 * x5
    
    # Extended terms for active predictors 6, 7, ...
    if (n_active >= 6) {
      for (idx in 6:n_active) {
        xj   <- X[, active_idx[idx]]
        xj_1 <- X[, active_idx[idx - 1]]
        lp   <- lp + bk(idx) * (sin(pi * xj * xj_1) + (xj - 0.5)^2)
      }
    }
    
  } else {
    stop("complexity must be 1, 2, 3, or 4")
  }
  
  return(lp)
}
