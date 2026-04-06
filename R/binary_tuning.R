# =============================================================================
# Tuning for a binary outcome model
#
# Goal
# ----
# Find (beta_signal, mu_lp) such that a large-sample call to
# generate_binary_data() achieves target_performance (AUC) and
# target_prevalence simultaneously.
#
# History of previous approaches and their failures
# -------------------------------------------------
# Original implementation (pre-revision)
#   Used the closed-form identity
#       AUC = Phi(sigma_lp / sqrt(2))
#       beta = beta_init * sqrt(sigma^2 / beta_init)    where sigma^2 = S * beta^2
#   Only valid under: complexity = 1, correlation = 0, strength = "strong",
#   standard-normal predictors. Broke under all other settings.
#
# First revision
#   Replaced S * beta^2 with simulation-based var_lp_unit, then applied the
#   same Gaussian AUC formula: beta = sqrt(2)*qnorm(AUC) / sqrt(var_lp_unit).
#   Correctly recovered var_lp_unit for each complexity, but the Gaussian AUC
#   formula itself requires LP ~ Normal. Under complexity 2-3 the LP is
#   right-skewed; the true AUC at a given sigma_lp depends on the LP shape,
#   so the formula still produced the wrong beta (AUC off by ~2-5 pp).
#   Additionally, the mu_lp bracket (fixed ±30 around mu_approx) failed when
#   beta was large (sd(lp_scaled) >> 30), causing uniroot() to error.
#
# Current approach: simulation-based bisection on beta
# ----------------------------------------------------
# 1. Simulate lp_unit (LP at beta = 1) once from the actual generator. All
#    bisection iterations simply rescale lp_unit, which is exact because
#    generate_linear_predictor() is linear in beta_signal.
#
# 2. For each candidate beta:
#      lp_scaled <- beta * lp_unit
#      mu_lp     <- find_mu(lp_scaled)     # E[plogis(mu + lp)] == target_prev
#      AUC       <- quickcstat(y, mu_lp + lp_scaled)
#    Note: mu_lp is an additive intercept on the log-odds scale and cannot
#    affect concordance (rank-invariant to monotone transformations), so AUC
#    depends on beta only.
#
# 3. AUC is strictly monotone increasing in beta for any LP distribution,
#    guaranteeing bisection convergence to a unique root.
#
# 4. find_mu() uses a robust adaptive bracket that scales with sd(lp_scaled),
#    ensuring uniroot() succeeds for any beta (including large values where
#    the fixed ±30 bracket failed in the first revision).
# =============================================================================

#' Tuning function for a binary outcome model
#'
#' Finds the \code{beta_signal} and log-odds intercept (\code{mu_lp}) that
#' jointly produce the target AUC and prevalence under the exact
#' data-generating settings (complexity, predictor_strength, correlation,
#' distribution, predictor_type).
#'
#' @param target_prevalence Target outcome prevalence. Must be in (0, 1).
#' @param target_performance Target AUC (c-statistic). Must be in (0.5, 1).
#' @param candidate_features Total number of predictors (signal + noise).
#' @param proportion_noise_features Proportion of \code{candidate_features}
#'   that are noise predictors. Must be in [0, 1).
#' @param complexity Integer 1-4. Default = 1.
#' @param predictor_strength One of \code{"strong"}, \code{"moderate"}, or
#'   \code{"weak"}. When \code{NULL} the complexity-level default is used.
#' @param correlation Common pairwise predictor correlation. Default = 0.
#' @param distribution Global continuous predictor distribution family.
#'   Default = \code{"normal"}.
#' @param predictor_type \code{"continuous"} (default) or \code{"binary"}.
#' @param binary_prevalence Bernoulli probability for binary predictors.
#' @param n_sim Sample size for the internal LP simulation. Larger values give
#'   more stable AUC estimates within the bisection. Default = 300 000.
#' @param beta_interval Search interval \code{c(lo, hi)} for beta_signal.
#'   Default = \code{c(1e-4, 20)}. The upper bound is doubled automatically
#'   if the AUC at \code{hi} is still below \code{target_performance}.
#' @param beta_tol Absolute convergence tolerance for the beta bisection.
#'   Default = 1e-4.
#' @param mu_tol Absolute convergence tolerance for the mu_lp root-finding.
#'   Default = 1e-6.
#'
#' @return A named numeric vector:
#'   \describe{
#'     \item{\code{mu_lp}}{Tuned log-odds intercept.}
#'     \item{\code{beta_signal}}{Tuned effect size.}
#'     \item{\code{prevalence_achieved}}{Empirical prevalence in validation.}
#'     \item{\code{auc_achieved}}{Empirical AUC in validation.}
#'     \item{\code{var_lp_unit}}{Estimated Var(LP) at beta_signal = 1.}
#'   }
#' @keywords internal
binary_tuning <- function(
    target_prevalence,
    target_performance,
    candidate_features,
    proportion_noise_features,
    complexity          = 1,
    predictor_strength  = NULL,
    correlation         = 0,
    distribution        = "normal",
    predictor_type      = "continuous",
    binary_prevalence   = 0,
    n_sim               = 300000,
    beta_interval       = c(1e-4, 20),
    beta_tol            = 1e-4,
    mu_tol              = 1e-6
) {
  
  # ---- input validation ------------------------------------------------------
  if (!is.numeric(target_prevalence) || target_prevalence <= 0 ||
      target_prevalence >= 1)
    stop("target_prevalence must be in (0, 1).")
  if (!is.numeric(target_performance) || target_performance <= 0.5 ||
      target_performance >= 1)
    stop("target_performance (AUC) must be in (0.5, 1).")
  if (!is.numeric(proportion_noise_features) ||
      proportion_noise_features < 0 || proportion_noise_features >= 1)
    stop("proportion_noise_features must be in [0, 1).")
  
  # ---- derive signal / noise counts ------------------------------------------
  n_signal <- candidate_features -
    round(candidate_features * proportion_noise_features)
  noise    <- candidate_features - n_signal
  
  if (n_signal < 1)
    stop("proportion_noise_features leaves no signal predictors.")
  
  # ---- resolve predictor strength --------------------------------------------
  predictor_strength <- resolve_strength(predictor_strength, complexity)
  
  # ---- Step 1: simulate lp_unit once -----------------------------------------
  # LP = beta * lp_unit (exact scaling: generate_linear_predictor is linear
  # in beta_signal). Fixing the predictor matrix across all bisection
  # iterations eliminates Monte Carlo variation between steps.
  
  X_unit <- generate_predictors(
    n                   = n_sim,
    n_signal_parameters = n_signal,
    noise_parameters    = noise,
    complexity          = complexity,
    predictor_type      = predictor_type,
    binary_prevalence   = binary_prevalence,
    correlation         = correlation,
    distribution        = distribution
  )
  
  lp_unit <- generate_linear_predictor(
    X                   = X_unit,
    n_signal_parameters = n_signal,
    noise_parameters    = noise,
    intercept           = 0,
    beta_signal         = 1,
    complexity          = complexity,
    predictor_strength  = predictor_strength
  )
  
  var_lp_unit <- stats::var(lp_unit)
  
  if (var_lp_unit <= 0)
    stop("Estimated Var(LP) at beta_signal = 1 is zero or negative.")
  
  # Pre-draw Uniform(0,1) variates for Bernoulli outcomes. Fixed across
  # bisection iterations to reduce Monte Carlo noise in AUC estimates.
  U_binom <- stats::runif(n_sim)
  
  # ---- Step 2: find_mu -- solve E[plogis(mu + lp_scaled)] = target_prev -----
  # Robust bracketing: start at the approximate solution and expand outward
  # by multiples of sd(lp_scaled) until the function changes sign. This
  # handles any beta, including large values where sd(lp_scaled) >> 30 and a
  # fixed ±30 bracket fails to contain the root.
  
  find_mu <- function(lp_scaled) {
    mu0 <- stats::qlogis(target_prevalence) - mean(lp_scaled)
    s   <- max(stats::sd(lp_scaled), 1)   # guard against near-zero sd
    lo  <- mu0 - 5 * s
    hi  <- mu0 + 5 * s
    for (k in seq_len(20)) {
      if (mean(stats::plogis(lo + lp_scaled)) < target_prevalence) break
      lo <- lo - 2^k * s
    }
    for (k in seq_len(20)) {
      if (mean(stats::plogis(hi + lp_scaled)) > target_prevalence) break
      hi <- hi + 2^k * s
    }
    stats::uniroot(
      f        = function(mu) mean(stats::plogis(mu + lp_scaled)) - target_prevalence,
      interval = c(lo, hi),
      tol      = mu_tol
    )$root
  }
  
  # ---- Step 3: auc_at_beta ---------------------------------------------------
  # mu_lp is rank-invariant (AUC unchanged by additive shift of lp), so AUC
  # depends on beta only. We still apply mu_lp when drawing y so the
  # Bernoulli labels have the correct prevalence, which matters for a faithful
  # case-control split in quickcstat().
  
  auc_at_beta <- function(beta) {
    lp_scaled <- beta * lp_unit
    mu        <- find_mu(lp_scaled)
    lp_final  <- mu + lp_scaled
    y         <- as.integer(U_binom < stats::plogis(lp_final))
    quickcstat(y, lp_final)
  }
  
  # ---- Step 4: bisect on beta to hit target AUC ------------------------------
  lo <- beta_interval[1]
  hi <- beta_interval[2]
  
  # Widen upper bound until AUC at hi brackets the target.
  for (expand in seq_len(8)) {
    if (auc_at_beta(hi) >= target_performance) break
    hi <- hi * 2
  }
  if (auc_at_beta(hi) < target_performance)
    stop("Cannot reach target_performance within the beta search range. ",
         "Consider increasing beta_interval[2] or reducing target_performance.")
  
  for (i in seq_len(60)) {
    mid     <- (lo + hi) / 2
    auc_mid <- auc_at_beta(mid)
    if (auc_mid < target_performance) lo <- mid else hi <- mid
    if ((hi - lo) < beta_tol) break
  }
  
  beta_signal <- (lo + hi) / 2
  
  # ---- Step 5: final mu_lp and validation ------------------------------------
  lp_final_scaled     <- beta_signal * lp_unit
  mu_lp               <- find_mu(lp_final_scaled)
  lp_final            <- mu_lp + lp_final_scaled
  y_final             <- as.integer(U_binom < stats::plogis(lp_final))
  prevalence_achieved <- mean(y_final)
  auc_achieved        <- quickcstat(y_final, lp_final)
  
  return(c(
    mu_lp               = mu_lp,
    beta_signal         = beta_signal,
    prevalence_achieved = prevalence_achieved,
    auc_achieved        = auc_achieved,
    var_lp_unit         = var_lp_unit
  ))
}

# -----------------------------------------------------------------------------
# quickcstat
# Fast AUC estimate via random subsampling. The smaller group is sampled
# from the larger group to equalise group sizes, then concordance is computed
# as the proportion of (case, control) pairs where the case score exceeds the
# control score.
# -----------------------------------------------------------------------------
quickcstat <- function(y, pred) {
  casepred <- pred[y == 1]
  conpred  <- pred[y == 0]
  
  if (length(conpred) > length(casepred)) {
    conpred <- conpred[sample(length(conpred), length(casepred),
                              replace = FALSE)]
    auc <- sum(casepred > conpred) / length(casepred)
  } else {
    casepred <- casepred[sample(length(casepred), length(conpred),
                                replace = FALSE)]
    auc <- sum(casepred > conpred) / length(conpred)
  }
  return(auc)
}
