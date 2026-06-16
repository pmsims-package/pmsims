# =============================================================================
# Tuning for a binary outcome model
#
# Goal
# ----
# Find (beta_signal, mu_lp) such that a large-sample call to
# generate_binary_data() achieves target_performance (AUC) and
# target_prevalence simultaneously.
#
#' Tuning function for a binary outcome model
#'
#' Finds the \code{beta_signal} and log-odds intercept (\code{mu_lp}) that
#' jointly produce the target AUC and prevalence under the exact
#' data-generating settings (complexity, nonlinear_strength, correlation,
#' distribution, predictor_type).
#'
#' @param target_prevalence Target outcome prevalence. Must be in (0, 1).
#' @param target_performance Target AUC (c-statistic). Must be in (0.5, 1).
#' @param candidate_features Total number of predictors (signal + noise).
#' @param proportion_noise_features Proportion of \code{candidate_features}
#'   that are noise predictors. Must be in [0, 1).
#' @param complexity Integer 1-4. Default = 1.
#' @param nonlinear_strength Fraction of signal variance carried by the
#'   nonlinear component (C2/C3 only), in [0, 1). When \code{NULL} (default), the
#'   complexity-level default is used: C1 = 0, C2 = 0.2, C3 = 0.3, C4 = 0.
#' @param correlation Common pairwise predictor correlation. Defaults to the
#'   SAME value as \code{generate_binary_data()} so the two never silently
#'   disagree. Whatever you pass here must match the generator call.
#' @param distribution Global continuous predictor distribution family.
#'   Defaults to match \code{generate_binary_data()}.
#' @param predictor_type \code{"continuous"} (default) or \code{"binary"}.
#'   Defaults to match \code{generate_binary_data()}.
#' @param binary_prevalence Bernoulli probability for binary predictors.
#'   Defaults to match \code{generate_binary_data()}.
#' @param n_sim Sample size for the internal LP simulation. Larger values give
#'   more stable AUC estimates within the bisection. Default = 300 000.
#' @param n_validate Sample size for the independent end-to-end validation.
#'   Defaults to \code{n_sim}.
#' @param beta_interval Search interval \code{c(lo, hi)} for beta_signal.
#'   Default = \code{c(1e-4, 20)}. The upper bound is doubled automatically
#'   if the AUC at \code{hi} is still below \code{target_performance}.
#' @param beta_tol Absolute convergence tolerance for the beta bisection.
#'   Default = 1e-4.
#' @param mu_tol Absolute convergence tolerance for the mu_lp root-finding.
#'   Default = 1e-6.
#' @param tolerance Acceptable absolute deviation of the validated prevalence
#'   and AUC from their targets before a warning is raised. Default = 0.02.
#'
#' @return A named numeric vector:
#'   \describe{
#'     \item{\code{mu_lp}}{Tuned log-odds intercept.}
#'     \item{\code{beta_signal}}{Tuned effect size.}
#'     \item{\code{prevalence_achieved}}{Prevalence from an independent
#'       generate_binary_data() validation draw.}
#'     \item{\code{auc_achieved}}{AUC of the oracle LP from the same draw.}
#'     \item{\code{var_lp_unit}}{Estimated Var(LP) at beta_signal = 1.}
#'   }
#' @keywords internal
binary_tuning <- function(
    target_prevalence,
    target_performance,
    candidate_features,
    proportion_noise_features,
    complexity          = 1,
    nonlinear_strength  = NULL,
    # ---- shared predictor-generation settings -------------------------------
    # These defaults MUST match generate_binary_data(). check_generator_defaults()
    # (called at the top of the body) errors out if they ever drift apart.
    correlation         = 0.3,
    distribution        = "normal",
    predictor_type      = "continuous",
    binary_prevalence   = 0,
    # -------------------------------------------------------------------------
    n_sim               = 300000,
    n_validate          = NULL,
    beta_interval       = c(1e-4, 20),
    beta_tol            = 1e-4,
    mu_tol              = 1e-6,
    tolerance           = 0.02
) {

  # ---- guard against future default drift ------------------------------------
  check_generator_defaults()

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

  if (is.null(n_validate)) n_validate <- n_sim

  # ---- derive signal / noise counts ------------------------------------------
  n_signal <- candidate_features -
    round(candidate_features * proportion_noise_features)
  noise    <- candidate_features - n_signal

  if (n_signal < 1)
    stop("proportion_noise_features leaves no signal predictors.")

  # ---- resolve predictor strength --------------------------------------------
  nonlinear_strength <- resolve_nonlinear_strength(nonlinear_strength, complexity)

  # ---- Step 1: simulate lp_unit once -----------------------------------------
  # LP = beta * lp_unit (exact scaling: generate_linear_predictor is linear
  # in beta_signal). Fixing the predictor matrix across all bisection
  # iterations eliminates Monte Carlo variation between steps.
  #
  # The SAME predictor settings are forwarded that the generator will use,
  # which is the whole point: independence vs. correlation, the distribution
  # family, continuous vs. binary predictors all change the LP distribution
  # and hence the beta/mu solution.

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
    nonlinear_strength  = nonlinear_strength
  )

  var_lp_unit <- stats::var(lp_unit)

  if (!is.finite(var_lp_unit) || var_lp_unit <= 0)
    stop("Estimated Var(LP) at beta_signal = 1 is not a positive finite ",
         "number (got ", format(var_lp_unit), "). This usually means the ",
         "linear predictor is degenerate for the requested configuration -- ",
         "e.g. binary predictors (x in {0,1}) under complexity 2 or 3, where ",
         "x^2 = x makes the quadratic term collapse to zero. Use continuous ",
         "predictors for those complexity levels, or complexity 1 for binary ",
         "predictors.")

  # Pre-draw Uniform(0,1) variates for Bernoulli outcomes. Fixed across
  # bisection iterations to reduce Monte Carlo noise in AUC estimates.
  U_binom <- stats::runif(n_sim)

  # ---- Step 2: find_mu -- solve E[plogis(mu + lp_scaled)] = target_prev -----
  # Robust bracketing: start at the approximate solution and expand outward
  # by multiples of sd(lp_scaled) until the function changes sign. This
  # handles any beta, including large values where sd(lp_scaled) >> 30 and a
  # fixed +/-30 bracket fails to contain the root.

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
  # Bernoulli labels have the correct prevalence for the case-control split.

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

  # ---- Step 5: final mu_lp ----------------------------------------------------
  lp_final_scaled <- beta_signal * lp_unit
  mu_lp           <- find_mu(lp_final_scaled)

  # ---- Step 6: HONEST end-to-end validation ----------------------------------
  # Independent draw from the REAL generator, scored on the oracle LP. This is
  # exactly what a caller gets, so it can catch any residual mismatch (e.g. a
  # caller who passes different settings to generate_binary_data()).

  val <- generate_binary_data(
    n                   = n_validate,
    n_signal_parameters = n_signal,
    noise_parameters    = noise,
    beta_signal         = beta_signal,
    complexity          = complexity,
    nonlinear_strength  = nonlinear_strength,
    predictor_type      = predictor_type,
    binary_prevalence   = binary_prevalence,
    correlation         = correlation,
    distribution        = distribution,
    mu_lp               = mu_lp
  )

  X_val  <- as.matrix(val[, setdiff(names(val), "y"), drop = FALSE])
  lp_val <- generate_linear_predictor(
    X                   = X_val,
    n_signal_parameters = n_signal,
    noise_parameters    = noise,
    intercept           = mu_lp,
    beta_signal         = beta_signal,
    complexity          = complexity,
    nonlinear_strength  = nonlinear_strength
  )

  prevalence_achieved <- mean(val$y)
  auc_achieved        <- cstat_full(val$y, lp_val)

  if (abs(prevalence_achieved - target_prevalence) > tolerance ||
      abs(auc_achieved - target_performance) > tolerance) {
    warning(sprintf(
      paste0("Validation deviates from target beyond tolerance (%.3f).\n",
             "  prevalence: target %.3f, achieved %.3f\n",
             "  AUC:        target %.3f, achieved %.3f\n",
             "Check that the settings passed to generate_binary_data() match ",
             "those passed to binary_tuning()."),
      tolerance, target_prevalence, prevalence_achieved,
      target_performance, auc_achieved
    ))
  }

  return(c(
    mu_lp               = mu_lp,
    beta_signal         = beta_signal,
    prevalence_achieved = prevalence_achieved,
    auc_achieved        = auc_achieved,
    var_lp_unit         = var_lp_unit
  ))
}

# -----------------------------------------------------------------------------
# check_generator_defaults
# Errors if the shared predictor-generation defaults of binary_tuning() and
# generate_binary_data() have drifted apart. This is what would have caught the
# correlation 0 vs 0.3 mismatch at source.
# -----------------------------------------------------------------------------
check_generator_defaults <- function() {
  shared <- c("correlation", "distribution", "predictor_type",
              "binary_prevalence")
  tf <- formals(binary_tuning)
  gf <- formals(generate_binary_data)
  for (a in shared) {
    tv <- tryCatch(eval(tf[[a]]), error = function(e) NULL)
    gv <- tryCatch(eval(gf[[a]]), error = function(e) NULL)
    if (!isTRUE(all.equal(tv, gv)))
      stop(sprintf(
        paste0("Default for '%s' differs between binary_tuning() (%s) and ",
               "generate_binary_data() (%s). Align them so tuned parameters ",
               "are valid for the generated data."),
        a, format(tv), format(gv)
      ))
  }
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# cstat_full
# Exact c-statistic (= AUC) via the Mann-Whitney rank identity. Used for the
# validation report so the returned AUC is accurate (no subsampling noise).
# -----------------------------------------------------------------------------
cstat_full <- function(y, pred) {
  if (length(unique(y)) < 2) return(NA_real_)
  r  <- rank(pred)                       # average ranks handle ties correctly
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (as.numeric(n1) * as.numeric(n0))
}

# -----------------------------------------------------------------------------
# quickcstat
# Fast AUC estimate via random subsampling, used inside the beta bisection
# where speed matters and a noisy estimate is acceptable. The smaller group is
# matched to the larger by random sampling, then concordance is the proportion
# of (case, control) pairs where the case score exceeds the control score.
# Ties contribute 0.5, matching the standard c-statistic definition -- this is
# essential when the linear predictor is discrete (e.g. binary predictors),
# where ignoring ties biases the estimate downward and the bisection then
# over-shoots beta.
# -----------------------------------------------------------------------------
quickcstat <- function(y, pred) {
  casepred <- pred[y == 1]
  conpred  <- pred[y == 0]

  if (length(casepred) == 0 || length(conpred) == 0) return(NA_real_)

  if (length(conpred) > length(casepred)) {
    conpred <- conpred[sample(length(conpred), length(casepred),
                              replace = FALSE)]
  } else {
    casepred <- casepred[sample(length(casepred), length(conpred),
                                replace = FALSE)]
  }
  mean((casepred > conpred) + 0.5 * (casepred == conpred))
}
