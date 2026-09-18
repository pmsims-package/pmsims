# =============================================================================
# Tuning for a survival outcome model
#
# Strategy
# --------
# The event rate is exact by construction (quantile censoring), and the
# C-index is monotone increasing in beta, so tuning reduces to a deterministic
# 1-D bisection on beta:
#   * Simulate lp_unit = LP at beta = 1 with the actual generator settings
#     (LP is linear in beta_signal, so lp = beta * lp_unit exactly).
#   * Fix one set of Exp(1) innovations E so the objective is deterministic.
#   * For each beta: T = E / exp(beta * lp_unit), censor at the
#     target_prevalence quantile, and evaluate the C-index with reverse = TRUE.
#   * Bisect beta to hit target_performance.
# Finally validate with an independent generate_survival_data() draw and warn
# if the achieved event rate or C-index deviates beyond a tolerance.
# =============================================================================

#' Tuning function for a survival outcome model
#'
#' Finds the \code{beta_signal} that produces the target C-index under the exact
#' data-generating settings (complexity, nonlinear_strength, correlation,
#' distribution, predictor_type). The event rate is set exactly by quantile
#' censoring, and the baseline hazard only fixes the time scale.
#'
#' @param target_prevalence Target event rate (proportion of events) in (0, 1).
#'   Equivalently 1 - censoring_rate.
#' @param target_performance Target C-index in (0.5, 1).
#' @param candidate_features Total number of predictors (signal + noise).
#' @param proportion_noise_features Proportion of \code{candidate_features}
#'   that are noise predictors. Must be in [0, 1).
#' @param complexity Integer 1-4. Default = 1.
#' @param nonlinear_strength Fraction of signal variance carried by the
#'   nonlinear component (C2/C3 only), in [0, 1). When \code{NULL} (default), the
#'   complexity-level default is used: C1 = 0, C2 = 0.2, C3 = 0.3, C4 = 0.
#' @param correlation Common pairwise predictor correlation. Default = 0.3 (to
#'   match generate_survival_data()).
#' @param distribution Global continuous predictor distribution family.
#'   Default = \code{"normal"}.
#' @param predictor_type \code{"continuous"} (default) or \code{"binary"}.
#' @param binary_prevalence Bernoulli probability for binary predictors.
#' @param n_sim Sample size for LP simulation and the bisection. Default 50000.
#' @param n_validate Sample size for the independent validation. Default 100000.
#' @param beta_interval Search interval c(lo, hi) for beta_signal. The upper
#'   bound is doubled automatically if the C-index at hi is below target.
#' @param beta_tol Absolute convergence tolerance for the beta bisection.
#' @param tolerance Acceptable absolute deviation of the validated event rate
#'   and C-index from their targets before a warning is raised. Default 0.02.
#'
#' @return A named numeric vector: \code{lambda_opt} (baseline hazard / time
#'   scale), \code{beta_signal}, \code{event_rate}, \code{cindex},
#'   \code{var_lp_unit}.
#' @keywords internal
survival_tuning <- function(
  target_prevalence,
  target_performance,
  candidate_features,
  proportion_noise_features,
  complexity = 1,
  nonlinear_strength = NULL,
  correlation = 0.3,
  distribution = "normal",
  predictor_type = "continuous",
  binary_prevalence = 0,
  n_sim = 50000,
  n_validate = 100000,
  beta_interval = c(1e-4, 20),
  beta_tol = 1e-4,
  tolerance = 0.02
) {
  # ---- input validation ------------------------------------------------------
  if (
    !is.numeric(target_prevalence) ||
      target_prevalence <= 0 ||
      target_prevalence >= 1
  ) {
    stop("target_prevalence (event rate) must be in (0, 1).")
  }
  if (
    !is.numeric(target_performance) ||
      target_performance <= 0.5 ||
      target_performance >= 1
  ) {
    stop("target_performance (C-index) must be in (0.5, 1).")
  }
  if (
    !is.numeric(proportion_noise_features) ||
      proportion_noise_features < 0 ||
      proportion_noise_features >= 1
  ) {
    stop("proportion_noise_features must be in [0, 1).")
  }
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("The 'survival' package is required.")
  }

  # ---- derive signal / noise counts ------------------------------------------
  n_signal <- candidate_features -
    round(candidate_features * proportion_noise_features)
  noise <- candidate_features - n_signal
  if (n_signal < 1) {
    stop("proportion_noise_features leaves no signal predictors.")
  }

  nonlinear_strength <- resolve_nonlinear_strength(
    nonlinear_strength,
    complexity
  )

  # ---- Step 1: simulate LP at beta_signal = 1 (fixed across the search) ------
  X_unit <- generate_predictors(
    n = n_sim,
    n_signal_parameters = n_signal,
    noise_parameters = noise,
    complexity = complexity,
    predictor_type = predictor_type,
    binary_prevalence = binary_prevalence,
    correlation = correlation,
    distribution = distribution
  )
  lp_unit <- generate_linear_predictor(
    X = X_unit,
    n_signal_parameters = n_signal,
    noise_parameters = noise,
    intercept = 0,
    beta_signal = 1,
    complexity = complexity,
    nonlinear_strength = nonlinear_strength
  )
  var_lp_unit <- stats::var(lp_unit)
  if (!is.finite(var_lp_unit) || var_lp_unit <= 0) {
    stop(
      "Var(LP) at beta = 1 is not positive and finite -- the linear ",
      "predictor is degenerate (e.g. binary predictors under complexity ",
      "2/3, where x^2 = x). Use continuous predictors for those levels."
    )
  }

  # ---- Step 2: deterministic C-index as a function of beta -------------------
  # Fixed Exp(1) innovations make the objective deterministic. lambda is set to
  # 1 here because it cancels from both the C-index and the (quantile-based)
  # event rate.
  E <- stats::rexp(n_sim)

  cindex_at_beta <- function(beta) {
    lp <- beta * lp_unit
    Tt <- E / exp(lp)
    cut <- stats::quantile(Tt, target_prevalence) # event rate = target_prevalence
    ev <- as.integer(Tt <= cut)
    tm <- pmin(Tt, cut)
    # reverse = TRUE: larger lp = higher hazard = shorter survival
    survival::concordance(
      survival::Surv(tm, ev) ~ lp,
      reverse = TRUE
    )$concordance
  }

  # ---- Step 3: bisect beta to hit the target C-index -------------------------
  lo <- beta_interval[1]
  hi <- beta_interval[2]
  for (k in seq_len(8)) {
    if (cindex_at_beta(hi) >= target_performance) {
      break
    }
    hi <- hi * 2
  }
  if (cindex_at_beta(hi) < target_performance) {
    stop(
      "Cannot reach target_performance within the beta search range. ",
      "Increase beta_interval[2] or lower target_performance."
    )
  }
  for (i in seq_len(60)) {
    mid <- (lo + hi) / 2
    if (cindex_at_beta(mid) < target_performance) {
      lo <- mid
    } else {
      hi <- mid
    }
    if ((hi - lo) < beta_tol) break
  }
  beta_opt <- (lo + hi) / 2

  # ---- Step 4: baseline hazard (time scale only) -----------------------------
  # Not identified by the C-index or event rate; set so the marginal median
  # event time is 1. Any positive value yields the same C-index / event rate.
  lambda_opt <- stats::median(E / exp(beta_opt * lp_unit))

  # ---- Step 5: honest validation via the real generator ----------------------
  val <- generate_survival_data(
    n = n_validate,
    n_signal_parameters = n_signal,
    noise_parameters = noise,
    beta_signal = beta_opt,
    baseline_hazard = lambda_opt,
    censoring_rate = 1 - target_prevalence,
    complexity = complexity,
    nonlinear_strength = nonlinear_strength,
    predictor_type = predictor_type,
    binary_prevalence = binary_prevalence,
    correlation = correlation,
    distribution = distribution
  )
  Xv <- as.matrix(val[, setdiff(names(val), c("time", "event")), drop = FALSE])
  lpv <- generate_linear_predictor(
    X = Xv,
    n_signal_parameters = n_signal,
    noise_parameters = noise,
    intercept = 0,
    beta_signal = beta_opt,
    complexity = complexity,
    nonlinear_strength = nonlinear_strength
  )
  event_rate <- mean(val$event)
  cindex <- survival::concordance(
    survival::Surv(val$time, val$event) ~ lpv,
    reverse = TRUE
  )$concordance

  if (
    abs(event_rate - target_prevalence) > tolerance ||
      abs(cindex - target_performance) > tolerance
  ) {
    warning(sprintf(
      paste0(
        "Validation deviates from target beyond tolerance (%.3f).\n",
        "  event rate: target %.3f, achieved %.3f\n",
        "  C-index:    target %.3f, achieved %.3f\n",
        "Check that the settings passed to generate_survival_data() ",
        "match those passed to survival_tuning()."
      ),
      tolerance,
      target_prevalence,
      event_rate,
      target_performance,
      cindex
    ))
  }

  c(
    lambda_opt = lambda_opt,
    beta_signal = beta_opt,
    event_rate = event_rate,
    cindex = cindex,
    var_lp_unit = var_lp_unit
  )
}
