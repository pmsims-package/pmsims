# =============================================================================
# Tuning for a continuous outcome model
#
# Estimate Var(LP | beta_signal = 1) under the requested generator settings,
# then scale beta_signal to reach the target R^2:
#
#   R^2 = Var(LP) / (Var(LP) + Var(epsilon))
#       = Var(LP) / (Var(LP) + 1)                  [epsilon ~ N(0,1)]
#
# Because generate_linear_predictor() is linear in beta_signal,
# Var(LP) = beta_signal^2 * V1, where V1 = Var(LP | beta_signal = 1).
# Solving for beta_signal gives:
#
#   beta_signal = sqrt( R2 / (V1 * (1 - R2)) )
#
# This applies across all supported complexities, correlations, predictor types,
# and continuous distributions.
# =============================================================================

#' Tuning function for a continuous outcome model
#'
#' Finds the \code{beta_signal} value that produces a target large-sample
#' \eqn{R^2} under the exact data-generating settings (complexity,
#' nonlinear_strength, correlation, distribution, predictor_type).
#'
#' @param r2 Target large-sample \eqn{R^2} (proportion of variance explained).
#'   Must be in (0, 1).
#' @param candidate_features Total number of predictors (signal + noise).
#' @param proportion_noise_features Proportion of \code{candidate_features}
#'   that are noise predictors (zero coefficient). Must be in [0, 1).
#' @param complexity Integer 1-4 controlling the functional form of the linear
#'   predictor passed to the data generator.  Default = 1.
#' @param nonlinear_strength Fraction of signal variance carried by the
#'   nonlinear component (C2/C3 only), in [0, 1). When \code{NULL} (default), the
#'   complexity-level default is used: C1 = 0, C2 = 0.2, C3 = 0.3, C4 = 0.
#' @param correlation Common pairwise predictor correlation.  Default = 0.
#' @param distribution Global continuous predictor distribution family passed
#'   to the data generator.  Default = \code{"normal"}.
#' @param predictor_type \code{"continuous"} (default) or \code{"binary"}.
#' @param binary_prevalence Bernoulli probability for binary predictors.
#'   Required (and used) only when \code{predictor_type = "binary"}.
#' @param n_sim Sample size used to estimate Var(LP).  Larger values give more
#'   stable estimates.  Default = 100 000.
#'
#' @return A named numeric vector:
#'   \describe{
#'     \item{\code{beta_signal}}{Tuned effect size.}
#'     \item{\code{r2_achieved}}{Empirical R^2 verified in a large simulation.}
#'     \item{\code{var_lp_unit}}{Estimated Var(LP) at beta_signal = 1 (the
#'       scaling constant).}
#'   }
#' @keywords internal
continuous_tuning <- function(
  r2,
  candidate_features,
  proportion_noise_features,
  complexity = 1,
  nonlinear_strength = NULL,
  correlation = 0,
  distribution = "normal",
  predictor_type = "continuous",
  binary_prevalence = 0,
  n_sim = 100000
) {
  # ---- input validation ------------------------------------------------------
  if (!is.numeric(r2) || length(r2) != 1 || r2 <= 0 || r2 >= 1) {
    stop("r2 must be a single numeric value in (0, 1).")
  }
  if (
    !is.numeric(proportion_noise_features) ||
      proportion_noise_features < 0 ||
      proportion_noise_features >= 1
  ) {
    stop("proportion_noise_features must be in [0, 1).")
  }

  # ---- derive signal / noise counts ------------------------------------------
  n_signal <- candidate_features -
    round(candidate_features * proportion_noise_features)
  noise <- candidate_features - n_signal

  if (n_signal < 1) {
    stop("proportion_noise_features leaves no signal predictors.")
  }

  # ---- resolve nonlinear strength --------------------------------------------
  nonlinear_strength <- resolve_nonlinear_strength(
    nonlinear_strength,
    complexity
  )

  # ---- Step 1: estimate Var(LP) with beta_signal = 1 ------------------------
  #
  # Run the predictor and linear-predictor generators at beta_signal = 1 and
  # measure the resulting LP variance directly, without outcome noise.

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
    beta_signal = 1, # unit beta; we scale analytically below
    complexity = complexity,
    nonlinear_strength = nonlinear_strength
  )

  var_lp_unit <- stats::var(lp_unit)

  if (var_lp_unit <= 0) {
    stop(
      "Estimated Var(LP) at beta_signal = 1 is zero or negative. ",
      "Check that n_signal >= 1 and the complexity/distribution settings ",
      "produce non-degenerate predictors."
    )
  }

  # ---- Step 2: solve for beta_signal analytically ----------------------------
  #
  # Var(LP | beta) = beta^2 * var_lp_unit   [LP is linear-in-beta by design]
  # R^2 = Var(LP) / (Var(LP) + 1)           [epsilon ~ N(0,1), Var = 1]
  # => beta^2 = R2 / (var_lp_unit * (1 - R2))

  beta_signal <- sqrt(r2 / (var_lp_unit * (1 - r2)))

  # ---- Step 3: verify with a second large simulation -------------------------
  lp_verify <- beta_signal * lp_unit # rescale the already-simulated lp
  # (exact because LP is linear in beta_signal)
  y_verify <- lp_verify + stats::rnorm(n_sim)
  r2_achieved <- 1 - stats::var(y_verify - lp_verify) / stats::var(y_verify)

  return(c(
    beta_signal = beta_signal,
    r2_achieved = r2_achieved,
    var_lp_unit = var_lp_unit
  ))
}
