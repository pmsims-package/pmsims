# =============================================================================
# Tuning for a survival outcome model
#
# Revision notes
# --------------
# The original implementation optimized (log_sigma, log_lambda) where sigma
# was the SD of lp ~ N(0, sigma^2) and lambda was the baseline hazard. It then
# derived beta_signal as:
#
#   beta_signal = sigma_opt / sqrt(S)            [assumes Var(LP) = S * beta^2]
#
# This back-calculation is valid only under complexity 1, zero correlation,
# strong predictor strength, and standard-normal predictors — exactly the
# conditions under which Var(LP) = S * beta^2.
#
# Revised strategy
# ----------------
# 1. Simulate LP at beta_signal = 1 using the actual generator settings and
#    estimate var_lp_unit = Var(LP | beta = 1).
#
# 2. Because Var(LP | beta) = beta^2 * var_lp_unit, the LP SD at beta is
#       sigma(beta) = beta * sqrt(var_lp_unit)
#    The optimiser now searches over (log_beta, log_lambda) instead of
#    (log_sigma, log_lambda), constructing lp by scaling lp_unit:
#       lp_i = beta * lp_unit_i
#    This ensures the LP is drawn from the correct non-normal distribution when
#    complexity > 1 or correlation > 0, rather than always from a normal.
#
# 3. The C-index and event rate are computed from the rescaled lp_unit, so the
#    optimiser explores the (beta, lambda) space while respecting the true LP
#    shape at every complexity / correlation / strength setting.
#
# 4. beta_signal is recovered directly from the optimised log_beta without any
#    further Var(LP) assumption.
# =============================================================================

#' Tuning function for a survival outcome model
#'
#' Finds the \code{beta_signal} and baseline hazard (\code{lambda}) that
#' jointly produce the target C-index and event rate under the exact
#' data-generating settings (complexity, predictor_strength, correlation,
#' distribution, predictor_type).
#'
#' @param target_prevalence Target event rate (proportion of events). Must be
#'   in (0, 1).
#' @param target_performance Target C-index. Must be in (0.5, 1).
#' @param candidate_features Total number of predictors (signal + noise).
#' @param proportion_noise_features Proportion of \code{candidate_features}
#'   that are noise predictors. Must be in [0, 1).
#' @param complexity Integer 1-4.  Default = 1.
#' @param predictor_strength One of \code{"strong"}, \code{"moderate"}, or
#'   \code{"weak"}.  When \code{NULL} the complexity-level default is used.
#' @param correlation Common pairwise predictor correlation.  Default = 0.
#' @param distribution Global continuous predictor distribution family.
#'   Default = \code{"normal"}.
#' @param predictor_type \code{"continuous"} (default) or \code{"binary"}.
#' @param binary_prevalence Bernoulli probability for binary predictors.
#' @param n_sim_optim Sample size for LP simulation and optimisation.
#'   Default = 50 000.
#' @param n_sim_final Sample size for final validation.  Default = 100 000.
#' @param min.opt Lower bounds for \code{c(log_beta, log_lambda)}.
#'   Default = \code{c(-5, -10)}.
#' @param max.opt Upper bounds for \code{c(log_beta, log_lambda)}.
#'   Default = \code{c(5, 10)}.
#' @param tolerance Passed to \code{optim} as \code{factr} (L-BFGS-B).
#'   Default = 1e-6.
#'
#' @return A named numeric vector:
#'   \describe{
#'     \item{\code{lambda_opt}}{Tuned baseline hazard.}
#'     \item{\code{beta_signal}}{Tuned effect size.}
#'     \item{\code{event_rate}}{Empirical event rate in validation.}
#'     \item{\code{cindex}}{Empirical C-index in validation.}
#'     \item{\code{var_lp_unit}}{Estimated Var(LP) at beta_signal = 1.}
#'   }
#' @keywords internal
survival_tuning <- function(
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
    n_sim_optim         = 50000,
    n_sim_final         = 100000,
    min.opt             = c(-5, -10),
    max.opt             = c(5,  10),
    tolerance           = 1e-6
) {
  
  # ---- input validation ------------------------------------------------------
  if (!is.numeric(target_prevalence) || target_prevalence <= 0 ||
      target_prevalence >= 1)
    stop("target_prevalence must be in (0, 1).")
  if (!is.numeric(target_performance) || target_performance <= 0.5 ||
      target_performance >= 1)
    stop("target_performance (C-index) must be in (0.5, 1).")
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
  
  # ---- Step 1: simulate LP at beta_signal = 1 --------------------------------
  # We fix the predictor matrix and lp_unit for the entire optimisation so
  # the objective function only needs to rescale lp_unit by beta — no repeated
  # calls to generate_predictors(), which is expensive.
  
  set.seed(42)   # reproducibility across the optimisation calls
  X_unit <- generate_predictors(
    n                   = n_sim_optim,
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
  
  # ---- Step 2: define the objective function ---------------------------------
  #
  # Parameters searched: x = c(log_beta, log_lambda)
  # lp = beta * lp_unit   (preserves the true LP shape at every iteration)
  
  obj_fun <- function(x) {
    beta   <- exp(x[1])
    lambda <- exp(x[2])
    lp     <- beta * lp_unit
    
    hazard_rate  <- lambda * exp(lp)
    event_time   <- stats::rexp(n_sim_optim, rate = hazard_rate)
    censor_time  <- rep(
      stats::quantile(event_time, 1 - target_prevalence), n_sim_optim
    )
    time_obs     <- pmin(event_time, censor_time)
    event_ind    <- as.numeric(event_time <= censor_time)
    
    event_rate_achieved <- mean(event_ind)
    
    surv_obj        <- survival::Surv(time_obs, event_ind)
    cindex_achieved <- survival::concordance(surv_obj ~ lp)$concordance
    
    (event_rate_achieved - target_prevalence)^2 +
      (cindex_achieved   - target_performance)^2
  }
  
  # ---- Step 3: optimise ------------------------------------------------------
  opt_result <- stats::optim(
    par    = c(0, 0),
    fn     = obj_fun,
    method = "L-BFGS-B",
    lower  = min.opt,
    upper  = max.opt,
    control = list(factr = tolerance / .Machine$double.eps)
  )
  
  beta_opt   <- exp(opt_result$par[1])
  lambda_opt <- exp(opt_result$par[2])
  
  # ---- Step 4: validate with a fresh, larger simulation ----------------------
  set.seed(NULL)  # remove the fixed seed for the final draw
  
  X_final <- generate_predictors(
    n                   = n_sim_final,
    n_signal_parameters = n_signal,
    noise_parameters    = noise,
    complexity          = complexity,
    predictor_type      = predictor_type,
    binary_prevalence   = binary_prevalence,
    correlation         = correlation,
    distribution        = distribution
  )
  
  lp_final <- generate_linear_predictor(
    X                   = X_final,
    n_signal_parameters = n_signal,
    noise_parameters    = noise,
    intercept           = 0,
    beta_signal         = beta_opt,
    complexity          = complexity,
    predictor_strength  = predictor_strength
  )
  
  hazard_rate_final  <- lambda_opt * exp(lp_final)
  event_time_final   <- stats::rexp(n_sim_final, rate = hazard_rate_final)
  censor_time_final  <- rep(
    stats::quantile(event_time_final, 1 - target_prevalence), n_sim_final
  )
  time_obs_final     <- pmin(event_time_final, censor_time_final)
  event_ind_final    <- as.numeric(event_time_final <= censor_time_final)
  
  event_rate_final <- mean(event_ind_final)
  surv_obj_final   <- survival::Surv(time_obs_final, event_ind_final)
  cindex_final     <- survival::concordance(surv_obj_final ~ lp_final)$concordance
  
  return(c(
    lambda_opt  = lambda_opt,
    beta_signal = beta_opt,
    event_rate  = event_rate_final,
    cindex      = cindex_final,
    var_lp_unit = var_lp_unit
  ))
}
