#' Tuning for a survival outcome model
#'
#' @param target_performance The desired model performance in a large sample
#' @param target_prevalence The desired model performance in a large sample
#' @param tolerance The tolerance in the large sample performance
#' @return The optimal value for the tuning parameter
#' @keywords internal
#' @export
#'
#' @examples

survival_tuning <- function(
  target_prevalence, # Target event rate (proportion of events)
  target_performance, # Target C-index
  min.opt = c(-3, -10), # Lower bounds for (log_sigma, log_lambda)
  max.opt = c(3, 10), # Upper bounds for (log_sigma, log_lambda)
  tolerance = 1e-6,
  proportion_noise_features,
  candidate_features,
  N_sim_optim = 20000, # Sample size for optimization
  N_sim_final = 50000 # Sample size for final validation
) {
  require(survival)

  # Objective function to minimize
  obj_fun <- function(x) {
    log_sigma <- x[1]
    log_lambda <- x[2]
    sigma <- exp(log_sigma) # Ensure positive standard deviation
    lambda <- exp(log_lambda) # Ensure positive baseline hazard

    # Simulate survival data
    set.seed(123) # For reproducibility during optimization
    lp <- rnorm(N_sim_optim, mean = 0, sd = sigma)
    hazard_rate <- lambda * exp(lp)
    event_time <- rexp(N_sim_optim, rate = hazard_rate)
    #censoring_time <- rep(1, N_sim_optim)  # Administrative censoring at t=1
    censoring_time <- rep(quantile(event_time, target_prevalence), N_sim_optim) # Administrative censoring at t=c
    time_obs <- pmin(event_time, censoring_time)
    event_ind <- as.numeric(event_time <= censoring_time)

    # Calculate achieved event rate and C-index
    event_rate_achieved <- mean(event_ind)
    surv_obj <- survival::Surv(time_obs, event_ind)
    cindex_achieved <- 1 - concordance(surv_obj ~ lp)$concordance

    # Sum of squared errors
    (event_rate_achieved - target_prevalence)^2 +
      (cindex_achieved - target_performance)^2
  }

  # Find optimal parameters
  opt_result <- optim(
    par = c(0, 0), # Initial guess (log_sigma=0, log_lambda=0)
    fn = obj_fun,
    method = "L-BFGS-B",
    lower = min.opt,
    upper = max.opt #,
    #control = list(abstol = tolerance)
  )

  # Extract optimal parameters
  sigma_opt <- exp(opt_result$par[1])
  lambda_opt <- exp(opt_result$par[2])

  # Validate with large simulation
  lp_final <- rnorm(N_sim_final, mean = 0, sd = sigma_opt)
  hazard_rate_final <- lambda_opt * exp(lp_final)
  event_time_final <- rexp(N_sim_final, rate = hazard_rate_final)
  #censoring_time_final <- rep(1, N_sim_final)
  censoring_time_final <- rep(
    quantile(event_time_final, target_prevalence),
    N_sim_final
  )
  time_obs_final <- pmin(event_time_final, censoring_time_final)
  event_ind_final <- as.numeric(event_time_final <= censoring_time_final)

  # Calculate final metrics
  event_rate_final <- mean(event_ind_final)
  surv_obj_final <- survival::Surv(time_obs_final, event_ind_final)
  cindex_final <- 1 - concordance(surv_obj_final ~ lp_final)$concordance

  # Compute beta_signal for features
  non_noise_predictors <- candidate_features -
    round(candidate_features * proportion_noise_features)
  beta_signal <- if (non_noise_predictors > 0) {
    sigma_opt / sqrt(non_noise_predictors)
  } else {
    0
  }

  return(c(
    lambda_opt = lambda_opt,
    sigma_sq = sigma_opt^2,
    beta_signal = beta_signal,
    event_rate = event_rate_final,
    cindex = cindex_final
  ))
}

### check

#survival_tuning (
#  target_prevalence=0.2,       # Target event rate (proportion of events)
#  target_performance=0.76,       # Target C-index (AUC equivalent)
# min.opt = c(0.001),       # Lower bound for lambda (baseline hazard)
#  max.opt = c(100),         # Upper bound for lambda
#  tolerance = 0.00001,
#  proportion_noise_features=0,
#  candidate_features=100
#)
