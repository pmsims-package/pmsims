# Profiling scenarios: common simulate_*() calls at package defaults
# (n_reps_total = 1000, test_n = 30000, adaptive start values on).

scenarios <- list(
  bin_glm_p20 = function() simulate_binary(
    signal_parameters = 20, noise_parameters = 0, complexity = 1,
    data_control = list(correlation = 0.3),
    outcome_prevalence = 0.30, maximum_achievable_cstatistic = 0.80,
    model = "glm", metric = "calibration_slope", target_performance = 0.85,
    n_reps_total = 1000, mean_or_assurance = "assurance", progress = FALSE
  ),
  cont_lm_p15 = function() simulate_continuous(
    signal_parameters = 15, noise_parameters = 0, complexity = 1,
    data_control = list(correlation = 0.3),
    maximum_achievable_rsquared = 0.50,
    model = "lm", metric = "calibration_slope", target_performance = 0.95,
    n_reps_total = 1000, mean_or_assurance = "assurance", progress = FALSE
  ),
  surv_cox_p10 = function() simulate_survival(
    signal_parameters = 10, noise_parameters = 0, complexity = 1,
    data_control = list(correlation = 0.3),
    maximum_achievable_cindex = 0.70, baseline_hazard = 0.01,
    censoring_rate = 0.30,
    model = "coxph", metric = "calibration_slope", target_performance = 0.9,
    n_reps_total = 1000, mean_or_assurance = "assurance", progress = FALSE
  ),
  bin_lasso_p20 = function() simulate_binary(
    signal_parameters = 10, noise_parameters = 10, complexity = 1,
    data_control = list(correlation = 0.3),
    outcome_prevalence = 0.30, maximum_achievable_cstatistic = 0.80,
    model = "lasso", metric = "calibration_slope", target_performance = 0.9,
    n_reps_total = 1000, mean_or_assurance = "assurance", progress = FALSE
  ),
  bin_glm_c3_p10 = function() simulate_binary(
    signal_parameters = 10, noise_parameters = 0, complexity = 3,
    data_control = list(correlation = 0.3),
    outcome_prevalence = 0.30, maximum_achievable_cstatistic = 0.80,
    model = "glm", metric = "auc", target_performance = 0.75,
    n_reps_total = 1000, mean_or_assurance = "assurance", progress = FALSE
  ),
  cont_lm_t_p10 = function() simulate_continuous(
    signal_parameters = 10, noise_parameters = 0, complexity = 1,
    data_control = list(correlation = 0.3, predictor_distribution = "t"),
    maximum_achievable_rsquared = 0.50,
    model = "lm", metric = "calibration_slope", target_performance = 0.9,
    n_reps_total = 1000, mean_or_assurance = "assurance", progress = FALSE
  ),
  bin_rf_p10 = function() simulate_binary(
    signal_parameters = 10, noise_parameters = 0, complexity = 1,
    data_control = list(correlation = 0.3),
    outcome_prevalence = 0.30, maximum_achievable_cstatistic = 0.80,
    model = "rf", metric = "auc", target_performance = 0.75,
    n_reps_total = 1000, mean_or_assurance = "assurance", progress = FALSE
  ),
  surv_rf_p10 = function() simulate_survival(
    signal_parameters = 10, noise_parameters = 0, complexity = 1,
    data_control = list(correlation = 0.3),
    maximum_achievable_cindex = 0.70, baseline_hazard = 0.01,
    censoring_rate = 0.30,
    model = "rf", metric = "cindex", target_performance = 0.67,
    n_reps_total = 1000, mean_or_assurance = "assurance", progress = FALSE
  )
)
