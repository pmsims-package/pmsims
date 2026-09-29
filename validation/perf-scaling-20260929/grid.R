# Sweep grids. `Rscript grid.R A` / `Rscript grid.R B` prints cell ids in run
# order; the cell scripts source this file to look a cell up by id.

smoke_grid <- nzchar(Sys.getenv("PERF_SMOKE"))

base_cfg <- function(outcome) {
  list(
    outcome = outcome,
    p_noise = 0, complexity = 1, distribution = "normal", correlation = 0.3,
    prevalence = 0.3,
    max_perf = switch(outcome, binary = 0.8, continuous = 0.5, survival = 0.7),
    censoring = 0.3,
    metric = "calibration_slope", target = 0.9
  )
}

# ---- Part A: per-replicate component costs vs predictors (p) and training n --
# One cell = one (outcome, model, complexity, distribution, p, n). Each cell
# times: train-set generation, test-set generation (test_n rows), model fit,
# primary metric (calibration slope / CSSE) and secondary metric (AUC / R2 /
# C-index), plus the one-off tuning cost for the configuration.
grid_A <- function() {
  ps <- if (smoke_grid) c(5, 10) else c(5, 20, 50, 100, 200)
  ns <- if (smoke_grid) c(200, 1000) else c(200, 2000, 20000, 100000)
  models <- list(
    binary = c("glm", "lasso", "rf", "xgboost"),
    continuous = c("lm", "lasso", "rf", "xgboost"),
    survival = c("coxph", "lasso", "rf", "xgboost")
  )
  fams <- list()
  for (o in names(models)) for (m in models[[o]]) {
    fams[[length(fams) + 1]] <- list(outcome = o, model = m, complexity = 1,
                                     distribution = "normal")
  }
  # Extra predictor-generation paths, with the default (regression) models.
  for (o in names(models)) {
    fams[[length(fams) + 1]] <- list(outcome = o, model = models[[o]][1],
                                     complexity = 3, distribution = "normal")
    fams[[length(fams) + 1]] <- list(outcome = o, model = models[[o]][1],
                                     complexity = 1, distribution = "t")
  }
  rows <- list()
  for (n in ns) for (p in ps) for (f in fams) {
    fam_id <- sprintf("A_%s_%s_c%d_%s_p%d", f$outcome, f$model, f$complexity,
                      f$distribution, p)
    rows[[length(rows) + 1]] <- data.frame(
      id = sprintf("%s_n%d", fam_id, n), family = fam_id,
      outcome = f$outcome, model = f$model, complexity = f$complexity,
      distribution = f$distribution, p = p, n = n)
  }
  do.call(rbind, rows) # ordered by n, then p: cheap cells first
}

# ---- Part B: end-to-end simulate_*() calls that push towards large n / p ------
# Each runs under Rprof with package defaults (n_reps_total = 1000,
# test_n = 30000, adaptive stage on) unless stated.
grid_B <- function() {
  bin <- function(...) modifyList(list(fn = "simulate_binary",
    signal_parameters = 20, noise_parameters = 0, complexity = 1,
    data_control = list(correlation = 0.3), outcome_prevalence = 0.3,
    maximum_achievable_cstatistic = 0.8, model = "glm",
    metric = "calibration_slope", target_performance = 0.9), list(...))
  con <- function(...) modifyList(list(fn = "simulate_continuous",
    signal_parameters = 20, noise_parameters = 0, complexity = 1,
    data_control = list(correlation = 0.3), maximum_achievable_rsquared = 0.5,
    model = "lm", metric = "calibration_slope", target_performance = 0.9),
    list(...))
  sur <- function(...) modifyList(list(fn = "simulate_survival",
    signal_parameters = 10, noise_parameters = 0, complexity = 1,
    data_control = list(correlation = 0.3), maximum_achievable_cindex = 0.7,
    baseline_hazard = 0.01, censoring_rate = 0.3, model = "coxph",
    metric = "calibration_slope", target_performance = 0.9), list(...))

  list(
    B01_bin_glm_p50       = bin(signal_parameters = 50),
    B02_bin_glm_p100      = bin(signal_parameters = 100),
    B03_bin_glm_prev05    = bin(outcome_prevalence = 0.05,
                                maximum_achievable_cstatistic = 0.7),
    B04_bin_glm_c3_p50    = bin(signal_parameters = 50, complexity = 3,
                                metric = "auc", target_performance = 0.75),
    B05_cont_lm_p100      = con(signal_parameters = 100),
    B06_cont_lm_t_p50     = con(signal_parameters = 50, data_control =
                                list(correlation = 0.3, predictor_distribution = "t")),
    B07_surv_cox_p50_cens70 = sur(signal_parameters = 50, censoring_rate = 0.7),
    B08_surv_cox_p20_cens90 = sur(signal_parameters = 20, censoring_rate = 0.9),
    B09_bin_lasso_p100    = bin(signal_parameters = 50, noise_parameters = 50,
                                model = "lasso"),
    B10_bin_rf_p50        = bin(signal_parameters = 50, model = "rf",
                                metric = "auc", target_performance = 0.75),
    B11_surv_rf_p20_cindex = sur(signal_parameters = 20, model = "rf",
                                 metric = "cindex", target_performance = 0.68),
    B12_surv_rf_p10_slope = sur(model = "rf"),
    B13_bin_xgb_p20       = bin(model = "xgboost"),
    B14_surv_xgb_p20      = sur(signal_parameters = 20, model = "xgboost")
  )
}

if (sys.nframe() == 0L) {
  part <- commandArgs(trailingOnly = TRUE)[1]
  ids <- if (identical(part, "A")) grid_A()$id else names(grid_B())
  if (smoke_grid && identical(part, "B")) ids <- ids[c(1, 7, 10, 11)]
  cat(ids, sep = "\n")
}
