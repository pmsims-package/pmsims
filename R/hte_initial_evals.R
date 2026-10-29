library(pmsims)
library(glmnet)
library(partykit)
library(grf)
library(evalHTE)
library(tictoc)
options(scipen = 6)
# =============================================================================
# 1. Component Registries (Data Generators, Models, Metrics)
# =============================================================================

# List of Data Generators
data_generators_list <- list(
  "hte_gen4" = hte_gen4
)

# List of Model Fitting Functions
models_list <- list(
  "OLS Regression" = fit_linear_regression,
  "Penalized Regression" = fit_penalized_regression
)
#   "Causal Forest"        = fit_causal_forest
#   "MOB-L-Tree"             = fit_mob_l_tree,


# List of Performance Metric Functions
metrics_list <- list(
  #  "Het_Test_GRF" = list(
  #    fn = metric_test_heterogeneity_grf,
  #    target = 0.8,
  #    mean_or_assurance = "mean"
  #  ), # I have removed this test because it is slow ~1s per run
  "Het_Test_evalHTE" = list(
    fn = metric_test_heterogeneity_evalhte,
    target = 0.8,
    mean_or_assurance = "mean"
  ),
  "calibration_for_benifit" = list(
    fn = metric_calibration_slope_blp,
    target = -(0.1^2),
    mean_or_assurance = "assurance"
    
  )
)

# Expand simulation grid over all Data Gen x Model x Metric combinations
sim_grid <- expand.grid(
  data_gen_name = names(data_generators_list),
  model_name    = names(models_list),
  metric_name   = names(metrics_list),
  stringsAsFactors = FALSE
)

sim_grid <- sim_grid[1,]

run_evaluation <- function(n_train, n_test = 30000, verbose = TRUE) {
  if (verbose) cat("\n=== Evaluating at training n =", n_train, "===\n")
  run_start <- proc.time()[["elapsed"]]
  rows <- list()
  
  for (d_name in names(data_generators_list)) {
    d_fn <- data_generators_list[[d_name]]
    train_data <- d_fn(n = n_train)
    test_data  <- d_fn(n = n_test)
    
    for (m_name in names(models_list)) {
      m_fn <- models_list[[m_name]]
      
      model_time <- system.time(
        fitted_mod <- tryCatch(
          m_fn(train_data),
          error = function(e) {
            if (verbose) message("Fit error in ", m_name, " (", d_name, "): ", e$message)
            NULL
          }
        )
      )[["elapsed"]]
      
      for (met_name in names(metrics_list)) {
        met_fn <- metrics_list[[met_name]]$fn
        
        metric_time <- system.time(
          score <- if (is.null(fitted_mod)) NA_real_ else tryCatch(
            met_fn(test_data, fitted_mod, m_name),
            error = function(e) {
              if (verbose) message("Error in ", m_name, " x ", met_name, ": ", e$message)
              NA_real_
            }
          )
        )[["elapsed"]]
        
        rows[[length(rows) + 1]] <- data.frame(
          n           = n_train,
          Data_Gen    = d_name,
          Model       = m_name,
          Metric      = met_name,
          Score       = score,
          Model_Time  = model_time,
          Metric_Time = metric_time,
          Total_Time  = model_time + metric_time,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (verbose) cat("Finished n =", n_train, "in",
                   round(proc.time()[["elapsed"]] - run_start, 1), "seconds\n")
  do.call(rbind, rows)
}

process_results <- function(results) {
  idx <- results$Metric == "calibration_for_benifit"
  results[idx, "Score"] <- 1 - sqrt(-results[idx, "Score"])
  results$Score <- round(results$Score, 4)
  results
}



eval_all <- do.call(rbind, lapply(c(100, 1000, 10000, 100000), run_evaluation))
eval_clean <- process_results(eval_all)
print(eval_clean)

saveRDS(eval_clean, file = "HTE Results/initial_eval_2")
