library(pmsims)
library(glmnet)
library(partykit)
library(grf)
library(evalHTE)
library(tictoc)
library(future)
library(future.apply)

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
  "calibration_for_benifit (binned)" = list(
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


results_dir <- normalizePath("HTE Results", mustWork = FALSE)
dir.create(results_dir, showWarnings = FALSE)

run_grid_row <- function(i) {
  d_name   <- sim_grid$data_gen_name[i]
  m_name   <- sim_grid$model_name[i]
  met_name <- sim_grid$metric_name[i]
  
  d_fn     <- data_generators_list[[d_name]]
  m_fn     <- models_list[[m_name]]
  met_info <- metrics_list[[met_name]]
  
  safe_name <- gsub("[^A-Za-z0-9_-]+", "_", paste(d_name, m_name, met_name, sep = "__"))
  out_file  <- file.path(results_dir, sprintf("row_%03d_%s.rds", i, safe_name))
  
  # Resume: skip rows already completed in an earlier run
  if (file.exists(out_file)) return(readRDS(out_file))
  
  error_msg <- NA_character_
  start <- Sys.time()
  sim_res <- tryCatch(
    pmsims::simulate_custom(
      data_function      = d_fn,
      model_function     = m_fn,
      metric_function    = met_info$fn,
      target_performance = met_info$target,
      mean_or_assurance  = met_info$mean_or_assurance,
      test_n             = 5000,
      n_reps_total       = 1000,
      n_reps_per         = 20,
      progress           = FALSE,
      verbose            = FALSE
    ),
    error = function(e) {
      error_msg <<- conditionMessage(e)
      NULL
    }
  )
  time_elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  
  est_n <- if (!is.null(sim_res) && !is.null(sim_res$min_n)) sim_res$min_n else NA_real_
  
  out <- list(
    summary = data.frame(
      Row         = i,
      Data_Gen    = d_name,
      Model       = m_name,
      Metric      = met_name,
      Target      = met_info$target,
      Estimated_N = est_n,
      Status      = if (!is.null(sim_res)) "Success" else "Failed",
      Error       = error_msg,
      Time_Secs   = time_elapsed,
      stringsAsFactors = FALSE
    ),
    sim_res = sim_res
  )
  
  saveRDS(out, out_file)
  out
}

cat("\n====================================================\n")
cat(" LOOP 3: Running simulate_custom search (parallel)   \n")
cat("====================================================\n\n")

n_workers <- max(1, parallelly::availableCores() - 1)
plan(multisession, workers = n_workers)

grid_results <- future_lapply(
  seq_len(nrow(sim_grid)),
  run_grid_row,
  future.seed       = TRUE,
  future.chunk.size = 1,
  future.packages   = c("benchtm", "glmnet", "grf", "partykit", "evalHTE", "pmsims")
)

plan(sequential)

custom_search_results <- do.call(rbind, lapply(grid_results, `[[`, "summary"))
cat("\n=== Final Simulation Grid Summary ===\n")
print(custom_search_results)