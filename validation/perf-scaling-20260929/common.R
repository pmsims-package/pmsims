# Shared setup for the scaling sweep. Sourced by cell_A.R, cell_B.R, grid.R.

script_dir <- local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
repo_root <- normalizePath(file.path(script_dir, "..", ".."))
results_dir <- Sys.getenv("PERF_RESULTS", file.path(script_dir, "results"))
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

smoke <- nzchar(Sys.getenv("PERF_SMOKE"))
test_n <- if (smoke) 2000L else 30000L

load_pmsims <- function() {
  suppressMessages(pkgload::load_all(repo_root, quiet = TRUE, export_all = TRUE))
}

# ---- memory -------------------------------------------------------------------
# Peak resident set size of this process in MB. On Linux the peak can be reset
# (clear_refs "5"), which gives a per-component high-water mark.
peak_rss_mb <- function() {
  if (file.exists("/proc/self/status")) {
    s <- readLines("/proc/self/status")
    v <- grep("^VmHWM:", s, value = TRUE)
    return(as.numeric(gsub("[^0-9]", "", v)) / 1024)
  }
  rss <- suppressWarnings(as.numeric(system2(
    "ps", c("-o", "rss=", "-p", Sys.getpid()), stdout = TRUE)))
  rss / 1024 # current, not peak, on macOS
}
reset_peak <- function() {
  if (file.exists("/proc/self/clear_refs")) {
    try(writeLines("5", "/proc/self/clear_refs"), silent = TRUE)
  }
  invisible(gc(reset = TRUE, full = TRUE))
}
r_heap_peak_mb <- function() { g <- gc(); sum(g[, ncol(g)]) } # "max used" (Mb)

# ---- build the three functions exactly as simulate_*() does -------------------
# cfg: list(outcome, model, p_signal, p_noise, complexity, distribution,
#           correlation, prevalence, max_perf, censoring, metric, target)
build_functions <- function(cfg) {
  data_control <- list(correlation = cfg$correlation,
                       predictor_distribution = cfg$distribution)
  dc <- resolve_data_control(data_control, cfg$complexity)
  csse_plan <- plan_internal_csse(cfg$metric, cfg$model, cfg$target)
  cand <- cfg$p_signal + cfg$p_noise
  prop_noise <- cfg$p_noise / cand

  tune_key <- paste(cfg$outcome, cfg$p_signal, cfg$p_noise, cfg$complexity,
                    cfg$distribution, cfg$correlation, cfg$prevalence,
                    cfg$max_perf, cfg$censoring, sep = "_")
  tune_file <- file.path(results_dir, "tune_cache", paste0(tune_key, ".rds"))
  dir.create(dirname(tune_file), showWarnings = FALSE)

  if (file.exists(tune_file)) {
    tc <- readRDS(tune_file)
  } else {
    t0 <- proc.time()[["elapsed"]]
    tp <- switch(cfg$outcome,
      binary = call_tuner(binary_tuning, list(
        target_prevalence = cfg$prevalence, target_performance = cfg$max_perf,
        candidate_features = cand, proportion_noise_features = prop_noise,
        .complexity = cfg$complexity), dc),
      continuous = call_tuner(continuous_tuning, list(
        r2 = cfg$max_perf, candidate_features = cand,
        proportion_noise_features = prop_noise, .complexity = cfg$complexity), dc),
      survival = call_tuner(survival_tuning, list(
        target_prevalence = 1 - cfg$censoring, target_performance = cfg$max_perf,
        candidate_features = cand, proportion_noise_features = prop_noise,
        .complexity = cfg$complexity), dc)
    )
    tc <- list(tp = tp, secs = proc.time()[["elapsed"]] - t0,
               peak_rss_mb = peak_rss_mb())
    saveRDS(tc, tune_file)
  }

  extra <- switch(cfg$outcome,
    binary = list(mu_lp = get_param(tc$tp, "mu_lp"),
                  beta_signal = get_param(tc$tp, "beta_signal"),
                  baseline_prob = cfg$prevalence),
    continuous = list(beta_signal = get_param(tc$tp, "beta_signal")),
    survival = list(baseline_hazard = 0.01,
                    beta_signal = get_param(tc$tp, "beta_signal"),
                    censoring_rate = cfg$censoring)
  )
  data_function <- default_data_generators(list(
    type = cfg$outcome,
    args = make_data_args(cfg$p_signal, cfg$p_noise, cfg$complexity, dc, extra)
  ))
  model_function <- default_model_generators(cfg$outcome, cfg$model)
  metric_primary <- default_metric_generator(csse_plan$metric, data_function)
  metric_secondary <- default_metric_generator(
    switch(cfg$outcome, binary = "auc", continuous = "r2", survival = "cindex"),
    data_function)

  list(data_function = data_function, model_function = model_function,
       metric_primary = metric_primary, metric_secondary = metric_secondary,
       tune_secs = tc$secs, tune_peak_rss_mb = tc$peak_rss_mb)
}

default_model <- c(binary = "glm", continuous = "lm", survival = "coxph")

if (!exists("%||%", baseenv())) `%||%` <- function(x, y) if (is.null(x)) y else x
