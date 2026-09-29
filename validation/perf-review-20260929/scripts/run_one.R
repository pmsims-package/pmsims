# Usage: Rscript run_one.R <scenario_name>
args <- commandArgs(trailingOnly = TRUE)
name <- args[[1]]
here <- Sys.getenv("PMSIMS_PERF_DIR")
suppressMessages(pkgload::load_all(file.path(here, "pmsims-fc"), quiet = TRUE))
source(file.path(here, "perf", "scenarios.R"))
out_dir <- file.path(here, "perf", "prof")
dir.create(out_dir, showWarnings = FALSE)

set.seed(2026)
prof_file <- file.path(out_dir, paste0(name, ".Rprof"))
Rprof(prof_file, interval = 0.01, line.profiling = TRUE, memory.profiling = TRUE,
      filter.callframes = FALSE)
t0 <- proc.time()
res <- scenarios[[name]]()
el <- proc.time() - t0
Rprof(NULL)

info <- list(
  name = name,
  elapsed = el[["elapsed"]],
  user = el[["user.self"]] + el[["user.child"]],
  sys = el[["sys.self"]],
  simulation_time = as.numeric(res$simulation_time),
  min_n = res$min_n,
  n_evaluated = length(res$data),
  design_points = vapply(res$data, function(d) d$x, numeric(1)),
  reps_per_point = vapply(res$data, function(d) length(d$y), numeric(1)),
  bounds = res$mlpwr_ds$boundaries
)
saveRDS(info, file.path(out_dir, paste0(name, ".info.rds")))
cat(sprintf("%s: elapsed %.1fs (engine %.1fs), min_n = %s\n",
            name, el[["elapsed"]], info$simulation_time, format(res$min_n)))
