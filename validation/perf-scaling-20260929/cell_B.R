# Usage: Rscript cell_B.R <scenario_id>
# Runs one end-to-end simulate_*() call under Rprof. Writes results/B/<id>.*
id <- commandArgs(trailingOnly = TRUE)[1]
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=",
  commandArgs(FALSE), value = TRUE)[1]))), "common.R"))
source(file.path(script_dir, "grid.R"))
spec <- grid_B()[[id]]
stopifnot(!is.null(spec))
out_dir <- file.path(results_dir, "B")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

info <- list(id = id, spec = spec, status = "ok", error = NA_character_,
             test_n = test_n)
save_info <- function(info) {
  saveRDS(info, file.path(out_dir, paste0(id, ".info.rds")))
  quit(save = "no", status = 0)
}
if (spec$model == "xgboost" && !requireNamespace("xgboost", quietly = TRUE)) {
  info$status <- "skipped"; info$error <- "xgboost not installed"
  save_info(info)
}

load_pmsims()
fn <- get(spec$fn)
args <- spec[setdiff(names(spec), "fn")]
args$n_reps_total <- if (smoke) 60L else 1000L
args$mean_or_assurance <- "assurance"
args$test_n <- test_n
args$progress <- FALSE

set.seed(2026)
Rprof(file.path(out_dir, paste0(id, ".Rprof")), interval = 0.05,
      line.profiling = TRUE, filter.callframes = FALSE)
t0 <- proc.time()
res <- tryCatch(do.call(fn, args), error = function(e) e)
el <- proc.time() - t0
Rprof(NULL)

info$elapsed <- el[["elapsed"]]
info$cpu <- el[["user.self"]] + el[["sys.self"]] + el[["user.child"]] + el[["sys.child"]]
info$peak_rss_mb <- peak_rss_mb()
if (inherits(res, "error")) {
  info$status <- "error"; info$error <- conditionMessage(res)
} else {
  info$min_n <- res$min_n
  info$engine_secs <- as.numeric(res$simulation_time)
  info$bounds <- res$mlpwr_ds$boundaries
  info$design_points <- vapply(res$data, function(d) d$x, numeric(1))
  info$reps_per_point <- vapply(res$data, function(d) length(d$y), numeric(1))
}
save_info(info)
