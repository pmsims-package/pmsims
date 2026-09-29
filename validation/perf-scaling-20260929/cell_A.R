# Usage: Rscript cell_A.R <cell_id>
# Times one replicate's components at a given (p, n). Writes results/A/<id>.csv.
id <- commandArgs(trailingOnly = TRUE)[1]
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=",
  commandArgs(FALSE), value = TRUE)[1]))), "common.R"))
source(file.path(script_dir, "grid.R"))
g <- grid_A()
row <- g[g$id == id, ]
stopifnot(nrow(row) == 1)
out_file <- file.path(results_dir, "A", paste0(id, ".csv"))
dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)

res <- data.frame(id = id, family = row$family, outcome = row$outcome,
                  model = row$model, complexity = row$complexity,
                  distribution = row$distribution, p = row$p, n = row$n,
                  test_n = test_n, status = "ok", error = NA_character_)

write_res <- function(res) {
  utils::write.csv(res, out_file, row.names = FALSE)
  quit(save = "no", status = 0)
}

if (row$model == "xgboost" && !requireNamespace("xgboost", quietly = TRUE)) {
  res$status <- "skipped"; res$error <- "xgboost not installed"; write_res(res)
}

load_pmsims()
set.seed(1)
cfg <- modifyList(base_cfg(row$outcome), list(
  model = row$model, p_signal = row$p, complexity = row$complexity,
  distribution = row$distribution))

fns <- tryCatch(build_functions(cfg), error = function(e) e)
if (inherits(fns, "error")) {
  res$status <- "error"; res$error <- paste("setup:", conditionMessage(fns))
  write_res(res)
}
res$tune_secs <- fns$tune_secs
res$tune_peak_rss_mb <- fns$tune_peak_rss_mb

max_reps <- if (smoke) 1L else 3L
slow_secs <- 20 # don't repeat a component slower than this

# Time a component: median elapsed over up to max_reps, peak RSS / R heap.
measure <- function(label, expr_fn) {
  secs <- numeric(0)
  value <- NULL
  reset_peak()
  for (r in seq_len(max_reps)) {
    t0 <- proc.time()[["elapsed"]]
    value <- tryCatch(expr_fn(), error = function(e) e)
    secs[r] <- proc.time()[["elapsed"]] - t0
    if (inherits(value, "error") || secs[r] > slow_secs) break
  }
  res[[paste0(label, "_secs")]] <<- stats::median(secs)
  res[[paste0(label, "_rss_mb")]] <<- peak_rss_mb()
  res[[paste0(label, "_heap_mb")]] <<- r_heap_peak_mb()
  if (inherits(value, "error")) {
    res$status <<- "error"
    res$error <<- paste0(label, ": ", conditionMessage(value))
    return(NULL)
  }
  value
}

mdl <- row$model
test <- measure("gen_test", function() fns$data_function(test_n))
train <- measure("gen_train", function() fns$data_function(row$n))
fit <- if (!is.null(train)) measure("fit", function() fns$model_function(train))
if (!is.null(fit) && !is.null(test)) {
  m1 <- measure("metric_primary", function() fns$metric_primary(test, fit, mdl))
  m2 <- measure("metric_secondary", function() fns$metric_secondary(test, fit, mdl))
  res$metric_primary_value <- if (is.numeric(m1)) m1 else NA
  res$metric_secondary_value <- if (is.numeric(m2)) m2 else NA
}
parts <- c("gen_test_secs", "gen_train_secs", "fit_secs", "metric_primary_secs")
res$replicate_secs <- sum(unlist(res[intersect(parts, names(res))]), na.rm = TRUE)
res$final_rss_mb <- peak_rss_mb()
write_res(res)
