source('validation/predictor-count-spikes/common.R')
cli_args <- commandArgs(trailingOnly = TRUE)
id <- cli_args[1]
mode <- cli_args[2]
seed <- as.integer(cli_args[3])
# Early and final scheduling can meet at the same completed case. One checker
# owns the cell; any second invocation waits for its result rather than fitting
# another 1000 models. No performance or RNG decisions depend on this lock.
checked_file <- result_path(paste0(id, '-', mode, '-heldout.csv'))
check_lock <- raw_path(paste0(id, '-', mode, '-check.lock'))
repeat {
  if (file.exists(checked_file)) quit(status=0L)
  if (dir.create(check_lock,showWarnings=FALSE)) {
    writeLines(as.character(Sys.getpid()),file.path(check_lock,'owner.pid'))
    break
  }
  owner_file <- file.path(check_lock,'owner.pid')
  if (file.exists(owner_file)) {
    owner <- suppressWarnings(as.integer(readLines(owner_file,warn=FALSE)[1]))
    alive <- tryCatch(tools::pskill(owner,signal=0L),error=function(e) TRUE)
    if (isFALSE(alive)) unlink(check_lock,recursive=TRUE)
  }
  Sys.sleep(1)
}
args <- readRDS(raw_path(paste0(id, '-prepared.rds')))$args
search <- readRDS(raw_path(paste0(id, '-', mode, '.rds')))$result
stopifnot(!inherits(search, 'error'))
n <- search$min_n
# Ancillary observations reuse the same fitted models and draw no extra RNG.
# The original internal metric is still returned unchanged.
original_model <- args$model_function
original_metric <- args$metric_function
metadata <- list()
args$model_function <- function(data) {
  fit <- original_model(data)
  metadata[[length(metadata)+1L]] <<- list(lambda_selected=fit$lambda.min,
    lambda_grid_min=min(fit$lambda))
  fit
}
attributes(args$model_function) <- attributes(original_model)
args$metric_function <- function(test, fit, model) {
  value <- original_metric(test, fit, model)
  slope <- tryCatch(binary_calib_slope(test, fit, model), error=function(e) NA_real_)
  k <- length(metadata)
  metadata[[k]]$raw_slope <<- slope
  metadata[[k]]$original_csse <<- value
  value
}
attributes(args$metric_function) <- attributes(original_metric)
result <- evaluate_values(args, n, seed, paste0(id, ' ', mode, ' N=', n))
meta <- do.call(rbind,lapply(metadata,as.data.frame))
stopifnot(nrow(meta)==1000L,all(is.finite(meta$raw_slope)),
  all(abs(meta$original_csse+(1-meta$raw_slope)^2)<1e-10))
saveRDS(meta,raw_path(paste0(id,'-',mode,'-ridge-metadata.rds')))
meta_summary <- data.frame(id=id,mode=mode,n=n,reps=nrow(meta),
  mean_raw_slope=mean(meta$raw_slope),raw_slope_q20=unname(quantile(meta$raw_slope,.2)),
  fraction_in_09_11_band=mean(meta$raw_slope>=.9 & meta$raw_slope<=1.1),
  fraction_below_09=mean(meta$raw_slope<.9),fraction_above_11=mean(meta$raw_slope>1.1),
  fraction_lambda_at_grid_min=mean(abs(meta$lambda_selected/meta$lambda_grid_min-1)<1e-10))
write.csv(meta_summary,result_path(paste0(id,'-',mode,'-ridge-metadata.csv')),row.names=FALSE)
saveRDS(result, raw_path(paste0(id, '-', mode, '-heldout.rds')))
row <- cbind(data.frame(id = id, mode = mode, n = n, validation_seed = seed,
  errors = length(result$errors), seconds = result$seconds), result$summary)
write.csv(row, result_path(paste0(id, '-', mode, '-heldout.csv')), row.names = FALSE)
print(row)

unlink(check_lock, recursive=TRUE)
