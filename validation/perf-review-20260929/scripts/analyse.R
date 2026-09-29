# Attribute Rprof samples to (stage, component) and find package hotspots.
here <- file.path(Sys.getenv("PMSIMS_PERF_DIR"), "perf", "prof")

parse_rprof <- function(path) {
  lines <- readLines(path)
  hdr <- lines[1]
  interval <- as.numeric(sub(".*sample.interval=([0-9]+).*", "\\1", hdr)) / 1e6
  files <- character()
  stacks <- list()
  for (ln in lines[-1]) {
    if (startsWith(ln, "#File ")) {
      id <- as.integer(sub("#File ([0-9]+):.*", "\\1", ln))
      files[id] <- basename(sub("#File [0-9]+: ", "", ln))
      next
    }
    ln <- sub("^:[0-9]+:[0-9]+:[0-9]+:[0-9]+:", "", ln)
    toks <- regmatches(ln, gregexpr('"[^"]*"|[0-9]+#[0-9]+', ln))[[1]]
    stacks[[length(stacks) + 1]] <- toks
  }
  list(interval = interval, files = files, stacks = stacks)
}

# srcref token immediately after (i.e. the caller-side line of) frame `fn`
caller_line <- function(toks, fn, files) {
  i <- match(paste0('"', fn, '"'), toks)
  if (is.na(i) || i == length(toks)) return(NA_character_)
  nx <- toks[i + 1]
  if (!grepl("^[0-9]+#", nx)) return(NA_character_)
  p <- as.integer(strsplit(nx, "#")[[1]])
  paste0(files[p[1]], ":", p[2])
}

classify <- function(toks, files) {
  fr <- gsub('"', "", toks[!grepl("^[0-9]+#", toks)])
  has <- function(f) any(fr %in% f)
  stage <- if (has("call_tuner")) "tuning"
  else if (has("compute_start_sample_sizes")) "start_values"
  else if (has("calculate_adaptive_bounds")) "adaptive"
  else if (has(c("find.design", "addval", "simfun", "fit.surrogate"))) "gp_search"
  else if (has(c("data_function", "model_function", "metric_function_2"))) "posthoc_metric2"
  else "other"

  comp <- "overhead"
  if (has("data_function")) {
    cl <- caller_line(toks, "data_function", files)
    is_test <- !is.na(cl) && cl %in% c("engines.R:231", "engines.R:393",
      "engines.R:657", "start_values.R:186", "simulate_wrappers.R:415",
      "simulate_wrappers.R:584", "simulate_wrappers.R:767")
    comp <- if (is_test) "data_test" else "data_train"
  } else if (has("model_function")) {
    comp <- "fit"
  } else if (has(c("metric_function", "metric_function_2"))) {
    comp <- if (has("predict_custom")) "metric_predict" else "metric_other"
  } else if (stage == "gp_search") {
    comp <- if (has(c("fit.surrogate", "gauss.fit", "km", "DiceKriging::km"))) "gp_fit"
      else if (has(c("DiceKriging::predict.km", "predict.km", "optim", "fn"))) "gp_predict"
      else if (has(c("noise_fun", "var_bootstrap"))) "noise_bootstrap"
      else if (has(c("aggregate_fun"))) "aggregate"
      else "mlpwr_other"
  } else if (stage == "tuning") {
    comp <- "tuning"
  }
  c(stage = stage, comp = comp)
}

innermost_pkg_line <- function(toks, files) {
  s <- toks[grepl("^[0-9]+#", toks)]
  if (!length(s)) return(NA_character_)
  p <- as.integer(strsplit(s[1], "#")[[1]])
  paste0(files[p[1]], ":", p[2])
}

analyse <- function(name) {
  pr <- parse_rprof(file.path(here, paste0(name, ".Rprof")))
  cls <- t(vapply(pr$stacks, classify, character(2), files = pr$files))
  leaf <- vapply(pr$stacks, function(t) gsub('"', "", t[!grepl("^[0-9]+#", t)][1]),
                 character(1))
  pline <- vapply(pr$stacks, innermost_pkg_line, character(1), files = pr$files)
  df <- data.frame(stage = cls[, 1], comp = cls[, 2], leaf = leaf, pline = pline,
                   sec = pr$interval)
  df$scenario <- name
  df
}

scen <- sub("\\.Rprof$", "", list.files(here, pattern = "\\.Rprof$"))
all <- do.call(rbind, lapply(scen, analyse))
saveRDS(all, file.path(here, "samples.rds"))

for (s in scen) {
  d <- all[all$scenario == s, ]
  info <- readRDS(file.path(here, paste0(s, ".info.rds")))
  cat("\n==========", s, sprintf("elapsed %.1fs, profiled %.1fs, min_n %s",
      info$elapsed, sum(d$sec), format(info$min_n)), "\n")
  cat("design points:", paste(info$design_points, collapse = ","), " bounds:",
      paste(info$bounds, collapse = "-"), "\n")
  st <- tapply(d$sec, d$stage, sum)
  print(round(sort(st, decreasing = TRUE), 2))
  tab <- xtabs(sec ~ stage + comp, d)
  print(round(tab, 2))
  cat("-- top leaf functions\n")
  print(round(head(sort(tapply(d$sec, d$leaf, sum), decreasing = TRUE), 12), 2))
  cat("-- top innermost package lines\n")
  print(round(head(sort(tapply(d$sec, d$pline, sum), decreasing = TRUE), 12), 2))
}
