# Combine sweep results and flag components that scale badly.
# Writes results/summary_A.csv, results/scaling_A.csv, results/summary_B.csv,
# results/profile_B.csv and prints a digest.
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=",
  commandArgs(FALSE), value = TRUE)[1]))), "common.R"))

bind_fill <- function(dfs) {
  cols <- unique(unlist(lapply(dfs, names)))
  do.call(rbind, lapply(dfs, function(d) {
    for (cl in setdiff(cols, names(d))) d[[cl]] <- NA
    d[cols]
  }))
}

# ---- Part A --------------------------------------------------------------------
fa <- list.files(file.path(results_dir, "A"), pattern = "\\.csv$", full.names = TRUE)
if (length(fa)) {
  A <- bind_fill(lapply(fa, utils::read.csv, stringsAsFactors = FALSE))
  # stubs only carry id/status/error: recover the grid fields from the id
  m <- regmatches(A$id, regexec("^A_([a-z]+)_([a-z]+)_c([0-9])_([a-z]+)_p([0-9]+)_n([0-9]+)$", A$id))
  A$outcome <- vapply(m, `[`, "", 2); A$model <- vapply(m, `[`, "", 3)
  A$complexity <- as.integer(vapply(m, `[`, "", 4))
  A$distribution <- vapply(m, `[`, "", 5)
  A$p <- as.integer(vapply(m, `[`, "", 6)); A$n <- as.integer(vapply(m, `[`, "", 7))
  A$family <- sub("_n[0-9]+$", "", A$id)
  A <- A[order(A$outcome, A$model, A$complexity, A$distribution, A$p, A$n), ]
  utils::write.csv(A, file.path(results_dir, "summary_A.csv"), row.names = FALSE)

  cat("\n== Part A status\n"); print(table(A$status))
  bad <- A[A$status != "ok", c("id", "status", "error")]
  if (nrow(bad)) { cat("\n-- non-ok cells\n"); print(bad, row.names = FALSE) }

  # Log-log scaling exponents. vs n: fixed (config, p); vs p: fixed (config, n).
  comps <- c("gen_train", "fit", "metric_primary", "metric_secondary", "gen_test")
  cfg_key <- paste(A$outcome, A$model, A$complexity, A$distribution, sep = "/")
  slope <- function(x, y) {
    ok <- is.finite(x) & is.finite(y) & y > 0.002 # ignore sub-2ms noise
    if (sum(ok) < 2) return(NA_real_)
    unname(stats::coef(stats::lm(log(y[ok]) ~ log(x[ok])))[2])
  }
  sc <- list()
  for (k in unique(cfg_key)) for (cmp in comps) {
    col <- paste0(cmp, "_secs")
    if (!col %in% names(A)) next
    d <- A[cfg_key == k & A$status == "ok", ]
    for (pp in unique(d$p)) {
      dd <- d[d$p == pp, ]
      sc[[length(sc) + 1]] <- data.frame(config = k, component = cmp, axis = "n",
        at = paste0("p=", pp), exponent = slope(dd$n, dd[[col]]),
        max_secs = suppressWarnings(max(dd[[col]], na.rm = TRUE)),
        max_rss_mb = suppressWarnings(max(dd[[paste0(cmp, "_rss_mb")]], na.rm = TRUE)))
    }
    for (nn in unique(d$n)) {
      dd <- d[d$n == nn, ]
      sc[[length(sc) + 1]] <- data.frame(config = k, component = cmp, axis = "p",
        at = paste0("n=", nn), exponent = slope(dd$p, dd[[col]]),
        max_secs = suppressWarnings(max(dd[[col]], na.rm = TRUE)),
        max_rss_mb = suppressWarnings(max(dd[[paste0(cmp, "_rss_mb")]], na.rm = TRUE)))
    }
  }
  S <- do.call(rbind, sc)
  S <- S[is.finite(S$exponent), ]
  utils::write.csv(S, file.path(results_dir, "scaling_A.csv"), row.names = FALSE)

  cat("\n== Super-linear scaling (exponent > 1.3, max time > 0.05 s)\n")
  flag <- S[S$exponent > 1.3 & S$max_secs > 0.05, ]
  print(flag[order(-flag$exponent), ], row.names = FALSE, digits = 3)

  cat("\n== Slowest single replicates (Part A, ok cells)\n")
  ok <- A[A$status == "ok", ]
  print(utils::head(ok[order(-ok$replicate_secs), c("id", "replicate_secs",
    "gen_test_secs", "fit_secs", "metric_primary_secs", "metric_secondary_secs",
    "final_rss_mb")], 25), row.names = FALSE, digits = 3)

  cat("\n== Tuning cost per configuration (one-off per run)\n")
  tt <- unique(ok[, c("outcome", "complexity", "distribution", "p", "tune_secs",
                      "tune_peak_rss_mb")])
  print(tt[order(-tt$tune_secs), ][seq_len(min(15, nrow(tt))), ], row.names = FALSE, digits = 3)
}

# ---- Part B --------------------------------------------------------------------
fb <- list.files(file.path(results_dir, "B"), pattern = "\\.info\\.rds$", full.names = TRUE)
if (length(fb)) {
  B <- bind_fill(lapply(fb, function(f) {
    x <- readRDS(f)
    data.frame(id = x$id, status = x$status, error = x$error %||% NA,
      elapsed = x$elapsed %||% NA, cpu = x$cpu %||% NA,
      peak_rss_mb = x$peak_rss_mb %||% NA,
      min_n = if (is.null(x$min_n)) NA else as.character(x$min_n),
      lower = if (is.null(x$bounds)) NA else x$bounds[[1]][1],
      upper = if (is.null(x$bounds)) NA else x$bounds[[1]][2],
      n_points = length(x$design_points))
  }))
  utils::write.csv(B, file.path(results_dir, "summary_B.csv"), row.names = FALSE)
  cat("\n== Part B end-to-end runs\n")
  print(B, row.names = FALSE, digits = 4)

  # Stage x component attribution from the Rprof files.
  parse_rprof <- function(path) {
    lines <- readLines(path)
    interval <- as.numeric(sub(".*sample.interval=([0-9]+).*", "\\1", lines[1])) / 1e6
    files <- character(); stacks <- list()
    for (ln in lines[-1]) {
      if (startsWith(ln, "#File ")) {
        files[as.integer(sub("#File ([0-9]+):.*", "\\1", ln))] <-
          basename(sub("#File [0-9]+: ", "", ln))
        next
      }
      stacks[[length(stacks) + 1]] <- regmatches(ln,
        gregexpr('"[^"]*"|[0-9]+#[0-9]+', ln))[[1]]
    }
    list(interval = interval, files = files, stacks = stacks)
  }
  test_lines <- c("engines.R:231", "engines.R:393", "engines.R:657",
    "start_values.R:186", "simulate_wrappers.R:415", "simulate_wrappers.R:584",
    "simulate_wrappers.R:767")
  classify <- function(toks, files) {
    fr <- gsub('"', "", toks[!grepl("^[0-9]+#", toks)])
    has <- function(f) any(fr %in% f)
    stage <- if (has("call_tuner")) "tuning"
      else if (has("calculate_adaptive_bounds")) "adaptive"
      else if (has(c("find.design", "addval", "simfun", "fit.surrogate"))) "gp_search"
      else "other"
    comp <- "overhead"
    if (has("data_function")) {
      i <- match('"data_function"', toks); nx <- toks[i + 1]
      cl <- if (!is.na(nx) && grepl("^[0-9]+#", nx)) {
        p <- as.integer(strsplit(nx, "#")[[1]]); paste0(files[p[1]], ":", p[2])
      } else NA
      comp <- if (!is.na(cl) && cl %in% test_lines) "data_test" else "data_train"
    } else if (has("model_function")) comp <- "fit"
    else if (has(c("metric_function", "metric_function_2"))) comp <- "metric"
    else if (stage == "gp_search") comp <- "mlpwr"
    else if (stage == "tuning") comp <- "tuning"
    paste(stage, comp, sep = "/")
  }
  prof <- list()
  for (f in list.files(file.path(results_dir, "B"), pattern = "\\.Rprof$", full.names = TRUE)) {
    pr <- parse_rprof(f)
    if (!length(pr$stacks)) next
    cls <- vapply(pr$stacks, classify, "", files = pr$files)
    leaf <- vapply(pr$stacks, function(t) {
      s <- t[grepl("^[0-9]+#", t)]
      if (!length(s)) return(NA_character_)
      p <- as.integer(strsplit(s[1], "#")[[1]]); paste0(pr$files[p[1]], ":", p[2])
    }, "")
    id <- sub("\\.Rprof$", "", basename(f))
    tab <- sort(prop.table(table(cls)) * 100, decreasing = TRUE)
    lines_tab <- sort(prop.table(table(leaf)) * 100, decreasing = TRUE)
    prof[[id]] <- data.frame(id = id, what = c(paste("stage", names(tab)),
      paste("line", utils::head(names(lines_tab), 8))),
      pct = c(as.numeric(tab), as.numeric(utils::head(lines_tab, 8))))
  }
  if (length(prof)) {
    P <- do.call(rbind, prof)
    utils::write.csv(P, file.path(results_dir, "profile_B.csv"), row.names = FALSE)
    cat("\n== Part B profile breakdown (% of samples; CPU-weighted for threaded code)\n")
    for (id in names(prof)) {
      cat("\n--", id, "\n")
      print(prof[[id]][prof[[id]]$pct >= 2, c("what", "pct")], row.names = FALSE, digits = 3)
    }
  }
}
