# Original calibration-slope search, with the seed as the only changed input.
# Source this file from R/Quarto, or run from the repository root:
# Rscript validation/calibration-slope-search/seed-reproduction/reproduce.R --seeds=1,298
# Add --stage-only to stop after the ORIGINAL preliminary search.

original_commit <- "a88e7011c9998bf02f7518d5cc4ead9d35b86fbb"

load_original_pmsims <- function(repo = ".") {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Install pkgload and the package dependencies first.")
  }
  repo <- normalizePath(repo, mustWork = TRUE)
  checkout <- tempfile("pmsims-original-")
  dir.create(checkout)
  archive <- tempfile(fileext = ".tar")
  status <- system2(
    "git",
    c("-C", shQuote(repo), "archive", original_commit),
    stdout = archive
  )
  if (status != 0L) {
    stop("Cannot read the original commit from this repository.")
  }
  utils::untar(archive, exdir = checkout)
  unlink(archive)
  # Load ALL original R files, not a mixture of original and patched helpers.
  pkgload::load_all(checkout, quiet = TRUE)
  checkout
}

run_original <- function(seed, out_dir, stage_only = FALSE) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  mode <- if (stage_only) "stage1" else "full"
  prefix <- file.path(out_dir, sprintf("seed%d-%s", seed, mode))
  stage <- NULL
  capture_stage <- function(x) {
    stage <<- x
    saveRDS(x, paste0(prefix, "-stage.rds"))
    trace_table <- do.call(
      rbind,
      lapply(x$track, function(z) {
        data.frame(n = z$n, performance = z$performance, reps = length(z$raw))
      })
    )
    write.csv(trace_table, paste0(prefix, "-trace.csv"), row.names = FALSE)
    if (stage_only) {
      stop(structure(
        list(
          message = "Stopped after original stage 1; GP was not run.",
          call = NULL
        ),
        class = c("stage1_complete", "error", "condition")
      ))
    }
  }
  old_options <- options(
    pmsims.confirm_long_runs = FALSE,
    pmsims.seed_repro.capture = capture_stage
  )
  on.exit(options(old_options), add = TRUE)
  # This exit hook records the unchanged search, without consuming any RNG.
  # Only --stage-only terminates early, AFTER the original bounds are computed.
  trace(
    "calculate_adaptive_bounds",
    exit = quote(getOption("pmsims.seed_repro.capture")(returnValue())),
    where = asNamespace("pmsims"),
    print = FALSE
  )
  on.exit(
    untrace("calculate_adaptive_bounds", where = asNamespace("pmsims")),
    add = TRUE
  )

  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  elapsed <- system.time({
    result <- tryCatch(
      pmsims::simulate_binary(
        signal_parameters = 5L,
        noise_parameters = 0L,
        complexity = 1L,
        outcome_prevalence = 0.20,
        maximum_achievable_cstatistic = 0.60,
        model = "glm",
        metric = "calibration_slope",
        target_performance = 0.95,
        n_reps_total = 1000L,
        mean_or_assurance = "assurance",
        progress = FALSE
      ),
      error = identity
    )
  })[["elapsed"]]
  if (is.null(stage)) {
    stop(
      "Original preliminary search did not complete: ",
      conditionMessage(result)
    )
  }
  if (inherits(result, "error") && !stage_only) {
    stop(conditionMessage(result))
  }
  if (!inherits(result, "error")) {
    saveRDS(result, paste0(prefix, "-result.rds"))
  }
  row <- data.frame(
    seed = seed,
    mode = mode,
    gp_budget = 1000L,
    gp_reps_used = if (stage_only) {
      NA_integer_
    } else {
      sum(vapply(
        result$mlpwr_ds$data,
        function(z) length(z$y),
        integer(1)
      ))
    },
    min_n = if (stage_only) NA_real_ else result$min_n,
    predicted_slope = if (stage_only) NA_real_ else result$perf_n,
    lower_bound = stage$min_sample_size,
    upper_bound = stage$max_sample_size,
    stage_reps = sum(vapply(
      stage$track,
      function(z) length(z$raw),
      integer(1)
    )),
    stage_stop_reason = stage$stop_reason,
    seconds = elapsed
  )
  write.csv(row, paste0(prefix, "-summary.csv"), row.names = FALSE)
  writeLines(
    c(
      paste("Original commit:", original_commit),
      paste("RNGkind:", paste(RNGkind(), collapse = " / ")),
      capture.output(sessionInfo())
    ),
    paste0(prefix, "-session.txt")
  )
  print(row)
  invisible(row)
}

validate_original_result <- function(seed, out_dir, workers = 4L) {
  prefix <- file.path(out_dir, sprintf("seed%d-full", seed))
  result <- readRDS(paste0(prefix, "-result.rds"))
  reps <- 1000L
  if (.Platform$OS.type == "windows") {
    workers <- 1L
  }
  values <- unlist(
    parallel::mclapply(
      seq_len(reps),
      function(i) {
        set.seed(7000000L + seed * 10000L + i)
        test <- result$data_function(30000L)
        train <- result$data_function(result$min_n)
        fit <- result$model_function(train)
        result$metric_function(test, fit, result$model)
      },
      mc.cores = workers,
      mc.set.seed = FALSE
    ),
    use.names = FALSE
  )
  stopifnot(length(values) == reps, all(is.finite(values)))
  ordered <- sort(values)
  ci <- ordered[c(qbinom(0.025, reps, 0.2), qbinom(0.975, reps, 0.2) + 1L)]
  # GLM searches directly on the slope scale. No CSSE conversion is needed.
  row <- data.frame(
    seed = seed,
    n = result$min_n,
    reps = reps,
    empirical_q20 = unname(quantile(values, 0.2, type = 1)),
    ci_low = ci[1L],
    ci_high = ci[2L],
    target = 0.95
  )
  saveRDS(values, paste0(prefix, "-validation.rds"))
  write.csv(row, paste0(prefix, "-validation.csv"), row.names = FALSE)
  print(row)
  invisible(row)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  seed_arg <- args[startsWith(args, "--seeds=")]
  seeds <- if (length(seed_arg)) {
    as.integer(strsplit(sub("--seeds=", "", seed_arg[1L]), ",", fixed = TRUE)[[
      1L
    ]])
  } else {
    1L
  }
  if (anyNA(seeds) || !length(seeds)) {
    stop("Supply integer seeds with --seeds=1,298.")
  }
  out_arg <- args[startsWith(args, "--out=")]
  out_dir <- if (length(out_arg)) {
    sub("--out=", "", out_arg[1L])
  } else {
    "validation/calibration-slope-search/seed-reproduction/runs"
  }
  load_original_pmsims(".")
  for (seed in seeds) {
    run_original(seed, out_dir, "--stage-only" %in% args)
    if ("--validate" %in% args && !("--stage-only" %in% args)) {
      validate_original_result(seed, out_dir)
    }
  }
}
