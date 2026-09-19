#!/usr/bin/env Rscript
# Run from repository root. Observers delegate once without consuming RNG.
source("validation/stability-20260918/case-lock.R")
argv <- commandArgs(trailingOnly = TRUE)
root <- 'validation/stability-20260918'
scenarios <- jsonlite::fromJSON(
  file.path(root, 'scenarios.json'),
  simplifyVector = FALSE
)
s <- scenarios[[match(argv[1], vapply(scenarios, `[[`, '', 'id'))]]
seed <- as.integer(argv[2])
role <- argv[3]
out <- file.path(root, 'results', paste0(s$id, '-', role, '-seed', seed))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
if (file.exists(file.path(out, 'DONE'))) {
  quit(status = 0)
}
pkgload::load_all('.', quiet = TRUE)
options(pmsims.confirm_long_runs = FALSE)
writeLines(
  c(
    paste('commit', system('git rev-parse HEAD', intern = TRUE)),
    trimws(capture.output(sessionInfo()), which = 'right'),
    vapply(
      c('glmnet', 'mlpwr', 'DiceKriging', 'rgenoud'),
      function(p) paste(p, packageVersion(p)),
      ''
    )
  ),
  file.path(out, 'session-info.txt')
)
engine <- get('calculate_mlpwr', asNamespace('pmsims'))
adaptive <- get('calculate_adaptive_bounds', asNamespace('pmsims'))
run <- function() {
  captured <- bounds <- NULL
  # Passive model observer counts fitting failures, including finite fallbacks.
  errors <- character()
  testthat::local_mocked_bindings(
    calculate_mlpwr = function(...) {
      captured <<- list(args = list(...), rng = .Random.seed)
      saveRDS(captured, file.path(out, 'prepared.rds'))
      a <- captured$args
      # The administrative ceiling prevents accidentally allocating millions of rows.
      # It aborts the study case; it never substitutes a smaller dataset or an answer.
      original_data <- a$data_function
      a$data_function <- function(n) {
        if (n > 200000) {
          stop(structure(
            list(
              message = 'STUDY_LIMIT: requested training N exceeds 200000',
              call = NULL
            ),
            class = c('study_limit', 'condition')
          ))
        }
        original_data(n)
      }
      formals(a$data_function) <- formals(original_data)
      attributes(a$data_function) <- attributes(original_data)
      original_fit <- a$model_function
      a$model_function <- function(dat) {
        tryCatch(original_fit(dat), error = function(e) {
          errors <<- c(errors, conditionMessage(e))
          stop(e)
        })
      }
      attributes(a$model_function) <- attributes(original_fit)
      do.call(engine, a)
    },
    calculate_adaptive_bounds = function(...) {
      bounds <<- adaptive(...)
      saveRDS(bounds, file.path(out, 'stage1.rds'))
      bounds
    },
    .package = 'pmsims'
  )
  set.seed(seed)
  start <- proc.time()[['elapsed']]
  fun <- get(paste0('simulate_', s$outcome), asNamespace('pmsims'))
  result <- tryCatch(
    do.call(
      fun,
      c(
        s$inputs,
        list(
          model = s$model,
          noise_parameters = 0L,
          complexity = 1L,
          metric = 'calibration_slope',
          mean_or_assurance = 'assurance',
          n_reps_total = 1000L,
          progress = FALSE
        )
      )
    ),
    error = identity,
    study_limit = identity
  )
  ok <- !inherits(result, 'condition') &&
    is.numeric(result$min_n) &&
    length(result$min_n) == 1L &&
    is.finite(result$min_n)
  val <- function(name, default = NA_real_) {
    if (is.null(bounds) || is.null(bounds[[name]])) default else bounds[[name]]
  }
  row <- data.frame(
    id = s$id,
    slice = s$slice,
    outcome = s$outcome,
    model = s$model,
    p = s$inputs$signal_parameters,
    seed = seed,
    role = role,
    cache_key = s$cache_key,
    cached_n = s$cached_n,
    target = s$inputs$target_performance,
    status = if (ok) {
      'estimated'
    } else if (inherits(result, 'condition')) {
      conditionMessage(result)
    } else {
      'Package did not return a finite numeric sample size'
    },
    stage1_stop = val('stop_reason', NA_character_),
    lower = val('min_sample_size'),
    upper = val('max_sample_size'),
    lower_perf = val('min_sample_size_perf'),
    upper_perf = val('max_sample_size_perf'),
    stage1_reps = val('reps_used'),
    n = if (ok) result$min_n else NA_real_,
    predicted_slope = if (ok) result$perf_n else NA_real_,
    actual_gp_reps = if (ok) {
      sum(vapply(result$data, function(x) length(x$y), integer(1)))
    } else {
      0
    },
    search_fit_errors = length(errors),
    seconds = proc.time()[['elapsed']] - start
  )
  write.csv(row, file.path(out, 'search.csv'), row.names = FALSE)
  writeLines(errors, file.path(out, 'search-errors.txt'))
  if (!is.null(bounds)) {
    trace <- do.call(
      rbind,
      lapply(bounds$track, function(x) {
        data.frame(
          n = x$n,
          performance = x$performance,
          se = x$se,
          reps = x$reps,
          call = x$call,
          n_fail = x$n_fail
        )
      })
    )
    write.csv(trace, file.path(out, 'stage1.csv'), row.names = FALSE)
  }
  print(row)
  if (!ok) {
    return(invisible(NULL))
  }
  saveRDS(result, file.path(out, 'search.rds'))
  a <- captured$args
  # Three independent points: smaller N, reported N, upper bound. No answer correction.
  points <- c(
    half_n = max(1L, round(result$min_n / 2)),
    reported_n = round(result$min_n),
    upper_bound = round(bounds$max_sample_size)
  )
  # User focus amendment: future runs retain only the returned-N diagnostic.
  if (Sys.getenv('PMSIMS_STABILITY_FULL_CHECKS') != '1') {
    points <- points['reported_n']
  }
  checks <- list()
  for (label in names(points)) {
    j <- match(label, c('half_n', 'reported_n', 'upper_bound'))
    n <- points[[label]]
    file <- file.path(out, paste0(label, '-values.rds'))
    check_seed <- 10000000L +
      match(s$id, vapply(scenarios, `[[`, '', 'id')) * 100000L +
      seed * 10L +
      j
    check_errors <- character()
    started <- proc.time()[['elapsed']]
    if (file.exists(file)) {
      saved <- readRDS(file)
      vals <- saved$values
      check_errors <- saved$errors
    } else {
      set.seed(check_seed)
      vals <- vapply(
        seq_len(1000L),
        function(i) {
          v <- tryCatch(
            {
              test <- a$data_function(a$test_n)
              train <- a$data_function(n)
              fit <- a$model_function(train)
              a$metric_function(test, fit, attr(a$model_function, 'model'))
            },
            error = function(e) {
              check_errors <<- c(check_errors, conditionMessage(e))
              a$value_on_error
            }
          )
          if (i %% 100L == 0L) {
            message(
              s$id,
              ' ',
              role,
              ' ',
              label,
              ' N=',
              n,
              ' ',
              i,
              '/1000 elapsed ',
              round(proc.time()[['elapsed']] - started),
              's'
            )
          }
          v
        },
        numeric(1)
      )
      saveRDS(
        list(values = vals, errors = check_errors, n = n, seed = check_seed),
        file
      )
    }
    finite <- vals[is.finite(vals)]
    m <- length(finite)
    q <- as.numeric(quantile(finite, .2, type = 7))
    ordered <- sort(finite)
    lo <- max(1L, qbinom(.025, m, .2))
    hi <- min(m, qbinom(.975, m, .2) + 1L)
    ci <- ordered[c(lo, hi)]
    metric <- attr(a$metric_function, 'metric')
    display <- function(v) {
      if (identical(metric, 'csse')) 1 - sqrt(pmax(0, -v)) else v
    }
    target <- a$target_performance
    evidence <- if (ci[2] < target) {
      'below'
    } else if (ci[1] > target) {
      'above'
    } else {
      'overlaps'
    }
    checks[[j]] <- data.frame(
      id = s$id,
      seed = seed,
      role = role,
      label = label,
      n = n,
      reps = length(vals),
      finite = m,
      fit_errors = length(check_errors),
      metric = metric,
      internal_target = target,
      q20 = q,
      ci_low = ci[1],
      ci_high = ci[2],
      slope = display(q),
      slope_ci_low = display(ci[1]),
      slope_ci_high = display(ci[2]),
      evidence = evidence,
      validation_seed = check_seed,
      seconds = proc.time()[['elapsed']] - started
    )
    write.csv(
      do.call(rbind, checks),
      file.path(out, 'validation.csv'),
      row.names = FALSE
    )
    print(checks[[j]])
  }
}
run()
writeLines(format(Sys.time(), tz = 'UTC'), file.path(out, 'DONE'))
