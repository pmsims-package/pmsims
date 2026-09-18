source('validation/predictor-count-spikes/common.R')
cli_args <- commandArgs(trailingOnly = TRUE)
id <- cli_args[1]
mode <- cli_args[2]
stopifnot(mode %in% c('baseline', 'common-bounds'))
prepared <- readRDS(raw_path(paste0(id, '-prepared.rds')))
args <- prepared$args
bounds <- NULL
engine <- get('calculate_mlpwr', asNamespace('pmsims'))
adaptive <- get('calculate_adaptive_bounds', asNamespace('pmsims'))
find_design <- get('find.design', asNamespace('mlpwr'))
gp_rng <- NULL
fixed_test <- NULL
run <- function() {
  if (mode == 'common-bounds') {
    finished_file <- result_path(paste0(id, '-common-bounds-search.csv'))
    search_lock <- raw_path(paste0(id, '-common-bounds-search.lock'))
    repeat {
      if (file.exists(finished_file)) {
        return(invisible(NULL))
      }
      if (dir.create(search_lock, showWarnings = FALSE)) {
        writeLines(
          as.character(Sys.getpid()),
          file.path(search_lock, 'owner.pid')
        )
        break
      }
      owner_file <- file.path(search_lock, 'owner.pid')
      if (file.exists(owner_file)) {
        owner <- suppressWarnings(as.integer(readLines(
          owner_file,
          warn = FALSE
        )[1]))
        alive <- tryCatch(
          tools::pskill(owner, signal = 0L),
          error = function(e) TRUE
        )
        if (isFALSE(alive)) unlink(search_lock, recursive = TRUE)
      }
      Sys.sleep(1)
    }
    on.exit(unlink(search_lock, recursive = TRUE), add = TRUE)
  }
  testthat::local_mocked_bindings(
    calculate_adaptive_bounds = function(...) {
      adargs <- list(...)
      data_fn <- adargs$data_function
      first <- TRUE
      # Record the one original fixed test draw without consuming extra RNG.
      adargs$data_function <- function(n) {
        d <- data_fn(n)
        if (first) {
          first <<- FALSE
          fixed_test <<- d
        }
        d
      }
      adargs$verbose <- TRUE # Messages only; search decisions and RNG unchanged.
      bounds <<- do.call(adaptive, adargs)
      saveRDS(bounds, raw_path(paste0(id, '-stage1.rds')))
      saveRDS(fixed_test, raw_path(paste0(id, '-fixed-test.rds')))
      trace <- do.call(
        rbind,
        lapply(bounds$track, function(x) {
          data.frame(
            n = x$n,
            reps = length(x$raw),
            internal_q20 = x$performance,
            equivalent_slope = display_slope(x$performance)
          )
        })
      )
      write.csv(
        trace,
        result_path(paste0(id, '-stage1.csv')),
        row.names = FALSE
      )
      bounds
    },
    .package = 'pmsims'
  )
  testthat::local_mocked_bindings(
    find.design = function(...) {
      gp_rng <<- .Random.seed
      message('GP entered; ', id, ' ', mode)
      result <- find_design(...)
      message('GP finished; ', id, ' ', mode)
      result
    },
    .package = 'mlpwr'
  )
  if (mode == 'baseline') {
    assign('.Random.seed', prepared$engine_rng, envir = .GlobalEnv)
  } else {
    baseline <- readRDS(raw_path(paste0(id, '-baseline.rds')))
    shared <- read.csv(result_path('common-bounds.csv'))
    args$min_sample_size <- as.numeric(shared$lower[1])
    args$max_sample_size <- as.numeric(shared$upper[1])
    assign('.Random.seed', baseline$gp_rng, envir = .GlobalEnv)
  }
  start <- proc.time()[['elapsed']]
  result <- tryCatch(do.call(engine, args), error = identity)
  seconds <- proc.time()[['elapsed']] - start
  ok <- !inherits(result, 'error')
  if (mode == 'common-bounds') {
    bounds <- list(
      min_sample_size = args$min_sample_size,
      max_sample_size = args$max_sample_size,
      stop_reason = 'bypassed',
      track = list()
    )
  }
  payload <- list(
    result = result,
    gp_rng = gp_rng,
    bounds = bounds,
    seconds = seconds
  )
  saveRDS(payload, raw_path(paste0(id, '-', mode, '.rds')))
  row <- data.frame(
    id = id,
    mode = mode,
    status = if (ok) 'estimated' else conditionMessage(result),
    lower = if (is.null(bounds)) NA_real_ else bounds$min_sample_size,
    upper = if (is.null(bounds)) NA_real_ else bounds$max_sample_size,
    stage1_stop = if (is.null(bounds)) NA_character_ else bounds$stop_reason,
    stage1_reps = if (is.null(bounds)) {
      NA_integer_
    } else {
      sum(vapply(bounds$track, function(x) length(x$raw), integer(1)))
    },
    n = if (ok) result$min_n else NA_real_,
    predicted_equivalent_slope = if (ok) {
      display_slope(result$perf_n)
    } else {
      NA_real_
    },
    gp_reps = if (ok) {
      sum(vapply(result$results, function(x) length(x$y), integer(1)))
    } else {
      NA_integer_
    },
    at_lower = if (ok) result$min_n == bounds$min_sample_size else NA,
    at_upper = if (ok) result$min_n == bounds$max_sample_size else NA,
    seconds = seconds
  )
  write.csv(
    row,
    result_path(paste0(id, '-', mode, '-search.csv')),
    row.names = FALSE
  )
  if (ok) {
    trace <- do.call(
      rbind,
      lapply(result$results, function(x) {
        q <- unname(quantile(x$y, .2, type = 7))
        data.frame(
          n = x$x,
          reps = length(x$y),
          internal_q20 = q,
          equivalent_slope = display_slope(q)
        )
      })
    )
    write.csv(
      trace,
      result_path(paste0(id, '-', mode, '-gp.csv')),
      row.names = FALSE
    )
  }
  if (!is.null(fixed_test)) {
    pars <- formals(args$data_function)
    lp <- pars$mu_lp + pars$beta_signal * rowSums(as.matrix(fixed_test[, -1]))
    slope <- unname(coef(glm(fixed_test$y ~ lp, family = binomial()))[2])
    write.csv(
      data.frame(
        id = id,
        true_lp_test_slope = slope,
        true_lp_test_csse = -(1 - slope)^2
      ),
      result_path(paste0(id, '-fixed-test-oracle.csv')),
      row.names = FALSE
    )
  }
  print(row)
  if (!ok) quit(status = 1L)
}
run()
