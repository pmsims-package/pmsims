# =============================================================================
# Simulation core: one evaluation function and keyed random-number streams
#
# Every stage of the search (the adaptive start-value search, the mlpwr and
# bisection searches, the final verification and the secondary metric) obtains
# replicate performance values from the same evaluator, so they all estimate
# the same quantity: a model trained on n fresh rows, scored on a fresh test
# set. Before this, the adaptive stage scored every replicate against a single
# test set while the Gaussian-process stage drew a new one per replicate, so a
# bracket found by the first stage need not contain the answer sought by the
# second.
#
# Random numbers. Each replicate runs on its own L'Ecuyer-CMRG stream, keyed by
# (base seed, purpose, n, replicate index). The base seed is drawn once from
# the caller's RNG, so set.seed() still controls the whole run. Because a
# replicate's data no longer depend on how many random numbers were consumed
# before it -- by other replicates, by mlpwr's surrogate fits and optimiser, or
# by any added check -- a change to one part of the search no longer reshuffles
# the data seen by every other part. The replicates themselves never touch the
# caller's RNG; the adaptive stage's bootstrap standard errors and mlpwr's
# surrogate do, but they run in the main process, so results are still
# reproducible and the same for any number of cores.
# =============================================================================

# Purposes have separate streams, so that, for example, the verification
# replicates at n are independent of the search replicates at the same n.
stream_purposes <- c(
  adaptive = 1L,
  search = 2L,
  verify = 3L,
  secondary = 4L,
  bootstrap = 5L
)

#' Create the keyed random-number streams for one run
#'
#' @param base_seed Optional integer. When `NULL`, one integer is drawn from
#'   the caller's RNG.
#' @return An environment holding the base seed and the stream cache.
#' @keywords internal
#' @noRd
new_simulation_streams <- function(base_seed = NULL) {
  if (is.null(base_seed)) {
    base_seed <- sample.int(.Machine$integer.max, 1L)
  }
  streams <- new.env(parent = emptyenv())
  streams$base_seed <- as.integer(base_seed)
  streams$cache <- list()
  streams
}

# Integer seed for the (purpose, n) stream family. Arithmetic is done modulo
# the prime 2^31 - 1 with a multiplier below 2^16, so every intermediate value
# is an exactly representable double.
stream_family_seed <- function(base_seed, purpose_code, n) {
  p <- 2147483647
  a <- 48271
  n <- round(n)
  h <- as.numeric(base_seed) %% p
  for (v in c(purpose_code, n %/% a, n %% a)) {
    h <- (h * a + v) %% p
  }
  as.integer(h)
}

lecuyer_state <- function(seed) {
  with_rng_restored({
    set.seed(
      seed,
      kind = "L'Ecuyer-CMRG",
      normal.kind = "Inversion",
      sample.kind = "Rejection"
    )
    get(".Random.seed", envir = globalenv())
  })
}

# Evaluate `expr`, then put the global RNG back exactly as it was. When a
# `state` is given, `expr` runs on that RNG state.
with_rng_restored <- function(expr, state = NULL) {
  genv <- globalenv()
  had_seed <- exists(".Random.seed", envir = genv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = genv)
  old_kind <- RNGkind()
  on.exit(
    {
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = genv)
      } else {
        RNGkind(old_kind[1], old_kind[2], old_kind[3])
        if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
          rm(".Random.seed", envir = genv)
        }
      }
    },
    add = TRUE
  )
  if (!is.null(state)) {
    assign(".Random.seed", state, envir = genv)
  }
  expr
}

#' RNG state for replicate `r` at sample size `n`
#'
#' Replicate r is r - 1 applications of [parallel::nextRNGStream()] from the
#' (purpose, n) family seed, so streams within a family never overlap. The
#' latest state per family is cached, so drawing replicates in order costs one
#' stream jump each.
#' @keywords internal
#' @noRd
stream_state <- function(streams, purpose, n, r) {
  code <- stream_purposes[[purpose]]
  key <- paste(code, format(round(n), scientific = FALSE), sep = ":")
  entry <- streams$cache[[key]]
  if (is.null(entry) || entry$index > r) {
    entry <- list(
      state = lecuyer_state(stream_family_seed(streams$base_seed, code, n)),
      index = 1L
    )
  }
  while (entry$index < r) {
    entry$state <- parallel::nextRNGStream(entry$state)
    entry$index <- entry$index + 1L
  }
  streams$cache[[key]] <- entry
  entry$state
}

#' Run `expr` on a keyed stream
#' @keywords internal
#' @noRd
with_stream <- function(streams, purpose, n, r, expr) {
  with_rng_restored(expr, state = stream_state(streams, purpose, n, r))
}

#' Create the evaluator shared by every stage of the search
#'
#' @param data_function,model_function,metric_function As in
#'   [simulate_custom()].
#' @param test_n Size of the test set drawn for every replicate.
#' @param value_on_error Value recorded for a replicate whose fit or metric
#'   fails or is not a single finite number.
#' @param streams Streams from [new_simulation_streams()].
#' @param parallel,cores Run the replicates of a batch with
#'   [parallel::mclapply()]. Results do not depend on the number of cores.
#'
#' @return A list of functions: `batch(n, reps, purpose)` returns `reps` new
#'   replicate values at `n`; `one(n, purpose)` returns one;
#'   `failures()` summarises failed replicates and warnings by purpose and n.
#' @keywords internal
#' @noRd
new_evaluator <- function(
  data_function,
  model_function,
  metric_function,
  test_n,
  value_on_error,
  streams = new_simulation_streams(),
  parallel = FALSE,
  cores = 1L
) {
  model <- attr(model_function, "model", exact = TRUE)
  counters <- new.env(parent = emptyenv())
  log <- new.env(parent = emptyenv())
  log$rows <- list()

  run_replicate <- function(n) {
    n_warnings <- 0L
    error <- NA_character_
    value <- withCallingHandlers(
      tryCatch(
        {
          test_data <- data_function(test_n)
          train_data <- data_function(n)
          fit <- model_function(train_data)
          metric_function(test_data, fit, model)
        },
        error = function(e) {
          error <<- conditionMessage(e)
          NA_real_
        }
      ),
      # Fitting warnings (separation, convergence) are expected across
      # thousands of replicates; they are counted rather than shown.
      warning = function(w) {
        n_warnings <<- n_warnings + 1L
        invokeRestart("muffleWarning")
      }
    )
    ok <- is.numeric(value) && length(value) == 1L && is.finite(value)
    list(
      value = if (ok) as.numeric(value) else value_on_error,
      failed = !ok,
      error = error,
      warnings = n_warnings
    )
  }

  next_indices <- function(purpose, n, reps) {
    key <- paste(purpose, format(round(n), scientific = FALSE))
    used <- counters[[key]] %||% 0L
    counters[[key]] <- used + reps
    used + seq_len(reps)
  }

  batch <- function(n, reps, purpose = "search") {
    n <- round(as.numeric(n))
    reps <- as.integer(reps)
    if (reps < 1L) {
      return(numeric(0))
    }
    idx <- next_indices(purpose, n, reps)
    # States are computed here, before any forking, so the stream cache is
    # updated in this process.
    states <- lapply(idx, function(r) stream_state(streams, purpose, n, r))
    one_rep <- function(state) {
      with_rng_restored(run_replicate(n), state = state)
    }
    res <- if (isTRUE(parallel) && cores > 1L && .Platform$OS.type == "unix") {
      # A worker that dies returns a try-error (or NULL) for its replicates:
      # count them as failed rather than abort the run.
      lapply(
        parallel::mclapply(states, one_rep, mc.cores = cores),
        function(r) {
          if (is.list(r)) {
            return(r)
          }
          list(
            value = value_on_error,
            failed = TRUE,
            error = if (inherits(r, "try-error")) {
              trimws(as.character(r))
            } else {
              "the parallel worker returned no result"
            },
            warnings = 0L
          )
        }
      )
    } else {
      lapply(states, one_rep)
    }
    failed <- vapply(res, `[[`, logical(1), "failed")
    errors <- vapply(res, `[[`, character(1), "error")
    log$rows[[length(log$rows) + 1L]] <- data.frame(
      purpose = purpose,
      n = n,
      reps = reps,
      failed = sum(failed),
      warnings = sum(vapply(res, `[[`, integer(1), "warnings")),
      first_error = if (any(!is.na(errors))) {
        errors[!is.na(errors)][1]
      } else {
        NA_character_
      },
      stringsAsFactors = FALSE
    )
    values <- vapply(res, `[[`, numeric(1), "value")
    # Which replicates failed (their value is the fallback), for callers that
    # need to tell a failure from a legitimate value equal to the fallback.
    attr(values, "failed") <- failed
    values
  }

  failures <- function() {
    if (!length(log$rows)) {
      return(data.frame(
        purpose = character(0),
        n = numeric(0),
        reps = integer(0),
        failed = integer(0),
        warnings = integer(0),
        first_error = character(0),
        stringsAsFactors = FALSE
      ))
    }
    rows <- do.call(rbind, log$rows)
    keys <- paste(rows$purpose, rows$n)
    out <- do.call(
      rbind,
      lapply(split(rows, factor(keys, unique(keys))), function(d) {
        data.frame(
          purpose = d$purpose[1],
          n = d$n[1],
          reps = sum(d$reps),
          failed = sum(d$failed),
          warnings = sum(d$warnings),
          first_error = if (any(!is.na(d$first_error))) {
            d$first_error[!is.na(d$first_error)][1]
          } else {
            NA_character_
          },
          stringsAsFactors = FALSE
        )
      })
    )
    rownames(out) <- NULL
    out
  }

  list(
    batch = batch,
    one = function(n, purpose = "search") batch(n, 1L, purpose),
    failures = failures,
    streams = streams,
    test_n = test_n,
    value_on_error = value_on_error
  )
}

# The 20th percentile of replicate values (assurance). The mlpwr and bisection
# engines use R's default estimator (type 7); the curve engine uses the
# approximately median-unbiased one (type 8), see calculate_curve().
quantile_20 <- function(x, type = 7L) {
  as.numeric(stats::quantile(x, probs = 0.2, type = type, na.rm = TRUE))
}

# The criterion the search targets: the mean, or the 20th percentile
# (assurance) of the replicate values.
criterion_function <- function(mean_or_assurance, type = 7L) {
  if (identical(mean_or_assurance, "mean")) {
    function(x) mean(x, na.rm = TRUE)
  } else {
    function(x) quantile_20(x, type)
  }
}

#' Check the returned sample size by simulating it again
#'
#' Draws `reps` fresh replicates at `n` (on their own streams) and compares the
#' observed criterion with the target. The result is "not verified" only when
#' the observed value is clearly below the target -- more than two bootstrap
#' standard errors -- so a correct answer, whose true criterion sits at the
#' target, passes about 97% of the time. `type` is the quantile estimator for
#' assurance: 8 for the curve engine, which fits that estimator, else 7.
#' @keywords internal
#' @noRd
verify_sample_size <- function(
  evaluator,
  n,
  target_performance,
  mean_or_assurance,
  reps = 100L,
  type = 7L
) {
  crit <- criterion_function(mean_or_assurance, type)
  vals <- evaluator$batch(n, reps, "verify")
  est <- crit(vals)
  se <- with_stream(evaluator$streams, "bootstrap", n, 1L, {
    stats::sd(replicate(200L, crit(sample(vals, length(vals), replace = TRUE))))
  })
  list(
    n = n,
    reps = reps,
    performance = est,
    se = se,
    verified = is.finite(est) && (est + 2 * se >= target_performance)
  )
}

# Metric names for which a smaller value is better. The search engines assume
# larger is better (mlpwr's goodvals = "high", and assurance as the 20th
# percentile), so these cannot be searched correctly yet.
lower_is_better_metrics <- c("calibration_in_the_large", "brier_score", "ibs")

check_metric_direction <- function(metric) {
  if (
    length(metric) == 1L &&
      !is.na(metric) &&
      metric %in% lower_is_better_metrics
  ) {
    stop(
      sprintf(
        paste(
          "Metric '%s' is better when smaller, but the sample-size search",
          "assumes larger is better, so it would return a wrong answer.",
          "This metric is not supported as a search target yet."
        ),
        metric
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Number of threads for ranger fits and predictions: the `pmsims.threads`
# option if set, otherwise the physical cores less two, and never below one.
# Status when at least half the replicates failed to fit or score, else NULL.
# Failed replicates are replaced by value_on_error, so the search would be
# working on that fallback rather than on the model's performance.
failure_status <- function(evaluator) {
  f <- evaluator$failures()
  total <- sum(f$reps)
  failed <- sum(f$failed)
  if (!total || failed / total < 0.5) {
    return(NULL)
  }
  err <- f$first_error[!is.na(f$first_error)][1]
  list(
    status = "replicates_failed",
    message = sprintf(
      "%d of %d simulation replicates failed to fit or score the model%s, so no sample size can be estimated.",
      failed,
      total,
      if (is.na(err)) "" else paste0(" (first error: ", err, ")")
    )
  )
}

# How to show a value of the searched metric in messages. Searches run on the
# CSSE scale for a calibration slope target describe values as slopes (see
# describe_as_calibration_slope()).
describe_value <- function(metric_function) {
  attr(metric_function, "describe", exact = TRUE) %||%
    function(x) format(signif(x, 4))
}

pmsims_threads <- function() {
  n <- getOption("pmsims.threads")
  if (is.null(n)) {
    cores <- parallel::detectCores(logical = FALSE)
    n <- if (is.na(cores)) 1L else cores - 2L
  }
  max(1L, as.integer(n))
}
