#!/usr/bin/env Rscript
# Confirm the final noisy-plateau guard preserves the ordinary study bounds.
# Replays recorded replicate metrics; no models or new random draws are used.
pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
paths <- if (length(args)) {
  args
} else {
  Sys.glob("validation/calibration-slope-search/results/*-fixed-stage1.rds")
}
rows <- lapply(paths, function(path) {
  original <- readRDS(path)
  track <- original$bounds$track
  used <- integer(length(track))
  curve <- function(n) {
    idx <- which(vapply(track, function(x) x$n == n, logical(1)))
    if (length(idx) != 1L || used[idx] >= length(track[[idx]]$raw)) {
      stop("Trace exhausted")
    }
    used[idx] <<- used[idx] + 1L
    track[[idx]]$raw[used[idx]]
  }
  bounds <- calculate_adaptive_bounds(
    function(n) list(n = n),
    function(data) data,
    function(data, fit, model) curve(fit$n),
    -Inf,
    start_n = track[[1L]]$n,
    test_n = 1L,
    n_reps_per = 20L,
    n_reps_total = 500L,
    target_performance = original$internal_target,
    mean_or_assurance = "assurance"
  )
  same <- identical(bounds$stop_reason, original$bounds$stop_reason) &&
    isTRUE(all.equal(
      bounds$min_sample_size,
      original$bounds$min_sample_size
    )) &&
    isTRUE(all.equal(bounds$max_sample_size, original$bounds$max_sample_size))
  same_order <- identical(
    vapply(track, `[[`, numeric(1), "n"),
    vapply(bounds$track, `[[`, numeric(1), "n")
  )
  same_batches <- identical(bounds$iterations, original$bounds$iterations)
  data.frame(
    trace = basename(path),
    same_bounds_and_stop_reason = same,
    same_evaluation_order = same_order,
    same_batches = same_batches,
    original_reason = original$bounds$stop_reason,
    replay_reason = bounds$stop_reason,
    replay_lower = bounds$min_sample_size,
    replay_upper = bounds$max_sample_size
  )
})
results <- do.call(rbind, rows)
write.csv(
  results,
  "validation/calibration-slope-search/results/replay-results.csv",
  row.names = FALSE
)
print(results)
stopifnot(
  all(results$same_bounds_and_stop_reason),
  all(results$same_evaluation_order),
  all(results$same_batches)
)
