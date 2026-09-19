#!/usr/bin/env Rscript
pkgload::load_all('.', quiet = TRUE)
root <- 'validation/stability-20260918'
adaptive <- get('calculate_adaptive_bounds', asNamespace('pmsims'))
# A deterministic below-target curve tests the contract, without Monte Carlo ambiguity.
flat <- adaptive(
  data_function = function(n) data.frame(y = rep(0, as.integer(n))),
  model_function = structure(function(d) list(n = nrow(d)), model = 'glm'),
  metric_function = function(test, fit, model) .8,
  value_on_error = 0,
  start_n = 10,
  test_n = 5000,
  n_reps_per = 20,
  n_reps_total = 500,
  target_performance = .95,
  threshold = .0001,
  mean_or_assurance = 'assurance',
  seed = 20240101L
)
monotone <- adaptive(
  data_function = function(n) list(n = n),
  model_function = structure(function(d) d, model = 'glm'),
  metric_function = function(test, fit, model) -.02 + .003 * log2(fit$n / 10),
  value_on_error = -1,
  start_n = 10,
  test_n = 30000,
  n_reps_per = 20,
  n_reps_total = 500,
  target_performance = -.0025,
  threshold = .0001,
  mean_or_assurance = 'assurance',
  seed = 20240101L
)
rows <- lapply(
  list(constant_below_target = flat, monotone_reachable_small_gains = monotone),
  function(x) {
    data.frame(
      stop_reason = x$stop_reason,
      lower = x$min_sample_size,
      upper = x$max_sample_size,
      upper_perf = x$max_sample_size_perf,
      observed_above = any(vapply(
        x$track,
        function(z) z$call == 'above',
        logical(1)
      )),
      reps_used = x$reps_used
    )
  }
)
z <- do.call(rbind, rows)
z$diagnostic <- rownames(z)
rownames(z) <- NULL
z$known_later_above_n <- c(NA_real_, 640)
z$known_later_performance <- c(NA_real_, -.02 + .003 * log2(640 / 10))
write.csv(z, file.path(root, 'review-diagnostics.csv'), row.names = FALSE)
saveRDS(monotone, file.path(root, 'monotone-plateau-diagnostic.rds'))
saveRDS(flat, file.path(root, 'review-diagnostics.rds'))
# Compare caller-stream preservation including the no-existing-seed case.
set.seed(812)
before <- .Random.seed
invisible(adaptive(
  data_function = function(n) data.frame(y = seq_len(n)),
  model_function = structure(function(d) list(n = nrow(d)), model = 'glm'),
  metric_function = function(test, fit, model) fit$n / 100,
  value_on_error = 0,
  start_n = 20,
  test_n = 5000,
  n_reps_per = 20,
  n_reps_total = 100,
  target_performance = .6,
  threshold = 0,
  mean_or_assurance = 'mean',
  seed = 20240101L
))
restored <- identical(before, .Random.seed)
rm('.Random.seed', envir = globalenv())
invisible(adaptive(
  data_function = function(n) data.frame(y = seq_len(n)),
  model_function = structure(function(d) list(n = nrow(d)), model = 'glm'),
  metric_function = function(test, fit, model) fit$n / 100,
  value_on_error = 0,
  start_n = 20,
  test_n = 5000,
  n_reps_per = 20,
  n_reps_total = 100,
  target_performance = .6,
  threshold = 0,
  mean_or_assurance = 'mean',
  seed = 20240101L
))
write.csv(
  data.frame(
    existing_stream_restored = restored,
    absent_stream_restored = !exists(
      '.Random.seed',
      envir = globalenv(),
      inherits = FALSE
    )
  ),
  file.path(root, 'rng-diagnostics.csv'),
  row.names = FALSE
)
