source('validation/predictor-count-spikes/common.R')
cache_file <- '../pmsims-chatbot/cache/versions/cache-1f-create-20260827-161720.rds'
cache <- readRDS(cache_file)$binary
source('../pmsims-chatbot/cache/scenarios_grid.R')
grid <- get_grid('1f')$binary
rows <- subset(
  grid,
  signal_parameters %in%
    c(10, 15, 20) &
    outcome_prevalence == .25 &
    abs(maximum_achievable_cstatistic - .85) < 1e-8 &
    target_performance == .90 &
    model == 'ridge'
)
rows$cache_seed <- as.integer(rownames(rows))
# Tibbles do not preserve subset row indices: derive seeds from the full grid.
rows$cache_seed <- which(
  grid$signal_parameters %in%
    c(10, 15, 20) &
    grid$outcome_prevalence == .25 &
    abs(grid$maximum_achievable_cstatistic - .85) < 1e-8 &
    grid$target_performance == .90 &
    grid$model == 'ridge'
)
keys <- sprintf(
  'p%d_prev0.25_cstat0.85_target0.90_mridge',
  rows$signal_parameters
)
stopifnot(identical(as.integer(match(keys, names(cache))), rows$cache_seed))
rows$key <- keys
rows$cached_n <- vapply(
  cache[keys],
  function(x) x$minimum_sample_size,
  numeric(1)
)
rows$cached_equivalent_slope <- vapply(
  cache[keys],
  function(x) x$calibration_slope_at_n,
  numeric(1)
)
write.csv(rows, result_path('cache-peak.csv'), row.names = FALSE)
cases <- do.call(
  rbind,
  lapply(seq_len(nrow(rows)), function(i) {
    data.frame(
      p = rows$signal_parameters[i],
      seed = c(rows$cache_seed[i], 48L),
      seed_role = c('cache', 'shared'),
      stringsAsFactors = FALSE
    )
  })
)
cases$id <- paste0('p', cases$p, '-seed', cases$seed)
write.csv(cases, result_path('cases.csv'), row.names = FALSE)
capture <- function(p, seed) {
  captured <- NULL
  testthat::local_mocked_bindings(
    calculate_mlpwr = function(...) {
      captured <<- list(args = list(...), engine_rng = .Random.seed)
      stop('Captured before search', call. = FALSE)
    },
    .package = 'pmsims'
  )
  set.seed(seed)
  result <- tryCatch(
    simulate_binary(
      signal_parameters = p,
      noise_parameters = 0,
      complexity = 1,
      outcome_prevalence = .25,
      maximum_achievable_cstatistic = .85,
      model = 'ridge',
      metric = 'calibration_slope',
      target_performance = .90,
      mean_or_assurance = 'assurance',
      n_reps_total = 1000,
      progress = FALSE
    ),
    error = identity
  )
  stopifnot(
    inherits(result, 'error'),
    conditionMessage(result) == 'Captured before search'
  )
  captured
}
for (i in seq_len(nrow(cases))) {
  file <- raw_path(paste0(cases$id[i], '-prepared.rds'))
  if (!file.exists(file)) saveRDS(capture(cases$p[i], cases$seed[i]), file)
}
writeLines(
  c(
    paste('Base commit:', system('git rev-parse HEAD', intern = TRUE)),
    paste('Cache MD5:', tools::md5sum(cache_file)),
    'Search and independent checks: 1000 reps; original adaptive budget 500, batches 20.',
    'Original assurance, internal CSSE scale, test_n=30000. No production-code changes.',
    trimws(capture.output(sessionInfo()), which = 'right'),
    '',
    'Explicit fitting dependencies:',
    vapply(
      c('glmnet', 'mlpwr', 'DiceKriging', 'rgenoud'),
      function(n) {
        paste(n, as.character(packageVersion(n)))
      },
      character(1)
    )
  ),
  file.path(study_root, 'session-info.txt')
)
# Small timing probe only; never used for performance inference.
a <- readRDS(raw_path('p15-seed1725-prepared.rds'))$args
set.seed(9000001)
for (n in c(1000L, 4000L, 10672L)) {
  start <- proc.time()[['elapsed']]
  for (i in 1:3) {
    test <- a$data_function(a$test_n)
    train <- a$data_function(n)
    fit <- a$model_function(train)
    a$metric_function(test, fit, 'ridge')
  }
  message(
    'Timing only: N=',
    n,
    ' seconds/fit=',
    round((proc.time()[['elapsed']] - start) / 3, 3)
  )
}
print(rows)
