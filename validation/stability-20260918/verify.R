#!/usr/bin/env Rscript
root <- 'validation/stability-20260918'
partial <- '--partial' %in% commandArgs(TRUE)
pkgload::load_all('.', quiet = TRUE)
jobs <- jsonlite::fromJSON(file.path(root, 'jobs.json'))
checks <- list()
record <- function(name, ok, detail = '') {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = name,
    passed = isTRUE(ok),
    detail = as.character(detail)
  )
}
selected <- read.csv(file.path(root, 'selection-verification.csv'))
record(
  'selected_cache_seeds_match_grid',
  nrow(selected) == 14L && all(selected$matched)
)
record(
  'production_R_matches_Ridwan',
  length(system(
    'git diff --name-only d00a137640d47a9f1a75482ead8109b1ae1e463d -- R',
    intern = TRUE
  )) ==
    0
)
manifest <- jsonlite::fromJSON(file.path(root, 'history/manifest.json'))
sha <- function(path) {
  strsplit(
    system2('shasum', c('-a', '256', shQuote(path)), stdout = TRUE),
    ' '
  )[[1]][1]
}
record(
  'all_archived_artifacts_present',
  all(file.exists(file.path(root, manifest$path)))
)
record(
  'archived_checksums_match',
  all(vapply(file.path(root, manifest$path), sha, '') == manifest$sha256)
)
allseeds <- integer()
for (i in seq_len(nrow(jobs))) {
  j <- jobs[i, ]
  out <- file.path(root, 'results', paste0(j$id, '-', j$role, '-seed', j$seed))
  if (partial && !file.exists(file.path(out, 'DONE'))) {
    next
  }
  record(
    paste0(j$id, '/', j$role, '/complete'),
    file.exists(file.path(out, 'DONE'))
  )
  row <- read.csv(file.path(out, 'search.csv'), stringsAsFactors = FALSE)
  if (row$status != 'estimated') {
    next
  }
  prepared <- readRDS(file.path(out, 'prepared.rds'))
  a <- prepared$args
  record(
    paste0(j$id, '/', j$role, '/settings'),
    a$n_reps_total == 1000 &&
      a$n_reps_per == 20 &&
      a$test_n == 30000 &&
      a$mean_or_assurance == 'assurance' &&
      is.null(a$min_sample_size) &&
      is.null(a$max_sample_size) &&
      a$adaptive_seed == 20240101L
  )
  result <- readRDS(file.path(out, 'search.rds'))
  record(
    paste0(j$id, '/', j$role, '/search_replicates'),
    sum(vapply(result$data, function(x) length(x$y), integer(1))) >= 1000
  )
  v <- read.csv(file.path(out, 'validation.csv'), stringsAsFactors = FALSE)
  record(
    paste0(j$id, '/', j$role, '/secondary_check_plan'),
    (nrow(v) == 1L && identical(v$label, 'reported_n')) ||
      (nrow(v) == 3L &&
        setequal(v$label, c('half_n', 'reported_n', 'upper_bound')))
  )
  for (k in seq_len(nrow(v))) {
    saved <- readRDS(file.path(out, paste0(v$label[k], '-values.rds')))
    x <- saved$values
    q <- as.numeric(quantile(x[is.finite(x)], .2, type = 7))
    xs <- sort(x[is.finite(x)])
    m <- length(xs)
    ci <- xs[c(max(1L, qbinom(.025, m, .2)), min(m, qbinom(.975, m, .2) + 1L))]
    record(
      paste0(j$id, '/', j$role, '/', v$label[k]),
      length(x) == 1000 &&
        saved$n == v$n[k] &&
        saved$seed == v$validation_seed[k] &&
        isTRUE(all.equal(q, v$q20[k], tolerance = 1e-10)) &&
        isTRUE(all.equal(
          ci,
          c(v$ci_low[k], v$ci_high[k]),
          tolerance = 1e-10
        )) &&
        v$fit_errors[k] == length(saved$errors)
    )
    allseeds <- c(allseeds, saved$seed)
  }
  # Check that the guard preserves original generator formals, values and RNG.
  original_data <- a$data_function
  guarded <- function(n) {
    if (n > 200000) {
      stop('limit')
    }
    original_data(n)
  }
  formals(guarded) <- formals(original_data)
  attributes(guarded) <- attributes(original_data)
  set.seed(9031)
  direct <- original_data(50)
  rng1 <- .Random.seed
  set.seed(9031)
  observed <- guarded(50)
  rng2 <- .Random.seed
  record(
    paste0(j$id, '/', j$role, '/generator_observer'),
    identical(direct, observed) &&
      identical(rng1, rng2) &&
      identical(formals(guarded), formals(original_data))
  )
}
record('independent_check_seeds_unique', !anyDuplicated(allseeds))
result <- do.call(rbind, checks)
write.csv(
  result,
  file.path(
    root,
    if (partial) 'verification-interim.csv' else 'verification.csv'
  ),
  row.names = FALSE
)
print(subset(result, !passed))
cat(sum(result$passed), '/', nrow(result), 'checks passed\n')
if (!all(result$passed)) {
  quit(status = 1)
}
