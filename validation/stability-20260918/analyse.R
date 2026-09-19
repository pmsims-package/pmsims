#!/usr/bin/env Rscript
root <- 'validation/stability-20260918'
files <- list.files(
  file.path(root, 'results'),
  pattern = 'search.csv$',
  recursive = TRUE,
  full.names = TRUE
)
files <- files[file.exists(file.path(dirname(files), 'DONE'))]
stopifnot(length(files) > 0)
search <- do.call(
  rbind,
  lapply(files, function(f) {
    r <- read.csv(f, stringsAsFactors = FALSE)
    obj <- sub('search.csv$', 'search.rds', f)
    if (file.exists(obj)) {
      x <- readRDS(obj)
      r$actual_gp_reps <- sum(vapply(
        x$data,
        function(z) length(z$y),
        integer(1)
      ))
    }
    write.csv(r, f, row.names = FALSE)
    r
  })
)
checks_files <- list.files(
  file.path(root, 'results'),
  pattern = 'validation.csv$',
  recursive = TRUE,
  full.names = TRUE
)
checks_files <- checks_files[file.exists(file.path(
  dirname(checks_files),
  'DONE'
))]
checks <- do.call(
  rbind,
  lapply(checks_files, read.csv, stringsAsFactors = FALSE)
)
write.csv(search, file.path(root, 'search-summary.csv'), row.names = FALSE)
write.csv(checks, file.path(root, 'validation-summary.csv'), row.names = FALSE)
scenarios <- jsonlite::fromJSON(
  file.path(root, 'scenarios.json'),
  simplifyVector = FALSE
)
pairs <- list()
for (role in c('cache', 'shared')) {
  for (sl in unique(search$slice)) {
    z <- search[search$role == role & search$slice == sl, ]
    z <- z[order(z$p), ]
    if (nrow(z) < 2L) {
      next
    }
    for (i in seq_len(nrow(z) - 1L)) {
      a <- z[i, ]
      b <- z[i + 1L, ]
      old <- max(a$cached_n, b$cached_n) / min(a$cached_n, b$cached_n)
      new <- if (all(c(a$status, b$status) == 'estimated')) {
        max(a$n, b$n) / min(a$n, b$n)
      } else {
        NA_real_
      }
      pairs[[length(pairs) + 1L]] <- data.frame(
        slice = sl,
        role = role,
        p_from = a$p,
        p_to = b$p,
        cached_from = a$cached_n,
        cached_to = b$cached_n,
        cached_ratio = old,
        new_from = a$n,
        new_to = b$n,
        new_ratio = new,
        historical_jump = old >= 3,
        new_jump = if (is.finite(new)) new >= 3 else NA,
        smoothed = if (is.finite(new)) old >= 3 & new < 3 else NA
      )
    }
  }
}
pairs <- do.call(rbind, pairs)
write.csv(pairs, file.path(root, 'adjacent-comparisons.csv'), row.names = FALSE)
# Compare literal interior positions; absent/limited neighbours are unavailable.
peaks <- list()
for (sl in unique(vapply(scenarios, `[[`, '', 'slice'))) {
  base <- scenarios[vapply(scenarios, function(x) x$slice == sl, FALSE)]
  base <- base[order(vapply(base, function(x) x$inputs$signal_parameters, 0))]
  if (length(base) < 3L) {
    next
  }
  for (i in 2:(length(base) - 1L)) {
    trio <- base[(i - 1L):(i + 1L)]
    old <- vapply(trio, `[[`, 0, 'cached_n')
    deviation <- function(x) {
      max(x[2] / mean(x[c(1, 3)]), mean(x[c(1, 3)]) / x[2])
    }
    agreement <- function(x) max(x[c(1, 3)]) / min(x[c(1, 3)])
    isolated <- function(x) deviation(x) >= 3 && agreement(x) <= 1.6
    for (role in c('cache', 'shared')) {
      candidates <- search[search$role == role, ]
      rows <- candidates[match(vapply(trio, `[[`, '', 'id'), candidates$id), ]
      ready <- all(!is.na(rows$status) & rows$status == 'estimated')
      new <- if (ready) rows$n else rep(NA_real_, 3)
      peaks[[length(peaks) + 1L]] <- data.frame(
        slice = sl,
        role = role,
        p = trio[[2]]$inputs$signal_parameters,
        historical_kind = if (old[2] > mean(old[c(1, 3)])) 'peak' else 'dip',
        cached_left = old[1],
        cached_centre = old[2],
        cached_right = old[3],
        cached_deviation = deviation(old),
        cached_neighbour_ratio = agreement(old),
        historical_isolated = isolated(old),
        new_left = new[1],
        new_centre = new[2],
        new_right = new[3],
        new_deviation = if (ready) deviation(new) else NA_real_,
        new_neighbour_ratio = if (ready) agreement(new) else NA_real_,
        new_isolated = if (ready) isolated(new) else NA,
        removed = if (ready) isolated(old) && !isolated(new) else NA
      )
    }
  }
}
peaks <- do.call(rbind, peaks)
write.csv(peaks, file.path(root, 'isolated-peaks.csv'), row.names = FALSE)
mdtable <- function(d) {
  if (nrow(d) == 0L) {
    return('(none)')
  }
  d[] <- lapply(d, function(x) {
    if (is.numeric(x)) {
      ifelse(
        is.na(x),
        '—',
        format(round(x, 3), trim = TRUE, scientific = FALSE)
      )
    } else {
      ifelse(is.na(x), '—', as.character(x))
    }
  })
  paste(
    c(
      paste0('| ', paste(names(d), collapse = ' | '), ' |'),
      paste0('| ', paste(rep('---', ncol(d)), collapse = ' | '), ' |'),
      apply(d, 1, function(x) paste0('| ', paste(x, collapse = ' | '), ' |'))
    ),
    collapse = '\n'
  )
}
reported <- subset(checks, label == 'reported_n')
half <- subset(checks, label == 'half_n')
upper <- subset(checks, label == 'upper_bound')
smallabove <- subset(half, evidence == 'above')
supported_upper <- merge(
  subset(upper, role == 'cache' & evidence == 'above'),
  search[, c('id', 'seed', 'role', 'cached_n')],
  by = c('id', 'seed', 'role')
)
supported_upper <- supported_upper[
  supported_upper$n < supported_upper$cached_n,
]
supported_upper$cache_to_checked_ratio <- supported_upper$cached_n /
  supported_upper$n
write.csv(
  supported_upper,
  file.path(root, 'supported-smaller-than-cache.csv'),
  row.names = FALSE
)
supported_table <- data.frame(
  Scenario = supported_upper$id,
  Cache_N = supported_upper$cached_n,
  Checked_upper_N = supported_upper$n,
  Independent_slope_95CI = sprintf(
    '%.3f [%.3f, %.3f]',
    supported_upper$slope,
    supported_upper$slope_ci_low,
    supported_upper$slope_ci_high
  ),
  Cache_to_checked_ratio = supported_upper$cache_to_checked_ratio
)
unsupported <- subset(
  search,
  status == 'estimated' &
    (!is.finite(upper_perf) | stage1_stop != 'target_bracketed')
)
# Stop labels alone are not evidence: inspect classifications at the returned bound.
search$upper_observed_above <- vapply(
  seq_len(nrow(search)),
  function(i) {
    f <- file.path(
      root,
      'results',
      paste0(search$id[i], '-', search$role[i], '-seed', search$seed[i]),
      'stage1.csv'
    )
    if (!file.exists(f)) {
      return(FALSE)
    }
    z <- read.csv(f)
    any(z$n == search$upper[i] & z$call == 'above')
  },
  logical(1)
)
search$lower_observed_below <- vapply(
  seq_len(nrow(search)),
  function(i) {
    f <- file.path(
      root,
      'results',
      paste0(search$id[i], '-', search$role[i], '-seed', search$seed[i]),
      'stage1.csv'
    )
    if (!file.exists(f)) {
      return(FALSE)
    }
    z <- read.csv(f)
    any(z$n == search$lower[i] & z$call == 'below')
  },
  logical(1)
)
write.csv(search, file.path(root, 'search-summary.csv'), row.names = FALSE)
count <- function(x) sum(x, na.rm = TRUE)
ratio_table <- pairs[, c(
  'slice',
  'role',
  'p_from',
  'p_to',
  'cached_ratio',
  'new_from',
  'new_to',
  'new_ratio',
  'smoothed'
)]
result_table <- merge(
  search[, c(
    'id',
    'p',
    'role',
    'seed',
    'cached_n',
    'target',
    'lower',
    'upper',
    'n',
    'predicted_slope',
    'stage1_stop',
    'lower_observed_below',
    'upper_observed_above'
  )],
  reported[, c(
    'id',
    'role',
    'seed',
    'slope',
    'slope_ci_low',
    'slope_ci_high',
    'evidence'
  )],
  by = c('id', 'role', 'seed'),
  all.x = TRUE,
  sort = FALSE
)
result_table <- result_table[
  order(
    match(result_table$id, vapply(scenarios, `[[`, '', 'id')),
    factor(result_table$role, levels = c('cache', 'shared'))
  ),
]
write.csv(
  result_table,
  file.path(root, 'results-with-evidence.csv'),
  row.names = FALSE
)
compact <- data.frame(
  Scenario = result_table$id,
  Seed = paste0(result_table$role, ' / ', result_table$seed),
  Cache_N = result_table$cached_n,
  Dev_N = result_table$n,
  Target = result_table$target,
  GP_prediction = result_table$predicted_slope,
  Independent_slope_95CI = ifelse(
    is.na(result_table$slope),
    '—',
    sprintf(
      '%.3f [%.3f, %.3f]',
      result_table$slope,
      result_table$slope_ci_low,
      result_table$slope_ci_high
    )
  ),
  Evidence = result_table$evidence
)
unique_pairs <- pairs[!duplicated(pairs[, c('slice', 'p_from', 'p_to')]), ]
conclusion <- sprintf(
  'Ridwan dev removes %d of %d historical peak comparisons and %d of %d dip comparisons currently assessable; %d peak/dip comparisons remain unavailable. The primary outcome is the shape of predictor count versus reported minimum sample size. Across both seed schedules, %d historical isolated peak/dip comparisons no longer meet the same rule and %d persist. Adjacent-step ratios below identify large jumps that remain despite peak removal.',
  count(peaks$historical_kind == 'peak' & peaks$removed),
  count(peaks$historical_kind == 'peak' & !is.na(peaks$new_isolated)),
  count(peaks$historical_kind == 'dip' & peaks$removed),
  count(peaks$historical_kind == 'dip' & !is.na(peaks$new_isolated)),
  count(is.na(peaks$new_isolated)),
  count(peaks$removed),
  count(peaks$historical_isolated & peaks$new_isolated)
)
completed <- sum(file.exists(file.path(dirname(files), 'DONE')))
lines <- c(
  if (completed < 28L) {
    '# Ridwan dev stability validation (interim)'
  } else {
    '# Ridwan dev stability validation'
  },
  '',
  paste(
    'Tested package: **d00a137640d47a9f1a75482ead8109b1ae1e463d**. Report generated',
    format(Sys.time(), tz = 'UTC'),
    'UTC.'
  ),
  '',
  conclusion,
  '',
  sprintf(
    'The study completed %d/%d planned cases; %d returned numeric sample sizes and %d returned errors or administrative limits. Four problematic predictor-count slices were tested with original cache seeds and a second shared seed. Each successful search requested 1,000 GP reps; every independent check used 1,000 fresh training/test/model draws.',
    sum(file.exists(file.path(dirname(files), 'DONE'))),
    28,
    count(search$status == 'estimated'),
    count(search$status != 'estimated')
  ),
  '',
  sprintf(
    'Of %d historical >=3x adjacent-step comparisons across the two seed roles, %d now fall below 3x, %d remain >=3x, and %d are unavailable. These counts repeat each historical pair once per new seed role; the study contains nine unique historical adjacent pairs.',
    count(pairs$historical_jump),
    count(pairs$smoothed),
    count(pairs$historical_jump & pairs$new_jump),
    count(pairs$historical_jump & is.na(pairs$new_ratio))
  ),
  '',
  'The shape improvement is qualified. Both shared-seed ridge curves retain a >3x 5-to-10 step (4.49x and 3.52x); the second ridge slice shifts its large low-N dip toward a high N at ten predictors. All six continuous-lasso searches and all six target-0.95 ridge searches ended on plateaus without an observed above-target upper bound. The continuous-lasso answers cluster at N=417–752, so disappearance of its old 32,768 peak may partly reflect early search termination, not a reliable minimum-N curve. No claim that minimum sample size itself is validated follows from smoothness.',
  '',
  '## Isolated peaks and dips: primary outcome',
  '',
  sprintf(
    'Peaks alone: %d/%d historical peak comparisons are removed, %d persist, %d unavailable. Dips: %d/%d are removed, %d persist, %d unavailable. There are three distinct historical peaks and two distinct dips, each tested twice.',
    count(peaks$historical_kind == 'peak' & peaks$removed),
    count(peaks$historical_kind == 'peak' & peaks$historical_isolated),
    count(peaks$historical_kind == 'peak' & peaks$new_isolated),
    count(peaks$historical_kind == 'peak' & is.na(peaks$new_isolated)),
    count(peaks$historical_kind == 'dip' & peaks$removed),
    count(peaks$historical_kind == 'dip' & peaks$historical_isolated),
    count(peaks$historical_kind == 'dip' & peaks$new_isolated),
    count(peaks$historical_kind == 'dip' & is.na(peaks$new_isolated))
  ),
  '',
  'The historical screen flags an interior point when it differs by at least 3x from the mean of its two neighbours and those neighbours agree within 1.6x. The same rule is applied to the new curves. Five distinct historical positions are each assessed under two seed schedules; a removed dip is also relevant to the same discontinuity problem. This is a descriptive threshold, not a hypothesis test or a monotonicity requirement.',
  '',
  mdtable(peaks[, c(
    'slice',
    'role',
    'p',
    'historical_kind',
    'cached_deviation',
    'new_left',
    'new_centre',
    'new_right',
    'new_deviation',
    'new_isolated',
    'removed'
  )]),
  '',
  '## Adjacent predictor-count steps',
  '',
  sprintf(
    'Eight of the nine unique historical predictor-count steps had >=3x jumps. Each is assessed in two new seed roles; these are 16 rerun comparisons of the eight historical jumps, rather than 16 distinct historical pairs.'
  ),
  '',
  mdtable(ratio_table),
  '',
  '![Historical and new estimates across predictor counts](neighbour-estimates.png)',
  '',
  '## Returned sample sizes and secondary target checks',
  '',
  sprintf(
    'The peak assessment above is the primary verdict. Target attainment is secondary: at returned N, %d/%d checks are above target, %d below, and %d overlap (pointwise 95%% intervals). Earlier runs also checked half N (%d cases) and the upper bound (%d cases); subsequent runs check returned N only, as recorded in the protocol amendment. These checks do not determine whether a peak has been removed.',
    count(reported$evidence == 'above'),
    nrow(reported),
    count(reported$evidence == 'below'),
    count(reported$evidence == 'overlaps'),
    nrow(half),
    nrow(upper)
  ),
  '',
  mdtable(compact),
  '',
  'Full bounds, pilot classifications and GP predictions are preserved in results-with-evidence.csv and search-summary.csv.',
  '',
  '![Independent target checks](independent-checks.png)',
  '',
  '## Supported smaller points than the historical cache',
  '',
  'The prespecified upper-bound checks in these original cache-seed runs are above target at Ns smaller than the old cached Ns. This is positive evidence that a smaller supported alternative is available in the tested version. These upper bounds are validation points, not corrected GP answers or verified minima. Historical environment differences and pointwise interval limitations still apply.',
  '',
  mdtable(supported_table),
  '',
  '## Search errors and bound handoff',
  '',
  mdtable(search[
    search$status != 'estimated',
    c('id', 'role', 'seed', 'status')
  ]),
  '',
  sprintf(
    '%d estimated cases had no observed above-target classification at their returned upper bound. The detailed stage-1 CSV traces preserve classifications, standard errors, replication counts, failures and stop reasons. An untested fallback bound is not supported merely because it is returned.',
    count(search$status == 'estimated' & !search$upper_observed_above)
  ),
  '',
  '## Interpretation and limits',
  '',
  'The cache is historical, not a new matched pre-fix run; changes include simulation noise and possible cache-build environment differences. This purposive study covers 14 scenarios and cannot establish a failure rate for all 13,680 cached scenarios or justify clearing all 900 exclusions. Two seeds give a local sensitivity check, not a comprehensive stability distribution.',
  '',
  'Ridge/lasso assurance is assessed on the original internal CSSE scale. Displayed equivalent slope is 1-sqrt(-q20(CSSE)); GLM uses raw slope q20. Confidence intervals are binomial order-statistic intervals for individual performance quantiles, not intervals for the minimum N. Overlap is inconclusive. Below is a target shortfall; above at half N establishes an independently tested smaller alternative. Answers have not been corrected.',
  '',
  sprintf(
    'Independent checks recorded %d fit errors and %d nonfinite metric values. Search model observers recorded %d fitting errors; stage-1 n_fail itself only counts nonfinite values.',
    sum(checks$fit_errors),
    sum(checks$reps - checks$finite),
    sum(search$search_fit_errors)
  ),
  '',
  'The review also reproduced a below-target plateau handing untested bounds to stage 2, an absent RNG stream not being restored, and two existing bisection/hybrid test errors from adaptive_seed forwarding. See [FIXES-AND-PROVENANCE.md](FIXES-AND-PROVENANCE.md), [existing-tests.txt](existing-tests.txt), and [review-diagnostics.csv](review-diagnostics.csv). Those findings are separate from the simulation outcomes.',
  '',
  '## Unchanged GP selection rule',
  '',
  'The installed mlpwr 1.1.1 selects a final design using an optimistic GP mean + 0.3 standard deviations and a soft cost penalty. It can return a below-target mean; its separate bad-prediction threshold is an absolute discrepancy of 0.4. [GP-REVIEW.md](GP-REVIEW.md), gp-diagnostics.csv and the captured dependency source document this for the saved current runs. This rule explains how below-target predictions can be returned; it does not isolate every independent shortfall.',
  '',
  '## Follow-up outside the peak assessment',
  '',
  'Broader validation should include more seed schedules around any remaining jump and a fresh matched pre-fix comparison. For the separate target-attainment question, require an independently supported upper bound before handing off to stage 2. A below-target plateau should yield an inconclusive search rather than a claimed sample size. Fresh test draws are needed to address the fixed-test shift; returned GP candidates should receive independent target checks and checks of smaller alternatives. The bisection/hybrid argument-forwarding regressions also need repair. This branch documents those recommendations without changing the package being evaluated.',
  '',
  '## Reproduction and consolidated history',
  '',
  'See [PROTOCOL.md](PROTOCOL.md), [scenarios.json](scenarios.json), [jobs.json](jobs.json), [search-summary.csv](search-summary.csv), [validation-summary.csv](validation-summary.csv), [adjacent-comparisons.csv](adjacent-comparisons.csv), and [isolated-peaks.csv](isolated-peaks.csv). Run python3 validation/stability-20260918/run-study.py from the repository root, then analyse.R and verify.R. Raw RDS draws remain local; all report summaries and session information are preserved. [EXECUTION.md](EXECUTION.md) records early instrumentation and reporting corrections, which were audited against the saved draws. Historical artifacts are indexed in [history/manifest.json](history/manifest.json).'
)
writeLines(lines, file.path(root, 'REPORT.md'))
library(ggplot2)
plotdata <- subset(
  search,
  slice != 'binary_glm_seed_failure' & status == 'estimated'
)
old <- plotdata[!duplicated(plotdata$id), ]
old$n <- old$cached_n
old$role <- 'historical cache'
pdat <- rbind(plotdata, old)
pdat$role <- factor(
  pdat$role,
  levels = c('historical cache', 'cache', 'shared'),
  labels = c(
    'Historical cache',
    'Dev: original cache seeds',
    'Dev: shared seed'
  )
)
pdat$slice <- factor(
  pdat$slice,
  levels = unique(vapply(scenarios, `[[`, '', 'slice'))
)
p <- ggplot(pdat, aes(p, n, colour = role, linetype = role, group = role)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(
    ~slice,
    scales = 'free_y',
    ncol = 2,
    labeller = as_labeller(c(
      binary_ridge_reported = 'Binary ridge: original peak (target 0.90)',
      binary_ridge_dip = 'Binary ridge: dip (target 0.95)',
      continuous_lasso_peak = 'Continuous lasso: peak (target 0.95)',
      survival_lasso_peak = 'Survival lasso: peak (target 0.95)'
    ))
  ) +
  scale_colour_manual(values = c('#64748b', '#2563eb', '#d97706')) +
  scale_y_log10(labels = scales::comma) +
  scale_x_continuous(breaks = c(5, 10, 15, 20)) +
  labs(
    x = 'Signal predictors',
    y = 'Reported sample size (log scale)',
    colour = NULL,
    linetype = NULL,
    title = "Adjacent predictor-count estimates",
    subtitle = 'Primary outcome: isolated peaks, dips and remaining adjacent jumps'
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = 'bottom')
ggsave(
  file.path(root, 'neighbour-estimates.png'),
  p,
  width = 11,
  height = 7,
  dpi = 150
)
qdat <- merge(
  checks,
  search[, c('id', 'seed', 'role', 'slice', 'target')],
  by = c('id', 'seed', 'role')
)
qdat$case <- paste0(qdat$id, ' / ', qdat$role)
qdat$case <- factor(
  qdat$case,
  levels = rev(paste0(result_table$id, ' / ', result_table$role))
)
qdat$label <- factor(
  qdat$label,
  levels = c('half_n', 'reported_n', 'upper_bound'),
  labels = c('Half returned N', 'Returned N', 'Upper bound')
)
qdat$delta <- qdat$slope - qdat$target
qdat$delta_low <- qdat$slope_ci_low - qdat$target
qdat$delta_high <- qdat$slope_ci_high - qdat$target
p <- ggplot(qdat, aes(delta, case, colour = label)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbar(
    aes(xmin = delta_low, xmax = delta_high),
    orientation = 'y',
    width = .2,
    position = position_dodge(width = .65)
  ) +
  geom_point(position = position_dodge(width = .65), size = 1.6) +
  scale_colour_manual(values = c('#d97706', '#2563eb', '#64748b')) +
  labs(
    x = 'Independent equivalent slope minus target (pointwise 95% interval)',
    y = NULL,
    colour = NULL,
    title = 'Independent performance checks',
    subtitle = 'Fresh training and test draws; 1,000 replicates per point'
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = 'bottom')
ggsave(
  file.path(root, 'independent-checks.png'),
  p,
  width = 12,
  height = 11,
  dpi = 150
)
print(table(reported$evidence))
print(pairs)
