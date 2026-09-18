source('validation/predictor-count-spikes/common.R')
comparison <- read.csv(result_path('comparison.csv'))
refs <- read.csv(result_path('reference-grid.csv'))
crossings <- read.csv(result_path('reference-crossings.csv'))
cache <- read.csv(result_path('cache-peak.csv'))
domain <- read.csv(result_path('common-bounds.csv'))
support <- read.csv(result_path('domain-support.csv'))
meta_files <- list.files(
  file.path(study_root, 'results'),
  pattern = '-ridge-metadata[.]csv$',
  full.names = TRUE
)
meta <- do.call(rbind, lapply(meta_files, read.csv))
stopifnot(nrow(meta) == 12, all(meta$reps == 1000))
write.csv(meta, result_path('ridge-metadata.csv'), row.names = FALSE)
oracle_files <- list.files(
  file.path(study_root, 'results'),
  pattern = '-fixed-test-oracle[.]csv$',
  full.names = TRUE
)
oracle <- do.call(rbind, lapply(oracle_files, read.csv))
oracle <- merge(
  comparison[
    comparison$seed_role == 'cache' & comparison$mode == 'baseline',
    c('id', 'p')
  ],
  oracle,
  by = 'id'
)
oracle <- oracle[order(oracle$p), ]
oracle_text <- sprintf(
  'A diagnostic using the true generating linear predictor on each original fixed test set gives test-set calibration slopes %s for the 10 / 15 / 20 cache-seed cases. With ridge slopes mostly above one, these directions are consistent with optimistic neighbouring pilots and a pessimistic middle pilot. This is a clue to fixed-test noise, not a causal separation of fixed-test noise from the original 20-rep quantile noise. Values are in results/*-fixed-test-oracle.csv.',
  paste(sprintf('%.3f', oracle$true_lp_test_slope), collapse = ' / ')
)
fmt_n <- function(x) {
  format(round(x), big.mark = ',', trim = TRUE, scientific = FALSE)
}
fmt_perf <- function(r) {
  sprintf(
    '%.3f [%.3f, %.3f]',
    r$equivalent_slope,
    r$slope_ci_low,
    r$slope_ci_high
  )
}
mdtable <- function(x) {
  c(
    paste0('| ', paste(names(x), collapse = ' | '), ' |'),
    paste0('| ', paste(rep('---', ncol(x)), collapse = ' | '), ' |'),
    apply(x, 1, function(r) paste0('| ', paste(r, collapse = ' | '), ' |'))
  )
}
row_for <- function(p, role, mode) {
  comparison[
    comparison$p == p & comparison$seed_role == role & comparison$mode == mode,
  ]
}
primary <- do.call(
  rbind,
  lapply(c(10, 15, 20), function(p) {
    b <- row_for(p, 'cache', 'baseline')
    w <- row_for(p, 'cache', 'common-bounds')
    data.frame(
      Predictors = p,
      Cache_N = fmt_n(cache$cached_n[cache$signal_parameters == p]),
      Local_baseline_N = fmt_n(b$n),
      Broad_domain_N = fmt_n(w$n),
      Broad_N_heldout = fmt_perf(w),
      Evidence = w$evidence,
      check.names = FALSE
    )
  })
)
secondary <- do.call(
  rbind,
  lapply(c(10, 15, 20), function(p) {
    b <- row_for(p, 'shared', 'baseline')
    w <- row_for(p, 'shared', 'common-bounds')
    data.frame(
      Predictors = p,
      Original_bounds_N = fmt_n(b$n),
      Broad_domain_N = fmt_n(w$n),
      Broad_N_heldout = fmt_perf(w),
      Evidence = w$evidence,
      check.names = FALSE
    )
  })
)
peak <- row_for(15, 'cache', 'baseline')
wide_peak <- row_for(15, 'cache', 'common-bounds')
above <- refs[refs$id == peak$id & refs$evidence == 'above', ]
above <- above[order(above$n), ]
proof <- if (nrow(above) && min(above$n) < peak$lower) {
  sprintf(
    'The original 15-predictor pilot excluded a smaller independently supported working N: at N=%s the equivalent slope was %s, above the 0.90 target. Its GP range started at %s. This is direct evidence of incorrect bounds for this peak.',
    fmt_n(above$n[1]),
    fmt_perf(above[1, ]),
    fmt_n(peak$lower)
  )
} else {
  'The independent grid has not established a smaller above-target N excluded by the original 15-predictor bounds.'
}
ablation <- sprintf(
  'Changing only the GP boundaries, while retaining the same tuned generator and baseline RNG at GP entry, changed the 15-predictor answer from %s to %s. The broad-domain answer independently measured %s (%s target).',
  fmt_n(peak$n),
  fmt_n(wide_peak$n),
  fmt_perf(wide_peak),
  wide_peak$evidence
)
unsupported <- support$id[!support$crossing_supported]
coverage <- if (length(unsupported)) {
  paste(
    'An independent below/above pair was not established for:',
    paste(unsupported, collapse = ', '),
    '. Their GP outputs must not be interpreted as validated minima. No unreachable-target claim is made.'
  )
} else {
  'Independent below/above points were observed inside the broad domain for all six tuned generators; this does not certify exact minima or target attainment at the GP answers.'
}
counts <- table(comparison$mode, comparison$evidence)
count_table <- data.frame(
  Mode = rownames(counts),
  as.data.frame.matrix(counts),
  check.names = FALSE
)
meta <- merge(
  comparison[c('id', 'p', 'seed', 'seed_role', 'mode')],
  meta,
  by = c('id', 'mode')
)
meta_primary <- meta[meta$seed_role == 'cache' & meta$mode == 'common-bounds', ]
meta_primary <- meta_primary[order(meta_primary$p), ]
meta_table <- data.frame(
  Predictors = meta_primary$p,
  Mean_raw_slope = sprintf('%.3f', meta_primary$mean_raw_slope),
  Within_09_11 = sprintf('%.1f%%', 100 * meta_primary$fraction_in_09_11_band),
  Above_11 = sprintf('%.1f%%', 100 * meta_primary$fraction_above_11),
  CV_at_weakest_tested_penalty = sprintf(
    '%.1f%%',
    100 * meta_primary$fraction_lambda_at_grid_min
  ),
  check.names = FALSE
)
lines <- c(
  '# Predictor-count spike: local validation',
  '',
  '**Problem:** a noisy preliminary search can hand stage two a range that excludes smaller sample sizes which already meet the target. Stage two cannot recover a minimum outside that range, so adjacent predictor counts can show artificial peaks. This study confirms that mechanism for the original 15-predictor peak; it does not establish that every irregularity has the same cause.',
  '',
  proof,
  '',
  ablation,
  '',
  '**Conclusion:** improving stage-one bounds is necessary for this case, but wider bounds alone are not a validated fix. The smaller GP answer fails its independent target check. The study diagnoses the problem without implementing a production change.',
  '',
  '## Original cache seeds',
  '',
  mdtable(primary),
  '',
  'The peak N=10,672 is reproduced exactly. Neighbouring GP estimates differ somewhat from the cache; software/platform details are recorded in session-info.txt. Each local search requested and completed at least 1,000 GP reps. Each held-out measurement used 1,000 independent fresh training/test/model draws.',
  '',
  '## Shared public-call seed 48',
  '',
  mdtable(secondary),
  '',
  '![Reported N across predictor counts](predictor-count-results.png)',
  '',
  '## Independent curves and domain support',
  '',
  '![Fresh 1,000-rep curves and original preliminary estimates](reference-curves.png)',
  '',
  oracle_text,
  '',
  sprintf(
    'The common GP domain was %s–%s: the union of all original pilot Ns and independent reference Ns. It includes the original large peak and upper limit, so the ablation permits the GP to choose its original answer.',
    fmt_n(domain$lower),
    fmt_n(domain$upper)
  ),
  '',
  coverage,
  '',
  'The common broad domain is a causal diagnostic, not a proposed operational replacement for adaptive bounds. Wider domains can spread a finite GP budget thinly. These under-sized broad-domain answers do not establish that improved, supported narrow brackets would fail with the unchanged GP.',
  '',
  'All plotted performance values are **equivalent slope = 1-sqrt(-q20(CSSE))**, not q20(raw slope). Ridge target 0.90 is searched internally as q20(CSSE)>=-0.01. For a single replicate this is the two-sided raw-slope band 0.90–1.10. The assurance level is 80%. Neither this criterion nor test N=30,000 was changed.',
  '',
  '## Independent checks of GP answers',
  '',
  mdtable(count_table),
  '',
  'A below-target interval is evidence of failure. An overlapping interval is inconclusive, not a successful validation. An above-target interval supports attainment at that N, not minimality. Removing a peak from the reported-N plot alone is insufficient evidence of a fix.',
  '',
  'The installed mlpwr 1.1.1 also uses GP mean + 0.3 × GP standard deviation when selecting its final desired-performance point. This can return a point whose predicted mean is below target. It is a separate concern from the initial bounds, and does not by itself explain all held-out discrepancies. See GP-NOTES.md and results/gp-decisions.csv.',
  '',
  '## Ancillary ridge observations',
  '',
  mdtable(meta_table),
  '',
  'These observations reuse the original fitted models. Selecting the weakest penalty in a tested path is a diagnostic clue, not proof that the path is inadequate. Raw slopes outside the 0.90–1.10 band explain CSSE failures but do not change the searched criterion. Full summaries for both seeds and both search modes are in results/ridge-metadata.csv.',
  '',
  '## Limits and next step',
  '',
  'This study tests one predictor-count slice and two seeds per count. It does not establish global monotonicity, an exact minimum N, simultaneous interval coverage, reliability across the full cache, or which particular stage-1 change is sufficient. Coarse reference grid crossings are intervals. Unsupported cases remain unresolved rather than becoming enormous required Ns or declarations of infeasibility.',
  '',
  'Keep the existing two-stage architecture for the next implementation experiment:',
  '',
  '1. First test fresh test draws in stage one, matching stage two, against the original code. This isolates sampling consistency before adding other changes.',
  '2. Add bounded extra sampling for ambiguous candidate bounds. A noisy point estimate, a plateau or budget exhaustion should not certify a crossing; unresolved searches should report that explicitly.',
  '3. Initially retain the GP unchanged. Check returned Ns independently with 1,000 reps and measure runtime, using these cases and the weak-signal GLM seed-298 case that stalled under the earlier fix. If failures persist, isolate GP fit/selection behaviour before claiming a fix. Test the ridge penalty-path hypothesis separately; the current observations do not justify changing the criterion.',
  '',
  'Smooth reported Ns are insufficient evidence of a fix. Production R code on this investigation branch is identical to main.',
  '',
  'See README.md, PROTOCOL-AMENDMENTS.md, the explicit JSON job plans, session-info.txt and results/*.csv for reproducibility. Raw draws and logs remain local under the study-specific .gitignore.'
)
writeLines(lines, file.path(study_root, 'RESULTS.md'))
