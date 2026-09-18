source('validation/predictor-count-spikes/common.R')
cases <- read.csv(result_path('cases.csv'))
collect <- function(pattern) {
  do.call(
    rbind,
    lapply(
      list.files(
        file.path(study_root, 'results'),
        pattern = pattern,
        full.names = TRUE
      ),
      read.csv
    )
  )
}
searches <- collect('-search[.]csv$')
held <- collect('-heldout[.]csv$')
refs <- collect('-grid-n[0-9]+[.]csv$')
expected <- as.vector(outer(
  cases$id,
  c('baseline', 'common-bounds'),
  paste,
  sep = ':'
))
stopifnot(
  nrow(searches) == 12,
  nrow(held) == 12,
  nrow(refs) == 17,
  setequal(paste(searches$id, searches$mode, sep = ':'), expected),
  setequal(paste(held$id, held$mode, sep = ':'), expected),
  all(searches$status == 'estimated'),
  all(searches$gp_reps >= 1000),
  all(held$reps == 1000),
  all(refs$reps == 1000),
  all(held$errors == 0),
  all(refs$errors == 0)
)
searches <- merge(cases, searches, by = 'id')
held <- merge(cases, held, by = 'id')
refs <- merge(cases, refs, by = 'id')
write.csv(searches, result_path('searches.csv'), row.names = FALSE)
write.csv(held, result_path('heldout.csv'), row.names = FALSE)
write.csv(refs, result_path('reference-grid.csv'), row.names = FALSE)
comparison <- merge(
  searches,
  held[c(
    'id',
    'mode',
    'equivalent_slope',
    'slope_ci_low',
    'slope_ci_high',
    'evidence'
  )],
  by = c('id', 'mode')
)
write.csv(comparison, result_path('comparison.csv'), row.names = FALSE)
crossings <- do.call(
  rbind,
  lapply(split(refs, refs$id), function(r) {
    below <- r$n[r$evidence == 'below']
    above <- r$n[r$evidence == 'above']
    data.frame(
      id = r$id[1],
      reference_below = if (length(below)) max(below) else NA_real_,
      reference_above = if (length(above)) min(above) else NA_real_
    )
  })
)
write.csv(crossings, result_path('reference-crossings.csv'), row.names = FALSE)
print(comparison[c(
  'p',
  'seed',
  'mode',
  'lower',
  'upper',
  'n',
  'equivalent_slope',
  'slope_ci_low',
  'slope_ci_high',
  'evidence'
)])
png(
  file.path(study_root, 'predictor-count-results.png'),
  width = 1600,
  height = 650,
  res = 140
)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
cache <- read.csv(result_path('cache-peak.csv'))
cols <- c(baseline = '#D65B37', 'common-bounds' = '#207C98')
for (role in c('cache', 'shared')) {
  r <- searches[searches$seed_role == role, ]
  plot(
    NA,
    xlim = c(9, 21),
    ylim = range(c(searches$n, cache$cached_n)),
    log = 'y',
    xlab = 'Signal predictors',
    ylab = 'Reported training N',
    main = if (role == 'cache') 'Original cache seeds' else 'Shared seed 48',
    xaxt = 'n'
  )
  axis(1, at = c(10, 15, 20))
  if (role == 'cache') {
    lines(
      cache$signal_parameters,
      cache$cached_n,
      type = 'b',
      pch = 1,
      lty = 3,
      col = 'grey40'
    )
  }
  for (mode in names(cols)) {
    s <- r[r$mode == mode, ]
    s <- s[order(s$p), ]
    lines(s$p, s$n, type = 'b', pch = 16, col = cols[mode], lwd = 2)
  }
  legend(
    'topright',
    legend = c(
      'Original adaptive bounds',
      'Common broad domain',
      if (role == 'cache') 'Historical cache'
    ),
    col = c(cols, if (role == 'cache') 'grey40'),
    lty = c(1, 1, if (role == 'cache') 3),
    pch = c(16, 16, if (role == 'cache') 1),
    bty = 'n',
    cex = .8
  )
}
dev.off()
decisions <- do.call(
  rbind,
  lapply(seq_len(nrow(searches)), function(i) {
    r <- searches[i, ]
    payload <- readRDS(raw_path(paste0(r$id, '-', r$mode, '.rds')))
    fit <- payload$result$mlpwr_ds$fit
    n <- payload$result$min_n
    mu <- as.numeric(fit$fitfun(n))
    sd <- as.numeric(fit$fitfun.sd(n))
    data.frame(
      id = r$id,
      mode = r$mode,
      n = n,
      gp_mean = mu,
      gp_sd = sd,
      optimistic_score = mu + 0.3 * sd,
      target = -0.01,
      mean_meets_target = mu >= -0.01,
      optimistic_score_meets_target = mu + 0.3 * sd >= -0.01
    )
  })
)
write.csv(decisions, result_path('gp-decisions.csv'), row.names = FALSE)
png(
  file.path(study_root, 'reference-curves.png'),
  width = 1700,
  height = 650,
  res = 140
)
par(mfrow = c(1, 3), mar = c(4.5, 4.5, 3, 1))
stage_files <- list.files(
  file.path(study_root, 'results'),
  pattern = '-stage1[.]csv$',
  full.names = TRUE
)
stages <- do.call(rbind, lapply(stage_files, read.csv))
curve_ylim <- range(c(
  refs$slope_ci_low,
  refs$slope_ci_high,
  stages$equivalent_slope,
  .9
))
curve_xlim <- range(c(refs$n, stages$n))
for (p in c(10, 15, 20)) {
  r <- refs[refs$p == p & refs$seed_role == 'cache', ]
  r <- r[order(r$n), ]
  plot(
    r$n,
    r$equivalent_slope,
    type = 'b',
    pch = 16,
    log = 'x',
    ylim = curve_ylim,
    xlim = curve_xlim,
    xlab = 'Training N (log scale)',
    ylab = 'Equivalent slope from q20(CSSE)',
    main = paste(p, 'predictors')
  )
  arrows(
    r$n,
    r$slope_ci_low,
    r$n,
    r$slope_ci_high,
    angle = 90,
    code = 3,
    length = .04
  )
  abline(h = .9, lty = 2, col = 'grey40')
  b <- read.csv(result_path(paste0(r$id[1], '-stage1.csv')))
  lines(b$n, b$equivalent_slope, type = 'b', pch = 1, col = cols['baseline'])
  legend(
    'bottomright',
    legend = c('Independent 1000-rep checks', 'Original 20-rep stage 1'),
    col = c('black', cols['baseline']),
    lty = 1,
    pch = c(16, 1),
    bty = 'n',
    cex = .75
  )
}
dev.off()
