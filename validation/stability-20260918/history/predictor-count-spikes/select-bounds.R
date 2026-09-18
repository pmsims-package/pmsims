source('validation/predictor-count-spikes/common.R')
cases <- read.csv(result_path('cases.csv'))
files <- list.files(
  file.path(study_root, 'results'),
  pattern = '-grid-n[0-9]+[.]csv$',
  full.names = TRUE
)
refs <- do.call(rbind, lapply(files, read.csv))
write.csv(refs, result_path('reference-grid.csv'), row.names = FALSE)
stopifnot(all(refs$reps == 1000), all(refs$errors == 0))
search_files <- list.files(
  file.path(study_root, 'results'),
  pattern = '-baseline-search[.]csv$',
  full.names = TRUE
)
searches <- do.call(rbind, lapply(search_files, read.csv))
stopifnot(nrow(searches) == 6, all(searches$status == 'estimated'))
stage_files <- list.files(
  file.path(study_root, 'results'),
  pattern = '-stage1[.]csv$',
  full.names = TRUE
)
stages <- do.call(rbind, lapply(stage_files, read.csv))
# Include every N the original pilots tried. The ablation leaves the GP free
# to choose the original large answer as well as independently checked Ns.
lower <- min(c(stages$n, refs$n))
upper <- max(c(stages$n, refs$n))
support <- do.call(
  rbind,
  lapply(cases$id, function(id) {
    r <- refs[refs$id == id, ]
    below <- r$n[r$evidence == 'below']
    above <- r$n[r$evidence == 'above']
    data.frame(
      id = id,
      independent_below = if (length(below)) max(below) else NA_real_,
      independent_above = if (length(above)) min(above) else NA_real_,
      crossing_supported = length(below) > 0 &&
        length(above) > 0 &&
        min(below) < max(above)
    )
  })
)
write.csv(support, result_path('domain-support.csv'), row.names = FALSE)
bounds <- data.frame(
  lower = lower,
  upper = upper,
  supporting_generators = sum(support$crossing_supported),
  total_generators = nrow(cases),
  reps_per_checked_n = 1000,
  internal_metric = 'q20(CSSE)',
  internal_target = refs$internal_target[1]
)
write.csv(bounds, result_path('common-bounds.csv'), row.names = FALSE)
print(refs[c(
  'id',
  'n',
  'equivalent_slope',
  'slope_ci_low',
  'slope_ci_high',
  'evidence'
)])
print(support)
print(bounds)
