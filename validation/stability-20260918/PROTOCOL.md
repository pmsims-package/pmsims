# Validation of Ridwan's dev repair, 18 September 2026

Pinned package: `d00a137640d47a9f1a75482ead8109b1ae1e463d`, fetched from origin/dev. Production R files are unchanged by this study. The package is loaded from this checkout with pkgload, not from the older installed package.

## Selection made before new results

Four historically problematic predictor-count slices: binary ridge at prevalence .25 / C-statistic .85 / target .90 (5,10,15,20 predictors); binary ridge at prevalence .50 / C-statistic .80 / target .95 (5,10,15); continuous lasso at maximum R-squared .10 / target .95 (5,10,15); survival lasso at C-index .75 / baseline hazard .50 / censoring .10 / target .95 (5,10,15). These cover peaks, a dip, all three outcome types, and literal 5-to-10 grid steps. One additional binary GLM case (.20 prevalence / .60 C-statistic / .95 target, 5 predictors) reproduces the historical seed-298 failure.

Each of the 14 cases uses its original cache seed plus shared public-call seed 48, except the GLM seed-failure case uses seed 1 alongside seed 298. All inputs and historical Ns are saved in scenarios.json. Cache seeds are one-based indices within each outcome's ordered cache; selection is checked against the historical scenario grid. The pinned cache is a historical comparator, not a fresh matched pre-fix experiment. Differences also include simulation noise and possible cache-build environment differences.

## Searches and independent checks

The public wrapper uses assurance, calibration slope, complexity 1, zero noise predictors, 1,000 requested GP reps, default 20-replicate batches, default 500-rep adaptive budget, and test_n=30,000. No manual search bounds or post-hoc correction. The default adaptive_seed is 20240101. Changing public-call seeds also changes tuning; identical adaptive seeds do not imply identical generators or full-package estimates.

Observers delegate the engine and adaptive function exactly once and consume no RNG. A model observer records fit failures. A study-only data wrapper raises an error above 200,000 training rows to prevent runaway allocations: affected cases must be reported as administratively limited, never as validated or corrected estimates. This ceiling does not modify datasets within the limit. Independent fitting jobs use fresh training and test datasets for every replicate.

After each successful search, 1,000 independent draws evaluate half the returned N, the returned N, and the adaptive upper bound, with distinct recorded seeds. CSV summaries preserve the internal metric, Type-7 20th percentile and binomial order-statistic 95% interval. For ridge/lasso, internal CSSE is -(1-slope)^2: the displayed equivalent slope is 1-sqrt(-q20(CSSE)), not the raw slope's q20. GLM uses raw calibration slope. Failed fits retain the package's error fallback and are counted; nonfinite values are explicitly counted. Intervals are pointwise, not simultaneous guarantees or confidence intervals for the minimum N.

A below-target interval at returned N is a shortfall; overlap is inconclusive; above supports target attainment for this tuned generator. If half N is also above, the returned N is unnecessarily large relative to that independently tested alternative. Smooth neighbouring estimates alone do not establish correctness. Successful upper-bound checks test the handoff; unsupported upper bounds remain flagged.

## Outcomes

For each seed role, compare pairwise max/min N ratios on each literal predictor-count step with the historical ratio. Report historical >=3x steps, whether the new ratio falls below 3x, and any remaining/new >=3x steps. Report search errors, unsupported adaptive bounds, target attainment, and smaller-N evidence alongside those ratios. This purposive local sample cannot estimate a cache-wide failure rate or justify removing all 900 cache exclusions.

Historical artifacts are preserved under history/, with exact source refs and SHA-256 checksums in history/manifest.json. Historical scripts retain their original paths and dependencies and are reference material; run-case.R is the new executable study.
