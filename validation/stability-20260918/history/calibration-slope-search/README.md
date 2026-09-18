# Calibration-slope search fix and local validation

**Status: development candidate, not a generally validated minimum-N solver.**
The report now separates structural corrections from heuristic safeguards and
includes counterexamples and a revised plan. See
[the generality assessment](results/REPORT.md#how-general-are-the-changes).

For a shareable demonstration of the original seed-sensitive behavior, see
[the Quarto reproduction](seed-reproduction/seed-instability.qmd) and its
[standalone R script](seed-reproduction/reproduce.R). It includes the historical
5,120,000-participant GLM example (seed 298), fresh original-code observations,
and commands for deliberately rerunning the expensive search with 1,000 reps.

## Review and implementation plan

The investigation correctly identifies an unsupported stage-1 range being passed
to a hard-bounded Gaussian-process search. Its description mixes two paths:
`simulate_binary()` defaults to `method = "mlpwr"`, which calls
`calculate_adaptive_bounds()` directly. The normal-approximation CIs and
`0.8`/`1.2` fallbacks in `adaptive_startvalues()` belong to `method = "mlpwr-bs"`.
Those fallbacks are a related problem, but are not on the default reproduction
path.

Two problems in the default path are more specific:

1. `calculate_adaptive_bounds()` declares a plateau when three consecutive
   changes are smaller than `0.005`, then returns the **last two doubled sample
   sizes** as bounds even if neither meets the target. For a slope target of
   `0.95`, the CSSE target is `-0.0025`. The absolute plateau threshold is larger
   than the distance from the target to perfect calibration. This explains the
   powers of two without needing repeated multiplier fallbacks.
2. Stage 1 conditions all replicates on one test dataset, but the GP stage uses
   a fresh test dataset per replicate. A single test draw can shift stage 1's
   curve. Increasing training N or training replicates cannot remove that
   conditional test-set bias. Both stages must estimate the same distribution.

The implemented plan is:

1. Generate independent test data for each preliminary replicate, as in the GP
   stage. Keep the CSSE conversion; its algebra is correct. Do not assume it
   guarantees monotonicity for every regularised learner.
2. Use the empirical 20th percentile (`type = 1`) consistently for assurance.
   Default interpolation is optimistic in small batches: on expected uniform
   order statistics, 20 draws give an interpolated estimate of 0.2286 for a
   population 0.2 percentile. Mean aggregation is unchanged.
   Use Student-t intervals for means and binomial order-statistic intervals for
   the 20th percentile. Pool extra batches at ambiguous sample sizes, up to
   five batches per candidate, within the existing 500-replicate preliminary
   budget. Require an interval below and an interval above the target.
3. Preserve the outer evidence envelope, rather than letting a noisy later
   estimate raise the lower bound drastically. Use gains relative to the
   remaining target gap for plateau detection. Stop noisy plateaus when recent
   confidence intervals overlap while all stay below target.
4. Leave bounds missing on plateau, exhausted budget, contradictory evidence,
   no movement or integer overflow. Every engine rejects missing automatic
   bounds before proceeding. These conditions mean **no crossing established**,
   not proof that a target is unreachable.
5. Keep the hybrid's original preliminary range (or explicit user range) through
   its noisy bisection stage. Remove its multiplier fallbacks. Explicitly
   supplied ranges continue to bypass the automatic search.
6. Independently confirm the GP candidate before reporting it. Use
   `n_reps_total` independent draws per candidate across up to five looks (1,000 draws per
   candidate in this study). Reject candidates whose upper interval bound stays
   below target, increase N by up to 1.5 within the range, and reserve the final
   look for the upper bound. Failed confirmation returns no estimate. Record
   the original GP result and confirmation trace; return confirmation performance.
   Compatibility is a pointwise diagnostic, not a guarantee of population target
   attainment or a global minimum. Requiring the entire interval above target
   would instead seek a conservative upper requirement and inflate estimates.
7. Exercise the failure mechanisms with deterministic regression tests, then
   run the local Monte Carlo study below and record independent validation.

The preliminary budget remains 500 replicates; fresh test data and repeat
batches can increase its runtime. A broader GP range avoids premature exclusion
but can require more GP evaluations for a precise answer. An unbracketed target
now raises an error where it previously could return an unsupported number.

## Reproduce the study

Run from the repository root with R, package dependencies, `pkgload`, `testthat`
and `glmnet` installed:

```sh
Rscript validation/calibration-slope-search/run-study.R
```

The script obtains baseline `R/start_values.R` from commit `a88e701` using
read-only `git show`. The rare-outcome lasso reproduction uses seed 47 and runs
**stage 1 only** for both baseline and fixed code; it tests whether unsupported
bounds are rejected, without launching an expensive GP. Four ordinary scenarios
use seeds 47 and 48 and run the full fixed search: the original 15-predictor
ridge report, a binary GLM control, a binary lasso control (5 predictors, 20%
prevalence, achievable C-statistic 0.75, target 0.90) and a continuous ridge
control.

Final searches use a **1,000-replicate GP budget**, 20 per batch, default tuning, test
size 30,000, no noise predictors, complexity 1 and assurance aggregation. The
preliminary budget stays at the package default of 500 replicates. Exploratory
200-replicate pilots were superseded by the final study.

For each successful fixed result, 1,000 independent training/test draws evaluate
performance at `0.5N`, `N`, and `2N`. The raw metric is evaluated using fresh test
sets of 30,000 participants. Search and validation use separate seeded RNG
streams. The empirical 20th percentile and its binomial confidence interval are recorded.
For internally converted models, the equivalent slope is
`1 - sqrt(-q20_CSSE)`: this measures a two-sided distance from slope 1, and is
not a raw one-sided slope quantile.

CSV summaries and session information are tracked; raw draws, fitted objects
and stage traces are saved locally as ignored RDS files. An optional output
folder is the first argument. `--baseline-only` and `--fixed-only` allow the
expensive baseline and fixed runs to be executed separately. A saved baseline
stage-1 RDS skips its rerun. CSV summaries preserve existing rows when resuming.
`--scenarios=ridge_reported,glm_control`, `--seeds=47` and `--gp-reps=1000` allow
focused runs. Do not run multiple processes into the same output folder. Use an
empty output folder to rerun everything.

The oracle check isolates test-set error using the known true predictor rather
than fitting a model. It evaluates seed 47's original fixed test draw, then 1,000
independent oracle test draws at each of 30,000 and 120,000 participants:

```sh
Rscript validation/calibration-slope-search/check-oracle.R
```

The public-API check also runs the original ridge scenario with seed 47 and
1,000 search reps, without mocks, supplied bounds or a stage RNG reset, then
validates its returned N with 1,000 independent replicates:

```sh
Rscript validation/calibration-slope-search/check-production.R validation/calibration-slope-search/results-production-final
```

The recorded ordinary GP fits were reused because the confirmation step is
appended after the unchanged GP search. To reproduce their separate confirmation
stream (seed `6000000 + scenario seed`) before independent validation:

```sh
Rscript validation/calibration-slope-search/confirm-saved.R validation/calibration-slope-search/results-empirical47 validation/calibration-slope-search/results-confirmed1000-47
```

Use the corresponding seed-48 folders for the other half of the study. The
public check above runs the complete final API with normal RNG continuation.

Saved ordinary searches can be validated independently without rerunning the GP:

```sh
Rscript validation/calibration-slope-search/validate-results.R validation/calibration-slope-search/results validation/calibration-slope-search/results
Rscript validation/calibration-slope-search/replay-traces.R
Rscript validation/calibration-slope-search/plot-results.R
```

The tracked final control checks use `validate-results.R`: 1,000 reps per point,
fresh test data followed by training data, and a deterministic per-replicate
seed `4000000 + 10000 * seed + 1000 * factor_n + i`. This stream differs from
`run-study.R`'s sequential validation stream. The public check uses seed
`3000000 + i`; the oracle check uses `2000000 + test_n`.
Saved validation RDS files are reused; remove them or use an empty validation
output directory for fresh draws. Saved searches must be regenerated after
changes to search code. Replay verifies that reused preliminary traces preserve
the final algorithm's evaluation order, batch count, bounds and stop reason.

See [the recorded results](results/REPORT.md) and [validation plot](results/validation.png).

This is an exploratory validation of the mechanism, with pointwise intervals.
Repeated looks during adaptive search are not a simultaneous 95% coverage
claim. The study does not audit the full chatbot cache or establish stability
for all targets, model families or survival scenarios. Confirmation adds bounded independent checks but cannot eliminate all
Monte Carlo error. The holdout results assess that remaining uncertainty.
The correction can give a conservative N and does not prove a global minimum.

The generality audit uses 1,000 synthetic replicate values per candidate to
show limitations of plateau stopping and confirmation of excessive Ns:

```sh
Rscript validation/calibration-slope-search/audit-generality.R
```
