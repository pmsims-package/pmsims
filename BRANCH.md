# Branch `release/mlpwr-fixes`: the mlpwr approach, with fixes

Base: `dev` at `e7e35ff`. The search is unchanged in design: adaptive start
values, then mlpwr's Gaussian-process search. This branch fixes known errors
and makes results checkable. It is meant to change as little as possible.
The new engine is a separate branch, `release/curve-engine`, built on this
one.

## What it contains

| Commit | What it does |
|---|---|
| updated adaptive start values | Ridwan's start-value search (fix-peaks). |
| Gaussian copula | **Correlated predictors were wrong**: the Cholesky factor was applied transposed, so pairwise correlations ranged from about 0 to 0.88 (mean 0.09) instead of the requested 0.3. Predictors are now drawn directly, which is also 3–7× faster. Results for any scenario with `correlation > 0` (the default is 0.3) change. |
| Non-finite metric values | NA or NaN metric values (e.g. a lasso that selects no predictors) count as failed replicates. They were dropped, which overstated performance. |
| GP restart | The mlpwr search restarts when its surrogate fails, instead of erroring. |
| Shared evaluator, keyed streams, result checks | Every stage evaluates replicates the same way: train on fresh data, score on a fresh test set. The start-value stage used one shared test set, so it could bracket the wrong range. Each replicate has its own random-number stream, so results are reproducible with `set.seed()` and identical on any number of cores. The returned `min_n` is simulated again (`verify_reps = 100`), and the result has a `status`: `ok`, `not_verified`, `not_bracketed` or `replicates_failed`. Metrics where smaller is better are rejected (they were searched in the wrong direction). |
| survival_auc, ranger threads | `survival_auc()`'s fallback returned 1 − C. ranger could be given zero threads. |
| Truth tests | Simulated data checked against prevalence, discrimination, calibration, correlation and metric direction. |
| xgboost threads | xgboost used every core, which oversubscribed the CPU: one replicate could take 175 s instead of under 1 s. |
| `max_n` by model | Default cap of 200,000 for random forests and xgboost, 1,000,000 otherwise. |
| mlpwr's optimistic pick (docs) | Documented, not changed (see open questions). |
| air formatting, tidy-ups | No behaviour changes. |
| Review fixes | `replicates_failed` status; the start-value search's last rung is `max_n` itself; messages for calibration-slope targets in slope terms; correct wording at the upper edge of the range; `parallel = TRUE` uses several cores again. |

## Changes users will notice

These are listed under *Breaking changes* in NEWS.md:

- `min_n` and `perf_n` are `NA` when no answer is found (they were a text message); see `status` and `status_message`.
- The `simulate_*()` wrappers no longer suppress warnings.
- `metric_2_at_n` is a mean over 10 replicates (it was one).

## Evidence

From pmsims-bench, with reference sample sizes simulated directly for each scenario and seed (five sample sizes around the answer, 600 replicates each):

| Tiers | Runs | Median error | Median absolute error | Within 10% |
|---|---|---|---|---|
| core + compat | 97 | −8.2% | 9.0% | 52% |
| wide (48 scenarios × 3 seeds) | 138 | −8.7% | 9.6% | 54% |

Answers are about 9% too small, mainly because of mlpwr's final pick (below). These figures were measured just before the plateau stop was restored (open question 2). The earlier consolidated version, which has the same search, gave the same figures: −8.7%, 9.6%, 53% on the wide tier. Tests: 590 pass.

## Open questions for the team

1. **mlpwr's final pick.** mlpwr returns the smallest n at which its surrogate's mean + 0.3 SD reaches the target (fixed inside mlpwr). That causes the −9% bias above, and more for penalised and ML models. It is left unchanged here; see "How the mlpwr engine picks its answer" in `?simulate_custom`.
2. **The start-value plateau stop is kept.** On a slowly rising curve it can leave the answer above the search range; the answer is then flagged `not_verified`. Removing the stop made unreachable and near-ceiling targets double to `max_n`, causing 16 timeouts or out-of-memory failures in the benchmark with no gain in accuracy.
3. **Test-set ceiling.** With a 30,000-row test set, the slope estimate itself varies by about ±0.015. So targets such as a calibration slope of 0.99 at 20% assurance are unreachable at any n.
4. **Seed-dependent tuning.** The wrappers tune the data generator by simulation, so a target close to the maximum achievable performance can give answers about 45% apart on different seeds.
