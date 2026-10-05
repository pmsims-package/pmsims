# Branch `release/curve-engine`: a learning-curve engine as the default

Base: `release/mlpwr-fixes` (see that branch's BRANCH.md for its fixes, all included here). This branch adds a new search engine, `method = "curve"`, and makes it the default in `simulate_custom()` and the `simulate_*()` wrappers. The previous engine is still available as `method = "mlpwr"` and behaves exactly as on `release/mlpwr-fixes`.

## What it contains

| Commit | What it does |
|---|---|
| Learning-curve engine | Fits a monotone learning curve, C(n) = a − b·n^−c, to the criterion at every simulated n, using every replicate. A pilot doubles or halves from the start value until it sees the criterion on both sides of the target. Each further batch goes where the fitted curve crosses the target, so there is no fixed search range to get wrong. The answer is where the final curve crosses, with a bootstrap interval. It is median-unbiased, with no optimism margin like mlpwr's mean + 0.3 SD. Stops only on confirmed evidence: `unreachable` (the curve levels off below the target, confirmed with extra replicates) or `not_bracketed` (the crossing is very likely beyond `max_n`). Flags targets close to the best achievable performance, and cross-checks the answer against a shape-free (isotonic) fit. |
| Plot, live plot, messages, `cores` | `plot()` shows the simulated points, the fitted curve, the target and the answer with its interval; `live_plot = TRUE` redraws it during the search. Stop messages for calibration-slope targets are given in slope terms. `cores` runs each batch's replicates in parallel, and results are identical for any number of cores. |
| Faster metrics, predictions and data generation | Exact fast paths, each with a fallback to the general code: two-parameter calibration fits instead of `glm()`/`lm()` on the 30,000-row test set, a direct survival calibration slope, linear predictors without model frames, AUC from ranks, a leaner concordance, running-sum correlated predictors and survival forests predicted from terminal nodes. Replicate values agree with the old code to within 2e-14. `tests/testthat/test-fast_paths.R` checks each fast path against the code it replaces. |
| air formatting, NEWS | No behaviour changes. |
| Review fixes | Stops with `replicates_failed` when 20% or more of replicates fail at every n. Searches reach their own limits. A target met with no usable curve is answered from the observed points. A crashed parallel worker counts as a failed replicate instead of aborting the run. `cores` is validated. The documentation is accurate. |

## Evidence

From pmsims-bench, with reference sample sizes simulated directly for each scenario and seed (five sample sizes around the answer, 600 replicates each):

| Tiers | Engine | Runs | Median error | Median absolute error | Within 10% |
|---|---|---|---|---|---|
| core + compat | curve (this branch) | 96 | +1.4% | 4.4% | 82% |
| core + compat | mlpwr (`release/mlpwr-fixes`) | 97 | −8.2% | 9.0% | 52% |
| wide (48 scenarios × 3 seeds) | curve (this branch) | 138 | +0.4% | 3.6% | 94% |
| wide | mlpwr | 138 | −8.7% | 9.6% | 54% |

- **Penalised and ML models.** The gap is largest here. mlpwr's answers are 10–16% too small for these groups (by median), and 20–47% too small on some random forest, lasso, ridge and xgboost scenarios. The curve engine stays within about ±9% on all of them.
- **Unreachable targets.** The curve engine stopped correctly on 5 of 6 runs; mlpwr stopped on none.
- **Speed.** Measured on a quiet machine, single thread, against the engine before the speed-ups. A full binary glm run went from 26 s to 17 s, and a full Cox run from 52 s to 14 s, with identical answers.
- **Tests.** 752 pass.

## Known limitations and open questions

1. **Targets at or near the ceiling are slow.** Ridge with 5 predictors (where the criterion sits at the target from about 40,000 to 150,000), an edge ridge case and survival lasso p5 still time out on some seeds. A time budget for such targets is an open question.
2. **The interval reflects Monte Carlo error given the curve's shape, not uncertainty about the shape.** The isotonic cross-check is reported alongside, and flagged when it disagrees by more than 10%.
3. **The test-set ceiling and seed-dependent tuning** described in `release/mlpwr-fixes` apply to both engines.
