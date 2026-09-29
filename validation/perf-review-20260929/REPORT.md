# pmsims performance review

**Date:** 2026-09-29
**Code profiled:** `faster-correlation` @ `bcf0dbe` (dev + Gaussian-copula correlation fix), loaded with `pkgload::load_all()`
**Machine:** 18-core Apple Silicon, R 4.6.0, reference BLAS/LAPACK
**Line references** are to `bcf0dbe`.

## Summary

The GP search stage accounts for 90–99% of runtime in every scenario. Tuning, start values and the mlpwr surrogate itself are cheap, so the cost is in the per-replicate loop. That loop always does the same three things:

1. **It generates a fresh 30,000-row test set on every replicate** (`engines.R:231`). This is the most widespread slowdown. It takes 30–50% of runtime for GLM/LM and 91% with t-distributed predictors. The bisection engine, mlpwr-bs and the adaptive stage all generate the test set once and reuse it.
2. **It scores 30,000 test rows using heavyweight general-purpose routines.** Examples are `pROC::auc`, formula-interface `glm()` for the calibration slope, and a `coxph()` fit on the test set just to get a baseline hazard. Metric evaluation is 48–51% of runtime for binary GLM and 90% for survival Cox. Each of these has an exact, much cheaper alternative.
3. **For random forests, it predicts the forest on 30,000 test rows.** This is inherent to the test size and takes about two-thirds of wall time. The fixed cost is made worse by a `detectCores()` system call on every fit and predict, and by using 16 threads for tiny training sets.

The fixes below give identical results for the metrics I checked, and none of them change the statistical design. The exception is Fix 1, which changes what each replicate's test set is (see the caveat there). Implemented together, they should roughly halve runtime for the common GLM/LM/Cox cases and cut runs with non-normal predictors by about 80%.

## Scenarios and headline timings

All calls use package defaults: `n_reps_total = 1000`, `test_n = 30000`, adaptive start values on, `correlation = 0.3`, `mean_or_assurance = "assurance"`, seed 2026. Each scenario ran in its own R process, one at a time.

| Scenario | Call | Wall time | Peak RSS | min_n |
|---|---|---:|---:|---:|
| `cont_lm_p15` | continuous, lm, 15 predictors, R² 0.5, slope 0.95 (vignette) | 21 s | 0.57 GB | 752 |
| `bin_glm_p20` | binary, glm, 20 predictors, prev 0.3, C 0.8, slope 0.85 (vignette) | 45 s | 1.0 GB | 1096 |
| `bin_glm_c3_p10` | binary, glm, complexity 3, 10 predictors, AUC 0.75 | 42 s | 0.86 GB | 149 |
| `cont_lm_t_p10` | continuous, lm, 10 **t-distributed** predictors, slope 0.9 | 94 s | 0.48 GB | 183 |
| `bin_lasso_p20` | binary, lasso, 10 signal + 10 noise, slope 0.9 | 116 s | 0.95 GB | 1490 |
| `bin_rf_p10` | binary, rf, 10 predictors, AUC 0.75 | 145 s | 0.81 GB | 80 |
| `surv_cox_p10` | survival, coxph, 10 predictors, C 0.7, 30% censored, slope 0.9 | 154 s | 0.64 GB | 608 |
| `surv_rf_p10` | survival, rf, 10 predictors, C-index 0.67 | 582 s | 1.2 GB | 95 |

## Where the time goes

This table shows each scenario's share of profiled time. For the two RF rows Rprof sums CPU time across ranger's threads, which exaggerates the prediction share; the wall-clock split is given in the RF section below.

| Scenario | Test-set generation | Metric (predict + score) | Model fit | mlpwr GP | Tuning | Other |
|---|---:|---:|---:|---:|---:|---:|
| `cont_lm_p15` | **49%** | 24% | 4% | 14% | 1% | 8% |
| `bin_glm_p20` | **30%** | **48%** | 4% | 10% | 4% | 4% |
| `bin_glm_c3_p10` | **29%** | **51%** | 1% | 12% | 4% | 3% |
| `cont_lm_t_p10` | **91%** | 4% | 1% | 2% | 0% | 2% |
| `bin_lasso_p20` | 11% | 17% | **67%** | 3% | 1% | 1% |
| `surv_cox_p10` | 5% | **90%** | 1% | 2% | 1% | 1% |
| `bin_rf_p10` | 0% | **98%** (CPU) | 1% | 0% | 0% | 0% |
| `surv_rf_p10` | 0% | **99%** (CPU) | 1% | 0% | 0% | 0% |

Generating training data is under 2% of runtime everywhere: training sets are small, and the test set dominates. The mlpwr surrogate (GP fit + `genoud` optimisation) is a fixed 1–3 s per run. Tuning is 0.1–1.5 s per run.

## Findings, ranked by impact

### 1. Test set regenerated on every replicate (all scenarios)

`calculate_mlpwr()`'s `mlpwr_simulation_function` calls `data_function(test_n)` inside the replicate (`R/engines.R:231`). A run therefore builds more than 1,000 test sets of 30,000 rows. `calculate_bisection()` (`engines.R:393`), `calculate_mlpwr_bs()` (`engines.R:657`) and `calculate_adaptive_bounds()` (`start_values.R:186`) each build one test set and reuse it.

Cost of one `data_function(30000)` call at p = 20: about 14 ms for binary and continuous, 17 ms for survival. Most of that is `rnorm(n*p)` (7.4 ms). Some configurations add a large per-dataset cost on top:

- **t predictors:** `normal_to_family()` computes `qt(pnorm(-|Z|), df = 5)` (`data_generators.R:446`), and `qt` is slow: **77 ms** for 30,000 × 10 against 0.05 ms for normal. This is why `cont_lm_t_p10` spends 91% of its time generating test sets.
- **Complexity 2/3:** every dataset, test sets included, builds the interaction sum and residualises it with a formula-interface `lm()` (`data_generators.R:754, 766`). That adds about 32 ms per 30,000-row dataset at p = 20.

**Fix:** generate `test_data` once, outside `mlpwr_simulation_function`, as the other engines already do.

**Estimated saving:** about 9 s of 21 s for `cont_lm_p15`, 11 s of 45 s for `bin_glm_p20`, 10 s of 42 s for C3, and **75 s of 94 s** for the t-distribution case.

**Caveat:** this is a design choice for you to make, not a pure refactor. At present each replicate's metric includes a fresh test-set draw, so a small amount of test-set sampling noise feeds into the replicate variance, and therefore into the assurance quantile and the GP noise estimate. At test_n = 30,000 that noise is negligible next to the training-set variation, and a fixed test set would match what the other engines do. It will still change results at a fixed seed, so it needs a check against the validation runs.

### 2. Binary AUC computed with `pROC::auc` (binary AUC scenarios)

`binary_auc_metric()` (`metric_generators.R:331`) takes **22 ms** per call on 30,000 rows. Most of that is pROC's `factor()`/`%in%` input handling. The package already has an exact Mann–Whitney implementation, `cstat_full()` in `binary_tuning.R`, which takes **2.1 ms** and gives an identical value (0.8810584459 from both).

**Estimated saving:** about 20 ms per replicate, which is about 17 s of 42 s for `bin_glm_c3_p10` and about 20 s of 145 s for `bin_rf_p10`.

### 3. Binary calibration slope uses formula `glm()` plus `predict.glm()` (binary GLM and lasso with slope or CSSE)

`binary_calib_slope()` and `binary_csse()` (`metric_generators.R:335–365`) take about 22 ms per call. The time splits into `predict.glm` (4.8 ms, mostly `model.frame` overhead on a 30,000-row data frame) and `glm(y ~ y_link)` (17 ms).

The same slope can be computed with `glm.fit(cbind(1, lp), y, binomial(), start = c(0, 1))`. Starting at the calibrated value converges in 3 IRLS iterations. That takes **7.7 ms** and gives an identical coefficient (0.90812279 from both). For GLM fits the linear predictor can also be computed as `cbind(1, X) %*% coef(fit)` (0.45 ms). Together the metric drops from about 22 ms to about 12 ms.

**Estimated saving:** about 10 s per 1,000 replicates, which is about 22% of `bin_glm_p20`. The same pattern (a formula `glm` refit on the test set) appears in `rf_recal_binary()` and the survival metrics.

### 4. Survival calibration slope fits `coxph()` on the test set to get a baseline hazard (survival scenarios)

`simulate_survival(metric = "calibration_slope")` routes to `survival_calib_slope()`, which costs **114 ms per replicate** at 30,000 rows. That breaks down as:

| Step | Line | Cost |
|---|---|---:|
| `predicted_survival_at_time()`: `coxph(Surv ~ offset(lp))` + `basehaz()` | `metric_generators.R:848–858` | **66 ms** |
| `ipcw_binary_at_time()`: KM of censoring distribution via `survfit` | `:867` | 12 ms |
| Weighted cloglog `glm()` | `:498–505` | 16 ms (12 ms with `glm.fit`) |
| `data[order(data$time), ]` data-frame reorder, predict, etc. | | the rest |

The `coxph` call exists only to compute the baseline cumulative hazard at one time point, with the linear predictor held fixed as an offset. That can be computed directly as `sum(d_i / Σ_{risk set} exp(lp))` over events up to t*, using a reverse cumulative sum on the already-sorted data. This takes **0.5 ms**.

**Caveat:** my quick implementation gave H0 = 0.27319 against `basehaz`'s 0.27263. `coxph` defaults to Efron ties and `basehaz` then returns the Efron-adjusted estimate. To be a pure refactor, the direct version has to reproduce that tie handling. Alternatively, you could accept the Breslow form deliberately.

**Estimated saving:** about 65–70 ms per replicate, roughly **70 s of 154 s** for `surv_cox_p10`. The same code path is used by rf/xgboost survival models.

### 5. Random-forest prediction on 30,000 test rows (RF scenarios)

Measured in wall-clock time with p = 10 and a 30,000-row test set:

| | 1 thread | 2 | 4 | 16 (current) |
|---|---:|---:|---:|---:|
| binary probability forest `predict` | 432 ms | 240 | 145 | **85 ms** |
| survival forest `predict` | 3,345 ms | 1,703 | 854 | **344 ms** |

Over roughly 1,100 replicates, this is about 95 s of `bin_rf_p10`'s 145 s and about 400 s of `surv_rf_p10`'s 582 s. The survival forest returns a 30,000 × (number of unique death times) matrix for both survival and cumulative hazard, and ranger has no way to request a single column. This cost is inherent to predicting 30,000 rows. The levers are:

- **A smaller `test_n` for forest models.** This scales roughly linearly and is the only large lever. Whether it is acceptable is a statistical question: the Monte Carlo error of a C-index or calibration slope at, say, 10,000 rows is still small next to the training-set variation.
- **`parallel::detectCores(logical = FALSE)` on every call.** On macOS this shells out to `sysctl` and costs **6.4 ms** per call. It runs on every rf fit (`model_generators.R:90, 175`) and every rf predict (`metric_generators.R:150`), about 2,000 times per binary-rf run, or about 13 s. It should be computed once, for example at package load or cached in an option.
- **Thread count for fits.** Binary and continuous rf fits use `ncores - 2` = 16 threads on training sets of 40–150 rows. Thread start-up dominates at that size: `bin_rf_p10` shows 224 s of system CPU against 145 s wall. Survival rf already hard-codes 2 threads for the fit. A fixed small thread count for fits would reduce contention.

### 6. Lasso: `cv.glmnet` dominates (lasso scenarios)

`cv.glmnet` (10-fold, full lambda path) is 67% of `bin_lasso_p20`, which is expected. If this needs to be faster, the options are modelling choices: `nfolds = 5`, a shorter `nlambda`, or reusing the lambda sequence between replicates. It is not a code inefficiency. The remaining lasso time is covered by Fixes 1 and 3 (`binary_csse`, line 355, about 12%).

### 7. Smaller items

- **Complexity 3 interaction term** (`data_generators.R:754`) is built in O(n·p²) with a `vapply` of `rowSums`. The identity Σ_{j<k} x_j x_k = (L² − Σ x_j²)/2, with L = Σ x_j, gives it in O(n·p). Residualising with `.lm.fit(cbind(1, Xs), Nraw)$residuals` instead of `residuals(lm(Nraw ~ Xs))` gives identical residuals and is about 1.7× faster. Most of this disappears with Fix 1, but it still runs on every training set.
- **Equicorrelated draws:** `Z %*% chol(R)` costs 3 ms at 30,000 × 20 with reference BLAS. For a common correlation, `sqrt(1-ρ)·E + sqrt(ρ)·w·1ᵀ` is O(np) rather than O(np²). This is minor next to `rnorm`, and it is on the path the `faster-correlation` branch has just changed, so I've left it alone.
- **mlpwr `hush()`** wraps every replicate in `sink("/dev/null")` plus `Sys.info()`. I measured 0.04 ms per call, which is negligible. `sink` shows 1–2 s of self time in the profiles, but that looks like GC being attributed to it rather than a real cost.
- **Tuning** costs 0.1–1.5 s per run, mostly the `binary_tuning` bisection on 300,000 simulated rows. It isn't worth touching.

## Estimated effect of Fixes 1–5

These are rough estimates: I measured each saving per replicate and scaled it by about 1,050–1,100 replicates. I have not re-profiled with the fixes applied.

| Scenario | Now | Main fixes | Estimated after |
|---|---:|---|---:|
| `cont_lm_p15` | 21 s | 1 | ~12 s |
| `bin_glm_p20` | 45 s | 1, 3 | ~24 s |
| `bin_glm_c3_p10` | 42 s | 1, 2 | ~15 s |
| `cont_lm_t_p10` | 94 s | 1 | ~18 s |
| `bin_lasso_p20` | 116 s | 1, 3 | ~95 s |
| `surv_cox_p10` | 154 s | 1, 4 | ~80 s |
| `bin_rf_p10` | 145 s | 2, 5 (detectCores, threads) | ~105 s (lower with a smaller `test_n`) |
| `surv_rf_p10` | 582 s | 5 (detectCores) | ~570 s (only a smaller `test_n` helps materially) |

## Observed in passing (not performance)

- `cont_lm_p15` returned min_n = 752, exactly the lower search bound. `bin_rf_p10` returned 80, exactly the upper bound. Both may be legitimate, but edge solutions are worth checking.
- `predicted_survival_at_time()` estimates the Breslow baseline hazard on the **test** data, with the model's linear predictor as an offset. That is a form of recalibration-in-the-large on the validation set. It may be intended, but it affects what the "calibration slope" measures.
- xgboost is not installed on this machine, so it was not profiled. Its per-replicate `xgb.cv` (5-fold, up to 500 rounds) is likely to behave like the lasso case: dominated by the fit.

## Reproducing

The scripts are in `scripts/`. They expect `PMSIMS_PERF_DIR` to contain a checkout of the branch at `pmsims-fc/` and the scripts at `perf/`:

```bash
git worktree add --detach "$PMSIMS_PERF_DIR/pmsims-fc" faster-correlation
mkdir -p "$PMSIMS_PERF_DIR/perf" && cp scripts/* "$PMSIMS_PERF_DIR/perf/"
PMSIMS_PERF_DIR=... "$PMSIMS_PERF_DIR/perf/run_all.sh"   # ~20 min: Rprof each scenario
PMSIMS_PERF_DIR=... Rscript "$PMSIMS_PERF_DIR/perf/analyse.R" # stage x component attribution
PMSIMS_PERF_DIR=... Rscript "$PMSIMS_PERF_DIR/perf/bench.R"   # micro-benchmarks
```

`analyse.R` assigns each Rprof sample to a stage (tuning / adaptive / GP search / post-hoc) and a component. It uses the call-site line of `data_function` to tell a test-set draw from a training-set draw.
