# pmsims 1.1.0: A learning-curve search engine

`pmsims` 1.1.0 introduces a new learning-curve search engine, which is
now the default.

## A new search engine

In 1.0.0, the search had two stages: an adaptive search for start
values, followed by a Gaussian-process search using the `mlpwr` package.
In 1.1.0 the default is a learning-curve engine, `method = "curve"`:

- Every replicate is kept, and a monotone learning curve, \\C(n) = a - b
  n^{-c}\\, is fitted to the criterion (e.g. the 20th percentile of the
  calibration slope) at each sample size simulated so far.
- A pilot doubles or halves \\n\\ from a heuristic start until the
  criterion has been seen on both sides of the target. Each further
  batch of replicates is placed where the fitted curve crosses the
  target.
- The minimum sample size is where the final curve crosses the target.
  It is a median-unbiased estimate, reported with a bootstrap interval
  (`diagnostics$curve$n_ci`) and a shape-free cross-check
  (`diagnostics$crosscheck_n`).

The previous engine is still available with `method = "mlpwr"`.

## Seeing the search

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) shows the
criterion at each simulated sample size, the fitted learning curve, the
target, and the minimum sample size with its interval. With
`live_plot = TRUE`, the plot is redrawn after each batch of replicates
during the search. The [getting-started
vignette](https://pmsims-package.github.io/pmsims/articles/pmsims.md)
shows an example.

The `cores` argument runs the replicates in each batch in parallel (on
Unix-like systems). Each replicate has its own random-number stream, so
results are reproducible with
[`set.seed()`](https://rdrr.io/r/base/Random.html) and are the same for
any number of cores.

## Checking the result

Every result now has a `status`. The minimum sample size is simulated
again with fresh replicates (`verify_reps`, default 100), and results
that fall clearly below the target are flagged:

| `status` | Meaning |
|----|----|
| `"ok"` | A sample size was found, and simulating it again confirmed the target is met |
| `"not_verified"` | Simulating the minimum sample size again gave performance clearly below the target |
| `"not_bracketed"` | No sample size up to `max_n` met the target |
| `"unreachable"` | The learning curve levels off below the target (curve engine only) |
| `"replicates_failed"` | Too many replicates failed to fit or score the model |

When no sample size is found, `min_n` is `NA` and `status_message`
explains why. For an unreachable target, the message says what
performance is achievable.

## Corrections

Several corrections were made to the simulation, so sample sizes may
differ from those given by 1.0.0. These include:

- Correlated predictors now have the requested pairwise correlation.
- Replicates whose metric is not finite count as failed replicates.
- Metrics where smaller is better (`"calibration_in_the_large"`,
  `"brier_score"`, `"ibs"`) can no longer be used as search targets.
- The concordance fallback in `survival_auc` has been corrected.

## Speed

Calibration slopes, AUC, concordance, linear predictors and correlated
predictors are all faster to compute, with unchanged results. A binary
`glm` search takes about two thirds of the time it did, and a Cox search
about a quarter.

## Upgrading from 1.0.0

- To use the previous search engine, set `method = "mlpwr"`.
- `min_n` and `perf_n` are `NA` when no sample size is found (they were
  a character message). Check `status` and `status_message`.
- The `simulate_*()` wrappers no longer suppress warnings. A result
  whose status is not `"ok"` raises a warning with its `status_message`.
- `metric_2_at_n` is now the mean over 10 replicates at `min_n` (it was
  a single replicate).
- Custom metric functions without a `"metric"` attribute count failed
  replicates as 0.5, with a message. Set
  `attr(metric_function, "value_on_error")` to choose another value.
- With `method = "mlpwr"`, `n_reps_total` is raised to 2000 for `"lm"`,
  `"glm"` and `"coxph"` models with at most 10 signal parameters. A
  message says so, and `auto_n_reps_total = FALSE` keeps the value
  supplied.

The [changelog](https://pmsims-package.github.io/pmsims/news/index.md)
lists every change.

## Install

``` r

install.packages("pmsims")
```

The overview paper by [Shamsutdinova et
al. (2026)](https://arxiv.org/abs/2602.23507) and the validation paper
by [Olaniran et al. (2026)](https://doi.org/10.1186/s12874-026-02935-9)
describe the package and its validation.
