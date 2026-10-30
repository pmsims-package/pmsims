# Minimum sample size for custom simulation workflows

Compute the minimum sample size required to achieve a target level of
predictive performance using user-defined simulation components.
`simulate_custom()` is the low-level interface in `pmsims`: users supply
a data-generating function, a model-fitting function, and a metric
function, and the chosen search engine estimates the smallest \\n\\
meeting the selected performance criterion.

## Usage

``` r
simulate_custom(
  data_function,
  model_function,
  metric_function,
  target_performance,
  c_statistic = NULL,
  mean_or_assurance = "assurance",
  test_n = 30000,
  min_sample_size = NULL,
  max_sample_size = NULL,
  n_reps_total = 1000,
  n_reps_per = 20,
  method = "curve",
  progress = TRUE,
  verbose = FALSE,
  verify_reps = 100,
  max_n = NULL,
  cores = 1L,
  live_plot = FALSE,
  ...
)
```

## Arguments

- data_function:

  Function taking a single argument, `n`, giving the training sample
  size, and returning a dataset that can be passed to `model_function`.

- model_function:

  Function that fits a model to the dataset returned by `data_function`.
  It must take the generated dataset as its only argument and return a
  fitted model object.

- metric_function:

  Function that evaluates predictive performance on test data. It must
  take three positional arguments in the order
  `(test_data, fitted_model, model_name)` and return a single numeric
  value. Optionally, users may set
  `attr(metric_function, "value_on_error")` to a single numeric fallback
  value to be returned if model fitting or metric evaluation fails
  during a simulation run.

- target_performance:

  Numeric target value for the chosen performance metric. The search
  aims to find the smallest sample size \\n\\ for which the selected
  criterion is met relative to this threshold.

- c_statistic:

  Optional numeric value used only by the internal start-value
  heuristics for some outcome and metric combinations. In most custom
  workflows this should be left as `NULL`.

- mean_or_assurance:

  Character string specifying the criterion used to define the minimum
  sample size. Must be either `"mean"` or `"assurance"`.

- test_n:

  Integer size of the test dataset used to evaluate model performance.
  This should usually be large enough that test-set variability is
  negligible relative to the training-sample search.

- min_sample_size:

  Optional integer lower bound for the sample-size search. If supplied,
  `max_sample_size` must also be supplied.

- max_sample_size:

  Optional integer upper bound for the sample-size search. If supplied,
  `min_sample_size` must also be supplied. Supplying both bounds defines
  the search space directly, so the adaptive starting-value search is
  skipped. Because the runtime estimate is extrapolated from that stage,
  no long-run warning is issued either.

- n_reps_total:

  Integer total number of simulation replications allocated to the
  search. The search evaluates approximately `n_reps_total / n_reps_per`
  candidate sample sizes.

- n_reps_per:

  Integer number of simulation replications performed at each candidate
  sample size.

- method:

  Character string specifying the search engine: `"curve"` (default;
  fits a learning curve to all replicates and searches where it crosses
  the target, see Details), `"mlpwr"` (adaptive start values, then
  mlpwr's Gaussian-process search), `"bisection"` or `"mlpwr-bs"`.

- progress:

  Logical flag controlling whether a progress bar is shown: the curve
  engine's own, or mlpwr's for the `mlpwr`-based methods.

- verbose:

  Logical flag controlling engine-specific diagnostic output when
  supported. For the bisection engine, setting `verbose = TRUE` stores
  the iteration history on the returned object.

- verify_reps:

  Integer number of fresh replicates simulated at the returned sample
  size to check that it meets the target (see `status`). Set to `0` to
  skip the check.

- max_n:

  Largest sample size the search may try. If performance there is still
  clearly below the target, the search stops with status
  `"not_bracketed"`. Defaults to 200,000 for random forests and xgboost
  (one batch of replicates at a million rows would take hours) and
  1,000,000 otherwise.

- cores:

  Number of cores for running the replicates of each batch in parallel
  (Unix-like systems only). Results are the same for any number of
  cores, because each replicate has its own random-number stream.

- live_plot:

  Logical; with `method = "curve"`, redraw the learning curve after each
  batch of replicates so the search can be watched as it runs
  (interactive sessions with a graphics device only; it does not change
  the results).

- ...:

  Additional arguments passed to the selected search engine.

## Value

An object of class `"pmsims"` containing the estimated minimum sample
size `min_n` (numeric; `NA` when no sample size could be found) and a
`status`:

- `"ok"`:

  The search found `min_n`, and simulating it again confirmed the target
  is met (or the check was skipped).

- `"not_verified"`:

  Simulating `min_n` again gave performance clearly below the target, so
  `min_n` is likely too small.

- `"not_bracketed"`:

  No sample size up to `max_n` met the target, or the search returned no
  sample size.

- `"unreachable"`:

  With `method = "curve"`: the learning curve levels off below the
  target, so no sample size reaches it.

- `"replicates_failed"`:

  At least half the simulation replicates failed to fit or score the
  model, so no sample size was estimated. With `method = "curve"`, also
  when 20% or more failed at every sample size tried, so no learning
  curve could be fitted.

`status_message` explains a status other than `"ok"`, `verification`
holds the check at `min_n`, and `diagnostics` records the search bounds,
whether `min_n` lies on one of them, mlpwr's warnings, Gaussian-process
restarts and failed replicates.

## Details

With `method = "curve"`, every replicate is kept and a monotone learning
curve \\C(n) = a - b n^{-c}\\ is fitted to the criterion at each sample
size evaluated (weighted by a smoothed model of the replicate spread;
the ceiling \\a\\ is capped at the metric's maximum). A pilot doubles
(or halves) from a heuristic start until the criterion is seen on both
sides of the target; each further batch of `n_reps_per` replicates is
placed where the fitted curve crosses the target. The search range is
therefore never fixed. The answer is where the final fitted curve
crosses the target. A target is declared unreachable if a bootstrap
upper bound for the curve's ceiling \\a\\ is below it; the search also
stops (status `"not_bracketed"`) when the crossing is very likely beyond
`max_n`. These stops extrapolate the fitted curve and should be read as
heuristics.

The answer is a median-unbiased estimate of where the criterion reaches
the target. The interval in `diagnostics$curve$n_ci` reflects Monte
Carlo error given the learning-curve shape, not uncertainty about the
shape; a shape-free check (`diagnostics$crosscheck_n`) is reported
alongside, and flagged when it differs by more than 10%. The
verification of the answer has limited power for skewed metrics such as
CSSE: passing it does not show that the target is met.

## Random numbers

Each simulation replicate runs on its own random-number stream, derived
from a single draw from the session's RNG. Results are therefore
reproducible with [`set.seed()`](https://rdrr.io/r/base/Random.html),
and a replicate's data do not depend on how many random numbers other
parts of the search consumed.

## See also

[`simulate_binary()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_binary.md),
[`simulate_continuous()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_continuous.md),
[`simulate_survival()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_survival.md)

## Examples

``` r
# Three independent predictors with a population R-squared of 0.5.
data_fun <- function(n) {
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  y <- (x1 + x2 + x3) / sqrt(3) + rnorm(n)
  data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
}

model_fun <- function(dat) {
  stats::lm(y ~ ., data = dat)
}

# Calibration slope evaluated on independent test data.
metric_fun <- function(test_data, fit, model) {
  preds <- stats::predict(fit, newdata = test_data)
  unname(stats::coef(stats::lm(test_data$y ~ preds))[2])
}
attr(metric_fun, "metric") <- "calibration_slope"

# \donttest{
set.seed(123)
est <- simulate_custom(
  data_function = data_fun,
  model_function = model_fun,
  metric_function = metric_fun,
  target_performance = 0.9,
  mean_or_assurance = "assurance",
  min_sample_size = 25,
  max_sample_size = 1000,
  n_reps_total = 1000,
  test_n = 30000,
  progress = FALSE
)
#> ℹ Using user-specified min_sample_size and max_sample_size. Adaptive starting values will not be used.
#> ℹ Estimating the learning curve... (pilot)
#> ℹ Refining the learning curve near the target...
est
#>                     ┌────────────────────────────────────────┐
#>                     │ pmsims: Sample size simulation summary │
#>                     └────────────────────────────────────────┘
#> 
#> ──────────────────────────────────── Inputs ────────────────────────────────────
#> 
#> Model and performance
#>   Sample-size criterion    Calibration slope ≥ 0.900
#> 
#> Simulation
#>   Mode                     Assurance
#>   Replications             1,000
#> 
#> ──────────────────────────────────── Results ───────────────────────────────────
#> 
#>   Minimum sample size      100
#> 
#>   Performance at N = 100
#>     Calibration slope      0.901    (target ≥ 0.900)
#> 
#>   Running time             13 seconds
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#> Assurance mode selects N so that the target is achieved with high probability
#> across repeated datasets.
est$min_n
#> [1] 100
plot(est)

# }
```
