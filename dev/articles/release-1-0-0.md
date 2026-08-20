# pmsims 1.0.0: More flexible data generation

`pmsims` 1.0.0 is the first stable release of the package. The [0.5.0
release](https://pmsims-package.github.io/pmsims/dev/articles/release-0-5-0.md)
established the simulation-based framework for minimum sample size
estimation; 1.0.0 focuses on making the simulated data look more like
the data researchers actually work with, and on making the sample-size
search more dependable when it does.

## More realistic data-generating mechanisms

In 0.5.0 the simulated signal was linear. That is a reasonable starting
point, but it flatters models that assume linearity and it understates
how much data a flexible model needs. 1.0.0 adds four levels of signal
complexity, selected with the new `complexity` argument:

| `complexity` | Signal structure                       |
|--------------|----------------------------------------|
| `1`          | Purely linear (the 0.5.0 behaviour)    |
| `2`          | Linear + quadratic                     |
| `3`          | Linear + quadratic + interaction       |
| `4`          | The Friedman (1991) benchmark function |

Alongside this, a new `data_control` list gives finer control over the
predictors themselves: `nonlinear_strength` (the fraction of signal
variance carried by the nonlinear, linearly-inaccessible component),
`correlation` between candidate predictors, the `predictor_distribution`
(normal, uniform, binary, exponential, lognormal, t, or Laplace), and
`binary_predictor_prevalence` for binary predictors.

These options only work if the tuning step that calibrates the data
generator against `maximum_achievable_cstatistic` (or
`maximum_achievable_rsquared` / `maximum_achievable_cindex`) sees the
same world as the simulation itself. The binary, continuous, and
survival tuners have therefore been updated to build their unit linear
predictor under the *same* data-generating configuration used for the
sample-size simulation, so that the requested large-sample performance
is the performance actually recovered.

## Models and metrics

Ridge regression joins lasso, random forest, and XGBoost as a model
option across binary, continuous, and survival outcomes, and the
random-forest defaults and XGBoost tuning have been refined. These sit
alongside the regression-based models (linear, logistic, and Cox) as
fully supported choices.

There is also a new metric, calibration slope squared error (`"csse"`),
defined as \\-(1 - s)^2\\ for a calibration slope \\s\\, so that larger
is better and `0` is perfect calibration. Unlike the calibration slope
itself, it penalises slopes above 1 as well as below it, which matters
for models that can under-fit as well as over-fit.

The reason it exists is monotonicity. The search looks for the sample
size at which performance *equals* the requested target, and that is
only well defined when the metric improves monotonically with \\n\\. The
calibration slope behaves that way for classical regression models,
which overfit at small \\n\\ and approach 1 from below. Penalised and
tree-based learners are different: depending on how strongly the fit is
shrunk, their slope can approach 1 from either side, so a one-sided
slope target can be met at more than one sample size and the search has
no unique solution. Because CSSE measures squared distance from perfect
calibration, it improves monotonically whichever direction the
miscalibration runs in, which restores a single well-defined crossing.

You mostly do not need to think about this. When you ask for
`metric = "calibration_slope"` with one of the machine-learning models,
`pmsims` optimises on the CSSE scale internally and translates the
answer back before returning it — `target_performance` is still given on
the calibration slope scale, and results obtained this way are marked
with a dagger in the printed output. `"csse"` can also be requested
directly, in which case you supply `target_performance` on the CSSE
scale (a slope target of `0.9` corresponds to a CSSE target of `-0.01`)
and results are reported on that scale.

Finally, metric identifiers now take one canonical form everywhere —
wrapper inputs, metric generators, returned objects, and printed output
all use `"calibration_slope"`, `"calibration_in_the_large"`, `"auc"`,
`"r2"`, `"cindex"`, and `"csse"`.

## A more robust search

Several changes make the sample-size search less likely to fail or
mislead: adaptive bounds have been improved, targets that appear to be
unreachable are now detected and reported rather than searched for
indefinitely, failed model fits and metric calculations fall back more
gracefully, and the metadata returned by the wrappers has been expanded
and standardised so that printed summaries say more about what was
actually run.

Long runs are also flagged before they start. The adaptive first stage
is timed and extrapolated to the full search, so you find out what you
are committing to while it is still cheap to change your mind: a run
expected to take more than 30 minutes reports the estimate, and one
expected to take more than two hours also warns and asks for
confirmation before continuing. The prompt only appears in interactive
sessions, so scripts, CI and vignette builds are unaffected, and
`options(pmsims.confirm_long_runs = FALSE)` switches it off.

## Upgrading from 0.5.0

Predictor configuration has moved. The top-level `predictor_type` and
`binary_predictor_prevalence` arguments are gone; both are now expressed
through `complexity` and `data_control`. In 0.5.0 you would write:

``` r

simulate_binary(
  signal_parameters = 20,
  noise_parameters = 0,
  predictor_type = "continuous",
  binary_predictor_prevalence = NULL,
  outcome_prevalence = 0.30,
  maximum_achievable_cstatistic = 0.80,
  model = "glm",
  metric = "calibration_slope",
  target_performance = 0.85,
  n_reps_total = 1000,
  mean_or_assurance = "assurance"
)
```

The 1.0.0 equivalent is:

``` r

simulate_binary(
  signal_parameters = 20,
  noise_parameters = 0,
  complexity = 1,
  data_control = list(correlation = 0.3),
  outcome_prevalence = 0.30,
  maximum_achievable_cstatistic = 0.80,
  model = "glm",
  metric = "calibration_slope",
  target_performance = 0.85,
  n_reps_total = 1000,
  mean_or_assurance = "assurance"
)
```

One thing to watch when comparing against 0.5.0: `complexity` defaults
to `1`, so the default signal structure is unchanged, but
`data_control$correlation` defaults to `0.3`, whereas 0.5.0 generated
independent predictors. Sample sizes are therefore not directly
comparable across versions unless you set `correlation = 0` explicitly.

The [getting-started
vignette](https://pmsims-package.github.io/pmsims/dev/articles/pmsims.md)
and the [function
reference](https://pmsims-package.github.io/pmsims/dev/reference/index.md)
document the current interface in full.

## Install

``` r

# install.packages("remotes")
remotes::install_github("pmsims-package/pmsims", ref = "v1.0.0")
```

Version `1.0.0` is available from GitHub and is not yet a CRAN release.

As with 0.5.0, the package and its validation work are described in two
accompanying preprints: the overview paper by [Shamsutdinova et
al. (2026)](https://arxiv.org/abs/2602.23507) and the validation paper
by [Olaniran et al. (2026)](https://arxiv.org/abs/2603.23688).
