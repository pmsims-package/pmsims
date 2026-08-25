# pmsims 1.0.0: More flexible data generation

`pmsims` 1.0.0 is the first stable release of the package. The [0.5.0
release](https://pmsims-package.github.io/pmsims/articles/release-0-5-0.md)
established the simulation-based framework for minimum sample size
estimation; 1.0.0 focuses on making the simulated data look more like
the data researchers actually work with, and on making the sample-size
search more dependable.

## More realistic data-generating mechanisms

In 0.5.0 the simulated signal was always linear, which makes life easy
for models that assume linearity and understates the sample size needed
by more flexible models. 1.0.0 adds four levels of signal complexity,
set with the new `complexity` argument:

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

These options change the data, so the tuning step that calibrates the
data generator against `maximum_achievable_cstatistic` (or
`maximum_achievable_rsquared` / `maximum_achievable_cindex`) has to take
them into account as well. The binary, continuous, and survival tuners
now build their unit linear predictor under the same data-generating
configuration as the sample-size simulation, so that the simulation
achieves the maximum achievable performance you asked for.

## Models and metrics

Ridge regression is new in 1.0.0. The full set of models is now:

| Outcome    | `model` options                                      |
|------------|------------------------------------------------------|
| Binary     | `"glm"`, `"lasso"`, `"ridge"`, `"rf"`, `"xgboost"`   |
| Continuous | `"lm"`, `"lasso"`, `"ridge"`, `"rf"`, `"xgboost"`    |
| Survival   | `"coxph"`, `"lasso"`, `"ridge"`, `"rf"`, `"xgboost"` |

Random-forest defaults and XGBoost tuning have also been refined.

There is also a new metric, calibration slope squared error (`"csse"`),
defined as \\-(1 - s)^2\\ for a calibration slope \\s\\, so that larger
is better and `0` is perfect calibration. We added it because the search
needs a metric that improves steadily as \\n\\ grows: it looks for the
sample size at which performance *equals* the requested target, and that
sample size is only unique if the metric moves in one direction. The
calibration slope does this for classical regression models, which
overfit at small \\n\\ and approach 1 from below. Penalised and
tree-based models can approach 1 from either side, depending on how
strongly the fit is shrunk, so a one-sided slope target can be met at
more than one sample size. CSSE measures squared distance from perfect
calibration, so it improves whichever direction the miscalibration runs
in.

In most cases this is handled for you. With
`metric = "calibration_slope"` and one of the machine-learning models,
`pmsims` optimises on the CSSE scale internally and converts the result
back. `target_performance` is still supplied on the calibration slope
scale, and results obtained this way are marked with a dagger in the
printed output.

`"csse"` can also be requested directly. In that case
`target_performance` is supplied on the CSSE scale (a slope target of
`0.9` corresponds to a CSSE target of `-0.01`) and results are reported
on that scale.

Finally, metric identifiers now take one canonical form everywhere —
wrapper inputs, metric generators, returned objects, and printed output
all use `"calibration_slope"`, `"calibration_in_the_large"`, `"auc"`,
`"r2"`, `"cindex"`, and `"csse"`.

## A more robust search

Several changes make the sample-size search less likely to fail:
adaptive bounds have been improved, targets that appear to be
unreachable are now detected and reported rather than searched for
indefinitely, failed model fits and metric calculations fall back more
gracefully, and the metadata returned by the wrappers has been expanded
and standardised so that printed summaries say more about what was
actually run.

Long runs are also flagged before they start. The adaptive first stage
is timed and extrapolated to the full search, so you find out what you
are committing to before running. The prompt only appears in interactive
sessions, so scripts, CI and vignette builds are unaffected. It can be
disabled with `options(pmsims.confirm_long_runs = FALSE)`.

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

One thing to note when comparing against 0.5.0: `complexity` defaults to
`1`, so the default signal structure is unchanged, but
`data_control$correlation` defaults to `0.3`, whereas 0.5.0 generated
independent predictors. Sample sizes are therefore not directly
comparable across versions unless you set `correlation = 0` explicitly.

The [getting-started
vignette](https://pmsims-package.github.io/pmsims/articles/pmsims.md)
and the [function
reference](https://pmsims-package.github.io/pmsims/reference/index.md)
document the current interface in full.

## Install

``` r

# install.packages("remotes")
remotes::install_github("pmsims-package/pmsims", ref = "v1.0.0")
```

Version `1.0.0` is available from GitHub and is not yet a CRAN release.

As with 0.5.0, the package and its validation work are described in two
accompanying papers: the overview paper by [Shamsutdinova et
al. (2026)](https://arxiv.org/abs/2602.23507), currently a preprint, and
the validation paper by [Olaniran et
al. (2026)](https://doi.org/10.1186/s12874-026-02935-9), published in
*BMC Medical Research Methodology*.
