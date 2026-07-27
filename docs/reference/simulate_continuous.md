# Minimum sample size for continuous‐outcome prediction models

Compute the minimum sample size required to develop a prediction model
with a **continuous** outcome. This wraps the same simulation engine as
[`simulate_binary()`](https://pmsims-package.github.io/pmsims/reference/simulate_binary.md),
combining bisection search with Gaussian-process learning-curve
modelling. From user inputs (maximum achievable performance, target
performance, etc.) it constructs a data-generating function,
model-fitting function, and metric function, then searches for the
smallest \\n\\ meeting the chosen criterion.

## Usage

``` r
simulate_continuous(
  signal_parameters,
  noise_parameters = 0,
  complexity = 1,
  data_control = NULL,
  maximum_achievable_rsquared,
  model = c("lm", "lasso", "ridge", "rf", "xgboost"),
  metric = "calibration_slope",
  target_performance,
  n_reps_total = 1000,
  mean_or_assurance = "assurance",
  ...
)
```

## Arguments

- signal_parameters:

  Integer. Number of candidate predictors associated with the outcome
  (i.e., true signal features).

- noise_parameters:

  Integer. Number of candidate predictors not associated with the
  outcome (noise features). Default is 0.

- complexity:

  Integer in 1:4 selecting the data-generating signal structure (see
  *Data control*). Default `1`.

- data_control:

  Optional named list controlling the predictors (see *Data control*).
  Default `NULL` (generator defaults).

- maximum_achievable_rsquared:

  Numeric in (0, 1). Maximum achievable \\R^2\\ with effectively
  unlimited data. This is used to calibrate the data-generating
  mechanism and is not the minimum acceptable threshold.

- model:

  Character string specifying the modelling algorithm. One of `"lm"`
  (linear regression), `"lasso"`, `"ridge"`, `"rf"` (random forest), or
  `"xgboost"` (gradient-boosted trees). The machine-learning options are
  experimental.

- metric:

  Character string naming the performance metric used to assess the
  sample size; defaults to `"calibration_slope"`. (Internally mapped to
  the engine's metric identifiers.)

- target_performance:

  Numeric. Minimum acceptable value of the selected performance metric
  \\M^\*\\; the algorithm searches for the smallest \\n\\ meeting the
  chosen criterion with respect to this threshold.

- n_reps_total:

  Integer. Total number of simulation replications used by the engine
  across the search.

- mean_or_assurance:

  Character string, either `"mean"` or `"assurance"`. Controls whether
  the minimum \\n\\ is defined by the mean-based criterion or the
  assurance-based criterion (with the assurance level \\\delta\\
  controlled by the engine's defaults or additional arguments in `...`).

- ...:

  Additional options passed to
  [`simulate_custom()`](https://pmsims-package.github.io/pmsims/reference/simulate_custom.md)
  (e.g., assurance level \\\delta\\, per-iteration settings).

## Value

An object of class `"pmsims"` containing the estimated minimum sample
size and simulation diagnostics (inputs, fitted GP curve, intermediate
evaluations, and summary metrics).

## Criteria

Two formulations are supported.

- **Mean-based**: find the smallest \\n\\ such that the expected model
  performance exceeds the target \\M^\*\\, i.e. \$\$\min_n \\
  \mathbb{E}\_{D_n}\\ M \mid D_n \\ \ge M^\*.\$\$

- **Assurance-based**: find the smallest \\n\\ such that the probability
  the performance exceeds \\M^\*\\ is at least \\\delta\\ (e.g. 0.80),
  i.e. \$\$\min_n \\ \mathbb{P}\_{D_n}\\\left( M \mid D_n \ge M^\*
  \right) \ge \delta.\$\$

Here, \\M\\ is the chosen performance metric and the
probability/expectation is over repeated samples of training data of
size \\n\\. The assurance criterion explicitly accounts for variability
across training sets; models with higher variance typically require
larger \\n\\ to satisfy it.

## Data control

`complexity` selects the signal structure of the data-generating
mechanism: `1` purely linear, `2` linear + quadratic, `3` linear +
quadratic + interaction, `4` the Friedman function. `data_control` is an
optional list fine-tuning the predictors:

- `nonlinear_strength`:

  Numeric in `[0, 1)`. Fraction of the signal variance carried by the
  nonlinear, linearly-inaccessible component. Applies to complexity 2
  and 3 only; ignored (with a warning) for 1 and 4. If omitted, the
  generator's per-complexity default is used.

- `correlation`:

  Numeric in \\\[-1, 1\]\\. Pairwise correlation among the candidate
  predictors. Default `0.3`.

- `predictor_distribution`:

  One of `"normal"`, `"uniform"`, `"binary"`, `"exponential"`,
  `"lognormal"`, `"t"`, `"laplace"`. `"binary"` selects 0/1 predictors
  and requires `binary_predictor_prevalence`; any other value selects
  continuous predictors from that family. Default `"normal"`.

- `binary_predictor_prevalence`:

  Numeric in `(0, 1)`. Prevalence of the binary predictors; required
  when `predictor_distribution = "binary"`, ignored (with a warning)
  otherwise. Note: binary predictors are incompatible with complexity
  2/3 because squaring a 0/1 variable returns itself.

## See also

[`simulate_binary()`](https://pmsims-package.github.io/pmsims/reference/simulate_binary.md),
[`simulate_survival()`](https://pmsims-package.github.io/pmsims/reference/simulate_survival.md),
[`simulate_custom()`](https://pmsims-package.github.io/pmsims/reference/simulate_custom.md)

## Examples

``` r
# \donttest{
set.seed(123)
# Small budgets keep this example fast; use larger values for an analysis.
est <- simulate_continuous(
  signal_parameters = 3,
  noise_parameters = 2,
  maximum_achievable_rsquared = 0.3,
  model = "lm",
  metric = "r2",
  target_performance = 0.15,
  n_reps_total = 20,
  mean_or_assurance = "mean",
  method = "bisection",
  min_sample_size = 20,
  max_sample_size = 80,
  n_reps_per = 5,
  test_n = 200,
  progress = FALSE
)
#> Using user-specified min_sample_size and max_sample_size. Adaptive starting values will not be used.
est
#>                     ┌────────────────────────────────────────┐
#>                     │ pmsims: Sample size simulation summary │
#>                     └────────────────────────────────────────┘
#> ──────────────────────────────────── Inputs ────────────────────────────────────
#>                                Outcome : continuous
#>                         Predictor type : continuous
#>                   Number of predictors : 3
#>                       Noise predictors : 2
#>      Expected large-sample performance : R² ('r2') = 0.300
#>   Target for chosen performance metric : R2 ('r2') = 0.150
#>                                  Model : lm
#>                        Simulation reps : 20
#> ──────────────────────────────────── Results ───────────────────────────────────
#>              Final minimum sample size : 35
#>             Estimated performance at N :  (R2 ('r2') = 0.150)
#>            Estimated other metric at N : 0.575 (Calibration slope ('calib_slope'))
#>                                  Model : lm
#>                                   Mode : Mean
#>                           Running time : 0 seconds
#>     Mean mode ensures the target metric is met on average across datasets.
# }
```
