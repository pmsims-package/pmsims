# Minimum sample size for survival‐outcome prediction models

Compute the minimum sample size required to develop a prediction model
with a **time-to-event (survival)** outcome. As with the other wrappers,
this uses a simulation-based learning-curve approach with
Gaussian-process surrogate modelling to locate the smallest \\n\\
meeting the chosen performance criterion.

## Usage

``` r
simulate_survival(
  signal_parameters,
  noise_parameters = 0,
  predictor_type = "continuous",
  binary_predictor_prevalence = NULL,
  maximum_achievable_cindex,
  baseline_hazard = 1,
  censoring_rate,
  model = "coxph",
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

- predictor_type:

  Character string, either `"continuous"` or `"binary"`. Specifies the
  type of simulated candidate predictors.

- binary_predictor_prevalence:

  Optional numeric in (0, 1). Prevalence of the binary predictors when
  `predictor_type = "binary"`. Ignored otherwise.

- maximum_achievable_cindex:

  Numeric in (0, 1). Maximum achievable C-index with effectively
  unlimited data. This is used to calibrate the data-generating
  mechanism and is not the minimum acceptable threshold.

- baseline_hazard:

  Numeric greater than 0. Baseline hazard level used by the
  data-generating mechanism (e.g., the constant hazard in an exponential
  baseline). Larger values imply shorter event times, all else equal.

- censoring_rate:

  Numeric in \[0, 1). Proportion of individuals expected to be censored
  in the simulated datasets (administrative or random censoring). Higher
  values imply fewer observed events for a fixed \\n\\.

- model:

  Character string; currently `"coxph"` (Cox proportional hazards).

- metric:

  Character string naming the performance metric used to assess the
  sample size; defaults to `"calibration_slope"`. (Internally mapped to
  the engine's metric identifiers.)

- target_performance:

  Numeric. Minimum acceptable value of the selected performance metric
  \\M^\\\*\\; the algorithm searches for the smallest \\n\\ meeting the
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

## See also

[`simulate_binary()`](https://pmsims-package.github.io/pmsims/reference/simulate_binary.md),
[`simulate_continuous()`](https://pmsims-package.github.io/pmsims/reference/simulate_continuous.md),
[`simulate_custom()`](https://pmsims-package.github.io/pmsims/reference/simulate_custom.md)

## Examples

``` r
if (FALSE) { # \dontrun{
est <- simulate_survival(
  signal_parameters = 10,
  noise_parameters = 10,
  predictor_type = "continuous",
  maximum_achievable_cindex = 0.70,
  baseline_hazard = 0.01,
  censoring_rate = 0.30,
  metric = "calibration_slope",
  target_performance = 0.9,
  n_reps_total = 1000,
  mean_or_assurance = "assurance"
)
est
} # }
```
