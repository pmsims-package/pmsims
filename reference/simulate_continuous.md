# Minimum sample size for continuous‐outcome prediction models

Compute the minimum sample size required to develop a prediction model
with a **continuous** outcome. This wraps the same simulation engine as
[`simulate_binary()`](https://pmsims-package.github.io/pmsims/reference/simulate_binary.md),
combining bisection search with Gaussian-process learning-curve
modelling. From user inputs (tuning performance, target performance,
etc.) it constructs a data-generating function, model-fitting function,
and metric function, then searches for the smallest \\n\\ meeting the
chosen criterion.

## Usage

``` r
simulate_continuous(
  signal_parameters,
  noise_parameters = 0,
  predictor_type = c("continuous"),
  binary_predictor_prevalence = NULL,
  tuning_rsquared,
  model = c("lm"),
  metric = c("calibration_slope", "r2"),
  target_performance,
  n_reps_total = 1000,
  mean_or_assurance = c("assurance", "mean"),
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

  Character string, currently only `"continuous"` supported, which is
  the default option. Specifies the type of simulated candidate
  predictors.

- binary_predictor_prevalence:

  Optional numeric in (0, 1). Prevalence of the binary predictors when
  `predictor_type = "binary"`. Ignored otherwise.

- tuning_rsquared:

  Numeric in (0, 1). Tuning target for expected large-sample \\R^2\\.
  This calibrates the data-generating mechanism and is not the minimum
  acceptable performance threshold.

- model:

  Character string specifying the modelling algorithm (e.g., `"glm"`).
  Passed to the internal model generator.

- metric:

  Character string naming the performance metric used to assess the
  sample size; defaults to `"calibration_slope"`. (Internally mapped to
  the engine's metric identifiers.).

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
  Defaults to `"assurance"`.

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
[`simulate_survival()`](https://pmsims-package.github.io/pmsims/reference/simulate_survival.md),
[`simulate_custom()`](https://pmsims-package.github.io/pmsims/reference/simulate_custom.md)

## Examples

``` r
if (FALSE) { # \dontrun{
est <- simulate_continuous(
  signal_parameters = 8,
  noise_parameters = 8,
  predictor_type = "continuous",
  tuning_rsquared = 0.50,
  metric = "calibration_slope",
  target_performance = 0.9,
  n_reps_total = 1000,
  mean_or_assurance = "assurance"
)
est
} # }
```
