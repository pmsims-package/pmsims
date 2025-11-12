# Minimum sample size for binary-outcome prediction models

Compute the minimum sample size required to develop a prediction model
with a binary outcome. The function wraps a simulation-based engine that
combines a bisection search with Gaussian-process curve fitting. From
user inputs (outcome prevalence, expected large-sample performance,
minimum acceptable performance, etc.) it constructs a data-generating
function, a model-fitting function, and a metric function, then searches
for the smallest \\n\\ that meets the chosen performance criterion.

## Usage

``` r
simulate_binary(
  signal_parameters,
  noise_parameters = 0,
  predictor_type = "continuous",
  binary_predictor_prevalence = NULL,
  outcome_prevalence,
  large_sample_cstatistic,
  model = "glm",
  metric = "calibration_slope",
  minimum_acceptable_performance,
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

- outcome_prevalence:

  Numeric in (0, 1). Target prevalence of the binary outcome in the
  intended modelling context.

- large_sample_cstatistic:

  Numeric in (0, 1). Expected C-statistic for a model developed on a
  very large sample (used to tune the data-generating mechanism).

- model:

  Character string specifying the modelling algorithm (e.g., `"glm"`).
  Passed to the internal model generator.

- metric:

  Character string naming the performance metric used to assess the
  sample size; defaults to `"calibration_slope"`. (Internally mapped to
  the engine's metric identifiers.)

- minimum_acceptable_performance:

  Numeric. The target threshold \\M^\\\*\\; the algorithm searches for
  the smallest \\n\\ meeting the chosen criterion with respect to this
  threshold.

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

[`simulate_continuous()`](https://pmsims-package.github.io/pmsims/reference/simulate_continuous.md),
[`simulate_survival()`](https://pmsims-package.github.io/pmsims/reference/simulate_survival.md),
[`simulate_custom()`](https://pmsims-package.github.io/pmsims/reference/simulate_custom.md)

## Examples

``` r
if (FALSE) { # \dontrun{
est <- simulate_binary(
  signal_parameters = 10,
  noise_parameters = 10,
  predictor_type = "continuous",
  outcome_prevalence = 0.2,
  large_sample_cstatistic = 0.75,
  metric = "calibration_slope",
  minimum_acceptable_performance = 0.9,
  n_reps_total = 1000,
  mean_or_assurance = "assurance"
)
est
} # }
```
