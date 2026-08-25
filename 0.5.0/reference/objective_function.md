# Objective function for pmsims

Computes the objective value \$\$\left\| M(n) - M^\* - \lambda\\C(n)
\right\|,\$\$ where \\M(n)\\ is performance at sample size \\n\\,
\\M^\*\\ is the target performance, \\C(n)\\ is a cost term increasing
with \\n\\, and \\\lambda\\ (set via `penalty_weight`) controls the
penalty on large samples.

## Usage

``` r
objective_function(
  n,
  penalty_weight,
  target_performance,
  min_sample_size,
  max_sample_size,
  value_on_error,
  data_function = NULL,
  model_function = NULL,
  metric_function = NULL
)
```

## Arguments

- n:

  Integer sample size.

- penalty_weight:

  Penalty weight \\\lambda\\ on the cost term \\C(n)\\; `0` minimises
  only the absolute performance gap.

- target_performance:

  Target performance \\M^\*\\ (numeric).

- min_sample_size:

  Minimum sample size considered (lower bound of search).

- max_sample_size:

  Maximum sample size considered (upper bound of search).

- value_on_error:

  Value to return if the objective cannot be evaluated.

- data_function:

  Data generator passed to
  [`calculate_metrics_perf()`](https://pmsims-package.github.io/pmsims/reference/calculate_metrics_perf.md).

- model_function:

  Model-fitting function passed to
  [`calculate_metrics_perf()`](https://pmsims-package.github.io/pmsims/reference/calculate_metrics_perf.md).

- metric_function:

  Metric function passed to
  [`calculate_metrics_perf()`](https://pmsims-package.github.io/pmsims/reference/calculate_metrics_perf.md).

## Value

A single numeric: the objective value at \\n\\.

## Examples

``` r
# \dontrun{
# Example usage (assuming helper functions exist):
# objective_function(
#   n = 500,
#   penalty_weight = 0.1,
#   target_performance = 0.75,
#   min_sample_size = 100,
#   max_sample_size = 5000,
#   value_on_error = Inf
# )
# }
```
