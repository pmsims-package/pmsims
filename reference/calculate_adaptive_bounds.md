# Adaptive starting value searching (model/metrics) agnostic.

Adaptive starting value searching (model/metrics) agnostic.

## Usage

``` r
calculate_adaptive_bounds(
  data_function,
  model_function,
  metric_function,
  value_on_error,
  start_n,
  test_n,
  n_reps_per,
  n_reps_total,
  target_performance,
  threshold = 0.01,
  mean_or_assurance = "mean",
  c_statistic = NULL,
  parallel = FALSE,
  cores = 20,
  verbose = FALSE
)
```

## Arguments

- verbose:
