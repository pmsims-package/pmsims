# Calculate performance metrics Calculates the performance metrics for a model given a sample size n.

Calculate performance metrics Calculates the performance metrics for a
model given a sample size n.

## Usage

``` r
calculate_metrics_perf(
  n,
  data_function,
  model_function,
  metric_function,
  value_on_error
)
```

## Arguments

- n:

  Integer sample size.

- value_on_error:

  Numeric fallback returned if the metric cannot be computed.

## Value

The calculated performance metric
