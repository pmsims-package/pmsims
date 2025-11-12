# mlpwr engine

mlpwr engine

## Usage

``` r
calculate_mlpwr(
  test_n,
  n_reps_total,
  n_reps_per,
  se_final,
  min_sample_size,
  max_sample_size,
  target_performance,
  c_statistic,
  mean_or_assurance,
  n_init,
  verbose,
  data_function,
  model_function,
  metric_function,
  value_on_error
)
```

## Arguments

- test_n:

  Integer size of the test datasets used to evaluate performance (should
  be large).

- n_reps_total:

  Integer total number of simulation replications allocated to the
  search.

- n_reps_per:

  Integer number of replications evaluated at each candidate sample
  size.

- se_final:

  Numeric standard error threshold used for early stopping (supply
  either `n_reps_total` or `se_final`).

- min_sample_size:

  Integer lower bound of the sample-size search region.

- max_sample_size:

  Integer upper bound of the sample-size search region.

- target_performance:

  Numeric target performance threshold the algorithm must meet or
  exceed.

- c_statistic:

  Numeric; anticipated large-sample discrimination used when tuning the
  data generator.

- mean_or_assurance:

  Character string `"mean"` or `"assurance"` indicating which criterion
  defines the minimum sample size.

- n_init:

  Integer number of initial sample sizes simulated before the
  Gaussian-process search begins.

- verbose:

  Logical flag passed to `mlpwr`; when `TRUE` verbose output is printed.

- data_function:

  A function that returns datasets. Must have a single argument, `n`,
  which controls the sample size.

- model_function:

  A function that fits models to the data. Takes the data object
  returned by `data_function` as its only argument.

- metric_function:

  A function that returns a performance metric. Must take test data, a
  fitted model and a model function as arguments. Must return a single
  value.

- value_on_error:

  Numeric fallback value used if model fitting or metric calculation
  fails.
