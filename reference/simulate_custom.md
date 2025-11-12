# Simulate Custom 'simulate_custom' is the interface for pmsims at the most basic level. It performs no processing of arguments and allows all possible options to be customised.

Simulate Custom 'simulate_custom' is the interface for pmsims at the
most basic level. It performs no processing of arguments and allows all
possible options to be customised.

## Usage

``` r
simulate_custom(
  data_function = NULL,
  model_function = NULL,
  metric_function = NULL,
  target_performance,
  c_statistic,
  mean_or_assurance = "assurance",
  test_n = 30000,
  min_sample_size,
  max_sample_size,
  n_reps_total = NULL,
  n_reps_per = 50,
  se_final = NULL,
  n_init = 4,
  method = "mlpwr",
  verbose = FALSE,
  ...
)
```

## Arguments

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

- target_performance:

  Numeric target performance threshold the algorithm must meet or
  exceed.

- c_statistic:

  Numeric; anticipated large-sample discrimination used when tuning the
  data generator.

- mean_or_assurance:

  Character string `"mean"` or `"assurance"` indicating which criterion
  defines the minimum sample size.

- test_n:

  Integer size of the test datasets used to evaluate performance (should
  be large).

- min_sample_size:

  Integer lower bound of the sample-size search region.

- max_sample_size:

  Integer upper bound of the sample-size search region.

- n_reps_total:

  Integer total number of simulation replications allocated to the
  search.

- n_reps_per:

  Integer number of replications evaluated at each candidate sample
  size.

- se_final:

  Numeric standard error threshold used for early stopping (supply
  either `n_reps_total` or `se_final`).

- n_init:

  Integer number of initial sample sizes explored before the main search
  algorithm begins.

- method:

  Character string selecting the search engine; currently `"mlpwr"`,
  `"bisection"`, or `"mlpwr-bs"`.

- verbose:

  Logical flag controlling printed progress information.

- ...:

  Additional arguments passed to the chosen engine (for example `tol`
  for bisection or `penalty_weight` for GA-based methods).

## Value

An object of class `"pmsims"` containing the estimated minimum sample
size and simulation diagnostics.
