# The Bisection Engine

Runs a bisection search over sample size using repeated simulations and
summaries of the chosen performance metric.

## Usage

``` r
calculate_bisection(
  data_function = data_function,
  model_function = model_function,
  metric_function = metric_function,
  value_on_error = value_on_error,
  min_sample_size = min_sample_size,
  max_sample_size = max_sample_size,
  test_n = test_n,
  n_reps_total = n_reps_total,
  n_reps_per = n_reps_per,
  target_performance = target_performance,
  c_statistic,
  mean_or_assurance = mean_or_assurance,
  tol = 0.001,
  parallel = FALSE,
  cores = 20,
  verbose = FALSE,
  budget = FALSE
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

- value_on_error:

  Numeric fallback returned when a simulation run fails.

- min_sample_size:

  Integer lower bound of the sample-size search region.

- max_sample_size:

  Integer upper bound of the sample-size search region.

- test_n:

  Integer size of the test datasets used to evaluate performance (should
  be large).

- n_reps_total:

  Integer total number of simulation replications allocated to the
  search.

- n_reps_per:

  Integer number of replications evaluated at each candidate sample
  size.

- target_performance:

  Numeric target performance threshold the algorithm must meet or
  exceed.

- c_statistic:

  Numeric; anticipated large-sample discrimination used when tuning the
  data generator.

- mean_or_assurance:

  Character string `"mean"` or `"assurance"` indicating which criterion
  defines the minimum sample size.

- tol:

  Numeric tolerance controlling when the bisection loop stops.

- parallel:

  Logical; if `TRUE` the per-sample-size simulations run in parallel via
  `foreach`.

- cores:

  Integer number of cores to use when `parallel = TRUE`.

- verbose:

  Logical flag passed to `mlpwr`; when `TRUE` verbose output is printed.

- budget:

  Logical; if `TRUE` the algorithm halts once the evaluation budget is
  exhausted instead of using `tol`.

## Value

A list containing the simulation `results`, performance `summaries`,
optional tracking `history`, and the `track_bisection` records.
