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

  Integer size of the fixed test dataset used to evaluate predictive
  performance. This should generally be large.

- n_reps_total:

  Integer total number of simulation replications allocated to the
  search. Supply exactly one of `n_reps_total` or `se_final`.

- n_reps_per:

  Integer number of replications evaluated at each candidate sample
  size.

- se_final:

  Optional numeric standard error target for early stopping. Supply
  exactly one of `n_reps_total` or `se_final`.

- min_sample_size, max_sample_size:

  Optional integer lower and upper bounds for the sample-size search. If
  omitted, engine-specific heuristics are used to choose starting
  bounds.

- target_performance:

  Numeric threshold the algorithm must meet or exceed.

- c_statistic:

  Optional numeric anticipated large-sample discrimination measure used
  by the internal search heuristics when needed.

- mean_or_assurance:

  Character string, either `"mean"` or `"assurance"`, specifying how
  performance is summarised when defining the minimum sample size.

- n_init:

  Integer number of initial sample sizes simulated before the
  Gaussian-process search begins.

- verbose:

  Logical flag passed to `mlpwr`; when `TRUE` verbose output is printed.

- data_function:

  Function taking a single integer argument `n` and returning a dataset
  of size `n`.

- model_function:

  Function that fits a model to the dataset returned by `data_function`.

- metric_function:

  Function that takes test data, a fitted model object, and a model
  identifier, and returns a single numeric performance value.

- value_on_error:

  Numeric fallback value used if model fitting or metric calculation
  fails.
