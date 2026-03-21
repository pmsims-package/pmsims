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
  budget = TRUE
)
```

## Arguments

- data_function:

  Function taking a single integer argument `n` and returning a dataset
  of size `n`.

- model_function:

  Function that fits a model to the dataset returned by `data_function`.

- metric_function:

  Function that takes test data, a fitted model object, and a model
  identifier, and returns a single numeric performance value.

- value_on_error:

  Numeric fallback returned when a simulation run fails.

- min_sample_size, max_sample_size:

  Optional integer lower and upper bounds for the sample-size search. If
  omitted, engine-specific heuristics are used to choose starting
  bounds.

- test_n:

  Integer size of the fixed test dataset used to evaluate predictive
  performance. This should generally be large.

- n_reps_total:

  Integer total number of simulation replications allocated to the
  search. Supply exactly one of `n_reps_total` or `se_final`.

- n_reps_per:

  Integer number of replications evaluated at each candidate sample
  size.

- target_performance:

  Numeric threshold the algorithm must meet or exceed.

- c_statistic:

  Optional numeric anticipated large-sample discrimination measure used
  by the internal search heuristics when needed.

- mean_or_assurance:

  Character string, either `"mean"` or `"assurance"`, specifying how
  performance is summarised when defining the minimum sample size.

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
