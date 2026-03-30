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

  Function taking a single argument, `n`, giving the training sample
  size, and returning a dataset that can be passed to `model_function`.

- model_function:

  Function that fits a model to the dataset returned by `data_function`.
  It must take the generated dataset as its only argument and return a
  fitted model object.

- metric_function:

  Function that evaluates predictive performance on test data. It must
  take three positional arguments in the order
  `(test_data, fitted_model, model_name)` and return a single numeric
  value. Optionally, users may set
  `attr(metric_function, "value_on_error")` to a single numeric fallback
  value to be returned if model fitting or metric evaluation fails
  during a simulation run.

- value_on_error:

  Numeric fallback returned when a simulation run fails.

- min_sample_size:

  Optional integer lower bound for the sample-size search. If supplied,
  `max_sample_size` must also be supplied.

- max_sample_size:

  Optional integer upper bound for the sample-size search. If supplied,
  `min_sample_size` must also be supplied.

- test_n:

  Integer size of the test dataset used to evaluate model performance.
  This should usually be large enough that test-set variability is
  negligible relative to the training-sample search.

- n_reps_total:

  Integer total number of simulation replications allocated to the
  search. The search evaluates approximately `n_reps_total / n_reps_per`
  candidate sample sizes.

- n_reps_per:

  Integer number of simulation replications performed at each candidate
  sample size.

- target_performance:

  Numeric target value for the chosen performance metric. The search
  aims to find the smallest sample size \\n\\ for which the selected
  criterion is met relative to this threshold.

- c_statistic:

  Optional numeric value used only by the internal start-value
  heuristics for some outcome and metric combinations. In most custom
  workflows this should be left as `NULL`.

- mean_or_assurance:

  Character string specifying the criterion used to define the minimum
  sample size. Must be either `"mean"` or `"assurance"`.

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
