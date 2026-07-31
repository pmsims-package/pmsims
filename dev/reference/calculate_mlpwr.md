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
  progress = TRUE,
  verbose,
  data_function,
  model_function,
  metric_function,
  value_on_error,
  ...
)
```

## Arguments

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

- min_sample_size:

  Optional integer lower bound for the sample-size search. If supplied,
  `max_sample_size` must also be supplied.

- max_sample_size:

  Optional integer upper bound for the sample-size search. If supplied,
  `min_sample_size` must also be supplied.

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

- n_init:

  Integer number of initial sample sizes simulated before the Gaussian
  process search begins.

- progress:

  Logical flag controlling whether the `mlpwr` progress bar is shown.

- verbose:

  Logical flag passed to `mlpwr`; when `TRUE` verbose output is printed.

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

  Numeric fallback value used if model fitting or metric calculation
  fails.

- ...:

  Additional options passed to
  [`mlpwr::find.design()`](https://rdrr.io/pkg/mlpwr/man/find.design.html).
