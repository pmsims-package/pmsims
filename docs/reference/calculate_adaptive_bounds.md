# Calculate adaptive start bounds

Derive lower and upper sample-size bounds by repeatedly simulating model
performance from an initial sample size.

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
  plateau_k = 3,
  plateau_tol = 0.005,
  large_perf_check = FALSE,
  large_n = NULL,
  large_n_tol = 0.05,
  c_statistic = NULL,
  parallel = FALSE,
  cores = 20,
  verbose = FALSE
)
```

## Arguments

- data_function:

  Function taking a sample size and returning a simulated training
  dataset.

- model_function:

  Function fitting a model to a simulated training dataset.

- metric_function:

  Function evaluating the fitted model on test data.

- value_on_error:

  Numeric fallback used when fitting or evaluation fails.

- start_n:

  Positive integer initial sample size.

- test_n:

  Positive integer size of the fixed test dataset.

- n_reps_per:

  Positive integer simulations performed at each sample size.

- n_reps_total:

  Positive integer total simulation budget.

- target_performance:

  Numeric performance threshold used to define the search bounds.

- threshold:

  Numeric tolerance around `target_performance`.

- mean_or_assurance:

  Character string selecting the mean or 20th-percentile performance
  summary.

- plateau_k:

  Positive integer number of recent iterations used to detect a
  performance plateau.

- plateau_tol:

  Numeric maximum change treated as a plateau.

- large_perf_check:

  Logical; whether to probe a large sample size before beginning the
  adaptive search.

- large_n:

  Optional positive integer sample size for the preliminary performance
  probe.

- large_n_tol:

  Numeric shortfall beyond which the target is considered unreachable at
  `large_n`.

- c_statistic:

  Reserved for compatibility with callers that supply an anticipated
  discrimination value.

- parallel:

  Logical; whether simulations at each sample size use a parallel
  backend.

- cores:

  Positive integer number of parallel workers.

- verbose:

  Logical; whether to report search progress.

## Value

A list containing lower and upper sample-size bounds, the associated
performance summaries, and the search trace.
