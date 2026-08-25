# Minimum sample size for custom simulation workflows

Compute the minimum sample size required to achieve a target level of
predictive performance using user-defined simulation components.
`simulate_custom()` is the low-level interface in `pmsims`: users supply
a data-generating function, a model-fitting function, and a metric
function, and the chosen search engine estimates the smallest \\n\\
meeting the selected performance criterion.

## Usage

``` r
simulate_custom(
  data_function,
  model_function,
  metric_function,
  target_performance,
  c_statistic = NULL,
  mean_or_assurance = "assurance",
  test_n = 30000,
  min_sample_size = NULL,
  max_sample_size = NULL,
  n_reps_total = 1000,
  n_reps_per = 20,
  method = "mlpwr",
  progress = TRUE,
  verbose = FALSE,
  ...
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

- test_n:

  Integer size of the test dataset used to evaluate model performance.
  This should usually be large enough that test-set variability is
  negligible relative to the training-sample search.

- min_sample_size:

  Optional integer lower bound for the sample-size search. If supplied,
  `max_sample_size` must also be supplied.

- max_sample_size:

  Optional integer upper bound for the sample-size search. If supplied,
  `min_sample_size` must also be supplied. Supplying both bounds defines
  the search space directly, so the adaptive starting-value search is
  skipped. Because the runtime estimate is extrapolated from that stage,
  no long-run warning is issued either.

- n_reps_total:

  Integer total number of simulation replications allocated to the
  search. The search evaluates approximately `n_reps_total / n_reps_per`
  candidate sample sizes.

- n_reps_per:

  Integer number of simulation replications performed at each candidate
  sample size.

- method:

  Character string specifying the search engine. Defaults to `"mlpwr"`.

- progress:

  Logical flag controlling whether the `mlpwr` progress bar is shown for
  `mlpwr`-based methods.

- verbose:

  Logical flag controlling engine-specific diagnostic output when
  supported. For the bisection engine, setting `verbose = TRUE` stores
  the iteration history on the returned object.

- ...:

  Additional arguments passed to the selected search engine.

## Value

An object of class `"pmsims"` containing the estimated minimum sample
size.

## See also

[`simulate_binary()`](https://pmsims-package.github.io/pmsims/reference/simulate_binary.md),
[`simulate_continuous()`](https://pmsims-package.github.io/pmsims/reference/simulate_continuous.md),
[`simulate_survival()`](https://pmsims-package.github.io/pmsims/reference/simulate_survival.md)

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(1234)

data_fun <- function(n) {
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  x4 <- rnorm(n)
  x5 <- rnorm(n)
  y <- 0.35 * x1 - 0.3 * x2 + 0.2 * x3 + 0.1 * x4 - 0.1 * x5 +
    rnorm(n, sd = 1)
  data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5)
}

model_fun <- function(dat) {
  stats::lm(y ~ ., data = dat)
}

metric_fun <- function(test_data, fit, model) {
  preds <- stats::predict(fit, newdata = test_data)
  1 - sum((test_data$y - preds)^2) /
    sum((test_data$y - mean(test_data$y))^2)
}
attr(metric_fun, "metric") <- "r2"

maximum_achievable_data <- data_fun(100000)
test_data <- data_fun(50000)
maximum_achievable_fit <- model_fun(maximum_achievable_data)
maximum_achievable_performance <- metric_fun(
  test_data,
  maximum_achievable_fit,
  "lm"
)

est <- simulate_custom(
  data_function = data_fun,
  model_function = model_fun,
  metric_function = metric_fun,
  target_performance = maximum_achievable_performance - 0.02
)
est
} # }
```
