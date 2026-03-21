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
  data_function = NULL,
  model_function = NULL,
  metric_function = NULL,
  target_performance,
  c_statistic,
  mean_or_assurance = "assurance",
  test_n = 30000,
  min_sample_size = NULL,
  max_sample_size = NULL,
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

  Function taking a single integer argument `n` and returning a dataset
  of size `n`.

- model_function:

  Function that fits a model to the dataset returned by `data_function`.

- metric_function:

  Function that takes test data, a fitted model object, and a model
  identifier, and returns a single numeric performance value.

- target_performance:

  Numeric threshold the algorithm must meet or exceed.

- c_statistic:

  Optional numeric anticipated large-sample discrimination measure used
  by the internal search heuristics when needed.

- mean_or_assurance:

  Character string, either `"mean"` or `"assurance"`, specifying how
  performance is summarised when defining the minimum sample size.

- test_n:

  Integer size of the fixed test dataset used to evaluate predictive
  performance. This should generally be large.

- min_sample_size, max_sample_size:

  Optional integer lower and upper bounds for the sample-size search. If
  omitted, engine-specific heuristics are used to choose starting
  bounds.

- n_reps_total:

  Integer total number of simulation replications allocated to the
  search. Supply exactly one of `n_reps_total` or `se_final`.

- n_reps_per:

  Integer number of replications evaluated at each candidate sample
  size.

- se_final:

  Optional numeric standard error target for early stopping. Supply
  exactly one of `n_reps_total` or `se_final`.

- n_init:

  Integer number of initial sample sizes explored before the
  Gaussian-process stage when relevant.

- method:

  Character string selecting the search engine; currently `"mlpwr"`,
  `"bisection"`, or `"mlpwr-bs"`.

- verbose:

  Logical; if `TRUE`, print progress information from the selected
  search engine.

- ...:

  Additional arguments passed to the selected engine (for example `tol`
  for bisection).

## Value

An object of class `"pmsims"` containing the estimated minimum sample
size and simulation diagnostics.

## See also

[`simulate_binary()`](https://pmsims-package.github.io/pmsims/reference/simulate_binary.md),
[`simulate_continuous()`](https://pmsims-package.github.io/pmsims/reference/simulate_continuous.md),
[`simulate_survival()`](https://pmsims-package.github.io/pmsims/reference/simulate_survival.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data_fun <- function(n) {
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- rbinom(n, 1, plogis(0.5 * x1 - 0.25 * x2))
  data.frame(y = y, x1 = x1, x2 = x2)
}

model_fun <- function(dat) {
  stats::glm(y ~ ., data = dat, family = stats::binomial())
}

metric_fun <- function(test_data, fit, model) {
  preds <- stats::predict(fit, newdata = test_data, type = "response")
  as.numeric(pROC::auc(test_data$y, preds, quiet = TRUE))
}
attr(metric_fun, "metric") <- "auc"

est <- simulate_custom(
  data_function = data_fun,
  model_function = model_fun,
  metric_function = metric_fun,
  target_performance = 0.75,
  c_statistic = 0.80,
  mean_or_assurance = "assurance",
  n_reps_total = 40,
  n_reps_per = 10,
  method = "mlpwr"
)
est
} # }
```
