# Plot sample-size learning curves for `pmsims` outputs

Produces a ggplot showing the simulated points and the fitted learning
curve (the learning-curve fit for `method = "curve"`, the
Gaussian-process surrogate for the mlpwr engines) stored inside a
`pmsims` object. Optionally returns the underlying data instead of
drawing the plot.

## Usage

``` r
# S3 method for class 'pmsims'
plot(x, metric_label = NULL, plot = TRUE, ...)
```

## Arguments

- x:

  A `pmsims` object returned by
  [`simulate_binary()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_binary.md),
  [`simulate_continuous()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_continuous.md),
  [`simulate_survival()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_survival.md),
  or
  [`simulate_custom()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_custom.md).

- metric_label:

  Optional string used for the y-axis label when the object does not
  already record the metric name.

- plot:

  Logical; if `TRUE` (default) the function prints the plot. If `FALSE`,
  the data used to build the plot are returned instead of drawing
  anything.

- ...:

  Currently unused.

## Value

Invisibly returns the `ggplot` object when `plot = TRUE`. When
`plot = FALSE`, returns a list with two data frames: for the mlpwr
engines, `observed_data` (simulated points) and `predicted_data`
(Gaussian-process predictions); for `method = "curve"`, `observed_data`
(the criterion at each simulated sample size, with replicates, standard
error and a +/- 2 SE interval) and `fitted_curve` (the fitted learning
curve, or `NULL` when none was fitted).
