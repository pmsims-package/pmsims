# Plot sample-size learning curves for `pmsims` outputs

Produces a ggplot showing the simulated points and the fitted
Gaussian-process learning curve stored inside a `pmsims` object.
Optionally returns the underlying data instead of drawing the plot.

## Usage

``` r
# S3 method for class 'pmsims'
plot(x, metric_label = NULL, plot = TRUE, ...)
```

## Arguments

- x:

  A `pmsims` object returned by
  [`simulate_binary()`](https://pmsims-package.github.io/pmsims/reference/simulate_binary.md),
  [`simulate_continuous()`](https://pmsims-package.github.io/pmsims/reference/simulate_continuous.md),
  [`simulate_survival()`](https://pmsims-package.github.io/pmsims/reference/simulate_survival.md),
  or
  [`simulate_custom()`](https://pmsims-package.github.io/pmsims/reference/simulate_custom.md).

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
`plot = FALSE`, returns a list with two data frames: `observed_data`
(simulated points) and `predicted_data` (Gaussian-process predictions).
