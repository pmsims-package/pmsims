# adaptive_startvalues Derive adaptive sample sizes

adaptive_startvalues Derive adaptive sample sizes

## Usage

``` r
adaptive_startvalues(
  output,
  aggregate_fun,
  var_bootstrap,
  target,
  ci_q = 0.975
)
```

## Arguments

- output:

  List-like object containing `track_bisection`, produced by
  [`calculate_bisection()`](https://pmsims-package.github.io/pmsims/reference/calculate_bisection.md).

- aggregate_fun:

  Function used to summarise replicate performance values (for example,
  `mean` or a quantile function).

- var_bootstrap:

  Function returning the bootstrap variance of the aggregated
  performance.

- target:

  Numeric target performance threshold.

- ci_q:

  Numeric quantile for confidence-interval construction (default 0.975
  gives a two-sided 95% interval).
