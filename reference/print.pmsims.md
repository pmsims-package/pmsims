# Print a `pmsims` result

Print a `pmsims` result

## Usage

``` r
# S3 method for class 'pmsims'
print(x, ..., max_width = 80, verbose = FALSE)
```

## Arguments

- x:

  A `pmsims` object.

- ...:

  Currently unused.

- max_width:

  Maximum console width used for the summary.

- verbose:

  Logical. If `TRUE`, add implementation-level detail: internal metric
  identifiers, the engine settings used for the search, and any
  quantities recorded on an internal search scale.
  [`summary.pmsims()`](https://pmsims-package.github.io/pmsims/reference/summary.pmsims.md)
  is the same display with `verbose = TRUE` by default.

## Value

`x`, invisibly.
