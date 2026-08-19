# Detailed summary of a `pmsims` result

The same display as
[`print.pmsims()`](https://pmsims-package.github.io/pmsims/dev/reference/print.pmsims.md),
with the implementation-level detail that the default print method
hides: internal metric identifiers, the engine settings used for the
search, and any quantities recorded on an internal search scale.

## Usage

``` r
# S3 method for class 'pmsims'
summary(object, ..., max_width = 80, verbose = TRUE)
```

## Arguments

- object:

  A `pmsims` object.

- ...:

  Currently unused.

- max_width:

  Maximum console width used for the summary.

- verbose:

  Logical. Set to `FALSE` for the default print display.

## Value

`object`, invisibly.
