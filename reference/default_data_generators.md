# Title Create default data generating functions

Title Create default data generating functions

## Usage

``` r
default_data_generators(opts)
```

## Arguments

- opts:

  A list of options to be used with the data generating function. Must
  include type as either "binary", "continuous", or "survival".
  Arguments to be passed to the data generating function must be stored
  in a list item named args. For options that can be passed to the
  different default generators see
  [generate_continuous_data](https://pmsims-package.github.io/pmsims/reference/generate_continuous_data.md),
  [generate_binary_data](https://pmsims-package.github.io/pmsims/reference/generate_binary_data.md),
  or
  [generate_survival_data](https://pmsims-package.github.io/pmsims/reference/generate_survival_data.md).

## Value

A function with default arguments set to the values passed with opts
