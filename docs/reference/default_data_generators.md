# Create default data generating functions

Create default data generating functions

## Usage

``` r
default_data_generators(opts)
```

## Arguments

- opts:

  A list with two elements:

  `type`

  : Outcome type: `"continuous"`, `"binary"`, or `"survival"`.

  `args`

  : Named list of arguments to pre-set on the corresponding generator
    function.

## Value

A partially-applied generator function whose formals have been set to
the values in `opts$args`.
