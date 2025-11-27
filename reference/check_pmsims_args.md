# check_pmsims_args - a custom version of the base R match.arg function with improved error message

`match.arg` matches a character `arg` against a table of candidate
values as specified by `choices`.

## Usage

``` r
check_pmsims_args(arg, choices, several.ok = FALSE)
```

## Arguments

- arg:

  a character vector (of length one unless `several.ok` is `TRUE`) or
  `NULL` which means to take `choices[1]`.

- choices:

  a character vector of candidate values, often missing, see ‘Details’.

- several.ok:

  logical specifying if `arg` should be allowed to have more than one
  element.

## Value

The unabbreviated version of the exact or unique partial match if there
is one; otherwise, an error is signalled if `several.ok` is false, as
per default. When `several.ok` is true and (at least) one element of
`arg` has a match, all unabbreviated versions of matches are returned.

## Details

In the one-argument form `match.arg(arg)`, the choices are obtained from
a default setting for the formal argument `arg` of the function from
which `match.arg` was called. (Since default argument matching will set
`arg` to `choices`, this is allowed as an exception to the ‘length one
unless `several.ok` is `TRUE`’ rule, and returns the first element.)

Matching is done using [`pmatch`](https://rdrr.io/r/base/pmatch.html),
so `arg` may be abbreviated and the empty string (`""`) never matches,
not even itself, see [`pmatch`](https://rdrr.io/r/base/pmatch.html).

## Warning

The error messages given are liable to change and did so in R 4.2.0. Do
not test them in packages.

## See also

[`pmatch`](https://rdrr.io/r/base/pmatch.html),
[`match.fun`](https://rdrr.io/r/base/match.fun.html),
[`match.call`](https://rdrr.io/r/base/match.call.html).

## Examples

``` r
require(stats)
## Extends the example for 'switch'
center <- function(x, type = c("mean", "median", "trimmed")) {
  type <- match.arg(type)
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}
x <- rcauchy(10)
center(x, "t")       # Works
#> [1] 1.187598
center(x, "med")     # Works
#> [1] 0.2808194
try(center(x, "m"))  # Error
#> Error in match.arg(type) : 
#>   'arg' should be one of “mean”, “median”, “trimmed”
stopifnot(identical(center(x),       center(x, "mean")),
          identical(center(x, NULL), center(x, "mean")) )

## Allowing more than one 'arg' and hence more than one match:
match.arg(c("gauss", "rect", "ep"),
          c("gaussian", "epanechnikov", "rectangular", "triangular"),
          several.ok = TRUE)
#> [1] "gaussian"     "rectangular"  "epanechnikov"
match.arg(c("a", ""),  c("", NA, "bb", "abc"), several.ok=TRUE) # |-->  "abc"
#> [1] "abc"
```
