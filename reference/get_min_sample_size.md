# get_min_sample_size: Heuristic starting-n for binary/continuous/survival prediction

get_min_sample_size: Heuristic starting-n for binary/continuous/survival
prediction

## Usage

``` r
get_min_sample_size(
  npar,
  prevalence = NULL,
  c_stat = NULL,
  calib_slope = NULL,
  epv_value = NULL,
  outcome_type = c("binary", "survival", "continuous")
)
```

## Arguments

- npar:

  Integer; number of predictors in the model.

- prevalence:

  Numeric in 0, 1; optional event rate or case fraction used for EPV
  calculations.

- c_stat:

  Numeric in (0.5, 1\]; anticipated discrimination (C-statistic). Lower
  values inflate the heuristic.

- calib_slope:

  Numeric; anticipated calibration slope. Values below 1 trigger a
  modest inflation.

- epv_value:

  Numeric; target events-per-variable (EPV) value applied when
  prevalence is supplied.

- outcome_type:

  Character string; must be one of `"binary"`, `"survival"`, or
  `"continuous"`.

## Value

Integer recommended starting value from which to calculate the minimum
sample size.
