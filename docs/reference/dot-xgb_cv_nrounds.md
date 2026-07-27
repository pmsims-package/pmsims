# default_model_generators Generate appropriate model based on input arguments

default_model_generators Generate appropriate model based on input
arguments

## Usage

``` r
.xgb_cv_nrounds(
  dtrain,
  params,
  nrounds_max = 500L,
  nfold = 5L,
  early_stopping_rounds = 20L
)
```

## Format

A named list of default model generator functions grouped by outcome
type.

## Value

`default_models` is a list containing built-in model generators for
binary, continuous, and survival outcomes.
