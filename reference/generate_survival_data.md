# Title Simulate survival data

Title Simulate survival data

## Usage

``` r
generate_survival_data(
  n,
  beta_signal,
  n_signal_parameters,
  noise_parameters,
  predictor_type,
  predictor_prop,
  baseline_hazard,
  censoring_rate
)
```

## Arguments

- n:

  Sample size of simulated dataset

- beta_signal:

  Association between signal predictors and the outcome

- n_signal_parameters:

  Number of predictors that have a non zero association with the outcome

- noise_parameters:

  Number of predictors with no association with outcome

- predictor_type:

  Type of predictor, can be "continuous" or "binary"

- predictor_prop:

  If predictor type is binary, the probability of a predictor taking
  value 1

- baseline_hazard:

  Baseline hazard

- censoring_rate:

  Early drop out/censoring rate

## Value

A data frame with a time ("time"), event status ("event") (0 = censored,
1 = event), and n_signal_parameters + noise_parameters predictor columns
("x1", "x2", ... .)
