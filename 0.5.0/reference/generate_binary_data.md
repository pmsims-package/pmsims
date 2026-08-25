# Title Simulate binary data

Title Simulate binary data

## Usage

``` r
generate_binary_data(
  n,
  mu_lp,
  beta_signal,
  n_signal_parameters,
  noise_parameters,
  predictor_type,
  predictor_prop = NULL,
  baseline_prob
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

- baseline_prob:

  Baseline probability of outcome (i.e., probability when all predictors
  are 0)

## Value

A data frame with one outcome column and n_signal_parameters +
noise_parameters predictor columns
