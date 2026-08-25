# Title Simulate continuous data

Title Simulate continuous data

## Usage

``` r
generate_continuous_data(
  n,
  beta_signal,
  n_signal_parameters,
  noise_parameters,
  predictor_type,
  predictor_prop = NULL
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

## Value

A data frame with one outcome column and n_signal_parameters +
noise_parameters predictor columns
