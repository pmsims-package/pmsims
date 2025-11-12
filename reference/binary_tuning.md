# Tuning for a binary outcome model

Tuning for a binary outcome model

## Usage

``` r
binary_tuning(
  target_prevalence,
  target_performance,
  min.opt = c(-10, 0),
  max.opt = c(0.02, 5),
  tolerance = 1e-05,
  proportion_noise_features,
  candidate_features
)
```

## Arguments

- target_prevalence:

  The expected outcome prevalence

- target_performance:

  The desired model performance in a large sample

- tolerance:

  Convergence parameters (TODO)

## Value

The optimal value for the tuning parameter
