# Tuning for a survival outcome model

Tuning for a survival outcome model

## Usage

``` r
survival_tuning(
  target_prevalence,
  target_performance,
  min.opt = c(-3, -10),
  max.opt = c(3, 10),
  tolerance = 1e-06,
  proportion_noise_features,
  candidate_features,
  N_sim_optim = 20000,
  N_sim_final = 50000
)
```

## Arguments

- target_prevalence:

  The desired model performance in a large sample

- target_performance:

  The desired model performance in a large sample

- tolerance:

  The tolerance in the large sample performance

## Value

The optimal value for the tuning parameter
