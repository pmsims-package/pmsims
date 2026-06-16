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

- min.opt:

  Numeric vector of lower bounds for the optimisation parameters.

- max.opt:

  Numeric vector of upper bounds for the optimisation parameters.

- tolerance:

  The tolerance in the large sample performance

- proportion_noise_features:

  Proportion of candidate features that should behave as noise features.

- candidate_features:

  Total number of candidate predictors.

- N_sim_optim:

  Integer optimisation-phase simulation size.

- N_sim_final:

  Integer validation-phase simulation size.

## Value

The optimal value for the tuning parameter
