# Tuning function for a survival outcome model

Finds the `beta_signal` that produces the target C-index under the exact
data-generating settings (complexity, nonlinear_strength, correlation,
distribution, predictor_type). The event rate is set exactly by quantile
censoring, and the baseline hazard only fixes the time scale.

## Usage

``` r
survival_tuning(
  target_prevalence,
  target_performance,
  candidate_features,
  proportion_noise_features,
  complexity = 1,
  nonlinear_strength = NULL,
  correlation = 0.3,
  distribution = "normal",
  predictor_type = "continuous",
  binary_prevalence = 0,
  n_sim = 50000,
  n_validate = 1e+05,
  beta_interval = c(1e-04, 20),
  beta_tol = 1e-04,
  tolerance = 0.02
)
```

## Arguments

- target_prevalence:

  Target event rate (proportion of events) in (0, 1). Equivalently 1 -
  censoring_rate.

- target_performance:

  Target C-index in (0.5, 1).

- candidate_features:

  Total number of predictors (signal + noise).

- proportion_noise_features:

  Proportion of `candidate_features` that are noise predictors. Must be
  in \[0, 1).

- complexity:

  Integer 1-4. Default = 1.

- nonlinear_strength:

  Fraction of signal variance carried by the nonlinear component (C2/C3
  only), in \[0, 1). When `NULL` (default), the complexity-level default
  is used: C1 = 0, C2 = 0.2, C3 = 0.3, C4 = 0.

- correlation:

  Common pairwise predictor correlation. Default = 0.3 (to match
  generate_survival_data()).

- distribution:

  Global continuous predictor distribution family. Default = `"normal"`.

- predictor_type:

  `"continuous"` (default) or `"binary"`.

- binary_prevalence:

  Bernoulli probability for binary predictors.

- n_sim:

  Sample size for LP simulation and the bisection. Default 50000.

- n_validate:

  Sample size for the independent validation. Default 100000.

- beta_interval:

  Search interval c(lo, hi) for beta_signal. The upper bound is doubled
  automatically if the C-index at hi is below target.

- beta_tol:

  Absolute convergence tolerance for the beta bisection.

- tolerance:

  Acceptable absolute deviation of the validated event rate and C-index
  from their targets before a warning is raised. Default 0.02.

## Value

A named numeric vector: `lambda_opt` (baseline hazard / time scale),
`beta_signal`, `event_rate`, `cindex`, `var_lp_unit`.
