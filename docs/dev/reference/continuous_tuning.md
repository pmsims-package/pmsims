# Tuning function for a continuous outcome model

Finds the `beta_signal` value that produces a target large-sample
\\R^2\\ under the exact data-generating settings (complexity,
nonlinear_strength, correlation, distribution, predictor_type).

## Usage

``` r
continuous_tuning(
  r2,
  candidate_features,
  proportion_noise_features,
  complexity = 1,
  nonlinear_strength = NULL,
  correlation = 0,
  distribution = "normal",
  predictor_type = "continuous",
  binary_prevalence = 0,
  n_sim = 1e+05
)
```

## Arguments

- r2:

  Target large-sample \\R^2\\ (proportion of variance explained). Must
  be in (0, 1).

- candidate_features:

  Total number of predictors (signal + noise).

- proportion_noise_features:

  Proportion of `candidate_features` that are noise predictors (zero
  coefficient). Must be in \[0, 1).

- complexity:

  Integer 1-4 controlling the functional form of the linear predictor
  passed to the data generator. Default = 1.

- nonlinear_strength:

  Fraction of signal variance carried by the nonlinear component (C2/C3
  only), in \[0, 1). When `NULL` (default), the complexity-level default
  is used: C1 = 0, C2 = 0.2, C3 = 0.3, C4 = 0.

- correlation:

  Common pairwise predictor correlation. Default = 0.

- distribution:

  Global continuous predictor distribution family passed to the data
  generator. Default = `"normal"`.

- predictor_type:

  `"continuous"` (default) or `"binary"`.

- binary_prevalence:

  Bernoulli probability for binary predictors. Required (and used) only
  when `predictor_type = "binary"`.

- n_sim:

  Sample size used to estimate Var(LP). Larger values give more stable
  estimates. Default = 100 000.

## Value

A named numeric vector:

- `beta_signal`:

  Tuned effect size.

- `r2_achieved`:

  Empirical R^2 verified in a large simulation.

- `var_lp_unit`:

  Estimated Var(LP) at beta_signal = 1 (the scaling constant).
