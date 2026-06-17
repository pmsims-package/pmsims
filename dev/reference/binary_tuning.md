# Tuning function for a binary outcome model

Finds the `beta_signal` and log-odds intercept (`mu_lp`) that jointly
produce the target AUC and prevalence under the exact data-generating
settings (complexity, nonlinear_strength, correlation, distribution,
predictor_type).

## Usage

``` r
binary_tuning(
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
  n_sim = 3e+05,
  n_validate = NULL,
  beta_interval = c(1e-04, 20),
  beta_tol = 1e-04,
  mu_tol = 1e-06,
  tolerance = 0.02
)
```

## Arguments

- target_prevalence:

  Target outcome prevalence. Must be in (0, 1).

- target_performance:

  Target AUC (c-statistic). Must be in (0.5, 1).

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

  Common pairwise predictor correlation. Defaults to the SAME value as
  [`generate_binary_data()`](https://pmsims-package.github.io/pmsims/dev/reference/generate_binary_data.md)
  so the two never silently disagree. Whatever you pass here must match
  the generator call.

- distribution:

  Global continuous predictor distribution family. Defaults to match
  [`generate_binary_data()`](https://pmsims-package.github.io/pmsims/dev/reference/generate_binary_data.md).

- predictor_type:

  `"continuous"` (default) or `"binary"`. Defaults to match
  [`generate_binary_data()`](https://pmsims-package.github.io/pmsims/dev/reference/generate_binary_data.md).

- binary_prevalence:

  Bernoulli probability for binary predictors. Defaults to match
  [`generate_binary_data()`](https://pmsims-package.github.io/pmsims/dev/reference/generate_binary_data.md).

- n_sim:

  Sample size for the internal LP simulation. Larger values give more
  stable AUC estimates within the bisection. Default = 300 000.

- n_validate:

  Sample size for the independent end-to-end validation. Defaults to
  `n_sim`.

- beta_interval:

  Search interval `c(lo, hi)` for beta_signal. Default = `c(1e-4, 20)`.
  The upper bound is doubled automatically if the AUC at `hi` is still
  below `target_performance`.

- beta_tol:

  Absolute convergence tolerance for the beta bisection. Default = 1e-4.

- mu_tol:

  Absolute convergence tolerance for the mu_lp root-finding. Default =
  1e-6.

- tolerance:

  Acceptable absolute deviation of the validated prevalence and AUC from
  their targets before a warning is raised. Default = 0.02.

## Value

A named numeric vector:

- `mu_lp`:

  Tuned log-odds intercept.

- `beta_signal`:

  Tuned effect size.

- `prevalence_achieved`:

  Prevalence from an independent generate_binary_data() validation draw.

- `auc_achieved`:

  AUC of the oracle LP from the same draw.

- `var_lp_unit`:

  Estimated Var(LP) at beta_signal = 1.
