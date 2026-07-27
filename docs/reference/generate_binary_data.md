# Simulate binary outcome data

Simulate binary outcome data

## Usage

``` r
generate_binary_data(
  n,
  n_signal_parameters,
  noise_parameters,
  beta_signal,
  complexity = 1,
  nonlinear_strength = NULL,
  predictor_type = "continuous",
  binary_prevalence = 0,
  correlation = 0.3,
  distribution = "normal",
  mu_lp = 0,
  baseline_prob = 0.5
)
```

## Arguments

- n:

  Sample size.

- n_signal_parameters:

  Number of signal predictors. These occupy the first
  `n_signal_parameters` columns (`x1` ... `x_S`).

- noise_parameters:

  Number of noise predictors (zero coefficient).

- beta_signal:

  Base effect size / overall scale of the signal.

- complexity:

  Integer 1-4 specifying the functional form of the linear predictor:

  1.  **Linear** — \\lp = \alpha + \beta\sum_j x_j\\.

  2.  **Quadratic** — linear + a quadratic nonlinear component.

  3.  **Quadratic + Interaction** — linear + quadratic and pairwise
      interaction nonlinear component.

  4.  **Friedman** — canonical Friedman (1991) benchmark.

- nonlinear_strength:

  Fraction of signal variance carried by the nonlinear component (C2/C3
  only), in \[0, 1). When `NULL` (default), the complexity-level default
  is used: C1 = 0, C2 = 0.2, C3 = 0.3, C4 = 0. Ignored for C1 (pure
  linear) and C4 (canonical Friedman).

- predictor_type:

  Type of predictors: `"continuous"` (default) or `"binary"`. When
  `"binary"`, all predictors are drawn as
  Bernoulli(`binary_prevalence`); `binary_prevalence` must be in (0, 1\]
  and `distribution` is ignored.

- binary_prevalence:

  Scalar in (0, 1\]. Bernoulli probability applied to all predictors
  when `predictor_type = "binary"`. Default = 0.

- correlation:

  Scalar in \\\[-1, 1\]\\. Common pairwise correlation applied via a
  Gaussian copula (equicorrelation, rank-based Cholesky). Default = 0.3.
  Set to 0 for independence.

- distribution:

  Distribution family for *all* continuous predictors. Default =
  `"normal"`. For complexity 4, if left at `"normal"` the framework uses
  `"uniform"` (Friedman canonical).

- mu_lp:

  Intercept on the log-odds scale. Default = 0.

- baseline_prob:

  Nominal baseline event probability (documentation only; the realised
  probability is determined by `mu_lp`).

## Value

A data frame with columns `y` (0/1), `x1`, `x2`, ...
