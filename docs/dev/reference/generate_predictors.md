# Generate the n x p predictor matrix

Draws all predictors from a single global distribution family, then
optionally applies an equicorrelation structure via a Gaussian copula.

## Usage

``` r
generate_predictors(
  n,
  n_signal_parameters,
  noise_parameters,
  complexity = 1,
  predictor_type = "continuous",
  binary_prevalence = 0,
  correlation = 0.3,
  distribution = "normal"
)
```

## Arguments

- n:

  Sample size.

- n_signal_parameters:

  Number of signal predictors.

- noise_parameters:

  Number of noise predictors.

- complexity:

  Integer 1-4 (used to resolve the C4 distribution default).

- predictor_type:

  `"continuous"` (default) or `"binary"`.

- binary_prevalence:

  Bernoulli probability; used when `predictor_type = "binary"`.

- correlation:

  Scalar common pairwise correlation; 0 = independent.

- distribution:

  Global continuous distribution family; used when
  `predictor_type = "continuous"`.

## Value

Named n x p numeric matrix (column names: x1, x2, ...).
