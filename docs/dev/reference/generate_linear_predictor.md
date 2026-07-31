# Construct the linear predictor

Construct the linear predictor

## Usage

``` r
generate_linear_predictor(
  X,
  n_signal_parameters,
  noise_parameters,
  intercept,
  beta_signal,
  complexity,
  nonlinear_strength = NULL
)
```

## Arguments

- X:

  n x p predictor matrix (colnames: x1, x2, ...).

- n_signal_parameters:

  Signal predictor count.

- noise_parameters:

  Noise predictor count.

- intercept:

  Scalar intercept (log-odds intercept for binary, log-hazard intercept
  for survival, mean intercept for continuous).

- beta_signal:

  Base effect size (overall scale). The tuning routines scale this to
  hit the target oracle metric.

- complexity:

  Integer 1-4.

- nonlinear_strength:

  Fraction of signal variance carried by the nonlinear component (C2/C3
  only), in \[0, 1). `NULL` uses the complexity-level default in
  `COMPLEXITY_NONLINEAR_STRENGTH_DEFAULTS`.

## Value

Numeric vector of length n.

## Details

**Why variance fraction rather than an R^2 split.** For a continuous
outcome whose target is R^2, splitting the signal by latent-scale
variance is exact: R^2 is a variance ratio, so a correctly specified
linear model recovers exactly the linear share. For binary (AUC) and
survival (Harrell's C) outcomes that logic is only approximate, because
AUC and C are rank/link-mediated. What carries over is the key property:
the nonlinear component is built to be *inaccessible to any linear-in-X
model*, so the discrimination it carries genuinely requires a nonlinear
learner and more sample size.

**Construction.** The linear predictor is \$\$lp = \alpha +
\beta\\\Bigl(\\\underbrace{\textstyle\sum_j x_j}\_{L} \\ + \\
\kappa\\\mathrm{sd}(L)\\ \underbrace{\tilde N}\_{N\\\mathrm{std}}
\Bigr)\$\$ where \\L = \sum_j x_j\\ is the linear score (identical to
complexity 1) and \\\tilde N\\ is the nonlinear aggregate (\\\sum_j
x_j^2\\ for C2; plus \\\sum\_{j\<k} x_j x_k\\ for C3) *residualised
against the full design* \\\[1, x_1, \ldots, x_S\]\\ and standardised to
unit SD. Because \\L\\ and \\\tilde N\\ are orthogonal, the nonlinear
variance fraction is \$\$f = \frac{\kappa^2}{1 +
\kappa^2},\qquad\text{equivalently}\qquad \kappa = \sqrt{\frac{f}{1 -
f}}.\$\$ The user supplies `nonlinear_strength` = \\f\\ (e.g. 0.2 = 20%
of the signal variance is nonlinear) and \\\kappa\\ is computed
internally so the realised fraction equals \\f\\ exactly.

Because the bracket is fixed given X, `lp` is linear in `beta_signal`,
so the tuning routines (which scale `beta_signal` to hit the target
oracle AUC / C-index) work unchanged.

**Calibrating difficulty.** \\f\\ sets the difficulty order but not, by
itself, a precise AUC/C-index gap: the metric reduction a given \\f\\
produces also depends on the tuned scale, the prevalence/censoring, and
the predictor family. To target a specific linear-accessible metric,
calibrate \\f\\ per complexity by simulation (raise it until a fitted
GLM/Cox on the linear terms reaches the desired value while
`beta_signal` keeps the oracle metric on target).

**Complexity 1 — Linear:** \\lp = \alpha + \beta \sum_j x_j\\.

**Complexity 4 — Friedman (1991)** (canonical, `nonlinear_strength`
ignored): \\lp = \alpha + \beta\[10\sin(\pi x_1 x_2) + 20(x_3-0.5)^2 +
10x_4 + 5x_5\]\\. Friedman contains linear-accessible terms (\\10x_4 +
5x_5\\); to place C4 on the same controlled ladder as C2/C3, residualise
the Friedman vector against Xs and apply the same kappa split.

## References

Friedman, J. H. (1991). Multivariate adaptive regression splines. *The
Annals of Statistics*, 19(1), 1-67.
