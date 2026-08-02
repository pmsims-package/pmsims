# Getting started with pmsims

## What pmsims does

**pmsims** estimates the **minimum sample size** needed to develop a
prediction model to achieve a target level of performance **with
assurance**. Rather than relying on simple rules of thumb or closed‑form
formulae, pmsims uses **simulation** to:

- Generate synthetic datasets that reflect your target setting (outcome
  type, prevalence or \\R^2\\, signal vs. noise predictors);
- Fit a specified **model** (e.g., logistic regression or linear
  regression);
- Evaluate a chosen **performance metric** (e.g., calibration slope,
  AUC); and
- Trace a **learning curve** of performance as the training size
  increases.

![A diagram showing the pmsims workflow, consisting of the data
generator, model function, metrics function, which are passed to the
simulation engine.](images/workflow.png)

The recommended design objective is **assurance**: the **smallest**
\\n\\ such that a high proportion of repeated studies (e.g., 80%) meet
the target performance. In pmsims, this is implemented via the **20th
percentile** of the simulated performance distribution at each \\n\\.

## Required inputs at a glance

There are three wrapper functions for binary, continuous, and survival
outcomes, respectively:

- [`simulate_binary()`](https://pmsims-package.github.io/pmsims/reference/simulate_binary.md)
- [`simulate_continuous()`](https://pmsims-package.github.io/pmsims/reference/simulate_continuous.md)
- [`simulate_survival()`](https://pmsims-package.github.io/pmsims/reference/simulate_survival.md)

All three functions share the same basic structure. The table below
lists the key inputs.

Select wrapper

Binary

Continuous

Survival

[TABLE]

> Notes:
>
> - `maximum_achievable_*` represents the best plausible performance
>   with effectively unlimited data and calibrates the data generator.
> - `target_performance` is the minimum acceptable performance threshold
>   used to determine the required sample size.
> - For reproducibility, set a random seed
>   ([`set.seed()`](https://rdrr.io/r/base/Random.html)).

## Installation

``` r

# install.packages("remotes")
# remotes::install_github("pmsims-package/pmsims")
library(pmsims)
```

## Binary-outcome example

We target the smallest *n* that meets the **assurance** criterion.

``` r

set.seed(123)

binary_example <- simulate_binary(
  signal_parameters = 20,
  noise_parameters  = 0,
  predictor_type = "continuous",
  binary_predictor_prevalence = NULL,
  outcome_prevalence = 0.30,
  maximum_achievable_cstatistic = 0.80,
  model = "glm",
  metric = "calibration_slope",
  target_performance = 0.85,
  n_reps_total = 1000,
  mean_or_assurance = "assurance"
)

binary_example
```

    #>                     ┌────────────────────────────────────────┐
    #>                     │ pmsims: Sample size simulation summary │
    #>                     └────────────────────────────────────────┘
    #> ──────────────────────────────────── Inputs ────────────────────────────────────
    #>                                Outcome : binary
    #>                         Predictor type : continuous
    #>                      Signal predictors : 20
    #>                       Noise predictors : 0
    #>                             Prevalence : 0.3
    #>      Expected large-sample performance : C-statistic ('maximum_achievable_cstatistic') = 0.800
    #>   Target for chosen performance metric : Calib Slope ('calib_slope') = 0.850
    #>                                  Model : glm
    #>                        Simulation reps : 1,000
    #> ──────────────────────────────────── Results ───────────────────────────────────
    #>              Final minimum sample size : 1,044
    #>             Estimated performance at N : 0.849 (Calib Slope ('calib_slope') = 0.850)
    #>            Estimated other metric at N : 0.782 (Auc ('auc'))
    #>                                  Model : glm
    #>                                   Mode : Assurance
    #>                           Running time : 2 minutes 39 seconds
    #>     Assurance mode ensures the target metric is met with high probability across repeated datasets.

Plot the estimated learning curve and identified sample size:

``` r

plot(binary_example)
```

![Plot showing learning curve for binary
outcome](pmsims_files/figure-html/unnamed-chunk-3-1.png)

## Continuous-outcome example

``` r

continuous_example <- simulate_continuous(
  signal_parameters = 15,
  noise_parameters = 0,
  predictor_type = "continuous",
  maximum_achievable_rsquared = 0.50,
  model = "lm",
  metric = "calibration_slope",
  target_performance = 0.90,
  n_reps_total = 1000,
  mean_or_assurance = "assurance"
)

continuous_example
```

    #>                     ┌────────────────────────────────────────┐
    #>                     │ pmsims: Sample size simulation summary │
    #>                     └────────────────────────────────────────┘
    #> ──────────────────────────────────── Inputs ────────────────────────────────────
    #>                                Outcome : continuous
    #>                         Predictor type : continuous
    #>                      Signal predictors : 15
    #>                       Noise predictors : 0
    #>      Expected large-sample performance : R² ('maximum_achievable_rsquared') = 0.500
    #>   Target for chosen performance metric : Calib Slope ('calib_slope') = 0.900
    #>                                  Model : lm
    #>                        Simulation reps : 1,000
    #> ──────────────────────────────────── Results ───────────────────────────────────
    #>              Final minimum sample size : 239
    #>             Estimated performance at N : 0.900 (Calib Slope ('calib_slope') = 0.900)
    #>            Estimated other metric at N : 0.486 (R2 ('r2'))
    #>                                  Model : lm
    #>                                   Mode : Assurance
    #>                           Running time : 52 seconds
    #>     Assurance mode ensures the target metric is met with high probability across repeated datasets.

``` r

plot(continuous_example)
```

![Plot showing learning curve for continuous
outcome](pmsims_files/figure-html/unnamed-chunk-6-1.png)
