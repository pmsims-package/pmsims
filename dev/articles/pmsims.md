# Getting started with pmsims

## What pmsims does

**pmsims** estimates the **minimum sample size** needed to develop a
prediction model to achieve a target level of performance **with
assurance**. Rather than relying on simple rules of thumb or closed‑form
formulae, pmsims uses **simulation** to:

- Generate synthetic datasets that reflect your target setting (outcome
  type, prevalence or \\R^2\\, signal vs. noise predictors, and how
  complex the underlying signal is);
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

- [`simulate_binary()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_binary.md)
- [`simulate_continuous()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_continuous.md)
- [`simulate_survival()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_survival.md)

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
> - `complexity` and `data_control` describe the data-generating
>   mechanism, not the model you plan to fit. The same configuration is
>   used both to calibrate the generator against `maximum_achievable_*`
>   and to simulate the data, so a more complex signal generally implies
>   a larger required sample size.
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
  complexity = 1,
  data_control = list(correlation = 0.3),
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
    #> 
    #> ──────────────────────────────────── Inputs ────────────────────────────────────
    #> 
    #> Data-generating scenario
    #>   Outcome                   Binary
    #>   Prevalence                0.30
    #>   Predictors                20 signal
    #>   Predictor distribution    Normal
    #>   Predictor correlation     0.30
    #>   Signal form               Linear
    #> 
    #> Model and performance
    #>   Model                     Logistic regression
    #>   Large-sample C-statistic  0.800
    #>   Sample-size criterion     Calibration slope ≥ 0.850
    #> 
    #> Simulation
    #>   Mode                      Assurance
    #>   Replications              1,000
    #> 
    #> ──────────────────────────────────── Results ───────────────────────────────────
    #> 
    #>   Minimum sample size       985
    #> 
    #>   Performance at N = 985
    #>     Calibration slope       0.849    (target ≥ 0.850)
    #>     C-statistic             0.794
    #> 
    #>   Running time              2 minutes 43 seconds
    #> 
    #> ────────────────────────────────────────────────────────────────────────────────
    #> Assurance mode selects N so that the target is achieved with high probability
    #> across repeated datasets.

The printed summary is a human-readable report. Implementation detail —
the internal metric identifiers, the engine settings used for the
search, and any quantities recorded on an internal search scale — is
available through `summary(binary_example)` or, equivalently,
`print(binary_example, verbose = TRUE)`.

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
  complexity = 1,
  data_control = list(correlation = 0.3),
  maximum_achievable_rsquared = 0.50,
  model = "lm",
  metric = "calibration_slope",
  target_performance = 0.95,
  n_reps_total = 1000,
  mean_or_assurance = "assurance"
)

continuous_example
```

    #>                     ┌────────────────────────────────────────┐
    #>                     │ pmsims: Sample size simulation summary │
    #>                     └────────────────────────────────────────┘
    #> 
    #> ──────────────────────────────────── Inputs ────────────────────────────────────
    #> 
    #> Data-generating scenario
    #>   Outcome                  Continuous
    #>   Predictors               15 signal
    #>   Predictor distribution   Normal
    #>   Predictor correlation    0.30
    #>   Signal form              Linear
    #> 
    #> Model and performance
    #>   Model                    Linear regression
    #>   Large-sample R²          0.500
    #>   Sample-size criterion    Calibration slope ≥ 0.950
    #> 
    #> Simulation
    #>   Mode                     Assurance
    #>   Replications             1,000
    #> 
    #> ──────────────────────────────────── Results ───────────────────────────────────
    #> 
    #>   Minimum sample size      685
    #> 
    #>   Performance at N = 685
    #>     Calibration slope      0.950    (target ≥ 0.950)
    #>     R²                     0.488
    #> 
    #>   Running time             1 minute 51 seconds
    #> 
    #> ────────────────────────────────────────────────────────────────────────────────
    #> Assurance mode selects N so that the target is achieved with high probability
    #> across repeated datasets.

``` r

plot(continuous_example)
```

![Plot showing learning curve for continuous
outcome](pmsims_files/figure-html/unnamed-chunk-6-1.png)
