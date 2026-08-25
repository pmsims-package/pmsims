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
    #> 
    #> ──────────────────────────────────── Inputs ────────────────────────────────────
    #> 
    #> Data-generating scenario
    #>   Outcome                   Binary
    #>   Prevalence                0.30
    #>   Predictors                20 signal
    #>   Predictor type            Continuous
    #> 
    #> Model and performance
    #>   Model                     Logistic regression
    #>   Large-sample C-statistic  0.800
    #>   Sample-size criterion     Calib slope ≥ 0.850
    #> 
    #> Simulation
    #>   Mode                      Assurance
    #>   Replications              1,000
    #> 
    #> ──────────────────────────────────── Results ───────────────────────────────────
    #> 
    #>   Minimum sample size       1,044
    #> 
    #>   Performance at N = 1,044
    #>     Calib slope             0.849    (target ≥ 0.850)
    #>     C-statistic             0.782
    #> 
    #>   Running time              2 minutes 39 seconds
    #> 
    #> ────────────────────────────────────────────────────────────────────────────────
    #> Assurance mode selects N so that the target is achieved with high probability
    #> across repeated datasets.

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
    #> 
    #> ──────────────────────────────────── Inputs ────────────────────────────────────
    #> 
    #> Data-generating scenario
    #>   Outcome                  Continuous
    #>   Predictors               15 signal
    #>   Predictor type           Continuous
    #> 
    #> Model and performance
    #>   Model                    Linear regression
    #>   Large-sample R²          0.500
    #>   Sample-size criterion    Calib slope ≥ 0.900
    #> 
    #> Simulation
    #>   Mode                     Assurance
    #>   Replications             1,000
    #> 
    #> ──────────────────────────────────── Results ───────────────────────────────────
    #> 
    #>   Minimum sample size      239
    #> 
    #>   Performance at N = 239
    #>     Calib slope            0.900    (target ≥ 0.900)
    #>     R²                     0.486
    #> 
    #>   Running time             52 seconds
    #> 
    #> ────────────────────────────────────────────────────────────────────────────────
    #> Assurance mode selects N so that the target is achieved with high probability
    #> across repeated datasets.

``` r

plot(continuous_example)
```

![Plot showing learning curve for continuous
outcome](pmsims_files/figure-html/unnamed-chunk-6-1.png)

## Session info

``` r

sessionInfo()
#> R version 4.6.1 (2026-06-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.4 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] pmsims_1.0.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] Matrix_1.7-5       gtable_0.3.6       jsonlite_2.0.0     dplyr_1.2.1       
#>  [5] compiler_4.6.1     tidyselect_1.2.1   jquerylib_0.1.4    splines_4.6.1     
#>  [9] systemfonts_1.3.2  scales_1.4.0       textshaping_1.0.5  yaml_2.3.12       
#> [13] fastmap_1.2.0      lattice_0.22-9     ggplot2_4.0.3      R6_2.6.1          
#> [17] labeling_0.4.3     generics_0.1.4     knitr_1.51         htmlwidgets_1.6.4 
#> [21] tibble_3.3.1       desc_1.4.3         pillar_1.11.1      bslib_0.12.0      
#> [25] RColorBrewer_1.1-3 rlang_1.3.0        cachem_1.1.0       xfun_0.60         
#> [29] fs_2.1.0           DiceKriging_1.6.1  sass_0.4.10        S7_0.2.2          
#> [33] otel_0.2.0         cli_3.6.6          withr_3.0.3        magrittr_2.0.5    
#> [37] pkgdown_2.2.1      digest_0.6.39      grid_4.6.1         lifecycle_1.0.5   
#> [41] mlpwr_1.1.1        vctrs_0.7.3        evaluate_1.0.5     glue_1.8.1        
#> [45] farver_2.1.2       ragg_1.5.2         survival_3.8-6     rmarkdown_2.31    
#> [49] pkgconfig_2.0.3    tools_4.6.1        htmltools_0.5.9
```
