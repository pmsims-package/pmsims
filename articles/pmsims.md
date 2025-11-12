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

![](workflow.png)

![A figure showing the package workflow.](workflow.png)

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

| Argument                         | Applies to      | Req | Default                      | Description                                                                                                                     |
|----------------------------------|-----------------|:---:|------------------------------|---------------------------------------------------------------------------------------------------------------------------------|
| `signal_parameters`              | all             |  ✓  | —                            | *(int)* Number of **true signal** predictors associated with the outcome.                                                       |
| `noise_parameters`               | all             |     | `0`                          | *(int)* Number of **noise** predictors unrelated to the outcome.                                                                |
| `predictor_type`                 | all             |     | `"continuous"`               | *(chr)* Type of simulated predictors (`"continuous"` or `"binary"`).                                                            |
| `binary_predictor_prevalence`    | all             |     | `NULL`                       | *(num 0–1)* Prevalence for binary predictors (used only if `predictor_type = "binary"`).                                        |
| `outcome_prevalence`             | binary only     |  ✓  | —                            | *(num 0–1)* Target prevalence of the binary outcome.                                                                            |
| `large_sample_cstatistic`        | binary only     |  ✓  | —                            | *(num 0–1)* Expected **C-statistic** for a very large training sample.                                                          |
| `large_sample_rsquared`          | continuous only |  ✓  | —                            | *(num 0–1)* Expected R² for a very large training sample.                                                                       |
| `large_sample_cindex`            | survival only   |  ✓  | —                            | *(num 0–1)* Expected **concordance index** for a very large training sample.                                                    |
| `model`                          | all             |     | `"glm"` / `"lm"` / `"coxph"` | *(chr)* Model used for fitting (e.g. logistic, linear, or Cox).                                                                 |
| `metric`                         | all             |     | `"calibration_slope"`        | *(chr)* **Performance metric** used to estimate the **minimum required sample size** (e.g. calibration slope, R², C-statistic). |
| `minimum_acceptable_performance` | all             |  ✓  | —                            | *(num)* **Minimum acceptable performance** in the **units of the chosen metric** (e.g. calibration slope ≥ 0.9).                |
| `n_reps_total`                   | all             |  ✓  | `1000`                       | *(int)* Total number of simulation replications.                                                                                |
| `mean_or_assurance`              | all             |     | `"assurance"`                | *(chr)* Criterion for summarising results; `"assurance"` recommended.                                                           |

> Notes:
>
> - The engine automatically tunes the data generator so the chosen
>   model reaches the specified **large‑sample performance**
>   (C‑statistic or \\R^2\\) on very large samples.
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
  signal_parameters = 15,
  noise_parameters  = 0,
  predictor_type = "continuous",
  binary_predictor_prevalence = NULL,
  outcome_prevalence = 0.20,
  large_sample_cstatistic = 0.80,
  model = "glm",
  metric = "calibration_slope",
  minimum_acceptable_performance = 0.90,
  n_reps_total = 1000,
  mean_or_assurance = "assurance"
)

binary_example
```

Plot the estimated learning curve and identified sample size:

``` r
plot(binary_example)
```

## Continuous-outcome example

``` r
continuous_example <- simulate_continuous(
  signal_parameters = 15,
  noise_parameters = 0,
  predictor_type = "continuous",
  large_sample_rsquared = 0.50,
  model = "lm",
  metric = "calibration_slope",
  minimum_acceptable_performance = 0.90,
  n_reps_total = 1000,
  mean_or_assurance = "assurance"
)

continuous_example
```

``` r
plot(continuous_example)
```

## Session info

``` r
sessionInfo()
#> R version 4.5.2 (2025-10-31)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.3 LTS
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
#> [1] pmsims_0.5.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] cli_3.6.5         knitr_1.50        rlang_1.1.6       xfun_0.54        
#>  [5] textshaping_1.0.4 jsonlite_2.0.0    htmltools_0.5.8.1 ragg_1.5.0       
#>  [9] sass_0.4.10       rmarkdown_2.30    grid_4.5.2        evaluate_1.0.5   
#> [13] jquerylib_0.1.4   fastmap_1.2.0     yaml_2.3.10       lifecycle_1.0.4  
#> [17] compiler_4.5.2    fs_1.6.6          htmlwidgets_1.6.4 lattice_0.22-7   
#> [21] systemfonts_1.3.1 digest_0.6.37     R6_2.6.1          splines_4.5.2    
#> [25] bslib_0.9.0       Matrix_1.7-4      tools_4.5.2       survival_3.8-3   
#> [29] pkgdown_2.2.0     cachem_1.1.0      desc_1.4.3
```
