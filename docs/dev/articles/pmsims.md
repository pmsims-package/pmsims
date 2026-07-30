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
> - For reproducibility, set a random seed
>   ([`set.seed()`](https://rdrr.io/r/base/Random.html)).

## Installation

`# install.packages("remotes")`` ``# remotes::install_github("pmsims-package/pmsims")`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`pmsims`](https://pmsims-package.github.io/pmsims/)`)`

## Binary-outcome example

We target the smallest *n* that meets the **assurance** criterion.

[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`` `` ``binary_example`` ``<-`` `[`simulate_binary`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_binary.md)`(`` `` signal_parameters ``=`` ``20``,`` `` noise_parameters ``=`` ``0``,`` `` predictor_type ``=`` ``"continuous"``,`` `` binary_predictor_prevalence ``=`` ``NULL``,`` `` outcome_prevalence ``=`` ``0.30``,`` `` maximum_achievable_cstatistic ``=`` ``0.80``,`` `` model ``=`` ``"glm"``,`` `` metric ``=`` ``"calibration_slope"``,`` `` target_performance ``=`` ``0.85``,`` `` n_reps_total ``=`` ``1000``,`` `` mean_or_assurance ``=`` ``"assurance"`` ``)`` `` ``binary_example`

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

[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``binary_example``)`

![Plot showing learning curve for binary
outcome](pmsims_files/figure-html/unnamed-chunk-3-1.png)

## Continuous-outcome example

`continuous_example`` ``<-`` `[`simulate_continuous`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_continuous.md)`(`` `` signal_parameters ``=`` ``15``,`` `` noise_parameters ``=`` ``0``,`` `` predictor_type ``=`` ``"continuous"``,`` `` maximum_achievable_rsquared ``=`` ``0.50``,`` `` model ``=`` ``"lm"``,`` `` metric ``=`` ``"calibration_slope"``,`` `` target_performance ``=`` ``0.90``,`` `` n_reps_total ``=`` ``1000``,`` `` mean_or_assurance ``=`` ``"assurance"`` ``)`` `` ``continuous_example`

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

[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``continuous_example``)`

![Plot showing learning curve for continuous
outcome](pmsims_files/figure-html/unnamed-chunk-6-1.png)

## Session info

[`sessionInfo`](https://rdrr.io/r/utils/sessionInfo.html)`(``)`` ``#> R version 4.6.0 (2026-04-24)`` ``#> Platform: aarch64-apple-darwin23`` ``#> Running under: macOS Tahoe 26.5.2`` ``#> `` ``#> Matrix products: default`` ``#> BLAS: /Library/Frameworks/R.framework/Versions/4.6/Resources/lib/libRblas.0.dylib `` ``#> LAPACK: /Library/Frameworks/R.framework/Versions/4.6/Resources/lib/libRlapack.dylib; LAPACK version 3.12.1`` ``#> `` ``#> locale:`` ``#> [1] C.UTF-8/C.UTF-8/C.UTF-8/C/C.UTF-8/C.UTF-8`` ``#> `` ``#> time zone: Europe/London`` ``#> tzcode source: internal`` ``#> `` ``#> attached base packages:`` ``#> [1] stats graphics grDevices utils datasets methods base `` ``#> `` ``#> other attached packages:`` ``#> [1] pmsims_0.5.0.9000`` ``#> `` ``#> loaded via a namespace (and not attached):`` ``#> [1] Matrix_1.7-5 gtable_0.3.6 jsonlite_2.0.0 dplyr_1.2.1 `` ``#> [5] compiler_4.6.0 crayon_1.5.3 tidyselect_1.2.1 jquerylib_0.1.4 `` ``#> [9] splines_4.6.0 systemfonts_1.3.2 scales_1.4.0 textshaping_1.0.5 `` ``#> [13] yaml_2.3.12 fastmap_1.2.0 lattice_0.22-9 ggplot2_4.0.3 `` ``#> [17] R6_2.6.1 labeling_0.4.3 generics_0.1.4 knitr_1.51 `` ``#> [21] htmlwidgets_1.6.4 tibble_3.3.1 desc_1.4.3 pillar_1.11.1 `` ``#> [25] bslib_0.11.0 RColorBrewer_1.1-3 rlang_1.3.0 cachem_1.1.0 `` ``#> [29] xfun_0.60 fs_2.1.0 DiceKriging_1.6.1 sass_0.4.10 `` ``#> [33] S7_0.2.2 otel_0.2.0 cli_3.6.6 withr_3.0.3 `` ``#> [37] magrittr_2.0.5 pkgdown_2.2.1 digest_0.6.39 grid_4.6.0 `` ``#> [41] lifecycle_1.0.5 mlpwr_1.1.1 vctrs_0.7.3 evaluate_1.0.5 `` ``#> [45] glue_1.8.1 farver_2.1.2 ragg_1.5.2 survival_3.8-6 `` ``#> [49] rmarkdown_2.31 pkgconfig_2.0.3 tools_4.6.0 htmltools_0.5.9`
