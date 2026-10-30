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

![](data:image/svg+xml;base64,PHN2ZyBjbGFzcz0icG1zaW1zLXdvcmtmbG93IiB2aWV3Ym94PSIwIDAgODYwIDI5MCIgcm9sZT0iaW1nIiBhcmlhLWxhYmVsPSJUaGUgcG1zaW1zIHdvcmtmbG93OiB0aGUgdXNlciYjMzk7cyBpbnB1dHMgZGVmaW5lIGEgZGF0YSBmdW5jdGlvbiwgbW9kZWwgZnVuY3Rpb24gYW5kIG1ldHJpYyBmdW5jdGlvbi4gVGhlIHNpbXVsYXRpb24gZW5naW5lIHVzZXMgdGhlc2UgdG8gZml0IGEgbGVhcm5pbmcgY3VydmUgYW5kIHJldHVybnMgdGhlIG1pbmltdW0gc2FtcGxlIHNpemUsIG1pbl9uLiI+PHN0eWxlPgogICAgLnBtc2ltcy13b3JrZmxvdyB7CiAgICAgIC0td2YtY2FyZDogdmFyKC0tYnMtdGVydGlhcnktYmcsICNmOGY5ZmEpOwogICAgICAtLXdmLWJvcmRlcjogdmFyKC0tYnMtYm9yZGVyLWNvbG9yLCAjZGVlMmU2KTsKICAgICAgLS13Zi10ZXh0OiB2YXIoLS1icy1ib2R5LWNvbG9yLCAjMzczYTNjKTsKICAgICAgLS13Zi1tdXRlZDogdmFyKC0tYnMtc2Vjb25kYXJ5LWNvbG9yLCAjNmM3NTdkKTsKICAgICAgLS13Zi1saW5lOiAjOGE5NTllOwogICAgICAtLXdmLXRlYWw6ICMwNjdmOGQ7ICAgICAgICAgIC8qIHNpdGUgbGluayBjb2xvdXI7IHdoaXRlIHRleHQgcGFzc2VzIEFBICovCiAgICAgIC0td2YtZGF0YTogIzA0YWRiZjsgICAgICAgICAgLyogc2l0ZSBwcmltYXJ5ICovCiAgICAgIC0td2YtbW9kZWw6ICM0NjgyYjQ7ICAgICAgICAgLyogc3RlZWxibHVlLCBhcyBpbiBwbG90KCkgKi8KICAgICAgLS13Zi1tZXRyaWM6ICNlMDhhMWU7ICAgICAgICAvKiBkYXJrb3JhbmdlIGZhbWlseSwgYXMgaW4gcGxvdCgpICovCiAgICAgIC0td2YtcGFuZWw6ICNmZmY7CiAgICAgIC0td2YtY3VydmU6ICM0NjgyYjQ7CiAgICAgIHdpZHRoOiAxMDAlOyBoZWlnaHQ6IGF1dG87IGRpc3BsYXk6IGJsb2NrOwogICAgICBmb250LWZhbWlseTogIlNvdXJjZSBTYW5zIFBybyIsICJTb3VyY2UgU2FucyAzIiwgc3lzdGVtLXVpLCBzYW5zLXNlcmlmOwogICAgfQogICAgW2RhdGEtYnMtdGhlbWU9ImRhcmsiXSAucG1zaW1zLXdvcmtmbG93IHsKICAgICAgLS13Zi1saW5lOiAjOGY5YWEzOwogICAgICAtLXdmLXRlYWw6ICMwYjhmOWU7CiAgICAgIC0td2YtcGFuZWw6ICMxYjFmMjM7CiAgICAgIC0td2YtY3VydmU6ICM3OWFlZTA7CiAgICB9CiAgICAucG1zaW1zLXdvcmtmbG93IC53Zi1ib3ggeyBmaWxsOiB2YXIoLS13Zi1jYXJkKTsgc3Ryb2tlOiB2YXIoLS13Zi1ib3JkZXIpOyBzdHJva2Utd2lkdGg6IDE7IH0KICAgIC5wbXNpbXMtd29ya2Zsb3cgLndmLWxhYmVsIHsgZm9udC1zaXplOiAyMHB4OyBmb250LXdlaWdodDogNjAwOyBmaWxsOiB2YXIoLS13Zi10ZXh0KTsgdGV4dC1hbmNob3I6IG1pZGRsZTsgfQogICAgLnBtc2ltcy13b3JrZmxvdyAud2YtY29kZSB7IGZvbnQtZmFtaWx5OiAiU291cmNlIENvZGUgUHJvIiwgdWktbW9ub3NwYWNlLCBtb25vc3BhY2U7IGZvbnQtc2l6ZTogMTNweDsKICAgICAgICAgICAgICAgICAgICAgICAgICAgICBmaWxsOiB2YXIoLS13Zi1tdXRlZCk7IHRleHQtYW5jaG9yOiBtaWRkbGU7IH0KICAgIC5wbXNpbXMtd29ya2Zsb3cgLndmLXRpbnQgeyBmaWxsOiBjb2xvci1taXgoaW4gc3JnYiwgdmFyKC0tYWNjZW50KSAxNiUsIHZhcigtLWJzLWJvZHktYmcsICNmZmYpKTsKICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzdHJva2U6IHZhcigtLWFjY2VudCk7IHN0cm9rZS13aWR0aDogMS41OyB9CiAgICAucG1zaW1zLXdvcmtmbG93IC53Zi13aXJlIHsgZmlsbDogbm9uZTsgc3Ryb2tlOiB2YXIoLS13Zi1saW5lKTsgc3Ryb2tlLXdpZHRoOiAyOyBzdHJva2UtbGluZWpvaW46IHJvdW5kOyB9CiAgPC9zdHlsZT4KPGRlZnM+PG1hcmtlciBpZD0icG1zaW1zLXdmLWhlYWQiIHZpZXdib3g9IjAgMCAxMCAxMCIgcmVmeD0iMTAiIHJlZnk9IjUiIG1hcmtlcndpZHRoPSI1IiBtYXJrZXJoZWlnaHQ9IjUiIG1hcmtlcnVuaXRzPSJzdHJva2VXaWR0aCIgb3JpZW50PSJhdXRvIj48cGF0aCBkPSJNMCwwIEwxMCw1IEwwLDEwIHoiIGZpbGw9InZhcigtLXdmLWxpbmUpIiAvPjwvbWFya2VyPjwvZGVmcz48IS0tIFVzZXIgaW5wdXRzIC0tPjxyZWN0IGNsYXNzPSJ3Zi1ib3giIHg9IjgiIHk9IjExMSIgd2lkdGg9IjE0NiIgaGVpZ2h0PSI2OCIgcng9IjgiIHN0cm9rZS1kYXNoYXJyYXk9IjUgNCIgLz48dGV4dCBjbGFzcz0id2YtbGFiZWwiIHg9IjgxIiB5PSIxNDIiPlVzZXIgaW5wdXRzPC90ZXh0Pjx0ZXh0IGNsYXNzPSJ3Zi1jb2RlIiB4PSI4MSIgeT0iMTY0Ij5zaW11bGF0ZV8qKCk8L3RleHQ+PCEtLSBXaXJlczogaW5wdXRzIHRvIGNvbXBvbmVudHMgLS0+PHBhdGggY2xhc3M9IndmLXdpcmUiIG1hcmtlci1lbmQ9InVybCgjcG1zaW1zLXdmLWhlYWQpIiBkPSJNMTU0LDE0NSBIMTgyIFY1MCBIMjA5IiAvPjxwYXRoIGNsYXNzPSJ3Zi13aXJlIiBtYXJrZXItZW5kPSJ1cmwoI3Btc2ltcy13Zi1oZWFkKSIgZD0iTTE1NCwxNDUgSDIwOSIgLz48cGF0aCBjbGFzcz0id2Ytd2lyZSIgbWFya2VyLWVuZD0idXJsKCNwbXNpbXMtd2YtaGVhZCkiIGQ9Ik0xNTQsMTQ1IEgxODIgVjI0MCBIMjA5IiAvPjwhLS0gQ29tcG9uZW50czogbmV1dHJhbCBjYXJkLCBjb2xvdXJlZCBhY2NlbnQgYmFyLCBmdW5jdGlvbiBhcmd1bWVudCBiZWxvdyAtLT48Zz48cmVjdCBjbGFzcz0id2YtYm94IHdmLXRpbnQiIHN0eWxlPSItLWFjY2VudDogdmFyKC0td2YtZGF0YSkiIHg9IjIxMCIgeT0iMTYiIHdpZHRoPSIyMTQiIGhlaWdodD0iNjgiIHJ4PSI4IiAvPjx0ZXh0IGNsYXNzPSJ3Zi1sYWJlbCIgeD0iMzE4IiB5PSI0NiI+RGF0YSBnZW5lcmF0b3I8L3RleHQ+PHRleHQgY2xhc3M9IndmLWNvZGUiIHg9IjMxOCIgeT0iNjgiPmRhdGFfZnVuY3Rpb248L3RleHQ+PC9nPjxnPjxyZWN0IGNsYXNzPSJ3Zi1ib3ggd2YtdGludCIgc3R5bGU9Ii0tYWNjZW50OiB2YXIoLS13Zi1tb2RlbCkiIHg9IjIxMCIgeT0iMTExIiB3aWR0aD0iMjE0IiBoZWlnaHQ9IjY4IiByeD0iOCIgLz48dGV4dCBjbGFzcz0id2YtbGFiZWwiIHg9IjMxOCIgeT0iMTQxIj5QcmVkaWN0aW9uIG1vZGVsPC90ZXh0Pjx0ZXh0IGNsYXNzPSJ3Zi1jb2RlIiB4PSIzMTgiIHk9IjE2MyI+bW9kZWxfZnVuY3Rpb248L3RleHQ+PC9nPjxnPjxyZWN0IGNsYXNzPSJ3Zi1ib3ggd2YtdGludCIgc3R5bGU9Ii0tYWNjZW50OiB2YXIoLS13Zi1tZXRyaWMpIiB4PSIyMTAiIHk9IjIwNiIgd2lkdGg9IjIxNCIgaGVpZ2h0PSI2OCIgcng9IjgiIC8+PHRleHQgY2xhc3M9IndmLWxhYmVsIiB4PSIzMTgiIHk9IjIzNiI+TWV0cmljczwvdGV4dD48dGV4dCBjbGFzcz0id2YtY29kZSIgeD0iMzE4IiB5PSIyNTgiPm1ldHJpY19mdW5jdGlvbjwvdGV4dD48L2c+PCEtLSBXaXJlczogY29tcG9uZW50cyB0byBlbmdpbmUgLS0+PHBhdGggY2xhc3M9IndmLXdpcmUiIG1hcmtlci1lbmQ9InVybCgjcG1zaW1zLXdmLWhlYWQpIiBkPSJNNDI0LDUwIEg0NTIgVjEyMiBINDgzIiAvPjxwYXRoIGNsYXNzPSJ3Zi13aXJlIiBtYXJrZXItZW5kPSJ1cmwoI3Btc2ltcy13Zi1oZWFkKSIgZD0iTTQyNCwxNDUgSDQ4MyIgLz48cGF0aCBjbGFzcz0id2Ytd2lyZSIgbWFya2VyLWVuZD0idXJsKCNwbXNpbXMtd2YtaGVhZCkiIGQ9Ik00MjQsMjQwIEg0NTIgVjE2OCBINDgzIiAvPjwhLS0gU2ltdWxhdGlvbiBlbmdpbmU6IGhlYWRlciBiYW5kIGluIHNpdGUgdGVhbCwgcGFuZWwgc3R5bGVkIGxpa2UgcGxvdCgpIC0tPjxnPjxyZWN0IGNsYXNzPSJ3Zi1ib3giIHg9IjQ4NCIgeT0iNDAiIHdpZHRoPSIyMjYiIGhlaWdodD0iMjEwIiByeD0iOCIgLz48cGF0aCBkPSJNNDg0LDQ4IGE4LDggMCAwIDEgOCwtOCBoMjEwIGE4LDggMCAwIDEgOCw4IHYyOCBoLTIyNnoiIGZpbGw9InZhcigtLXdmLXRlYWwpIiAvPjx0ZXh0IGNsYXNzPSJ3Zi1sYWJlbCIgeD0iNTk3IiB5PSI2NSIgc3R5bGU9ImZpbGw6I2ZmZiI+U2ltdWxhdGlvbiBlbmdpbmU8L3RleHQ+PHJlY3QgeD0iNDk4IiB5PSI5MCIgd2lkdGg9IjE5OCIgaGVpZ2h0PSIxNDYiIHJ4PSIzIiBmaWxsPSJ2YXIoLS13Zi1wYW5lbCkiIHN0cm9rZT0idmFyKC0td2YtYm9yZGVyKSIgLz48IS0tIGludGVydmFsIGFyb3VuZCBtaW5fbiAtLT48cmVjdCB4PSI1ODQiIHk9IjkxIiB3aWR0aD0iMzAiIGhlaWdodD0iMTQ0IiBmaWxsPSJ2YXIoLS13Zi1jdXJ2ZSkiIG9wYWNpdHk9Ii4xNCIgLz48IS0tIHRhcmdldCAtLT48bGluZSB4MT0iNDk5IiB5MT0iMTQ2IiB4Mj0iNjk1IiB5Mj0iMTQ2IiBzdHJva2U9InZhcigtLXdmLW11dGVkKSIgc3Ryb2tlLXdpZHRoPSIxLjUiIHN0cm9rZS1kYXNoYXJyYXk9IjUgNCI+PC9saW5lPjwhLS0gbWluX24gLS0+PGxpbmUgeDE9IjU5OCIgeTE9IjkxIiB4Mj0iNTk4IiB5Mj0iMjM1IiBzdHJva2U9InZhcigtLXdmLWN1cnZlKSIgc3Ryb2tlLXdpZHRoPSIxLjUiPjwvbGluZT48IS0tIGN1cnZlIC0tPjxwYXRoIGQ9Ik01MTAsMjIyIEM1NjAsMTc0IDYwMCwxMzkgNjg0LDEyMCIgZmlsbD0ibm9uZSIgc3Ryb2tlPSJ2YXIoLS13Zi1jdXJ2ZSkiIHN0cm9rZS13aWR0aD0iMi41IiBzdHJva2UtbGluZWNhcD0icm91bmQiIC8+PCEtLSByZXBsaWNhdGVzIChwb2ludCBzaXplID0gbnVtYmVyIG9mIHJlcGxpY2F0ZXMpIC0tPjxnIGZpbGw9InZhcigtLXdmLXBhbmVsKSIgc3Ryb2tlPSJ2YXIoLS13Zi10ZXh0KSIgc3Ryb2tlLXdpZHRoPSIxLjMiPjxjaXJjbGUgY3g9IjUxNiIgY3k9IjIxNCIgcj0iMi41Ij48L2NpcmNsZT48Y2lyY2xlIGN4PSI1NDIiIGN5PSIxOTkiIHI9IjMiPjwvY2lyY2xlPjxjaXJjbGUgY3g9IjU3MiIgY3k9IjE2NiIgcj0iMy41Ij48L2NpcmNsZT48Y2lyY2xlIGN4PSI1OTYiIGN5PSIxNDkiIHI9IjUiPjwvY2lyY2xlPjxjaXJjbGUgY3g9IjYxMiIgY3k9IjEzOSIgcj0iNC41Ij48L2NpcmNsZT48Y2lyY2xlIGN4PSI2NTQiIGN5PSIxMjQiIHI9IjMiPjwvY2lyY2xlPjwvZz48L2c+PCEtLSBPdXRwdXQgLS0+PHBhdGggY2xhc3M9IndmLXdpcmUiIG1hcmtlci1lbmQ9InVybCgjcG1zaW1zLXdmLWhlYWQpIiBkPSJNNzEwLDE0NSBINzU1IiAvPjxyZWN0IHg9Ijc1NiIgeT0iMTIyIiB3aWR0aD0iOTYiIGhlaWdodD0iNDYiIHJ4PSIyMyIgZmlsbD0idmFyKC0td2YtdGVhbCkiIC8+PHRleHQgeD0iODA0IiB5PSIxNTIiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZpbGw9IiNmZmYiIGZvbnQtc2l6ZT0iMjQiIGZvbnQtd2VpZ2h0PSI2MDAiIGZvbnQtc3R5bGU9Iml0YWxpYyI+Tjx0c3BhbiBkeT0iLTciIGZvbnQtc2l6ZT0iMTYiPio8L3RzcGFuPjwvdGV4dD48dGV4dCBjbGFzcz0id2YtY29kZSIgeD0iODA0IiB5PSIxOTIiPm1pbl9uPC90ZXh0Pjwvc3ZnPg==)

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

All three take the same core inputs:

| Argument | Default | Description |
|----|----|----|
| `signal_parameters` | required | Number of predictors associated with the outcome |
| `noise_parameters` | `0` | Number of predictors unrelated to the outcome |
| `complexity` | `1` | Signal structure: `1` linear, `2` adds quadratic terms, `3` adds an interaction, `4` the Friedman function |
| `data_control` | `NULL` | Correlation, distribution and nonlinear strength of the predictors (see [`simulate_binary()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_binary.md)) |
| `model` | `"glm"`, `"lm"` or `"coxph"` | Model fitted to each simulated dataset (the penalised and machine-learning options are experimental) |
| `metric` | `"calibration_slope"` | Performance metric: `"calibration_slope"`, `"auc"`, `"r2"`, `"cindex"` or `"csse"` |
| `target_performance` | required | Minimum acceptable performance |
| `n_reps_total` | `1000` | Total number of simulation replicates |
| `mean_or_assurance` | `"assurance"` | Criterion used to choose the sample size |

The remaining inputs depend on the outcome:

| Function | Outcome-specific inputs | Models |
|----|----|----|
| [`simulate_binary()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_binary.md) | `maximum_achievable_cstatistic`, `outcome_prevalence` | `"glm"`, `"lasso"`, `"ridge"`, `"rf"`, `"xgboost"` |
| [`simulate_continuous()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_continuous.md) | `maximum_achievable_rsquared` | `"lm"`, `"lasso"`, `"ridge"`, `"rf"`, `"xgboost"` |
| [`simulate_survival()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_survival.md) | `maximum_achievable_cindex`, `censoring_rate`, `baseline_hazard` (optional; default `1`) | `"coxph"`, `"lasso"`, `"ridge"`, `"rf"`, `"xgboost"` |

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

Install the stable release from
[CRAN](https://CRAN.R-project.org/package=pmsims):

``` r

# install.packages("pmsims")
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
    #>   Minimum sample size       1,000
    #> 
    #>   Performance at N = 1,000
    #>     Calibration slope       0.850    (target ≥ 0.850)
    #>     C-statistic             0.790
    #> 
    #>   Running time              27 seconds
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

The animation below shows the same search as it ran. With
`live_plot = TRUE`, the plot is redrawn after each batch of 20
replicates. The final frame is what `plot(binary_example)` draws: the
20th percentile of the calibration slope at each simulated sample size
(point size shows the number of replicates, with ±2 standard errors),
the fitted learning curve, the target (dashed line), the minimum sample
size (solid line) with its interval (shaded), and a shape-free
cross-check (dotted line).

## How the search works

By default (`method = "curve"`), `pmsims` fits a learning curve, \\C(n)
= a - b n^{-c}\\, to the criterion at every sample size simulated so
far. A pilot doubles or halves \\n\\ until the criterion has been seen
on both sides of the target. Each further batch is placed where the
fitted curve crosses the target, and the answer is where the final curve
crosses it. The answer is then simulated again with fresh replicates to
check that it meets the target.

Every result has a `status`. `"ok"` means a sample size was found and
the check confirmed it. Otherwise `min_n` may be `NA`, and
`status_message` explains why (for example, `"unreachable"` when the
curve levels off below the target). The previous Gaussian-process engine
is available with `method = "mlpwr"`; see
[`?simulate_custom`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_custom.md)
for details of both engines.

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
    #>   Minimum sample size      673
    #> 
    #>   Performance at N = 673
    #>     Calibration slope      0.950    (target ≥ 0.950)
    #>     R²                     0.491
    #> 
    #>   Running time             16 seconds
    #> 
    #> ────────────────────────────────────────────────────────────────────────────────
    #> Assurance mode selects N so that the target is achieved with high probability
    #> across repeated datasets.

``` r

plot(continuous_example)
```

![Plot showing learning curve for continuous
outcome](pmsims_files/figure-html/unnamed-chunk-6-1.png)
