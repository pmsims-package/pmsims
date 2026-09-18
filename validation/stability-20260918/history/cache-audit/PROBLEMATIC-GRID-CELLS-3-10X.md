# Problematic grid cells: 3–10× neighbour differences

**Cache:** `cache-1f-create-20260827-161720`  |  **Report date:** 17 September 2026

## Summary

Of the **206 isolated spikes identified in the earlier investigation, 188 cells have at least one neighbouring sample-size estimate 3–10× higher or lower**. All 188 are already in the cache exclusion list. **180** qualify using literal immediately adjacent grid points; the other **8** qualify only when failed grid points are skipped.

The clearest example is the originally reported binary/ridge cell: **15 predictors, prevalence 0.25, maximum C-statistic 0.85, target slope 0.90**. Its cached N is **10,672**, versus **1,288** at 10 predictors and **1,764** at 20 predictors: **8.29× and 6.05× higher**, respectively. Along the prevalence axis, the neighbours are **1,767** at 0.20 and **1,522** at 0.30: **6.04× and 7.01× higher**. The earlier investigation records a fresh local estimate of **1,691**; that is a separate rerun, not a grid neighbour.

## Distribution of identified isolated spikes

| Outcome | Model | Earlier isolated spikes, all magnitudes | Cells with a 3–10× neighbour difference |
|---|---|---:|---:|
| binary | glm | 35 | 27 |
| binary | lasso | 39 | 37 |
| binary | ridge | 96 | 90 |
| continuous | lasso | 1 | 1 |
| continuous | lm | 0 | 0 |
| continuous | ridge | 0 | 0 |
| survival | coxph | 9 | 7 |
| survival | lasso | 9 | 9 |
| survival | ridge | 17 | 17 |
| **Total** | | **206** | **188** |

**137 of the 188 cells (72.9%) have target slope 0.95.** Ridge and lasso account for **154 of 188 (81.9%)**. Binary outcomes account for **154**, survival **33**, and continuous **1**. These are patterns in the flagged cells, not independently verified failure rates.

## What the neighbour screen means

- Compare `minimum_sample_size` within each outcome and model, holding all inputs fixed except one swept dimension.
- A 3–10× difference means `max(N_cell, N_neighbour) / min(N_cell, N_neighbour)` is between **3 and 10 inclusive**. Differences above 10× are outside this report’s requested range.
- The earlier isolated-spike rule additionally requires two valid neighbours that agree within **1.6×**, with the candidate differing by at least **3×** from their average. The 188-cell list below applies the requested pairwise range to those previously identified spikes.
- The earlier audit used previous/next **available successful** points. Reproducing that convention exactly recovers its **206 isolated cells** and **1,497 adjacent comparisons ≥5×**. Skipping failures can bridge a grid gap; this report marks those comparisons.
- Multiple comparisons can implicate the same cell. Cell counts are deduplicated; pair counts are not. Comparisons never cross models or outcomes.

## Broader 3–10× jumps

A screen without the smooth-neighbour requirement finds **3,708 comparisons involving 5,151 distinct cells** under the earlier available-point convention. **868** of those cells are in the existing exclusion list. With literal immediate grid neighbours, the counts are **3,650 comparisons**, **5,093 distinct cells**, and **861 excluded cells**. The remaining **58 comparisons** bridge failed grid points.

| Outcome | Model | 3–10× comparisons, available-point convention |
|---|---|---:|
| binary | glm | 487 |
| binary | lasso | 750 |
| binary | ridge | 1,012 |
| continuous | lasso | 48 |
| continuous | lm | 22 |
| continuous | ridge | 53 |
| survival | coxph | 267 |
| survival | lasso | 511 |
| survival | ridge | 558 |
| **Total** | | **3,708** |

**1,963 of 3,708 comparisons (52.9%) vary target slope.** Large changes on this axis can also reflect a genuinely demanding target. A jump identifies a large difference between a pair; by itself it does not determine whether either endpoint is wrong.

The [complete comparison appendix](PROBLEMATIC-GRID-CELLS-3-10X-COMPARISONS.md) lists every qualifying pair, both Ns, the direction and ratio, exclusion status, and grid gaps.

## Cause, confidence, and current handling

The investigation reproduced a sample-size search defect in selected scenarios: an unlucky preliminary test dataset and a plateau below the target could produce a misleading bracket; the final search was then constrained inside that bracket. This explains the reproduced cases. The flagged cells have not each been re-simulated, and this report does not establish corrected sample sizes.

The existing exclusion file contains **900 cells**: 621 binary, 12 continuous, and 267 survival. It covers a broader set than the isolated-spike list. For this matching cache version, backend lookups omit excluded rows and choose a remaining neighbour. That is a workaround; it does not repair the stored estimates.

## Identified cells in the requested range

One row per cell is shown below. Where several axes qualify, a slice without gaps is preferred, then the slice with the largest candidate-to-neighbour-average deviation. The full appendix contains all qualifying pairwise comparisons.

`Previous/current/next` refer to the swept input, with every other input and the model held fixed. Ratios describe the **candidate’s N relative to the neighbour’s N**; “lower” means divide the neighbour’s N by the candidate’s N. Both neighbour ratios are shown even if one lies outside 3–10×. **Gap** means this displayed slice skips at least one failed grid point. Scenario keys encode all scenario inputs; `p` is predictor count, `prev` prevalence, `cstat`/`cindex` discrimination, `r2` maximum R², `hazard` baseline hazard, `censor` censoring, `target` target slope, and `m` model. Keys round some input values: for example, `prev0.07` represents actual prevalence **0.075** in this cache; exact values are in the JSON and the swept-input column.

### Binary

| Scenario key | Swept input: previous → current → next | Previous N | Cell N | Next N | Cell vs previous | Cell vs next | Gap |
|---|---|---:|---:|---:|---|---|---|
| `p100_prev0.05_cstat0.75_target0.85_mridge` | Target slope: 0.8 → 0.85 → 0.9 | 6,437 | **26,667** | 10,000 | 4.14× higher | 2.67× higher | — |
| `p100_prev0.05_cstat0.75_target0.90_mridge` | C-statistic: 0.7 → 0.75 → 0.8 | 42,557 | **10,000** | 50,000 | 4.26× lower | 5.00× lower | — |
| `p100_prev0.07_cstat0.60_target0.95_mlasso` | Prevalence: 0.05 → 0.075 → 0.1 | 157,206 | **533,336** | 164,124 | 3.39× higher | 3.25× higher | — |
| `p100_prev0.07_cstat0.75_target0.90_mridge` | Prevalence: 0.05 → 0.075 → 0.1 | 10,000 | **71,108** | 13,333 | 7.11× higher | 5.33× higher | — |
| `p100_prev0.10_cstat0.75_target0.95_mridge` | C-statistic: 0.7 → 0.75 → 0.8 | 21,028 | **106,664** | 25,000 | 5.07× higher | 4.27× higher | — |
| `p100_prev0.15_cstat0.80_target0.95_mlasso` | C-statistic: 0.75 → 0.8 → 0.85 | 14,568 | **4,174** | 15,688 | 3.49× lower | 3.76× lower | — |
| `p100_prev0.20_cstat0.60_target0.80_mridge` | Prevalence: 0.15 → 0.2 → 0.25 | 18,717 | **2,585** | 20,000 | 7.24× lower | 7.74× lower | — |
| `p100_prev0.25_cstat0.80_target0.95_mlasso` | Prevalence: 0.2 → 0.25 → 0.3 | 6,066 | **16,844** | 4,241 | 2.78× higher | 3.97× higher | — |
| `p100_prev0.30_cstat0.65_target0.95_mlasso` | Prevalence: 0.25 → 0.3 → 0.4 | 21,418 | **56,128** | 15,819 | 2.62× higher | 3.55× higher | — |
| `p100_prev0.40_cstat0.85_target0.95_mlasso` | Prevalence: 0.3 → 0.4 → 0.5 | 3,508 | **10,528** | 3,164 | 3.00× higher | 3.33× higher | — |
| `p10_prev0.05_cstat0.60_target0.80_mlasso` | Predictors: 5 → 10 → 15 | 5,000 | **20,000** | 3,750 | 4.00× higher | 5.33× higher | — |
| `p10_prev0.05_cstat0.65_target0.95_mglm` | Predictors: 5 → 10 → 15 | 36,057 | **7,218** | 55,384 | 5.00× lower | 7.67× lower | — |
| `p10_prev0.07_cstat0.65_target0.90_mglm` | Predictors: 5 → 10 → 15 | 6,167 | **24,616** | 8,975 | 3.99× higher | 2.74× higher | — |
| `p10_prev0.07_cstat0.65_target0.95_mridge` | Predictors: 5 → 10 → 15 | 196,928 | **24,616** | 295,392 | 8.00× lower | 12.00× lower | — |
| `p10_prev0.07_cstat0.70_target0.90_mridge` | Predictors: 5 → 10 → 15 | 6,414 | **22,856** | 8,041 | 3.56× higher | 2.84× higher | — |
| `p10_prev0.07_cstat0.70_target0.95_mlasso` | Prevalence: 0.05 → 0.075 → 0.1 | 31,977 | **182,848** | 34,289 | 5.72× higher | 5.33× higher | — |
| `p10_prev0.07_cstat0.75_target0.95_mridge` | C-statistic: 0.7 → 0.75 → 0.8 | 65,945 | **12,994** | 82,409 | 5.08× lower | 6.34× lower | — |
| `p10_prev0.07_cstat0.80_target0.95_mridge` | C-statistic: 0.75 → 0.8 → 0.85 | 12,994 | **82,409** | 18,916 | 6.34× higher | 4.36× higher | — |
| `p10_prev0.10_cstat0.60_target0.95_mridge` | Predictors: 5 → 10 → 15 | 320,000 | **80,000** | 240,000 | 4.00× lower | 3.00× lower | — |
| `p10_prev0.10_cstat0.70_target0.95_mglm` | Prevalence: 0.075 → 0.1 → 0.15 | 5,523 | **34,288** | 5,714 | 6.21× higher | 6.00× higher | — |
| `p10_prev0.10_cstat0.75_target0.95_mridge` | C-statistic: 0.7 → 0.75 → 0.8 | 68,576 | **21,328** | 80,000 | 3.22× lower | 3.75× lower | — |
| `p10_prev0.10_cstat0.80_target0.95_mridge` | Predictors: 5 → 10 → 15 | 15,295 | **80,000** | 15,701 | 5.23× higher | 5.10× higher | — |
| `p10_prev0.15_cstat0.80_target0.95_mlasso` | Prevalence: 0.1 → 0.15 → 0.2 | 4,908 | **13,344** | 3,758 | 2.72× higher | 3.55× higher | — |
| `p10_prev0.20_cstat0.65_target0.95_mglm` | Predictors: 5 → 10 → 15 | 18,464 | **4,603** | 27,696 | 4.01× lower | 6.02× lower | — |
| `p10_prev0.20_cstat0.70_target0.95_mridge` | Prevalence: 0.15 → 0.2 → 0.25 | 45,712 | **11,757** | 53,888 | 3.89× lower | 4.58× lower | — |
| `p10_prev0.20_cstat0.85_target0.90_mridge` | Prevalence: 0.15 → 0.2 → 0.25 | 1,534 | **9,408** | 1,288 | 6.13× higher | 7.30× higher | — |
| `p10_prev0.25_cstat0.65_target0.95_mridge` | C-statistic: 0.6 → 0.65 → 0.7 | 40,550 | **12,466** | 53,888 | 3.25× lower | 4.32× lower | — |
| `p10_prev0.25_cstat0.70_target0.95_mglm` | Prevalence: 0.2 → 0.25 → 0.3 | 4,050 | **26,944** | 5,736 | 6.65× higher | 4.70× higher | — |
| `p10_prev0.25_cstat0.70_target0.95_mridge` | Prevalence: 0.2 → 0.25 → 0.3 | 11,757 | **53,888** | 7,375 | 4.58× higher | 7.31× higher | — |
| `p10_prev0.25_cstat0.85_target0.90_mridge` | Predictors: 5 → 10 → 15 | 7,075 | **1,288** | 10,672 | 5.49× lower | 8.29× lower | — |
| `p10_prev0.30_cstat0.65_target0.95_mlasso` | Prevalence: 0.25 → 0.3 → 0.4 | 6,736 | **27,514** | 8,416 | 4.08× higher | 3.27× higher | — |
| `p10_prev0.40_cstat0.85_target0.95_mridge` | Predictors: 5 → 10 → 15 | 16,896 | **3,246** | 12,640 | 5.21× lower | 3.89× lower | — |
| `p10_prev0.50_cstat0.65_target0.95_mridge` | Predictors: 5 → 10 → 15 | 20,164 | **65,550** | 20,616 | 3.25× higher | 3.18× higher | — |
| `p10_prev0.50_cstat0.80_target0.95_mridge` | Predictors: 5 → 10 → 15 | 13,440 | **1,688** | 10,112 | 7.96× lower | 5.99× lower | — |
| `p10_prev0.50_cstat0.85_target0.95_mridge` | Predictors: 5 → 10 → 15 | 13,440 | **1,688** | 12,549 | 7.96× lower | 7.43× lower | — |
| `p15_prev0.05_cstat0.60_target0.80_mlasso` | Predictors: 10 → 15 → 20 | 20,000 | **3,750** | 20,000 | 5.33× lower | 5.33× lower | — |
| `p15_prev0.05_cstat0.85_target0.95_mridge` | Predictors: 10 → 15 → 20 | 18,824 | **56,464** | 14,088 | 3.00× higher | 4.01× higher | — |
| `p15_prev0.07_cstat0.60_target0.85_mridge` | Predictors: 10 → 15 → 20 | 13,334 | **80,000** | 19,086 | 6.00× higher | 4.19× higher | — |
| `p15_prev0.07_cstat0.75_target0.85_mridge` | Predictors: 10 → 15 → 20 | 2,650 | **10,668** | 3,556 | 4.03× higher | 3.00× higher | — |
| `p15_prev0.07_cstat0.80_target0.95_mridge` | Predictors: 10 → 15 → 20 | 82,409 | **9,634** | 106,688 | 8.55× lower | 11.07× lower | — |
| `p15_prev0.10_cstat0.60_target0.95_mlasso` | Prevalence: 0.075 → 0.1 → 0.15 | 160,000 | **45,917** | 247,819 | 3.48× lower | 5.40× lower | — |
| `p15_prev0.10_cstat0.65_target0.95_mridge` | C-statistic: 0.6 → 0.65 → 0.7 | 240,000 | **27,692** | 205,728 | 8.67× lower | 7.43× lower | — |
| `p15_prev0.15_cstat0.85_target0.95_mridge` | Prevalence: 0.1 → 0.15 → 0.2 | 14,120 | **3,861** | 14,112 | 3.66× lower | 3.66× lower | — |
| `p15_prev0.20_cstat0.60_target0.85_mridge` | Prevalence: 0.15 → 0.2 → 0.25 | 40,000 | **12,236** | 45,184 | 3.27× lower | 3.69× lower | — |
| `p15_prev0.25_cstat0.60_target0.85_mridge` | Prevalence: 0.2 → 0.25 → 0.3 | 12,236 | **45,184** | 9,408 | 3.69× higher | 4.80× higher | — |
| `p15_prev0.25_cstat0.60_target0.90_mridge` | Target slope: 0.85 → 0.9 → 0.95 | 45,184 | **341,504** | 33,827 | 7.56× higher | 10.10× higher | — |
| `p15_prev0.25_cstat0.60_target0.95_mridge` | Prevalence: 0.2 → 0.25 → 0.3 | 118,676 | **33,827** | 134,656 | 3.51× lower | 3.98× lower | — |
| `p15_prev0.25_cstat0.85_target0.90_mridge` | Predictors: 10 → 15 → 20 | 1,288 | **10,672** | 1,764 | 8.29× higher | 6.05× higher | — |
| `p15_prev0.30_cstat0.80_target0.95_mglm` | C-statistic: 0.75 → 0.8 → 0.85 | 4,208 | **16,832** | 2,844 | 4.00× higher | 5.92× higher | — |
| `p15_prev0.30_cstat0.85_target0.95_mridge` | Prevalence: 0.25 → 0.3 → 0.4 | 10,112 | **2,104** | 12,640 | 4.81× lower | 6.01× lower | — |
| `p15_prev0.40_cstat0.60_target0.95_mlasso` | Predictors: 10 → 15 → 20 | 33,664 | **11,475** | 50,418 | 2.93× lower | 4.39× lower | — |
| `p15_prev0.40_cstat0.60_target0.95_mridge` | Predictors: 10 → 15 → 20 | 67,328 | **202,240** | 67,328 | 3.00× higher | 3.00× higher | — |
| `p15_prev0.40_cstat0.65_target0.85_mridge` | Predictors: 10 → 15 → 20 | 2,352 | **7,056** | 2,318 | 3.00× higher | 3.04× higher | — |
| `p20_prev0.05_cstat0.60_target0.85_mridge` | Predictors: 15 → 20 → 25 | 27,483 | **80,000** | 24,395 | 2.91× higher | 3.28× higher | — |
| `p20_prev0.07_cstat0.75_target0.90_mridge` | Prevalence: 0.05 → 0.075 → 0.1 | 10,603 | **28,448** | 8,017 | 2.68× higher | 3.55× higher | — |
| `p20_prev0.20_cstat0.65_target0.95_mglm` | Predictors: 15 → 20 → 25 | 27,696 | **147,680** | 22,562 | 5.33× higher | 6.55× higher | — |
| `p20_prev0.25_cstat0.60_target0.95_mlasso` | Prevalence: 0.2 → 0.25 → 0.3 | 80,000 | **26,818** | 89,856 | 2.98× lower | 3.35× lower | — |
| `p20_prev0.30_cstat0.60_target0.95_mridge` | Predictors: 15 → 20 → 25 | 134,656 | **22,464** | 112,256 | 5.99× lower | 5.00× lower | — |
| `p20_prev0.30_cstat0.80_target0.95_mlasso` | Prevalence: 0.25 → 0.3 → 0.4 | 3,642 | **11,232** | 3,157 | 3.08× higher | 3.56× higher | — |
| `p25_prev0.05_cstat0.60_target0.95_mglm` | Predictors: 15 → 25 → 30 | 240,000 | **1,215,964** | 175,813 | 5.07× higher | 6.92× higher | Yes |
| `p25_prev0.05_cstat0.75_target0.95_mglm` | Predictors: 20 → 25 → 30 | 32,253 | **106,672** | 38,749 | 3.31× higher | 2.75× higher | — |
| `p25_prev0.07_cstat0.60_target0.90_mridge` | Predictors: 20 → 25 → 30 | 53,332 | **533,344** | 80,000 | 10.00× higher | 6.67× higher | — |
| `p25_prev0.07_cstat0.65_target0.85_mglm` | Prevalence: 0.05 → 0.075 → 0.1 | 11,281 | **61,540** | 8,204 | 5.46× higher | 7.50× higher | — |
| `p25_prev0.10_cstat0.60_target0.85_mlasso` | Predictors: 20 → 25 → 30 | 9,652 | **100,000** | 10,177 | 10.36× higher | 9.83× higher | — |
| `p25_prev0.10_cstat0.85_target0.95_mridge` | Predictors: 20 → 25 → 30 | 37,648 | **5,882** | 32,205 | 6.40× lower | 5.48× lower | — |
| `p25_prev0.20_cstat0.60_target0.95_mridge` | Prevalence: 0.15 → 0.2 → 0.25 | 65,214 | **400,000** | 62,323 | 6.13× higher | 6.42× higher | — |
| `p25_prev0.20_cstat0.70_target0.95_mridge` | Prevalence: 0.15 → 0.2 → 0.25 | 28,434 | **85,712** | 22,118 | 3.01× higher | 3.88× higher | — |
| `p25_prev0.20_cstat0.75_target0.95_mglm` | Predictors: 20 → 25 → 30 | 7,636 | **26,672** | 7,617 | 3.49× higher | 3.50× higher | — |
| `p25_prev0.20_cstat0.80_target0.95_mridge` | Prevalence: 0.15 → 0.2 → 0.25 | 10,295 | **49,984** | 8,115 | 4.86× higher | 6.16× higher | — |
| `p25_prev0.25_cstat0.60_target0.85_mridge` | Prevalence: 0.2 → 0.25 → 0.3 | 12,500 | **2,923** | 15,680 | 4.28× lower | 5.36× lower | — |
| `p25_prev0.25_cstat0.75_target0.95_mlasso` | Predictors: 20 → 25 → 30 | 5,980 | **16,848** | 3,921 | 2.82× higher | 4.30× higher | — |
| `p25_prev0.30_cstat0.60_target0.95_mlasso` | Predictors: 20 → 25 → 30 | 89,856 | **449,024** | 67,392 | 5.00× higher | 6.66× higher | — |
| `p25_prev0.30_cstat0.70_target0.95_mglm` | C-statistic: 0.65 → 0.7 → 0.75 | 7,016 | **28,064** | 6,823 | 4.00× higher | 4.11× higher | — |
| `p25_prev0.30_cstat0.75_target0.95_mridge` | Prevalence: 0.25 → 0.3 → 0.4 | 8,424 | **56,128** | 9,515 | 6.66× higher | 5.90× higher | — |
| `p25_prev0.40_cstat0.60_target0.95_mridge` | Predictors: 20 → 25 → 30 | 67,328 | **336,896** | 50,114 | 5.00× higher | 6.72× higher | — |
| `p25_prev0.40_cstat0.65_target0.95_mglm` | Predictors: 20 → 25 → 30 | 12,812 | **168,448** | 18,490 | 13.15× higher | 9.11× higher | — |
| `p25_prev0.50_cstat0.80_target0.95_mridge` | Predictors: 20 → 25 → 30 | 6,736 | **34,030** | 8,014 | 5.05× higher | 4.25× higher | — |
| `p30_prev0.05_cstat0.60_target0.90_mridge` | Predictors: 25 → 30 → 40 | 149,087 | **43,545** | 153,338 | 3.42× lower | 3.52× lower | — |
| `p30_prev0.05_cstat0.70_target0.90_mridge` | Predictors: 25 → 30 → 40 | 28,455 | **6,017** | 31,547 | 4.73× lower | 5.24× lower | — |
| `p30_prev0.05_cstat0.80_target0.95_mglm` | Predictors: 25 → 30 → 40 | 25,000 | **7,500** | 20,000 | 3.33× lower | 2.67× lower | — |
| `p30_prev0.05_cstat0.85_target0.95_mridge` | Predictors: 25 → 30 → 40 | 11,555 | **56,472** | 18,321 | 4.89× higher | 3.08× higher | — |
| `p30_prev0.07_cstat0.60_target0.85_mridge` | Prevalence: 0.05 → 0.075 → 0.15 | 30,000 | **80,000** | 18,996 | 2.67× higher | 4.21× higher | Yes |
| `p30_prev0.07_cstat0.60_target0.95_mglm` | Prevalence: 0.05 → 0.075 → 0.1 | 175,813 | **640,000** | 239,905 | 3.64× higher | 2.67× higher | — |
| `p30_prev0.07_cstat0.60_target0.95_mridge` | Predictors: 15 → 30 → 50 | 101,621 | **320,000** | 105,914 | 3.15× higher | 3.02× higher | Yes |
| `p30_prev0.07_cstat0.75_target0.95_mridge` | Predictors: 25 → 30 → 40 | 35,458 | **10,666** | 54,074 | 3.32× lower | 5.07× lower | — |
| `p30_prev0.10_cstat0.60_target0.95_mglm` | Predictors: 25 → 30 → 40 | 64,666 | **239,905** | 76,711 | 3.71× higher | 3.13× higher | — |
| `p30_prev0.10_cstat0.60_target0.95_mridge` | Prevalence: 0.075 → 0.1 → 0.2 | 320,000 | **120,000** | 480,000 | 2.67× lower | 4.00× lower | Yes |
| `p30_prev0.10_cstat0.65_target0.80_mridge` | Predictors: 25 → 30 → 40 | 11,538 | **3,153** | 9,231 | 3.66× lower | 2.93× lower | — |
| `p30_prev0.10_cstat0.70_target0.95_mglm` | Predictors: 25 → 30 → 40 | 42,856 | **12,185** | 33,262 | 3.52× lower | 2.73× lower | — |
| `p30_prev0.15_cstat0.65_target0.95_mridge` | Prevalence: 0.1 → 0.15 → 0.2 | 37,806 | **147,696** | 55,384 | 3.91× higher | 2.67× higher | — |
| `p30_prev0.20_cstat0.60_target0.95_mlasso` | Prevalence: 0.15 → 0.2 → 0.25 | 320,000 | **30,000** | 241,151 | 10.67× lower | 8.04× lower | — |
| `p30_prev0.25_cstat0.60_target0.95_mlasso` | Predictors: 25 → 30 → 40 | 67,392 | **241,151** | 44,295 | 3.58× higher | 5.44× higher | — |
| `p30_prev0.25_cstat0.85_target0.95_mridge` | Predictors: 25 → 30 → 40 | 16,848 | **5,052** | 26,944 | 3.33× lower | 5.33× lower | — |
| `p30_prev0.30_cstat0.60_target0.95_mglm` | Prevalence: 0.25 → 0.3 → 0.4 | 40,416 | **269,568** | 46,356 | 6.67× higher | 5.82× higher | — |
| `p30_prev0.40_cstat0.60_target0.95_mridge` | Predictors: 25 → 30 → 40 | 336,896 | **50,114** | 344,162 | 6.72× lower | 6.87× lower | — |
| `p30_prev0.40_cstat0.65_target0.95_mridge` | Predictors: 25 → 30 → 40 | 42,112 | **201,984** | 41,623 | 4.80× higher | 4.85× higher | — |
| `p30_prev0.50_cstat0.60_target0.95_mridge` | Predictors: 25 → 30 → 40 | 134,656 | **20,224** | 107,776 | 6.66× lower | 5.33× lower | — |
| `p30_prev0.50_cstat0.70_target0.80_mlasso` | Predictors: 25 → 30 → 40 | 558 | **187** | 690 | 2.98× lower | 3.69× lower | — |
| `p40_prev0.05_cstat0.65_target0.90_mlasso` | Predictors: 30 → 40 → 50 | 17,109 | **73,846** | 23,077 | 4.32× higher | 3.20× higher | — |
| `p40_prev0.07_cstat0.65_target0.90_mlasso` | C-statistic: 0.6 → 0.65 → 0.7 | 40,679 | **13,452** | 45,714 | 3.02× lower | 3.40× lower | — |
| `p40_prev0.07_cstat0.65_target0.95_mglm` | Prevalence: 0.05 → 0.075 → 0.1 | 295,652 | **94,540** | 295,392 | 3.13× lower | 3.12× lower | — |
| `p40_prev0.07_cstat0.70_target0.90_mlasso` | Predictors: 30 → 40 → 50 | 6,146 | **45,714** | 7,709 | 7.44× higher | 5.93× higher | — |
| `p40_prev0.07_cstat0.70_target0.95_mlasso` | Predictors: 30 → 40 → 50 | 25,523 | **91,428** | 22,834 | 3.58× higher | 4.00× higher | — |
| `p40_prev0.10_cstat0.65_target0.95_mglm` | Predictors: 30 → 40 → 50 | 52,164 | **295,392** | 46,154 | 5.66× higher | 6.40× higher | — |
| `p40_prev0.10_cstat0.70_target0.95_mlasso` | Predictors: 30 → 40 → 50 | 19,986 | **68,572** | 19,009 | 3.43× higher | 3.61× higher | — |
| `p40_prev0.15_cstat0.60_target0.85_mridge` | Predictors: 30 → 40 → 75 | 18,996 | **53,332** | 12,500 | 2.81× higher | 4.27× higher | Yes |
| `p40_prev0.15_cstat0.60_target0.95_mridge` | Predictors: 25 → 40 → 75 | 65,214 | **313,459** | 100,000 | 4.81× higher | 3.13× higher | Yes |
| `p40_prev0.20_cstat0.65_target0.95_mridge` | Predictors: 30 → 40 → 50 | 55,384 | **295,392** | 43,737 | 5.33× higher | 6.75× higher | — |
| `p40_prev0.25_cstat0.65_target0.95_mridge` | Predictors: 30 → 40 → 50 | 37,567 | **13,008** | 54,320 | 2.89× lower | 4.18× lower | — |
| `p40_prev0.30_cstat0.65_target0.95_mlasso` | Predictors: 30 → 40 → 50 | 13,905 | **44,896** | 14,040 | 3.23× higher | 3.20× higher | — |
| `p40_prev0.50_cstat0.60_target0.95_mridge` | Predictors: 30 → 40 → 50 | 20,224 | **107,776** | 30,971 | 5.33× higher | 3.48× higher | — |
| `p40_prev0.50_cstat0.75_target0.90_mlasso` | Predictors: 30 → 40 → 50 | 1,213 | **3,556** | 1,111 | 2.93× higher | 3.20× higher | — |
| `p50_prev0.05_cstat0.60_target0.80_mlasso` | Predictors: 40 → 50 → 75 | 20,000 | **7,851** | 31,408 | 2.55× lower | 4.00× lower | — |
| `p50_prev0.05_cstat0.70_target0.85_mridge` | C-statistic: 0.65 → 0.7 → 0.75 | 11,342 | **42,857** | 11,649 | 3.78× higher | 3.68× higher | — |
| `p50_prev0.05_cstat0.75_target0.95_mglm` | C-statistic: 0.7 → 0.75 → 0.8 | 85,714 | **21,436** | 100,000 | 4.00× lower | 4.67× lower | — |
| `p50_prev0.07_cstat0.60_target0.85_mridge` | Predictors: 40 → 50 → 75 | 64,623 | **16,298** | 100,000 | 3.97× lower | 6.14× lower | — |
| `p50_prev0.10_cstat0.70_target0.95_mlasso` | Predictors: 40 → 50 → 75 | 68,572 | **19,009** | 64,286 | 3.61× lower | 3.38× lower | — |
| `p50_prev0.10_cstat0.70_target0.95_mridge` | Predictors: 40 → 50 → 75 | 66,704 | **20,763** | 58,267 | 3.21× lower | 2.81× lower | — |
| `p50_prev0.15_cstat0.70_target0.95_mlasso` | Predictors: 40 → 50 → 75 | 11,429 | **57,144** | 10,714 | 5.00× higher | 5.33× higher | — |
| `p50_prev0.15_cstat0.75_target0.95_mglm` | Prevalence: 0.1 → 0.15 → 0.2 | 13,119 | **71,104** | 12,990 | 5.42× higher | 5.47× higher | — |
| `p50_prev0.20_cstat0.85_target0.95_mridge` | Prevalence: 0.15 → 0.2 → 0.25 | 11,478 | **47,056** | 8,095 | 4.10× higher | 5.81× higher | — |
| `p50_prev0.30_cstat0.60_target0.90_mridge` | Prevalence: 0.25 → 0.3 → 0.4 | 34,036 | **118,528** | 22,224 | 3.48× higher | 5.33× higher | — |
| `p50_prev0.30_cstat0.70_target0.95_mridge` | Prevalence: 0.25 → 0.3 → 0.4 | 34,883 | **112,320** | 24,516 | 3.22× higher | 4.58× higher | — |
| `p50_prev0.30_cstat0.80_target0.95_mglm` | Prevalence: 0.25 → 0.3 → 0.4 | 9,961 | **28,080** | 7,779 | 2.82× higher | 3.61× higher | — |
| `p50_prev0.30_cstat0.85_target0.95_mridge` | Prevalence: 0.25 → 0.3 → 0.4 | 8,095 | **28,080** | 9,506 | 3.47× higher | 2.95× higher | — |
| `p50_prev0.40_cstat0.75_target0.80_mlasso` | Prevalence: 0.3 → 0.4 → 0.5 | 764 | **195** | 529 | 3.92× lower | 2.71× lower | — |
| `p50_prev0.40_cstat0.85_target0.95_mridge` | Prevalence: 0.3 → 0.4 → 0.5 | 28,080 | **9,506** | 33,696 | 2.95× lower | 3.54× lower | — |
| `p5_prev0.05_cstat0.75_target0.85_mridge` | C-statistic: 0.7 → 0.75 → 0.8 | 3,210 | **21,328** | 2,500 | 6.64× higher | 8.53× higher | — |
| `p5_prev0.07_cstat0.80_target0.95_mridge` | Prevalence: 0.05 → 0.075 → 0.1 | 17,372 | **53,376** | 15,295 | 3.07× higher | 3.49× higher | — |
| `p5_prev0.10_cstat0.65_target0.95_mlasso` | C-statistic: 0.6 → 0.65 → 0.7 | 160,000 | **36,928** | 137,152 | 4.33× lower | 3.71× lower | — |
| `p5_prev0.15_cstat0.60_target0.85_mridge` | Prevalence: 0.1 → 0.15 → 0.2 | 10,555 | **3,334** | 10,000 | 3.17× lower | 3.00× lower | — |
| `p5_prev0.15_cstat0.65_target0.95_mlasso` | Prevalence: 0.1 → 0.15 → 0.2 | 36,928 | **9,476** | 29,846 | 3.90× lower | 3.15× lower | — |
| `p5_prev0.15_cstat0.65_target0.95_mridge` | Prevalence: 0.1 → 0.15 → 0.2 | 36,928 | **12,304** | 36,928 | 3.00× lower | 3.00× lower | — |
| `p5_prev0.20_cstat0.70_target0.95_mglm` | Prevalence: 0.15 → 0.2 → 0.25 | 6,220 | **2,142** | 6,752 | 2.90× lower | 3.15× lower | — |
| `p5_prev0.20_cstat0.75_target0.95_mglm` | C-statistic: 0.7 → 0.75 → 0.8 | 2,142 | **10,656** | 2,496 | 4.97× higher | 4.27× higher | — |
| `p5_prev0.20_cstat0.80_target0.95_mridge` | C-statistic: 0.75 → 0.8 → 0.85 | 5,328 | **26,299** | 7,304 | 4.94× higher | 3.60× higher | — |
| `p5_prev0.25_cstat0.70_target0.95_mglm` | Prevalence: 0.2 → 0.25 → 0.3 | 2,142 | **6,752** | 1,770 | 3.15× higher | 3.81× higher | — |
| `p5_prev0.30_cstat0.80_target0.95_mridge` | C-statistic: 0.75 → 0.8 → 0.85 | 11,264 | **2,816** | 8,903 | 4.00× lower | 3.16× lower | — |
| `p5_prev0.40_cstat0.80_target0.95_mridge` | C-statistic: 0.75 → 0.8 → 0.85 | 12,863 | **3,216** | 16,896 | 4.00× lower | 5.25× lower | — |
| `p75_prev0.05_cstat0.60_target0.85_mridge` | Target slope: 0.8 → 0.85 → 0.95 | 150,000 | **35,982** | 131,882 | 4.17× lower | 3.67× lower | Yes |
| `p75_prev0.05_cstat0.70_target0.95_mlasso` | Predictors: 50 → 75 → 100 | 42,857 | **257,144** | 42,857 | 6.00× higher | 6.00× higher | — |
| `p75_prev0.05_cstat0.80_target0.95_mlasso` | C-statistic: 0.75 → 0.8 → 0.85 | 20,000 | **75,000** | 17,647 | 3.75× higher | 4.25× higher | — |
| `p75_prev0.07_cstat0.70_target0.95_mlasso` | Predictors: 50 → 75 → 100 | 22,834 | **171,428** | 28,180 | 7.51× higher | 6.08× higher | — |
| `p75_prev0.07_cstat0.80_target0.85_mridge` | C-statistic: 0.75 → 0.8 → 0.85 | 13,333 | **3,125** | 11,765 | 4.27× lower | 3.76× lower | — |
| `p75_prev0.10_cstat0.65_target0.85_mridge` | Predictors: 50 → 75 → 100 | 9,584 | **69,230** | 11,532 | 7.22× higher | 6.00× higher | — |
| `p75_prev0.10_cstat0.65_target0.95_mglm` | Prevalence: 0.075 → 0.1 → 0.15 | 46,154 | **138,460** | 34,765 | 3.00× higher | 3.98× higher | — |
| `p75_prev0.10_cstat0.75_target0.95_mridge` | C-statistic: 0.7 → 0.75 → 0.8 | 58,267 | **9,945** | 38,918 | 5.86× lower | 3.91× lower | — |
| `p75_prev0.15_cstat0.60_target0.80_mridge` | Predictors: 50 → 75 → 100 | 17,298 | **4,884** | 18,717 | 3.54× lower | 3.83× lower | — |
| `p75_prev0.15_cstat0.60_target0.85_mridge` | Predictors: 40 → 75 → 100 | 53,332 | **12,500** | 43,882 | 4.27× lower | 3.51× lower | Yes |
| `p75_prev0.25_cstat0.60_target0.90_mridge` | Predictors: 50 → 75 → 100 | 34,036 | **106,656** | 35,552 | 3.13× higher | 3.00× higher | — |
| `p75_prev0.30_cstat0.60_target0.80_mridge` | Prevalence: 0.25 → 0.3 → 0.4 | 11,533 | **3,083** | 9,178 | 3.74× lower | 2.98× lower | — |
| `p75_prev0.40_cstat0.70_target0.95_mridge` | C-statistic: 0.65 → 0.7 → 0.75 | 15,763 | **63,168** | 14,895 | 4.01× higher | 4.24× higher | — |
| `p75_prev0.50_cstat0.60_target0.90_mridge` | Predictors: 50 → 75 → 100 | 36,308 | **6,668** | 27,106 | 5.45× lower | 4.07× lower | — |
| `p75_prev0.50_cstat0.60_target0.95_mlasso` | Predictors: 50 → 75 → 100 | 52,574 | **18,582** | 67,360 | 2.83× lower | 3.63× lower | — |

### Continuous

| Scenario key | Swept input: previous → current → next | Previous N | Cell N | Next N | Cell vs previous | Cell vs next | Gap |
|---|---|---:|---:|---:|---|---|---|
| `p10_r20.10_target0.95_mlasso` | Predictors: 5 → 10 → 15 | 4,861 | **32,768** | 6,402 | 6.74× higher | 5.12× higher | — |

### Survival

| Scenario key | Swept input: previous → current → next | Previous N | Cell N | Next N | Cell vs previous | Cell vs next | Gap |
|---|---|---:|---:|---:|---|---|---|
| `p10_cindex0.60_hazard1.00_censor0.30_target0.95_mlasso` | Predictors: 5 → 10 → 15 | 3,776 | **15,232** | 4,700 | 4.03× higher | 3.24× higher | — |
| `p10_cindex0.65_hazard1.00_censor0.50_target0.95_mlasso` | Baseline hazard: 0.5 → 1 → 2 | 3,701 | **9,856** | 2,464 | 2.66× higher | 4.00× higher | — |
| `p10_cindex0.70_hazard2.00_censor0.50_target0.95_mcoxph` | C-index: 0.65 → 0.7 → 0.75 | 4,928 | **1,144** | 4,272 | 4.31× lower | 3.73× lower | — |
| `p10_cindex0.75_hazard0.50_censor0.10_target0.95_mlasso` | Predictors: 5 → 10 → 15 | 600 | **2,368** | 866 | 3.95× higher | 2.73× higher | — |
| `p10_cindex0.75_hazard2.00_censor0.50_target0.95_mcoxph` | C-index: 0.7 → 0.75 → 0.8 | 1,144 | **4,272** | 1,062 | 3.73× higher | 4.02× higher | — |
| `p15_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | Baseline hazard: 0.5 → 1 → 2 | 4,388 | **17,792** | 5,930 | 4.05× higher | 3.00× higher | — |
| `p15_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | Censoring: 0.1 → 0.3 → 0.5 | 17,792 | **91,392** | 16,000 | 5.14× higher | 5.71× higher | — |
| `p15_cindex0.65_hazard0.50_censor0.10_target0.80_mlasso` | Predictors: 10 → 15 → 20 | 207 | **64** | 275 | 3.23× lower | 4.30× lower | — |
| `p20_cindex0.60_hazard1.00_censor0.10_target0.95_mlasso` | Baseline hazard: 0.5 → 1 → 2 | 4,644 | **23,680** | 6,110 | 5.10× higher | 3.88× higher | — |
| `p20_cindex0.60_hazard1.00_censor0.30_target0.95_mridge` | Baseline hazard: 0.5 → 1 → 2 | 61,056 | **6,458** | 61,056 | 9.45× lower | 9.45× lower | — |
| `p20_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | Censoring: 0.1 → 0.3 → 0.5 | 11,840 | **61,056** | 10,672 | 5.16× higher | 5.72× higher | — |
| `p20_cindex0.70_hazard0.50_censor0.50_target0.95_mridge` | C-index: 0.65 → 0.7 → 0.75 | 9,840 | **2,265** | 8,528 | 4.34× lower | 3.77× lower | — |
| `p20_cindex0.75_hazard0.50_censor0.50_target0.95_mridge` | C-index: 0.7 → 0.75 → 0.8 | 2,265 | **8,528** | 2,215 | 3.77× higher | 3.85× higher | — |
| `p20_cindex0.80_hazard1.00_censor0.30_target0.95_mridge` | Baseline hazard: 0.5 → 1 → 2 | 1,551 | **5,728** | 1,439 | 3.69× higher | 3.98× higher | — |
| `p25_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | Predictors: 20 → 25 → 30 | 23,680 | **7,024** | 35,520 | 3.37× lower | 5.06× lower | — |
| `p25_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | Baseline hazard: 0.5 → 1 → 2 | 13,328 | **106,624** | 13,328 | 8.00× higher | 8.00× higher | — |
| `p25_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | Predictors: 20 → 25 → 30 | 11,840 | **59,264** | 16,874 | 5.01× higher | 3.51× higher | — |
| `p25_cindex0.85_hazard0.50_censor0.10_target0.85_mlasso` | Predictors: 20 → 25 → 30 | 124 | **40** | 139 | 3.10× lower | 3.48× lower | — |
| `p25_cindex0.85_hazard2.00_censor0.30_target0.95_mridge` | Censoring: 0.1 → 0.3 → 0.5 | 1,308 | **6,720** | 1,521 | 5.14× higher | 4.42× higher | — |
| `p30_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | Predictors: 25 → 30 → 40 | 34,851 | **128,000** | 43,324 | 3.67× higher | 2.95× higher | — |
| `p30_cindex0.70_hazard2.00_censor0.50_target0.95_mridge` | Predictors: 25 → 30 → 40 | 5,722 | **27,424** | 9,144 | 4.79× higher | 3.00× higher | — |
| `p30_cindex0.85_hazard0.50_censor0.50_target0.95_mridge` | Predictors: 25 → 30 → 40 | 1,656 | **5,648** | 2,037 | 3.41× higher | 2.77× higher | — |
| `p30_cindex0.85_hazard1.00_censor0.30_target0.95_mridge` | Predictors: 25 → 30 → 40 | 1,680 | **8,080** | 1,344 | 4.81× higher | 6.01× higher | — |
| `p30_cindex0.85_hazard1.00_censor0.50_target0.95_mridge` | Predictors: 25 → 30 → 40 | 1,584 | **5,648** | 1,882 | 3.57× higher | 3.00× higher | — |
| `p5_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | Baseline hazard: 0.5 → 1 → 2 | 2,672 | **10,688** | 1,682 | 4.00× higher | 6.35× higher | — |
| `p5_cindex0.65_hazard2.00_censor0.30_target0.95_mridge` | Censoring: 0.1 → 0.3 → 0.5 | 2,752 | **13,952** | 2,464 | 5.07× higher | 5.66× higher | — |
| `p5_cindex0.70_hazard1.00_censor0.50_target0.95_mlasso` | Baseline hazard: 0.5 → 1 → 2 | 2,156 | **9,152** | 3,422 | 4.24× higher | 2.67× higher | — |
| `p5_cindex0.70_hazard2.00_censor0.30_target0.95_mcoxph` | C-index: 0.65 → 0.7 → 0.75 | 872 | **3,232** | 685 | 3.71× higher | 4.72× higher | — |
| `p5_cindex0.75_hazard2.00_censor0.10_target0.95_mridge` | C-index: 0.7 → 0.75 → 0.8 | 1,280 | **4,800** | 1,120 | 3.75× higher | 4.29× higher | — |
| `p5_cindex0.80_hazard0.50_censor0.50_target0.95_mridge` | C-index: 0.75 → 0.8 → 0.85 | 1,064 | **12,288** | 1,424 | 11.55× higher | 8.63× higher | — |
| `p5_cindex0.80_hazard1.00_censor0.50_target0.95_mridge` | C-index: 0.75 → 0.8 → 0.85 | 4,078 | **1,000** | 4,855 | 4.08× lower | 4.86× lower | — |
| `p5_cindex0.80_hazard2.00_censor0.10_target0.95_mridge` | C-index: 0.75 → 0.8 → 0.85 | 4,800 | **1,120** | 4,111 | 4.29× lower | 3.67× lower | — |
| `p75_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | Predictors: 50 → 75 → 100 | 6,987 | **22,208** | 5,425 | 3.18× higher | 4.09× higher | — |

## Sources and reproducibility

- [Cache JSON](versions/cache-1f-create-20260827-161720.json): all grid values and input levels used in this report.
- [Existing exclusion list](known_bad_cells.json): cells already excluded from serving.
- [Earlier investigation](../../pmsims/CALIBRATION-SLOPE-SEARCH-INSTABILITY.md): original audit rules, totals, and rerun evidence.
- [Updated problem and fix notes](../../pmsims/CALIBRATION-SLOPE-PROBLEM-AND-FIX.md): refined explanation and remaining uncertainty.
- [Backend lookup implementation](../backend/app/lookup.py): exclusion-list version check and fallback behaviour.

Counts were recomputed from the pinned JSON cache, using positive finite Ns from successful rows. The cache contains 13,680 scenarios, including 142 failed rows. No simulations were rerun for this report. The older investigation’s fix-status statement is historical; it is not used here to claim the package’s present fix status.
