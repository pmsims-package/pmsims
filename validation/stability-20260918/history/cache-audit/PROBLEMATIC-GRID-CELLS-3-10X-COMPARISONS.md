# All 3–10× grid comparisons

Cache: `cache-1f-create-20260827-161720`. Generated 17 September 2026.

[Summary and isolated-cell catalogue](PROBLEMATIC-GRID-CELLS-3-10X.md).

Each row is a pair within one outcome/model slice. “Previous” and “next” mean consecutive successful available points; **Gap = Yes** indicates failed intervening grid points. **3,708 pairs**, of which **3,650** are literally adjacent. The ratio is always ≥1. Excluded endpoints are already in `known_bad_cells.json`; an isolated endpoint belongs to the earlier 206-cell isolated-spike list. A flagged pair does not itself establish which endpoint is wrong.

## Binary

| Swept input | Previous cell | Previous N | Next cell | Next N | Next vs previous | Gap | Excluded endpoints | Earlier isolated endpoints |
|---|---|---:|---|---:|---|---|---|---|
| C-statistic: 0.6 → 0.65 | `p100_prev0.05_cstat0.60_target0.90_mglm` | 333,368 | `p100_prev0.05_cstat0.65_target0.90_mglm` | 109,896 | 3.03× lower | — | — | — |
| C-statistic: 0.8 → 0.85 | `p100_prev0.05_cstat0.80_target0.95_mglm` | 49,189 | `p100_prev0.05_cstat0.85_target0.95_mglm` | 188,232 | 3.83× higher | — | next | — |
| C-statistic: 0.6 → 0.65 | `p100_prev0.10_cstat0.60_target0.85_mglm` | 71,934 | `p100_prev0.10_cstat0.65_target0.85_mglm` | 23,077 | 3.12× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p100_prev0.15_cstat0.60_target0.95_mglm` | 204,200 | `p100_prev0.15_cstat0.65_target0.95_mglm` | 30,769 | 6.64× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p100_prev0.15_cstat0.65_target0.90_mglm` | 61,538 | `p100_prev0.15_cstat0.70_target0.90_mglm` | 19,985 | 3.08× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p100_prev0.20_cstat0.60_target0.90_mglm` | 100,000 | `p100_prev0.20_cstat0.65_target0.90_mglm` | 28,971 | 3.45× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p100_prev0.20_cstat0.60_target0.95_mglm` | 387,633 | `p100_prev0.20_cstat0.65_target0.95_mglm` | 44,863 | 8.64× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p10_prev0.05_cstat0.60_target0.80_mglm` | 20,000 | `p10_prev0.05_cstat0.65_target0.80_mglm` | 4,615 | 4.33× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p10_prev0.05_cstat0.60_target0.95_mglm` | 40,000 | `p10_prev0.05_cstat0.65_target0.95_mglm` | 7,218 | 5.54× lower | — | previous, next | next |
| C-statistic: 0.65 → 0.7 | `p10_prev0.07_cstat0.65_target0.90_mglm` | 24,616 | `p10_prev0.07_cstat0.70_target0.90_mglm` | 5,714 | 4.31× lower | — | previous | previous |
| C-statistic: 0.65 → 0.7 | `p10_prev0.07_cstat0.65_target0.95_mglm` | 37,664 | `p10_prev0.07_cstat0.70_target0.95_mglm` | 5,523 | 6.82× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p10_prev0.10_cstat0.60_target0.90_mglm` | 40,000 | `p10_prev0.10_cstat0.65_target0.90_mglm` | 7,901 | 5.06× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p10_prev0.10_cstat0.65_target0.85_mglm` | 9,230 | `p10_prev0.10_cstat0.70_target0.85_mglm` | 2,144 | 4.31× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p10_prev0.10_cstat0.75_target0.95_mglm` | 21,328 | `p10_prev0.10_cstat0.80_target0.95_mglm` | 6,084 | 3.51× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p10_prev0.20_cstat0.60_target0.80_mglm` | 5,000 | `p10_prev0.20_cstat0.65_target0.80_mglm` | 1,555 | 3.22× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p10_prev0.20_cstat0.60_target0.90_mglm` | 20,000 | `p10_prev0.20_cstat0.65_target0.90_mglm` | 4,616 | 4.33× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p10_prev0.25_cstat0.70_target0.95_mglm` | 26,944 | `p10_prev0.25_cstat0.75_target0.95_mglm` | 5,482 | 4.91× lower | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p10_prev0.30_cstat0.60_target0.85_mglm` | 6,272 | `p10_prev0.30_cstat0.65_target0.85_mglm` | 1,525 | 4.11× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p10_prev0.30_cstat0.60_target0.95_mglm` | 22,464 | `p10_prev0.30_cstat0.65_target0.95_mglm` | 5,614 | 4.00× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p10_prev0.50_cstat0.60_target0.95_mglm` | 24,975 | `p10_prev0.50_cstat0.65_target0.95_mglm` | 6,752 | 3.70× lower | — | previous | — |
| C-statistic: 0.75 → 0.85 | `p10_prev0.50_cstat0.75_target0.95_mglm` | 6,752 | `p10_prev0.50_cstat0.85_target0.95_mglm` | 1,878 | 3.60× lower | Yes | — | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.05_cstat0.60_target0.90_mglm` | 240,000 | `p15_prev0.05_cstat0.65_target0.90_mglm` | 27,692 | 8.67× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.05_cstat0.60_target0.95_mglm` | 240,000 | `p15_prev0.05_cstat0.65_target0.95_mglm` | 55,384 | 4.33× lower | — | previous, next | — |
| C-statistic: 0.65 → 0.7 | `p15_prev0.05_cstat0.65_target0.85_mglm` | 10,798 | `p15_prev0.05_cstat0.70_target0.85_mglm` | 3,222 | 3.35× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p15_prev0.05_cstat0.65_target0.90_mglm` | 27,692 | `p15_prev0.05_cstat0.70_target0.90_mglm` | 6,428 | 4.31× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.07_cstat0.60_target0.85_mglm` | 40,000 | `p15_prev0.07_cstat0.65_target0.85_mglm` | 7,003 | 5.71× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.07_cstat0.60_target0.90_mglm` | 41,457 | `p15_prev0.07_cstat0.65_target0.90_mglm` | 8,975 | 4.62× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.07_cstat0.60_target0.95_mglm` | 120,289 | `p15_prev0.07_cstat0.65_target0.95_mglm` | 36,924 | 3.26× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.10_cstat0.60_target0.90_mglm` | 23,929 | `p15_prev0.10_cstat0.65_target0.90_mglm` | 7,546 | 3.17× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.15_cstat0.60_target0.95_mglm` | 97,489 | `p15_prev0.15_cstat0.65_target0.95_mglm` | 27,548 | 3.54× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p15_prev0.15_cstat0.65_target0.95_mglm` | 27,548 | `p15_prev0.15_cstat0.70_target0.95_mglm` | 8,572 | 3.21× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p15_prev0.20_cstat0.65_target0.95_mglm` | 27,696 | `p15_prev0.20_cstat0.70_target0.95_mglm` | 6,270 | 4.42× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p15_prev0.25_cstat0.65_target0.95_mglm` | 18,115 | `p15_prev0.25_cstat0.70_target0.95_mglm` | 4,986 | 3.63× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.30_cstat0.60_target0.95_mglm` | 67,328 | `p15_prev0.30_cstat0.65_target0.95_mglm` | 7,567 | 8.90× lower | — | previous | — |
| C-statistic: 0.75 → 0.8 | `p15_prev0.30_cstat0.75_target0.95_mglm` | 4,208 | `p15_prev0.30_cstat0.80_target0.95_mglm` | 16,832 | 4.00× higher | — | next | next |
| C-statistic: 0.8 → 0.85 | `p15_prev0.30_cstat0.80_target0.95_mglm` | 16,832 | `p15_prev0.30_cstat0.85_target0.95_mglm` | 2,844 | 5.92× lower | — | previous | previous |
| C-statistic: 0.65 → 0.7 | `p15_prev0.40_cstat0.65_target0.95_mglm` | 10,368 | `p15_prev0.40_cstat0.70_target0.95_mglm` | 2,936 | 3.53× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p15_prev0.50_cstat0.70_target0.95_mglm` | 20,224 | `p15_prev0.50_cstat0.75_target0.95_mglm` | 5,056 | 4.00× lower | — | previous | — |
| C-statistic: 0.8 → 0.85 | `p20_prev0.05_cstat0.80_target0.95_mglm` | 20,000 | `p20_prev0.05_cstat0.85_target0.95_mglm` | 150,592 | 7.53× higher | — | next | next |
| C-statistic: 0.65 → 0.7 | `p20_prev0.15_cstat0.65_target0.95_mglm` | 23,619 | `p20_prev0.15_cstat0.70_target0.95_mglm` | 5,715 | 4.13× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.20_cstat0.60_target0.95_mglm` | 40,000 | `p20_prev0.20_cstat0.65_target0.95_mglm` | 147,680 | 3.69× higher | — | next | next |
| C-statistic: 0.6 → 0.65 | `p20_prev0.25_cstat0.60_target0.95_mglm` | 53,888 | `p20_prev0.25_cstat0.65_target0.95_mglm` | 13,391 | 4.02× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p20_prev0.25_cstat0.65_target0.95_mglm` | 13,391 | `p20_prev0.25_cstat0.70_target0.95_mglm` | 3,368 | 3.98× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.30_cstat0.60_target0.85_mglm` | 12,560 | `p20_prev0.30_cstat0.65_target0.85_mglm` | 3,140 | 4.00× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.40_cstat0.60_target0.80_mglm` | 3,951 | `p20_prev0.40_cstat0.65_target0.80_mglm` | 1,250 | 3.16× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.50_cstat0.60_target0.95_mglm` | 31,806 | `p20_prev0.50_cstat0.65_target0.95_mglm` | 6,678 | 4.76× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.05_cstat0.60_target0.80_mglm` | 25,000 | `p25_prev0.05_cstat0.65_target0.80_mglm` | 5,786 | 4.32× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p25_prev0.05_cstat0.65_target0.90_mglm` | 46,154 | `p25_prev0.05_cstat0.70_target0.90_mglm` | 15,145 | 3.05× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p25_prev0.05_cstat0.75_target0.95_mglm` | 106,672 | `p25_prev0.05_cstat0.80_target0.95_mglm` | 25,000 | 4.27× lower | — | previous | previous |
| C-statistic: 0.65 → 0.7 | `p25_prev0.07_cstat0.65_target0.85_mglm` | 61,540 | `p25_prev0.07_cstat0.70_target0.85_mglm` | 6,420 | 9.59× lower | — | previous | previous |
| C-statistic: 0.65 → 0.7 | `p25_prev0.07_cstat0.65_target0.95_mglm` | 246,160 | `p25_prev0.07_cstat0.70_target0.95_mglm` | 30,310 | 8.12× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.10_cstat0.60_target0.90_mglm` | 39,054 | `p25_prev0.10_cstat0.65_target0.90_mglm` | 11,122 | 3.51× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p25_prev0.10_cstat0.70_target0.95_mglm` | 42,856 | `p25_prev0.10_cstat0.75_target0.95_mglm` | 13,332 | 3.21× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.15_cstat0.60_target0.90_mglm` | 23,623 | `p25_prev0.15_cstat0.65_target0.90_mglm` | 7,692 | 3.07× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.20_cstat0.60_target0.90_mglm` | 25,000 | `p25_prev0.20_cstat0.65_target0.90_mglm` | 7,722 | 3.24× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p25_prev0.20_cstat0.75_target0.95_mglm` | 26,672 | `p25_prev0.20_cstat0.80_target0.95_mglm` | 6,904 | 3.86× lower | — | previous | previous |
| C-statistic: 0.65 → 0.7 | `p25_prev0.30_cstat0.65_target0.95_mglm` | 7,016 | `p25_prev0.30_cstat0.70_target0.95_mglm` | 28,064 | 4.00× higher | — | next | next |
| C-statistic: 0.7 → 0.75 | `p25_prev0.30_cstat0.70_target0.95_mglm` | 28,064 | `p25_prev0.30_cstat0.75_target0.95_mglm` | 6,823 | 4.11× lower | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p30_prev0.05_cstat0.60_target0.85_mglm` | 62,046 | `p30_prev0.05_cstat0.65_target0.85_mglm` | 16,875 | 3.68× lower | — | — | — |
| C-statistic: 0.6 → 0.7 | `p30_prev0.05_cstat0.60_target0.95_mglm` | 175,813 | `p30_prev0.05_cstat0.70_target0.95_mglm` | 24,284 | 7.24× lower | Yes | previous | — |
| C-statistic: 0.65 → 0.7 | `p30_prev0.05_cstat0.65_target0.90_mglm` | 55,384 | `p30_prev0.05_cstat0.70_target0.90_mglm` | 17,640 | 3.14× lower | — | previous | — |
| C-statistic: 0.75 → 0.8 | `p30_prev0.05_cstat0.75_target0.95_mglm` | 38,749 | `p30_prev0.05_cstat0.80_target0.95_mglm` | 7,500 | 5.17× lower | — | previous, next | next |
| C-statistic: 0.6 → 0.7 | `p30_prev0.07_cstat0.60_target0.80_mglm` | 18,478 | `p30_prev0.07_cstat0.70_target0.80_mglm` | 4,824 | 3.83× lower | Yes | — | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.07_cstat0.60_target0.90_mglm` | 80,000 | `p30_prev0.07_cstat0.65_target0.90_mglm` | 8,888 | 9.00× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.07_cstat0.60_target0.95_mglm` | 640,000 | `p30_prev0.07_cstat0.65_target0.95_mglm` | 98,785 | 6.48× lower | — | previous, next | previous |
| C-statistic: 0.7 → 0.75 | `p30_prev0.07_cstat0.70_target0.95_mglm` | 68,572 | `p30_prev0.07_cstat0.75_target0.95_mglm` | 10,092 | 6.79× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.10_cstat0.60_target0.85_mglm` | 21,670 | `p30_prev0.10_cstat0.65_target0.85_mglm` | 6,923 | 3.13× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.10_cstat0.60_target0.90_mglm` | 45,925 | `p30_prev0.10_cstat0.65_target0.90_mglm` | 13,846 | 3.32× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.10_cstat0.60_target0.95_mglm` | 239,905 | `p30_prev0.10_cstat0.65_target0.95_mglm` | 52,164 | 4.60× lower | — | previous | previous |
| C-statistic: 0.65 → 0.7 | `p30_prev0.10_cstat0.65_target0.95_mglm` | 52,164 | `p30_prev0.10_cstat0.70_target0.95_mglm` | 12,185 | 4.28× lower | — | next | next |
| C-statistic: 0.7 → 0.75 | `p30_prev0.15_cstat0.70_target0.95_mglm` | 34,284 | `p30_prev0.15_cstat0.75_target0.95_mglm` | 10,668 | 3.21× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.20_cstat0.60_target0.90_mglm` | 30,000 | `p30_prev0.20_cstat0.65_target0.90_mglm` | 9,911 | 3.03× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p40_prev0.05_cstat0.70_target0.90_mglm` | 68,572 | `p40_prev0.05_cstat0.75_target0.90_mglm` | 13,646 | 5.03× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p40_prev0.05_cstat0.70_target0.95_mglm` | 137,144 | `p40_prev0.05_cstat0.75_target0.95_mglm` | 42,668 | 3.21× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.07_cstat0.60_target0.85_mglm` | 42,174 | `p40_prev0.07_cstat0.65_target0.85_mglm` | 12,307 | 3.43× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.07_cstat0.60_target0.90_mglm` | 42,896 | `p40_prev0.07_cstat0.65_target0.90_mglm` | 12,307 | 3.49× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.07_cstat0.60_target0.95_mglm` | 426,672 | `p40_prev0.07_cstat0.65_target0.95_mglm` | 94,540 | 4.51× lower | — | previous, next | next |
| C-statistic: 0.7 → 0.75 | `p40_prev0.07_cstat0.70_target0.95_mglm` | 47,451 | `p40_prev0.07_cstat0.75_target0.95_mglm` | 14,222 | 3.34× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.10_cstat0.60_target0.90_mglm` | 80,000 | `p40_prev0.10_cstat0.65_target0.90_mglm` | 24,652 | 3.25× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.10_cstat0.60_target0.95_mglm` | 76,711 | `p40_prev0.10_cstat0.65_target0.95_mglm` | 295,392 | 3.85× higher | — | next | next |
| C-statistic: 0.65 → 0.7 | `p40_prev0.10_cstat0.65_target0.95_mglm` | 295,392 | `p40_prev0.10_cstat0.70_target0.95_mglm` | 33,262 | 8.88× lower | — | previous | previous |
| C-statistic: 0.7 → 0.75 | `p40_prev0.15_cstat0.70_target0.95_mglm` | 45,716 | `p40_prev0.15_cstat0.75_target0.95_mglm` | 14,224 | 3.21× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.20_cstat0.60_target0.95_mglm` | 320,000 | `p40_prev0.20_cstat0.65_target0.95_mglm` | 39,751 | 8.05× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p40_prev0.50_cstat0.65_target0.95_mglm` | 53,888 | `p40_prev0.50_cstat0.70_target0.95_mglm` | 12,762 | 4.22× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p50_prev0.05_cstat0.70_target0.95_mglm` | 85,714 | `p50_prev0.05_cstat0.75_target0.95_mglm` | 21,436 | 4.00× lower | — | next | next |
| C-statistic: 0.75 → 0.8 | `p50_prev0.05_cstat0.75_target0.95_mglm` | 21,436 | `p50_prev0.05_cstat0.80_target0.95_mglm` | 100,000 | 4.67× higher | — | previous, next | previous |
| C-statistic: 0.8 → 0.85 | `p50_prev0.05_cstat0.80_target0.95_mglm` | 100,000 | `p50_prev0.05_cstat0.85_target0.95_mglm` | 11,765 | 8.50× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p50_prev0.07_cstat0.60_target0.90_mglm` | 133,332 | `p50_prev0.07_cstat0.65_target0.90_mglm` | 34,618 | 3.85× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p50_prev0.07_cstat0.60_target0.95_mglm` | 263,335 | `p50_prev0.07_cstat0.65_target0.95_mglm` | 60,243 | 4.37× lower | — | — | — |
| C-statistic: 0.8 → 0.85 | `p50_prev0.07_cstat0.80_target0.95_mglm` | 33,336 | `p50_prev0.07_cstat0.85_target0.95_mglm` | 7,844 | 4.25× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p50_prev0.10_cstat0.60_target0.95_mglm` | 173,793 | `p50_prev0.10_cstat0.65_target0.95_mglm` | 46,154 | 3.77× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p50_prev0.10_cstat0.70_target0.95_mglm` | 42,227 | `p50_prev0.10_cstat0.75_target0.95_mglm` | 13,119 | 3.22× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p50_prev0.15_cstat0.60_target0.90_mglm` | 66,668 | `p50_prev0.15_cstat0.65_target0.90_mglm` | 18,447 | 3.61× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p50_prev0.15_cstat0.60_target0.95_mglm` | 133,336 | `p50_prev0.15_cstat0.65_target0.95_mglm` | 39,356 | 3.39× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p50_prev0.15_cstat0.65_target0.80_mglm` | 15,385 | `p50_prev0.15_cstat0.70_target0.80_mglm` | 4,154 | 3.70× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p50_prev0.15_cstat0.75_target0.95_mglm` | 71,104 | `p50_prev0.15_cstat0.80_target0.95_mglm` | 14,441 | 4.92× lower | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p50_prev0.20_cstat0.60_target0.85_mglm` | 23,153 | `p50_prev0.20_cstat0.65_target0.85_mglm` | 5,840 | 3.96× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p50_prev0.20_cstat0.60_target0.95_mglm` | 142,619 | `p50_prev0.20_cstat0.65_target0.95_mglm` | 35,689 | 4.00× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p50_prev0.20_cstat0.70_target0.95_mglm` | 42,856 | `p50_prev0.20_cstat0.75_target0.95_mglm` | 12,990 | 3.30× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p50_prev0.25_cstat0.60_target0.95_mglm` | 105,390 | `p50_prev0.25_cstat0.65_target0.95_mglm` | 16,840 | 6.26× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p50_prev0.30_cstat0.60_target0.95_mglm` | 99,319 | `p50_prev0.30_cstat0.65_target0.95_mglm` | 32,061 | 3.10× lower | — | — | — |
| C-statistic: 0.8 → 0.85 | `p50_prev0.30_cstat0.80_target0.95_mglm` | 28,080 | `p50_prev0.30_cstat0.85_target0.95_mglm` | 7,812 | 3.59× lower | — | previous | previous |
| C-statistic: 0.65 → 0.7 | `p50_prev0.40_cstat0.65_target0.95_mglm` | 84,224 | `p50_prev0.40_cstat0.70_target0.95_mglm` | 10,528 | 8.00× lower | — | previous | — |
| C-statistic: 0.6 → 0.7 | `p50_prev0.50_cstat0.60_target0.90_mglm` | 17,776 | `p50_prev0.50_cstat0.70_target0.90_mglm` | 5,107 | 3.48× lower | Yes | — | — |
| C-statistic: 0.65 → 0.7 | `p50_prev0.50_cstat0.65_target0.95_mglm` | 27,560 | `p50_prev0.50_cstat0.70_target0.95_mglm` | 8,424 | 3.27× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.05_cstat0.60_target0.80_mglm` | 10,000 | `p5_prev0.05_cstat0.65_target0.80_mglm` | 2,867 | 3.49× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.05_cstat0.60_target0.90_mglm` | 40,000 | `p5_prev0.05_cstat0.65_target0.90_mglm` | 9,632 | 4.15× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p5_prev0.05_cstat0.65_target0.95_mglm` | 36,057 | `p5_prev0.05_cstat0.70_target0.95_mglm` | 6,824 | 5.28× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.07_cstat0.60_target0.90_mglm` | 26,664 | `p5_prev0.07_cstat0.65_target0.90_mglm` | 6,167 | 4.32× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p5_prev0.07_cstat0.65_target0.95_mglm` | 36,059 | `p5_prev0.07_cstat0.70_target0.95_mglm` | 11,428 | 3.16× lower | — | previous | — |
| C-statistic: 0.8 → 0.85 | `p5_prev0.07_cstat0.80_target0.95_mglm` | 3,230 | `p5_prev0.07_cstat0.85_target0.95_mglm` | 12,560 | 3.89× higher | — | next | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.10_cstat0.60_target0.85_mglm` | 10,000 | `p5_prev0.10_cstat0.65_target0.85_mglm` | 2,308 | 4.33× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p5_prev0.10_cstat0.65_target0.90_mglm` | 9,232 | `p5_prev0.10_cstat0.70_target0.90_mglm` | 2,187 | 4.22× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p5_prev0.10_cstat0.65_target0.95_mglm` | 9,232 | `p5_prev0.10_cstat0.70_target0.95_mglm` | 34,288 | 3.71× higher | — | next | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.15_cstat0.60_target0.80_mglm` | 3,334 | `p5_prev0.15_cstat0.65_target0.80_mglm` | 982 | 3.40× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.15_cstat0.60_target0.85_mglm` | 6,668 | `p5_prev0.15_cstat0.65_target0.85_mglm` | 1,580 | 4.22× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.20_cstat0.60_target0.85_mglm` | 10,000 | `p5_prev0.20_cstat0.65_target0.85_mglm` | 1,303 | 7.67× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p5_prev0.20_cstat0.65_target0.95_mglm` | 18,464 | `p5_prev0.20_cstat0.70_target0.95_mglm` | 2,142 | 8.62× lower | — | previous, next | next |
| C-statistic: 0.7 → 0.75 | `p5_prev0.20_cstat0.70_target0.95_mglm` | 2,142 | `p5_prev0.20_cstat0.75_target0.95_mglm` | 10,656 | 4.97× higher | — | previous, next | previous, next |
| C-statistic: 0.75 → 0.8 | `p5_prev0.20_cstat0.75_target0.90_mglm` | 2,664 | `p5_prev0.20_cstat0.80_target0.90_mglm` | 868 | 3.07× lower | — | previous | — |
| C-statistic: 0.75 → 0.8 | `p5_prev0.20_cstat0.75_target0.95_mglm` | 10,656 | `p5_prev0.20_cstat0.80_target0.95_mglm` | 2,496 | 4.27× lower | — | previous | previous |
| C-statistic: 0.65 → 0.7 | `p5_prev0.25_cstat0.65_target0.85_mglm` | 1,880 | `p5_prev0.25_cstat0.70_target0.85_mglm` | 578 | 3.25× lower | — | — | — |
| C-statistic: 0.8 → 0.85 | `p5_prev0.25_cstat0.80_target0.95_mglm` | 3,376 | `p5_prev0.25_cstat0.85_target0.95_mglm` | 710 | 4.75× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.30_cstat0.60_target0.90_mglm` | 5,952 | `p5_prev0.30_cstat0.65_target0.90_mglm` | 1,488 | 4.00× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p5_prev0.30_cstat0.65_target0.95_mglm` | 5,632 | `p5_prev0.30_cstat0.70_target0.95_mglm` | 1,770 | 3.18× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.40_cstat0.60_target0.90_mglm` | 17,792 | `p5_prev0.40_cstat0.65_target0.90_mglm` | 1,798 | 9.90× lower | — | previous | — |
| C-statistic: 0.75 → 0.8 | `p5_prev0.40_cstat0.75_target0.95_mglm` | 4,224 | `p5_prev0.40_cstat0.80_target0.95_mglm` | 1,047 | 4.03× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.50_cstat0.60_target0.85_mglm` | 3,776 | `p5_prev0.50_cstat0.65_target0.85_mglm` | 792 | 4.77× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p75_prev0.05_cstat0.60_target0.85_mglm` | 150,162 | `p75_prev0.05_cstat0.65_target0.85_mglm` | 48,600 | 3.09× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p75_prev0.05_cstat0.65_target0.85_mglm` | 48,600 | `p75_prev0.05_cstat0.70_target0.85_mglm` | 16,071 | 3.02× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p75_prev0.05_cstat0.65_target0.90_mglm` | 138,462 | `p75_prev0.05_cstat0.70_target0.90_mglm` | 45,679 | 3.03× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p75_prev0.07_cstat0.60_target0.90_mglm` | 100,000 | `p75_prev0.07_cstat0.65_target0.90_mglm` | 21,919 | 4.56× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p75_prev0.07_cstat0.60_target0.95_mglm` | 197,945 | `p75_prev0.07_cstat0.65_target0.95_mglm` | 46,154 | 4.29× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p75_prev0.07_cstat0.70_target0.95_mglm` | 69,863 | `p75_prev0.07_cstat0.75_target0.95_mglm` | 213,328 | 3.05× higher | — | next | — |
| C-statistic: 0.75 → 0.8 | `p75_prev0.07_cstat0.75_target0.95_mglm` | 213,328 | `p75_prev0.07_cstat0.80_target0.95_mglm` | 37,862 | 5.63× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p75_prev0.10_cstat0.65_target0.95_mglm` | 138,460 | `p75_prev0.10_cstat0.70_target0.95_mglm` | 43,363 | 3.19× lower | — | previous | previous |
| C-statistic: 0.8 → 0.85 | `p75_prev0.25_cstat0.80_target0.95_mglm` | 50,528 | `p75_prev0.25_cstat0.85_target0.95_mglm` | 11,497 | 4.39× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p75_prev0.30_cstat0.60_target0.95_mglm` | 168,448 | `p75_prev0.30_cstat0.65_target0.95_mglm` | 35,698 | 4.72× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p75_prev0.30_cstat0.65_target0.95_mglm` | 35,698 | `p75_prev0.30_cstat0.70_target0.95_mglm` | 336,896 | 9.44× higher | — | next | next |
| C-statistic: 0.6 → 0.65 | `p75_prev0.50_cstat0.60_target0.95_mglm` | 113,610 | `p75_prev0.50_cstat0.65_target0.95_mglm` | 24,937 | 4.56× lower | — | — | — |
| Prevalence: 0.05 → 0.075 | `p100_prev0.05_cstat0.85_target0.95_mglm` | 188,232 | `p100_prev0.07_cstat0.85_target0.95_mglm` | 26,076 | 7.22× lower | — | previous | — |
| Prevalence: 0.1 → 0.15 | `p100_prev0.10_cstat0.65_target0.95_mglm` | 92,308 | `p100_prev0.15_cstat0.65_target0.95_mglm` | 30,769 | 3.00× lower | — | — | — |
| Prevalence: 0.05 → 0.075 | `p10_prev0.05_cstat0.65_target0.95_mglm` | 7,218 | `p10_prev0.07_cstat0.65_target0.95_mglm` | 37,664 | 5.22× higher | — | previous, next | previous |
| Prevalence: 0.05 → 0.075 | `p10_prev0.05_cstat0.80_target0.90_mglm` | 5,000 | `p10_prev0.07_cstat0.80_target0.90_mglm` | 1,666 | 3.00× lower | — | — | — |
| Prevalence: 0.075 → 0.1 | `p10_prev0.07_cstat0.65_target0.90_mglm` | 24,616 | `p10_prev0.10_cstat0.65_target0.90_mglm` | 7,901 | 3.12× lower | — | previous | previous |
| Prevalence: 0.075 → 0.1 | `p10_prev0.07_cstat0.70_target0.95_mglm` | 5,523 | `p10_prev0.10_cstat0.70_target0.95_mglm` | 34,288 | 6.21× higher | — | next | next |
| Prevalence: 0.1 → 0.15 | `p10_prev0.10_cstat0.60_target0.95_mglm` | 40,000 | `p10_prev0.15_cstat0.60_target0.95_mglm` | 8,564 | 4.67× lower | — | — | — |
| Prevalence: 0.1 → 0.15 | `p10_prev0.10_cstat0.70_target0.95_mglm` | 34,288 | `p10_prev0.15_cstat0.70_target0.95_mglm` | 5,714 | 6.00× lower | — | previous | previous |
| Prevalence: 0.2 → 0.25 | `p10_prev0.20_cstat0.60_target0.90_mglm` | 20,000 | `p10_prev0.25_cstat0.60_target0.90_mglm` | 5,471 | 3.66× lower | — | — | — |
| Prevalence: 0.2 → 0.25 | `p10_prev0.20_cstat0.70_target0.95_mglm` | 4,050 | `p10_prev0.25_cstat0.70_target0.95_mglm` | 26,944 | 6.65× higher | — | next | next |
| Prevalence: 0.25 → 0.3 | `p10_prev0.25_cstat0.70_target0.95_mglm` | 26,944 | `p10_prev0.30_cstat0.70_target0.95_mglm` | 5,736 | 4.70× lower | — | previous | previous |
| Prevalence: 0.05 → 0.075 | `p15_prev0.05_cstat0.60_target0.90_mglm` | 240,000 | `p15_prev0.07_cstat0.60_target0.90_mglm` | 41,457 | 5.79× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p15_prev0.05_cstat0.65_target0.90_mglm` | 27,692 | `p15_prev0.07_cstat0.65_target0.90_mglm` | 8,975 | 3.09× lower | — | — | — |
| Prevalence: 0.2 → 0.25 | `p15_prev0.20_cstat0.60_target0.90_mglm` | 14,316 | `p15_prev0.25_cstat0.60_target0.90_mglm` | 85,376 | 5.96× higher | — | next | — |
| Prevalence: 0.25 → 0.3 | `p15_prev0.25_cstat0.60_target0.90_mglm` | 85,376 | `p15_prev0.30_cstat0.60_target0.90_mglm` | 8,896 | 9.60× lower | — | previous | — |
| Prevalence: 0.25 → 0.3 | `p15_prev0.25_cstat0.80_target0.95_mglm` | 5,056 | `p15_prev0.30_cstat0.80_target0.95_mglm` | 16,832 | 3.33× higher | — | next | next |
| Prevalence: 0.3 → 0.4 | `p15_prev0.30_cstat0.80_target0.95_mglm` | 16,832 | `p15_prev0.40_cstat0.80_target0.95_mglm` | 3,444 | 4.89× lower | — | previous | previous |
| Prevalence: 0.4 → 0.5 | `p15_prev0.40_cstat0.70_target0.95_mglm` | 2,936 | `p15_prev0.50_cstat0.70_target0.95_mglm` | 20,224 | 6.89× higher | — | next | — |
| Prevalence: 0.05 → 0.075 | `p20_prev0.05_cstat0.60_target0.90_mglm` | 79,498 | `p20_prev0.07_cstat0.60_target0.90_mglm` | 26,086 | 3.05× lower | — | — | — |
| Prevalence: 0.05 → 0.075 | `p20_prev0.05_cstat0.65_target0.90_mglm` | 36,924 | `p20_prev0.07_cstat0.65_target0.90_mglm` | 12,308 | 3.00× lower | — | — | — |
| Prevalence: 0.1 → 0.15 | `p20_prev0.10_cstat0.70_target0.95_mglm` | 17,418 | `p20_prev0.15_cstat0.70_target0.95_mglm` | 5,715 | 3.05× lower | — | — | — |
| Prevalence: 0.15 → 0.2 | `p20_prev0.15_cstat0.65_target0.95_mglm` | 23,619 | `p20_prev0.20_cstat0.65_target0.95_mglm` | 147,680 | 6.25× higher | — | next | next |
| Prevalence: 0.3 → 0.4 | `p20_prev0.30_cstat0.60_target0.95_mglm` | 89,856 | `p20_prev0.40_cstat0.60_target0.95_mglm` | 16,832 | 5.34× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p25_prev0.05_cstat0.65_target0.85_mglm` | 11,281 | `p25_prev0.07_cstat0.65_target0.85_mglm` | 61,540 | 5.46× higher | — | next | next |
| Prevalence: 0.05 → 0.075 | `p25_prev0.05_cstat0.75_target0.95_mglm` | 106,672 | `p25_prev0.07_cstat0.75_target0.95_mglm` | 17,384 | 6.14× lower | — | previous | previous |
| Prevalence: 0.05 → 0.075 | `p25_prev0.05_cstat0.80_target0.95_mglm` | 25,000 | `p25_prev0.07_cstat0.80_target0.95_mglm` | 8,124 | 3.08× lower | — | — | — |
| Prevalence: 0.075 → 0.1 | `p25_prev0.07_cstat0.65_target0.85_mglm` | 61,540 | `p25_prev0.10_cstat0.65_target0.85_mglm` | 8,204 | 7.50× lower | — | previous | previous |
| Prevalence: 0.1 → 0.15 | `p25_prev0.10_cstat0.70_target0.95_mglm` | 42,856 | `p25_prev0.15_cstat0.70_target0.95_mglm` | 228,576 | 5.33× higher | — | next | next |
| Prevalence: 0.2 → 0.25 | `p25_prev0.20_cstat0.75_target0.95_mglm` | 26,672 | `p25_prev0.25_cstat0.75_target0.95_mglm` | 7,438 | 3.59× lower | — | previous | previous |
| Prevalence: 0.25 → 0.3 | `p25_prev0.25_cstat0.60_target0.95_mglm` | 67,392 | `p25_prev0.30_cstat0.60_target0.95_mglm` | 13,195 | 5.11× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p30_prev0.05_cstat0.60_target0.85_mglm` | 62,046 | `p30_prev0.07_cstat0.60_target0.85_mglm` | 19,136 | 3.24× lower | — | — | — |
| Prevalence: 0.05 → 0.075 | `p30_prev0.05_cstat0.60_target0.95_mglm` | 175,813 | `p30_prev0.07_cstat0.60_target0.95_mglm` | 640,000 | 3.64× higher | — | previous, next | next |
| Prevalence: 0.05 → 0.075 | `p30_prev0.05_cstat0.65_target0.90_mglm` | 55,384 | `p30_prev0.07_cstat0.65_target0.90_mglm` | 8,888 | 6.23× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p30_prev0.05_cstat0.75_target0.95_mglm` | 38,749 | `p30_prev0.07_cstat0.75_target0.95_mglm` | 10,092 | 3.84× lower | — | previous | — |
| Prevalence: 0.075 → 0.1 | `p30_prev0.07_cstat0.70_target0.95_mglm` | 68,572 | `p30_prev0.10_cstat0.70_target0.95_mglm` | 12,185 | 5.63× lower | — | previous, next | next |
| Prevalence: 0.1 → 0.15 | `p30_prev0.10_cstat0.60_target0.95_mglm` | 239,905 | `p30_prev0.15_cstat0.60_target0.95_mglm` | 40,000 | 6.00× lower | — | previous | previous |
| Prevalence: 0.25 → 0.3 | `p30_prev0.25_cstat0.60_target0.95_mglm` | 40,416 | `p30_prev0.30_cstat0.60_target0.95_mglm` | 269,568 | 6.67× higher | — | next | next |
| Prevalence: 0.3 → 0.4 | `p30_prev0.30_cstat0.60_target0.95_mglm` | 269,568 | `p30_prev0.40_cstat0.60_target0.95_mglm` | 46,356 | 5.82× lower | — | previous | previous |
| Prevalence: 0.05 → 0.075 | `p40_prev0.05_cstat0.65_target0.95_mglm` | 295,652 | `p40_prev0.07_cstat0.65_target0.95_mglm` | 94,540 | 3.13× lower | — | previous, next | next |
| Prevalence: 0.05 → 0.075 | `p40_prev0.05_cstat0.70_target0.90_mglm` | 68,572 | `p40_prev0.07_cstat0.70_target0.90_mglm` | 14,689 | 4.67× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p40_prev0.05_cstat0.75_target0.95_mglm` | 42,668 | `p40_prev0.07_cstat0.75_target0.95_mglm` | 14,222 | 3.00× lower | — | — | — |
| Prevalence: 0.075 → 0.1 | `p40_prev0.07_cstat0.60_target0.95_mglm` | 426,672 | `p40_prev0.10_cstat0.60_target0.95_mglm` | 76,711 | 5.56× lower | — | previous | — |
| Prevalence: 0.075 → 0.1 | `p40_prev0.07_cstat0.65_target0.95_mglm` | 94,540 | `p40_prev0.10_cstat0.65_target0.95_mglm` | 295,392 | 3.12× higher | — | previous, next | previous, next |
| Prevalence: 0.15 → 0.2 | `p40_prev0.15_cstat0.60_target0.95_mglm` | 53,332 | `p40_prev0.20_cstat0.60_target0.95_mglm` | 320,000 | 6.00× higher | — | next | — |
| Prevalence: 0.05 → 0.075 | `p50_prev0.05_cstat0.60_target0.80_mglm` | 800,000 | `p50_prev0.07_cstat0.60_target0.80_mglm` | 133,332 | 6.00× lower | — | previous, next | — |
| Prevalence: 0.05 → 0.075 | `p50_prev0.05_cstat0.65_target0.95_mglm` | 184,616 | `p50_prev0.07_cstat0.65_target0.95_mglm` | 60,243 | 3.06× lower | — | — | — |
| Prevalence: 0.075 → 0.1 | `p50_prev0.07_cstat0.60_target0.80_mglm` | 133,332 | `p50_prev0.10_cstat0.60_target0.80_mglm` | 25,036 | 5.33× lower | — | previous | — |
| Prevalence: 0.1 → 0.15 | `p50_prev0.10_cstat0.75_target0.95_mglm` | 13,119 | `p50_prev0.15_cstat0.75_target0.95_mglm` | 71,104 | 5.42× higher | — | next | next |
| Prevalence: 0.15 → 0.2 | `p50_prev0.15_cstat0.75_target0.95_mglm` | 71,104 | `p50_prev0.20_cstat0.75_target0.95_mglm` | 12,990 | 5.47× lower | — | previous | previous |
| Prevalence: 0.3 → 0.4 | `p50_prev0.30_cstat0.80_target0.95_mglm` | 28,080 | `p50_prev0.40_cstat0.80_target0.95_mglm` | 7,779 | 3.61× lower | — | previous | previous |
| Prevalence: 0.4 → 0.5 | `p50_prev0.40_cstat0.65_target0.95_mglm` | 84,224 | `p50_prev0.50_cstat0.65_target0.95_mglm` | 27,560 | 3.06× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p5_prev0.05_cstat0.60_target0.80_mglm` | 10,000 | `p5_prev0.07_cstat0.60_target0.80_mglm` | 3,160 | 3.16× lower | — | — | — |
| Prevalence: 0.075 → 0.1 | `p5_prev0.07_cstat0.65_target0.95_mglm` | 36,059 | `p5_prev0.10_cstat0.65_target0.95_mglm` | 9,232 | 3.91× lower | — | previous | — |
| Prevalence: 0.075 → 0.1 | `p5_prev0.07_cstat0.70_target0.95_mglm` | 11,428 | `p5_prev0.10_cstat0.70_target0.95_mglm` | 34,288 | 3.00× higher | — | next | — |
| Prevalence: 0.075 → 0.1 | `p5_prev0.07_cstat0.85_target0.95_mglm` | 12,560 | `p5_prev0.10_cstat0.85_target0.95_mglm` | 2,352 | 5.34× lower | — | previous | — |
| Prevalence: 0.1 → 0.15 | `p5_prev0.10_cstat0.60_target0.95_mglm` | 9,799 | `p5_prev0.15_cstat0.60_target0.95_mglm` | 53,344 | 5.44× higher | — | next | — |
| Prevalence: 0.1 → 0.15 | `p5_prev0.10_cstat0.70_target0.95_mglm` | 34,288 | `p5_prev0.15_cstat0.70_target0.95_mglm` | 6,220 | 5.51× lower | — | previous | — |
| Prevalence: 0.15 → 0.2 | `p5_prev0.15_cstat0.75_target0.95_mglm` | 3,552 | `p5_prev0.20_cstat0.75_target0.95_mglm` | 10,656 | 3.00× higher | — | next | next |
| Prevalence: 0.2 → 0.25 | `p5_prev0.20_cstat0.60_target0.85_mglm` | 10,000 | `p5_prev0.25_cstat0.60_target0.85_mglm` | 1,941 | 5.15× lower | — | previous | — |
| Prevalence: 0.2 → 0.25 | `p5_prev0.20_cstat0.65_target0.95_mglm` | 18,464 | `p5_prev0.25_cstat0.65_target0.95_mglm` | 5,094 | 3.62× lower | — | previous | — |
| Prevalence: 0.2 → 0.25 | `p5_prev0.20_cstat0.70_target0.95_mglm` | 2,142 | `p5_prev0.25_cstat0.70_target0.95_mglm` | 6,752 | 3.15× higher | — | previous, next | previous, next |
| Prevalence: 0.2 → 0.25 | `p5_prev0.20_cstat0.75_target0.90_mglm` | 2,664 | `p5_prev0.25_cstat0.75_target0.90_mglm` | 546 | 4.88× lower | — | previous | — |
| Prevalence: 0.25 → 0.3 | `p5_prev0.25_cstat0.70_target0.95_mglm` | 6,752 | `p5_prev0.30_cstat0.70_target0.95_mglm` | 1,770 | 3.81× lower | — | previous | previous |
| Prevalence: 0.25 → 0.3 | `p5_prev0.25_cstat0.85_target0.95_mglm` | 710 | `p5_prev0.30_cstat0.85_target0.95_mglm` | 2,816 | 3.97× higher | — | next | — |
| Prevalence: 0.4 → 0.5 | `p5_prev0.40_cstat0.60_target0.90_mglm` | 17,792 | `p5_prev0.50_cstat0.60_target0.90_mglm` | 3,552 | 5.01× lower | — | previous | — |
| Prevalence: 0.4 → 0.5 | `p5_prev0.40_cstat0.70_target0.95_mglm` | 3,712 | `p5_prev0.50_cstat0.70_target0.95_mglm` | 840 | 4.42× lower | — | — | — |
| Prevalence: 0.05 → 0.075 | `p75_prev0.05_cstat0.65_target0.90_mglm` | 138,462 | `p75_prev0.07_cstat0.65_target0.90_mglm` | 21,919 | 6.32× lower | — | previous | — |
| Prevalence: 0.075 → 0.1 | `p75_prev0.07_cstat0.75_target0.95_mglm` | 213,328 | `p75_prev0.10_cstat0.75_target0.95_mglm` | 40,000 | 5.33× lower | — | previous | — |
| Prevalence: 0.1 → 0.15 | `p75_prev0.10_cstat0.65_target0.95_mglm` | 138,460 | `p75_prev0.15_cstat0.65_target0.95_mglm` | 34,765 | 3.98× lower | — | previous | previous |
| Prevalence: 0.25 → 0.3 | `p75_prev0.25_cstat0.60_target0.95_mglm` | 50,144 | `p75_prev0.30_cstat0.60_target0.95_mglm` | 168,448 | 3.36× higher | — | — | — |
| Predictors: 10 → 15 | `p10_prev0.05_cstat0.60_target0.95_mglm` | 40,000 | `p15_prev0.05_cstat0.60_target0.95_mglm` | 240,000 | 6.00× higher | — | previous, next | — |
| Predictors: 10 → 15 | `p10_prev0.05_cstat0.65_target0.90_mglm` | 9,030 | `p15_prev0.05_cstat0.65_target0.90_mglm` | 27,692 | 3.07× higher | — | — | — |
| Predictors: 10 → 15 | `p10_prev0.05_cstat0.65_target0.95_mglm` | 7,218 | `p15_prev0.05_cstat0.65_target0.95_mglm` | 55,384 | 7.67× higher | — | previous, next | previous |
| Predictors: 10 → 15 | `p10_prev0.07_cstat0.60_target0.85_mglm` | 11,839 | `p15_prev0.07_cstat0.60_target0.85_mglm` | 40,000 | 3.38× higher | — | next | — |
| Predictors: 10 → 15 | `p10_prev0.07_cstat0.60_target0.95_mglm` | 26,668 | `p15_prev0.07_cstat0.60_target0.95_mglm` | 120,289 | 4.51× higher | — | — | — |
| Predictors: 10 → 15 | `p10_prev0.07_cstat0.70_target0.95_mglm` | 5,523 | `p15_prev0.07_cstat0.70_target0.95_mglm` | 18,790 | 3.40× higher | — | — | — |
| Predictors: 10 → 15 | `p10_prev0.07_cstat0.80_target0.90_mglm` | 1,666 | `p15_prev0.07_cstat0.80_target0.90_mglm` | 5,000 | 3.00× higher | — | — | — |
| Predictors: 10 → 15 | `p10_prev0.10_cstat0.60_target0.85_mglm` | 5,000 | `p15_prev0.10_cstat0.60_target0.85_mglm` | 15,000 | 3.00× higher | — | — | — |
| Predictors: 10 → 15 | `p10_prev0.10_cstat0.70_target0.95_mglm` | 34,288 | `p15_prev0.10_cstat0.70_target0.95_mglm` | 10,316 | 3.32× lower | — | previous | previous |
| Predictors: 10 → 15 | `p10_prev0.20_cstat0.60_target0.95_mglm` | 8,241 | `p15_prev0.20_cstat0.60_target0.95_mglm` | 44,835 | 5.44× higher | — | next | — |
| Predictors: 10 → 15 | `p10_prev0.20_cstat0.65_target0.95_mglm` | 4,603 | `p15_prev0.20_cstat0.65_target0.95_mglm` | 27,696 | 6.02× higher | — | previous, next | previous |
| Predictors: 10 → 15 | `p10_prev0.25_cstat0.70_target0.95_mglm` | 26,944 | `p15_prev0.25_cstat0.70_target0.95_mglm` | 4,986 | 5.40× lower | — | previous | previous |
| Predictors: 10 → 15 | `p10_prev0.30_cstat0.80_target0.95_mglm` | 3,257 | `p15_prev0.30_cstat0.80_target0.95_mglm` | 16,832 | 5.17× higher | — | next | next |
| Predictors: 10 → 15 | `p10_prev0.40_cstat0.85_target0.95_mglm` | 2,104 | `p15_prev0.40_cstat0.85_target0.95_mglm` | 6,320 | 3.00× higher | — | next | — |
| Predictors: 10 → 15 | `p10_prev0.50_cstat0.70_target0.95_mglm` | 5,440 | `p15_prev0.50_cstat0.70_target0.95_mglm` | 20,224 | 3.72× higher | — | previous, next | — |
| Predictors: 15 → 20 | `p15_prev0.05_cstat0.60_target0.90_mglm` | 240,000 | `p20_prev0.05_cstat0.60_target0.90_mglm` | 79,498 | 3.02× lower | — | previous | — |
| Predictors: 15 → 25 | `p15_prev0.05_cstat0.60_target0.95_mglm` | 240,000 | `p25_prev0.05_cstat0.60_target0.95_mglm` | 1,215,964 | 5.07× higher | Yes | previous, next | next |
| Predictors: 15 → 20 | `p15_prev0.15_cstat0.60_target0.95_mglm` | 97,489 | `p20_prev0.15_cstat0.60_target0.95_mglm` | 25,118 | 3.88× lower | — | previous | — |
| Predictors: 15 → 20 | `p15_prev0.20_cstat0.65_target0.95_mglm` | 27,696 | `p20_prev0.20_cstat0.65_target0.95_mglm` | 147,680 | 5.33× higher | — | previous, next | next |
| Predictors: 15 → 20 | `p15_prev0.25_cstat0.60_target0.90_mglm` | 85,376 | `p20_prev0.25_cstat0.60_target0.90_mglm` | 15,084 | 5.66× lower | — | previous | — |
| Predictors: 20 → 25 | `p20_prev0.05_cstat0.75_target0.95_mglm` | 32,253 | `p25_prev0.05_cstat0.75_target0.95_mglm` | 106,672 | 3.31× higher | — | next | next |
| Predictors: 20 → 25 | `p20_prev0.07_cstat0.65_target0.85_mglm` | 9,520 | `p25_prev0.07_cstat0.65_target0.85_mglm` | 61,540 | 6.46× higher | — | next | next |
| Predictors: 20 → 25 | `p20_prev0.07_cstat0.65_target0.95_mglm` | 49,232 | `p25_prev0.07_cstat0.65_target0.95_mglm` | 246,160 | 5.00× higher | — | next | — |
| Predictors: 20 → 25 | `p20_prev0.20_cstat0.65_target0.95_mglm` | 147,680 | `p25_prev0.20_cstat0.65_target0.95_mglm` | 22,562 | 6.55× lower | — | previous | previous |
| Predictors: 20 → 25 | `p20_prev0.20_cstat0.75_target0.95_mglm` | 7,636 | `p25_prev0.20_cstat0.75_target0.95_mglm` | 26,672 | 3.49× higher | — | next | next |
| Predictors: 20 → 25 | `p20_prev0.25_cstat0.70_target0.95_mglm` | 3,368 | `p25_prev0.25_cstat0.70_target0.95_mglm` | 16,848 | 5.00× higher | — | next | — |
| Predictors: 20 → 25 | `p20_prev0.30_cstat0.60_target0.95_mglm` | 89,856 | `p25_prev0.30_cstat0.60_target0.95_mglm` | 13,195 | 6.81× lower | — | previous | — |
| Predictors: 20 → 25 | `p20_prev0.30_cstat0.70_target0.95_mglm` | 4,222 | `p25_prev0.30_cstat0.70_target0.95_mglm` | 28,064 | 6.65× higher | — | next | next |
| Predictors: 25 → 30 | `p25_prev0.05_cstat0.60_target0.95_mglm` | 1,215,964 | `p30_prev0.05_cstat0.60_target0.95_mglm` | 175,813 | 6.92× lower | — | previous, next | previous |
| Predictors: 25 → 40 | `p25_prev0.05_cstat0.65_target0.95_mglm` | 92,308 | `p40_prev0.05_cstat0.65_target0.95_mglm` | 295,652 | 3.20× higher | Yes | next | — |
| Predictors: 25 → 30 | `p25_prev0.05_cstat0.80_target0.95_mglm` | 25,000 | `p30_prev0.05_cstat0.80_target0.95_mglm` | 7,500 | 3.33× lower | — | next | next |
| Predictors: 25 → 30 | `p25_prev0.07_cstat0.60_target0.95_mglm` | 121,388 | `p30_prev0.07_cstat0.60_target0.95_mglm` | 640,000 | 5.27× higher | — | next | next |
| Predictors: 25 → 30 | `p25_prev0.07_cstat0.65_target0.85_mglm` | 61,540 | `p30_prev0.07_cstat0.65_target0.85_mglm` | 10,794 | 5.70× lower | — | previous | previous |
| Predictors: 25 → 30 | `p25_prev0.10_cstat0.60_target0.95_mglm` | 64,666 | `p30_prev0.10_cstat0.60_target0.95_mglm` | 239,905 | 3.71× higher | — | next | next |
| Predictors: 25 → 30 | `p25_prev0.10_cstat0.70_target0.95_mglm` | 42,856 | `p30_prev0.10_cstat0.70_target0.95_mglm` | 12,185 | 3.52× lower | — | next | next |
| Predictors: 25 → 30 | `p25_prev0.15_cstat0.70_target0.95_mglm` | 228,576 | `p30_prev0.15_cstat0.70_target0.95_mglm` | 34,284 | 6.67× lower | — | previous, next | previous |
| Predictors: 25 → 30 | `p25_prev0.20_cstat0.75_target0.95_mglm` | 26,672 | `p30_prev0.20_cstat0.75_target0.95_mglm` | 7,617 | 3.50× lower | — | previous | previous |
| Predictors: 25 → 30 | `p25_prev0.25_cstat0.85_target0.95_mglm` | 4,147 | `p30_prev0.25_cstat0.85_target0.95_mglm` | 20,208 | 4.87× higher | — | next | — |
| Predictors: 25 → 30 | `p25_prev0.40_cstat0.65_target0.95_mglm` | 168,448 | `p30_prev0.40_cstat0.65_target0.95_mglm` | 18,490 | 9.11× lower | — | previous | previous |
| Predictors: 30 → 40 | `p30_prev0.05_cstat0.70_target0.90_mglm` | 17,640 | `p40_prev0.05_cstat0.70_target0.90_mglm` | 68,572 | 3.89× higher | — | next | — |
| Predictors: 30 → 40 | `p30_prev0.05_cstat0.70_target0.95_mglm` | 24,284 | `p40_prev0.05_cstat0.70_target0.95_mglm` | 137,144 | 5.65× higher | — | next | — |
| Predictors: 30 → 40 | `p30_prev0.10_cstat0.60_target0.95_mglm` | 239,905 | `p40_prev0.10_cstat0.60_target0.95_mglm` | 76,711 | 3.13× lower | — | previous | previous |
| Predictors: 30 → 40 | `p30_prev0.10_cstat0.65_target0.95_mglm` | 52,164 | `p40_prev0.10_cstat0.65_target0.95_mglm` | 295,392 | 5.66× higher | — | next | next |
| Predictors: 30 → 40 | `p30_prev0.20_cstat0.60_target0.95_mglm` | 53,762 | `p40_prev0.20_cstat0.60_target0.95_mglm` | 320,000 | 5.95× higher | — | next | — |
| Predictors: 30 → 40 | `p30_prev0.30_cstat0.60_target0.95_mglm` | 269,568 | `p40_prev0.30_cstat0.60_target0.95_mglm` | 44,896 | 6.00× lower | — | previous | previous |
| Predictors: 40 → 50 | `p40_prev0.05_cstat0.80_target0.95_mglm` | 20,000 | `p50_prev0.05_cstat0.80_target0.95_mglm` | 100,000 | 5.00× higher | — | next | — |
| Predictors: 40 → 50 | `p40_prev0.07_cstat0.60_target0.80_mglm` | 26,667 | `p50_prev0.07_cstat0.60_target0.80_mglm` | 133,332 | 5.00× higher | — | next | — |
| Predictors: 40 → 50 | `p40_prev0.07_cstat0.60_target0.90_mglm` | 42,896 | `p50_prev0.07_cstat0.60_target0.90_mglm` | 133,332 | 3.11× higher | — | — | — |
| Predictors: 40 → 50 | `p40_prev0.10_cstat0.65_target0.95_mglm` | 295,392 | `p50_prev0.10_cstat0.65_target0.95_mglm` | 46,154 | 6.40× lower | — | previous | previous |
| Predictors: 40 → 50 | `p40_prev0.15_cstat0.75_target0.95_mglm` | 14,224 | `p50_prev0.15_cstat0.75_target0.95_mglm` | 71,104 | 5.00× higher | — | next | next |
| Predictors: 40 → 50 | `p40_prev0.25_cstat0.60_target0.95_mglm` | 862,208 | `p50_prev0.25_cstat0.60_target0.95_mglm` | 105,390 | 8.18× lower | — | previous, next | — |
| Predictors: 40 → 50 | `p40_prev0.30_cstat0.80_target0.95_mglm` | 5,522 | `p50_prev0.30_cstat0.80_target0.95_mglm` | 28,080 | 5.09× higher | — | next | next |
| Predictors: 50 → 75 | `p50_prev0.05_cstat0.60_target0.80_mglm` | 800,000 | `p75_prev0.05_cstat0.60_target0.80_mglm` | 80,327 | 9.96× lower | — | previous | — |
| Predictors: 50 → 75 | `p50_prev0.05_cstat0.65_target0.90_mglm` | 45,166 | `p75_prev0.05_cstat0.65_target0.90_mglm` | 138,462 | 3.07× higher | — | next | — |
| Predictors: 50 → 75 | `p50_prev0.05_cstat0.75_target0.95_mglm` | 21,436 | `p75_prev0.05_cstat0.75_target0.95_mglm` | 81,000 | 3.78× higher | — | previous | previous |
| Predictors: 50 → 75 | `p50_prev0.07_cstat0.75_target0.95_mglm` | 35,116 | `p75_prev0.07_cstat0.75_target0.95_mglm` | 213,328 | 6.07× higher | — | next | — |
| Predictors: 50 → 75 | `p50_prev0.07_cstat0.85_target0.95_mglm` | 7,844 | `p75_prev0.07_cstat0.85_target0.95_mglm` | 26,489 | 3.38× higher | — | — | — |
| Predictors: 50 → 75 | `p50_prev0.10_cstat0.75_target0.95_mglm` | 13,119 | `p75_prev0.10_cstat0.75_target0.95_mglm` | 40,000 | 3.05× higher | — | — | — |
| Predictors: 50 → 75 | `p50_prev0.10_cstat0.80_target0.95_mglm` | 12,500 | `p75_prev0.10_cstat0.80_target0.95_mglm` | 37,500 | 3.00× higher | — | — | — |
| Predictors: 50 → 75 | `p50_prev0.25_cstat0.80_target0.95_mglm` | 9,961 | `p75_prev0.25_cstat0.80_target0.95_mglm` | 50,528 | 5.07× higher | — | next | — |
| Predictors: 50 → 75 | `p50_prev0.40_cstat0.70_target0.95_mglm` | 10,528 | `p75_prev0.40_cstat0.70_target0.95_mglm` | 31,584 | 3.00× higher | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.05_cstat0.65_target0.95_mglm` | 36,057 | `p10_prev0.05_cstat0.65_target0.95_mglm` | 7,218 | 5.00× lower | — | previous, next | next |
| Predictors: 5 → 10 | `p5_prev0.05_cstat0.75_target0.95_mglm` | 5,332 | `p10_prev0.05_cstat0.75_target0.95_mglm` | 21,336 | 4.00× higher | — | next | — |
| Predictors: 5 → 10 | `p5_prev0.07_cstat0.65_target0.90_mglm` | 6,167 | `p10_prev0.07_cstat0.65_target0.90_mglm` | 24,616 | 3.99× higher | — | next | next |
| Predictors: 5 → 10 | `p5_prev0.10_cstat0.60_target0.90_mglm` | 10,000 | `p10_prev0.10_cstat0.60_target0.90_mglm` | 40,000 | 4.00× higher | — | next | — |
| Predictors: 5 → 10 | `p5_prev0.10_cstat0.60_target0.95_mglm` | 9,799 | `p10_prev0.10_cstat0.60_target0.95_mglm` | 40,000 | 4.08× higher | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.10_cstat0.65_target0.85_mglm` | 2,308 | `p10_prev0.10_cstat0.65_target0.85_mglm` | 9,230 | 4.00× higher | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.10_cstat0.75_target0.95_mglm` | 2,647 | `p10_prev0.10_cstat0.75_target0.95_mglm` | 21,328 | 8.06× higher | — | next | — |
| Predictors: 5 → 10 | `p5_prev0.15_cstat0.60_target0.95_mglm` | 53,344 | `p10_prev0.15_cstat0.60_target0.95_mglm` | 8,564 | 6.23× lower | — | previous | — |
| Predictors: 5 → 10 | `p5_prev0.20_cstat0.60_target0.90_mglm` | 5,000 | `p10_prev0.20_cstat0.60_target0.90_mglm` | 20,000 | 4.00× higher | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.20_cstat0.65_target0.95_mglm` | 18,464 | `p10_prev0.20_cstat0.65_target0.95_mglm` | 4,603 | 4.01× lower | — | previous, next | next |
| Predictors: 5 → 10 | `p5_prev0.20_cstat0.75_target0.95_mglm` | 10,656 | `p10_prev0.20_cstat0.75_target0.95_mglm` | 2,668 | 3.99× lower | — | previous | previous |
| Predictors: 5 → 10 | `p5_prev0.20_cstat0.85_target0.95_mglm` | 1,176 | `p10_prev0.20_cstat0.85_target0.95_mglm` | 4,704 | 4.00× higher | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.25_cstat0.70_target0.95_mglm` | 6,752 | `p10_prev0.25_cstat0.70_target0.95_mglm` | 26,944 | 3.99× higher | — | previous, next | previous, next |
| Predictors: 5 → 10 | `p5_prev0.25_cstat0.75_target0.90_mglm` | 546 | `p10_prev0.25_cstat0.75_target0.90_mglm` | 1,776 | 3.25× higher | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.25_cstat0.85_target0.95_mglm` | 710 | `p10_prev0.25_cstat0.85_target0.95_mglm` | 3,368 | 4.74× higher | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.30_cstat0.70_target0.95_mglm` | 1,770 | `p10_prev0.30_cstat0.70_target0.95_mglm` | 5,736 | 3.24× higher | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.50_cstat0.60_target0.95_mglm` | 3,360 | `p10_prev0.50_cstat0.60_target0.95_mglm` | 24,975 | 7.43× higher | — | next | — |
| Predictors: 5 → 10 | `p5_prev0.50_cstat0.70_target0.95_mglm` | 840 | `p10_prev0.50_cstat0.70_target0.95_mglm` | 5,440 | 6.48× higher | — | next | — |
| Predictors: 5 → 10 | `p5_prev0.50_cstat0.75_target0.95_mglm` | 2,154 | `p10_prev0.50_cstat0.75_target0.95_mglm` | 6,752 | 3.13× higher | — | — | — |
| Predictors: 75 → 100 | `p75_prev0.05_cstat0.85_target0.95_mglm` | 35,294 | `p100_prev0.05_cstat0.85_target0.95_mglm` | 188,232 | 5.33× higher | — | next | — |
| Predictors: 75 → 100 | `p75_prev0.07_cstat0.75_target0.95_mglm` | 213,328 | `p100_prev0.07_cstat0.75_target0.95_mglm` | 71,108 | 3.00× lower | — | previous | — |
| Predictors: 75 → 100 | `p75_prev0.20_cstat0.60_target0.95_mglm` | 75,000 | `p100_prev0.20_cstat0.60_target0.95_mglm` | 387,633 | 5.17× higher | — | next | — |
| Predictors: 75 → 100 | `p75_prev0.30_cstat0.60_target0.95_mglm` | 168,448 | `p100_prev0.30_cstat0.60_target0.95_mglm` | 48,269 | 3.49× lower | — | — | — |
| Target slope: 0.85 → 0.9 | `p100_prev0.05_cstat0.60_target0.85_mglm` | 74,512 | `p100_prev0.05_cstat0.60_target0.90_mglm` | 333,368 | 4.47× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.07_cstat0.70_target0.90_mglm` | 33,453 | `p100_prev0.07_cstat0.70_target0.95_mglm` | 109,626 | 3.28× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.07_cstat0.75_target0.90_mglm` | 21,908 | `p100_prev0.07_cstat0.75_target0.95_mglm` | 71,108 | 3.25× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.15_cstat0.60_target0.90_mglm` | 66,666 | `p100_prev0.15_cstat0.60_target0.95_mglm` | 204,200 | 3.06× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.15_cstat0.80_target0.90_mglm` | 8,334 | `p100_prev0.15_cstat0.80_target0.95_mglm` | 33,336 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.20_cstat0.60_target0.90_mglm` | 100,000 | `p100_prev0.20_cstat0.60_target0.95_mglm` | 387,633 | 3.88× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.20_cstat0.85_target0.90_mglm` | 7,195 | `p100_prev0.20_cstat0.85_target0.95_mglm` | 23,528 | 3.27× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.50_cstat0.65_target0.90_mglm` | 17,776 | `p100_prev0.50_cstat0.65_target0.95_mglm` | 67,360 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.50_cstat0.70_target0.90_mglm` | 8,888 | `p100_prev0.50_cstat0.70_target0.95_mglm` | 28,000 | 3.15× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.50_cstat0.75_target0.90_mglm` | 8,888 | `p100_prev0.50_cstat0.75_target0.95_mglm` | 33,680 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.05_cstat0.75_target0.90_mglm` | 2,612 | `p10_prev0.05_cstat0.75_target0.95_mglm` | 21,336 | 8.17× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.05_cstat0.85_target0.90_mglm` | 2,353 | `p10_prev0.05_cstat0.85_target0.95_mglm` | 9,412 | 4.00× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p10_prev0.07_cstat0.65_target0.85_mglm` | 5,149 | `p10_prev0.07_cstat0.65_target0.90_mglm` | 24,616 | 4.78× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p10_prev0.07_cstat0.75_target0.90_mglm` | 3,790 | `p10_prev0.07_cstat0.75_target0.95_mglm` | 14,216 | 3.75× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.07_cstat0.80_target0.90_mglm` | 1,666 | `p10_prev0.07_cstat0.80_target0.95_mglm` | 6,530 | 3.92× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p10_prev0.10_cstat0.60_target0.85_mglm` | 5,000 | `p10_prev0.10_cstat0.60_target0.90_mglm` | 40,000 | 8.00× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p10_prev0.10_cstat0.65_target0.80_mglm` | 2,516 | `p10_prev0.10_cstat0.65_target0.85_mglm` | 9,230 | 3.67× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.10_cstat0.70_target0.90_mglm` | 4,312 | `p10_prev0.10_cstat0.70_target0.95_mglm` | 34,288 | 7.95× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p10_prev0.10_cstat0.75_target0.90_mglm` | 2,666 | `p10_prev0.10_cstat0.75_target0.95_mglm` | 21,328 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.10_cstat0.80_target0.90_mglm` | 1,564 | `p10_prev0.10_cstat0.80_target0.95_mglm` | 6,084 | 3.89× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p10_prev0.20_cstat0.60_target0.85_mglm` | 5,384 | `p10_prev0.20_cstat0.60_target0.90_mglm` | 20,000 | 3.71× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.20_cstat0.80_target0.90_mglm` | 1,023 | `p10_prev0.20_cstat0.80_target0.95_mglm` | 4,449 | 4.35× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.20_cstat0.85_target0.90_mglm` | 1,176 | `p10_prev0.20_cstat0.85_target0.95_mglm` | 4,704 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.25_cstat0.65_target0.90_mglm` | 3,648 | `p10_prev0.25_cstat0.65_target0.95_mglm` | 13,472 | 3.69× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.25_cstat0.75_target0.90_mglm` | 1,776 | `p10_prev0.25_cstat0.75_target0.95_mglm` | 5,482 | 3.09× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.25_cstat0.85_target0.90_mglm` | 1,064 | `p10_prev0.25_cstat0.85_target0.95_mglm` | 3,368 | 3.17× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.30_cstat0.60_target0.90_mglm` | 7,361 | `p10_prev0.30_cstat0.60_target0.95_mglm` | 22,464 | 3.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.30_cstat0.75_target0.90_mglm` | 1,058 | `p10_prev0.30_cstat0.75_target0.95_mglm` | 4,419 | 4.18× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.30_cstat0.80_target0.90_mglm` | 966 | `p10_prev0.30_cstat0.80_target0.95_mglm` | 3,257 | 3.37× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.40_cstat0.70_target0.90_mglm` | 1,743 | `p10_prev0.40_cstat0.70_target0.95_mglm` | 5,303 | 3.04× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.40_cstat0.80_target0.90_mglm` | 942 | `p10_prev0.40_cstat0.80_target0.95_mglm` | 2,963 | 3.15× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.50_cstat0.60_target0.90_mglm` | 5,772 | `p10_prev0.50_cstat0.60_target0.95_mglm` | 24,975 | 4.33× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.50_cstat0.70_target0.90_mglm` | 1,776 | `p10_prev0.50_cstat0.70_target0.95_mglm` | 5,440 | 3.06× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.50_cstat0.75_target0.90_mglm` | 1,776 | `p10_prev0.50_cstat0.75_target0.95_mglm` | 6,752 | 3.80× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.05_cstat0.70_target0.90_mglm` | 6,428 | `p15_prev0.05_cstat0.70_target0.95_mglm` | 24,717 | 3.85× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.05_cstat0.75_target0.90_mglm` | 3,928 | `p15_prev0.05_cstat0.75_target0.95_mglm` | 16,000 | 4.07× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p15_prev0.07_cstat0.60_target0.80_mglm` | 10,195 | `p15_prev0.07_cstat0.60_target0.85_mglm` | 40,000 | 3.92× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.07_cstat0.65_target0.90_mglm` | 8,975 | `p15_prev0.07_cstat0.65_target0.95_mglm` | 36,924 | 4.11× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.07_cstat0.75_target0.90_mglm` | 5,334 | `p15_prev0.07_cstat0.75_target0.95_mglm` | 21,336 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.10_cstat0.85_target0.90_mglm` | 1,679 | `p15_prev0.10_cstat0.85_target0.95_mglm` | 6,174 | 3.68× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.15_cstat0.60_target0.90_mglm` | 16,138 | `p15_prev0.15_cstat0.60_target0.95_mglm` | 97,489 | 6.04× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.15_cstat0.65_target0.90_mglm` | 7,224 | `p15_prev0.15_cstat0.65_target0.95_mglm` | 27,548 | 3.81× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.20_cstat0.60_target0.90_mglm` | 14,316 | `p15_prev0.20_cstat0.60_target0.95_mglm` | 44,835 | 3.13× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.20_cstat0.65_target0.90_mglm` | 6,924 | `p15_prev0.20_cstat0.65_target0.95_mglm` | 27,696 | 4.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.20_cstat0.75_target0.90_mglm` | 1,976 | `p15_prev0.20_cstat0.75_target0.95_mglm` | 5,977 | 3.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.25_cstat0.65_target0.90_mglm` | 5,336 | `p15_prev0.25_cstat0.65_target0.95_mglm` | 18,115 | 3.39× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.25_cstat0.80_target0.90_mglm` | 1,334 | `p15_prev0.25_cstat0.80_target0.95_mglm` | 5,056 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.25_cstat0.85_target0.90_mglm` | 1,287 | `p15_prev0.25_cstat0.85_target0.95_mglm` | 4,141 | 3.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.30_cstat0.60_target0.90_mglm` | 8,896 | `p15_prev0.30_cstat0.60_target0.95_mglm` | 67,328 | 7.57× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.30_cstat0.70_target0.90_mglm` | 2,250 | `p15_prev0.30_cstat0.70_target0.95_mglm` | 7,248 | 3.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.40_cstat0.85_target0.90_mglm` | 834 | `p15_prev0.40_cstat0.85_target0.95_mglm` | 6,320 | 7.58× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.50_cstat0.65_target0.90_mglm` | 2,897 | `p15_prev0.50_cstat0.65_target0.95_mglm` | 10,505 | 3.63× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.50_cstat0.70_target0.90_mglm` | 2,227 | `p15_prev0.50_cstat0.70_target0.95_mglm` | 20,224 | 9.08× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.50_cstat0.75_target0.90_mglm` | 1,655 | `p15_prev0.50_cstat0.75_target0.95_mglm` | 5,056 | 3.05× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p20_prev0.05_cstat0.60_target0.85_mglm` | 20,000 | `p20_prev0.05_cstat0.60_target0.90_mglm` | 79,498 | 3.97× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.05_cstat0.75_target0.90_mglm` | 10,666 | `p20_prev0.05_cstat0.75_target0.95_mglm` | 32,253 | 3.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.05_cstat0.80_target0.90_mglm` | 4,918 | `p20_prev0.05_cstat0.80_target0.95_mglm` | 20,000 | 4.07× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.07_cstat0.60_target0.90_mglm` | 26,086 | `p20_prev0.07_cstat0.60_target0.95_mglm` | 80,842 | 3.10× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.07_cstat0.65_target0.90_mglm` | 12,308 | `p20_prev0.07_cstat0.65_target0.95_mglm` | 49,232 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.07_cstat0.80_target0.90_mglm` | 4,207 | `p20_prev0.07_cstat0.80_target0.95_mglm` | 13,336 | 3.17× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.07_cstat0.85_target0.90_mglm` | 3,308 | `p20_prev0.07_cstat0.85_target0.95_mglm` | 12,552 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.10_cstat0.85_target0.90_mglm` | 2,356 | `p20_prev0.10_cstat0.85_target0.95_mglm` | 9,412 | 3.99× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.15_cstat0.65_target0.90_mglm` | 6,154 | `p20_prev0.15_cstat0.65_target0.95_mglm` | 23,619 | 3.84× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.20_cstat0.60_target0.90_mglm` | 10,000 | `p20_prev0.20_cstat0.60_target0.95_mglm` | 40,000 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.25_cstat0.60_target0.90_mglm` | 15,084 | `p20_prev0.25_cstat0.60_target0.95_mglm` | 53,888 | 3.57× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.30_cstat0.60_target0.90_mglm` | 12,537 | `p20_prev0.30_cstat0.60_target0.95_mglm` | 89,856 | 7.17× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.30_cstat0.80_target0.90_mglm` | 1,755 | `p20_prev0.30_cstat0.80_target0.95_mglm` | 5,616 | 3.20× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.40_cstat0.70_target0.90_mglm` | 2,224 | `p20_prev0.40_cstat0.70_target0.95_mglm` | 7,650 | 3.44× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.40_cstat0.80_target0.90_mglm` | 2,224 | `p20_prev0.40_cstat0.80_target0.95_mglm` | 8,416 | 3.78× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.40_cstat0.85_target0.90_mglm` | 1,132 | `p20_prev0.40_cstat0.85_target0.95_mglm` | 4,208 | 3.72× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.50_cstat0.60_target0.90_mglm` | 7,104 | `p20_prev0.50_cstat0.60_target0.95_mglm` | 31,806 | 4.48× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.50_cstat0.70_target0.90_mglm` | 2,645 | `p20_prev0.50_cstat0.70_target0.95_mglm` | 13,472 | 5.09× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.50_cstat0.75_target0.90_mglm` | 1,689 | `p20_prev0.50_cstat0.75_target0.95_mglm` | 5,348 | 3.17× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_prev0.05_cstat0.60_target0.85_mglm` | 25,000 | `p25_prev0.05_cstat0.60_target0.90_mglm` | 100,000 | 4.00× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_prev0.05_cstat0.65_target0.85_mglm` | 11,281 | `p25_prev0.05_cstat0.65_target0.90_mglm` | 46,154 | 4.09× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.05_cstat0.80_target0.90_mglm` | 6,655 | `p25_prev0.05_cstat0.80_target0.95_mglm` | 25,000 | 3.76× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.07_cstat0.60_target0.90_mglm` | 33,334 | `p25_prev0.07_cstat0.60_target0.95_mglm` | 121,388 | 3.64× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p25_prev0.07_cstat0.65_target0.80_mglm` | 7,105 | `p25_prev0.07_cstat0.65_target0.85_mglm` | 61,540 | 8.66× higher | — | next | next |
| Target slope: 0.85 → 0.9 | `p25_prev0.07_cstat0.65_target0.85_mglm` | 61,540 | `p25_prev0.07_cstat0.65_target0.90_mglm` | 15,385 | 4.00× lower | — | previous | previous |
| Target slope: 0.9 → 0.95 | `p25_prev0.07_cstat0.85_target0.90_mglm` | 3,987 | `p25_prev0.07_cstat0.85_target0.95_mglm` | 15,684 | 3.93× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_prev0.10_cstat0.60_target0.85_mglm` | 12,500 | `p25_prev0.10_cstat0.60_target0.90_mglm` | 39,054 | 3.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.10_cstat0.70_target0.90_mglm` | 8,814 | `p25_prev0.10_cstat0.70_target0.95_mglm` | 42,856 | 4.86× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.10_cstat0.85_target0.90_mglm` | 2,941 | `p25_prev0.10_cstat0.85_target0.95_mglm` | 8,887 | 3.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.15_cstat0.75_target0.90_mglm` | 3,797 | `p25_prev0.15_cstat0.75_target0.95_mglm` | 13,139 | 3.46× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.20_cstat0.75_target0.90_mglm` | 3,297 | `p25_prev0.20_cstat0.75_target0.95_mglm` | 26,672 | 8.09× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p25_prev0.25_cstat0.60_target0.90_mglm` | 8,861 | `p25_prev0.25_cstat0.60_target0.95_mglm` | 67,392 | 7.61× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.25_cstat0.70_target0.90_mglm` | 4,293 | `p25_prev0.25_cstat0.70_target0.95_mglm` | 16,848 | 3.92× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.25_cstat0.75_target0.90_mglm` | 2,222 | `p25_prev0.25_cstat0.75_target0.95_mglm` | 7,438 | 3.35× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.30_cstat0.70_target0.90_mglm` | 3,946 | `p25_prev0.30_cstat0.70_target0.95_mglm` | 28,064 | 7.11× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p25_prev0.30_cstat0.80_target0.90_mglm` | 2,022 | `p25_prev0.30_cstat0.80_target0.95_mglm` | 7,016 | 3.47× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.30_cstat0.85_target0.90_mglm` | 1,815 | `p25_prev0.30_cstat0.85_target0.95_mglm` | 7,016 | 3.87× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_prev0.40_cstat0.65_target0.85_mglm` | 3,126 | `p25_prev0.40_cstat0.65_target0.90_mglm` | 11,104 | 3.55× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.50_cstat0.65_target0.90_mglm` | 5,261 | `p25_prev0.50_cstat0.65_target0.95_mglm` | 16,832 | 3.20× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.05_cstat0.60_target0.90_mglm` | 57,930 | `p30_prev0.05_cstat0.60_target0.95_mglm` | 175,813 | 3.03× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p30_prev0.05_cstat0.65_target0.85_mglm` | 16,875 | `p30_prev0.05_cstat0.65_target0.90_mglm` | 55,384 | 3.28× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.05_cstat0.75_target0.90_mglm` | 8,000 | `p30_prev0.05_cstat0.75_target0.95_mglm` | 38,749 | 4.84× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p30_prev0.07_cstat0.60_target0.85_mglm` | 19,136 | `p30_prev0.07_cstat0.60_target0.90_mglm` | 80,000 | 4.18× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.07_cstat0.60_target0.90_mglm` | 80,000 | `p30_prev0.07_cstat0.60_target0.95_mglm` | 640,000 | 8.00× higher | — | previous, next | next |
| Target slope: 0.9 → 0.95 | `p30_prev0.07_cstat0.70_target0.90_mglm` | 12,788 | `p30_prev0.07_cstat0.70_target0.95_mglm` | 68,572 | 5.36× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.07_cstat0.80_target0.90_mglm` | 6,062 | `p30_prev0.07_cstat0.80_target0.95_mglm` | 20,000 | 3.30× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.07_cstat0.85_target0.90_mglm` | 4,749 | `p30_prev0.07_cstat0.85_target0.95_mglm` | 18,824 | 3.96× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.10_cstat0.60_target0.90_mglm` | 45,925 | `p30_prev0.10_cstat0.60_target0.95_mglm` | 239,905 | 5.22× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p30_prev0.10_cstat0.65_target0.90_mglm` | 13,846 | `p30_prev0.10_cstat0.65_target0.95_mglm` | 52,164 | 3.77× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.10_cstat0.80_target0.90_mglm` | 4,122 | `p30_prev0.10_cstat0.80_target0.95_mglm` | 15,000 | 3.64× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.10_cstat0.85_target0.90_mglm` | 4,244 | `p30_prev0.10_cstat0.85_target0.95_mglm` | 14,116 | 3.33× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.15_cstat0.70_target0.90_mglm` | 6,820 | `p30_prev0.15_cstat0.70_target0.95_mglm` | 34,284 | 5.03× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.15_cstat0.80_target0.90_mglm` | 2,805 | `p30_prev0.15_cstat0.80_target0.95_mglm` | 10,000 | 3.57× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.15_cstat0.85_target0.90_mglm` | 2,745 | `p30_prev0.15_cstat0.85_target0.95_mglm` | 9,412 | 3.43× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.25_cstat0.75_target0.90_mglm` | 3,436 | `p30_prev0.25_cstat0.75_target0.95_mglm` | 20,208 | 5.88× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.25_cstat0.80_target0.90_mglm` | 2,719 | `p30_prev0.25_cstat0.80_target0.95_mglm` | 10,104 | 3.72× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.25_cstat0.85_target0.90_mglm` | 2,221 | `p30_prev0.25_cstat0.85_target0.95_mglm` | 20,208 | 9.10× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.30_cstat0.85_target0.90_mglm` | 1,984 | `p30_prev0.30_cstat0.85_target0.95_mglm` | 8,424 | 4.25× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.40_cstat0.60_target0.90_mglm` | 15,405 | `p30_prev0.40_cstat0.60_target0.95_mglm` | 46,356 | 3.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.40_cstat0.70_target0.90_mglm` | 3,283 | `p30_prev0.40_cstat0.70_target0.95_mglm` | 12,624 | 3.85× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.50_cstat0.60_target0.90_mglm` | 10,672 | `p30_prev0.50_cstat0.60_target0.95_mglm` | 40,448 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.50_cstat0.75_target0.90_mglm` | 2,668 | `p30_prev0.50_cstat0.75_target0.95_mglm` | 10,112 | 3.79× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p40_prev0.05_cstat0.70_target0.85_mglm` | 13,085 | `p40_prev0.05_cstat0.70_target0.90_mglm` | 68,572 | 5.24× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.05_cstat0.75_target0.90_mglm` | 13,646 | `p40_prev0.05_cstat0.75_target0.95_mglm` | 42,668 | 3.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.07_cstat0.60_target0.90_mglm` | 42,896 | `p40_prev0.07_cstat0.60_target0.95_mglm` | 426,672 | 9.95× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.07_cstat0.65_target0.90_mglm` | 12,307 | `p40_prev0.07_cstat0.65_target0.95_mglm` | 94,540 | 7.68× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p40_prev0.07_cstat0.70_target0.90_mglm` | 14,689 | `p40_prev0.07_cstat0.70_target0.95_mglm` | 47,451 | 3.23× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.10_cstat0.80_target0.90_mglm` | 5,000 | `p40_prev0.10_cstat0.80_target0.95_mglm` | 15,966 | 3.19× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.15_cstat0.70_target0.90_mglm` | 8,774 | `p40_prev0.15_cstat0.70_target0.95_mglm` | 45,716 | 5.21× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.15_cstat0.80_target0.90_mglm` | 3,337 | `p40_prev0.15_cstat0.80_target0.95_mglm` | 13,336 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.20_cstat0.65_target0.90_mglm` | 12,999 | `p40_prev0.20_cstat0.65_target0.95_mglm` | 39,751 | 3.06× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.30_cstat0.65_target0.90_mglm` | 8,845 | `p40_prev0.30_cstat0.65_target0.95_mglm` | 44,896 | 5.08× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.40_cstat0.60_target0.90_mglm` | 17,784 | `p40_prev0.40_cstat0.60_target0.95_mglm` | 70,098 | 3.94× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.40_cstat0.65_target0.90_mglm` | 8,888 | `p40_prev0.40_cstat0.65_target0.95_mglm` | 33,696 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.40_cstat0.75_target0.90_mglm` | 4,444 | `p40_prev0.40_cstat0.75_target0.95_mglm` | 16,848 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.50_cstat0.65_target0.90_mglm` | 8,658 | `p40_prev0.50_cstat0.65_target0.95_mglm` | 53,888 | 6.22× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p50_prev0.05_cstat0.60_target0.80_mglm` | 800,000 | `p50_prev0.05_cstat0.60_target0.85_mglm` | 99,225 | 8.06× lower | — | previous | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.05_cstat0.65_target0.90_mglm` | 45,166 | `p50_prev0.05_cstat0.65_target0.95_mglm` | 184,616 | 4.09× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.05_cstat0.80_target0.90_mglm` | 12,500 | `p50_prev0.05_cstat0.80_target0.95_mglm` | 100,000 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.07_cstat0.70_target0.90_mglm` | 14,285 | `p50_prev0.07_cstat0.70_target0.95_mglm` | 54,206 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.07_cstat0.80_target0.90_mglm` | 8,335 | `p50_prev0.07_cstat0.80_target0.95_mglm` | 33,336 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.15_cstat0.85_target0.90_mglm` | 4,500 | `p50_prev0.15_cstat0.85_target0.95_mglm` | 15,684 | 3.49× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.20_cstat0.60_target0.90_mglm` | 27,504 | `p50_prev0.20_cstat0.60_target0.95_mglm` | 142,619 | 5.19× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.20_cstat0.65_target0.90_mglm` | 11,538 | `p50_prev0.20_cstat0.65_target0.95_mglm` | 35,689 | 3.09× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.20_cstat0.70_target0.90_mglm` | 8,921 | `p50_prev0.20_cstat0.70_target0.95_mglm` | 42,856 | 4.80× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.25_cstat0.60_target0.90_mglm` | 17,776 | `p50_prev0.25_cstat0.60_target0.95_mglm` | 105,390 | 5.93× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.25_cstat0.75_target0.90_mglm` | 5,417 | `p50_prev0.25_cstat0.75_target0.95_mglm` | 16,840 | 3.11× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.30_cstat0.60_target0.90_mglm` | 29,632 | `p50_prev0.30_cstat0.60_target0.95_mglm` | 99,319 | 3.35× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.30_cstat0.75_target0.90_mglm` | 3,719 | `p50_prev0.30_cstat0.75_target0.95_mglm` | 11,818 | 3.18× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.30_cstat0.80_target0.90_mglm` | 3,536 | `p50_prev0.30_cstat0.80_target0.95_mglm` | 28,080 | 7.94× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p50_prev0.40_cstat0.60_target0.90_mglm` | 20,483 | `p50_prev0.40_cstat0.60_target0.95_mglm` | 168,448 | 8.22× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.40_cstat0.65_target0.90_mglm` | 10,406 | `p50_prev0.40_cstat0.65_target0.95_mglm` | 84,224 | 8.09× higher | — | next | — |
| Target slope: 0.85 → 0.95 | `p50_prev0.50_cstat0.65_target0.85_mglm` | 5,955 | `p50_prev0.50_cstat0.65_target0.95_mglm` | 27,560 | 4.63× higher | Yes | — | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.05_cstat0.60_target0.85_mglm` | 7,850 | `p5_prev0.05_cstat0.60_target0.90_mglm` | 40,000 | 5.10× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.05_cstat0.65_target0.90_mglm` | 9,632 | `p5_prev0.05_cstat0.65_target0.95_mglm` | 36,057 | 3.74× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.05_cstat0.85_target0.90_mglm` | 1,959 | `p5_prev0.05_cstat0.85_target0.95_mglm` | 6,023 | 3.07× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.07_cstat0.60_target0.85_mglm` | 6,666 | `p5_prev0.07_cstat0.60_target0.90_mglm` | 26,664 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.07_cstat0.65_target0.90_mglm` | 6,167 | `p5_prev0.07_cstat0.65_target0.95_mglm` | 36,059 | 5.85× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.07_cstat0.70_target0.90_mglm` | 2,660 | `p5_prev0.07_cstat0.70_target0.95_mglm` | 11,428 | 4.30× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.07_cstat0.75_target0.90_mglm` | 1,758 | `p5_prev0.07_cstat0.75_target0.95_mglm` | 7,362 | 4.19× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.07_cstat0.85_target0.90_mglm` | 1,570 | `p5_prev0.07_cstat0.85_target0.95_mglm` | 12,560 | 8.00× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.10_cstat0.65_target0.85_mglm` | 2,308 | `p5_prev0.10_cstat0.65_target0.90_mglm` | 9,232 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.10_cstat0.80_target0.90_mglm` | 1,250 | `p5_prev0.10_cstat0.80_target0.95_mglm` | 3,829 | 3.06× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.15_cstat0.60_target0.90_mglm` | 6,963 | `p5_prev0.15_cstat0.60_target0.95_mglm` | 53,344 | 7.66× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.15_cstat0.70_target0.90_mglm` | 1,429 | `p5_prev0.15_cstat0.70_target0.95_mglm` | 6,220 | 4.35× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.15_cstat0.80_target0.85_mglm` | 532 | `p5_prev0.15_cstat0.80_target0.90_mglm` | 1,664 | 3.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.15_cstat0.85_target0.90_mglm` | 784 | `p5_prev0.15_cstat0.85_target0.95_mglm` | 3,139 | 4.00× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p5_prev0.20_cstat0.60_target0.80_mglm` | 1,720 | `p5_prev0.20_cstat0.60_target0.85_mglm` | 10,000 | 5.81× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.20_cstat0.65_target0.90_mglm` | 2,230 | `p5_prev0.20_cstat0.65_target0.95_mglm` | 18,464 | 8.28× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.20_cstat0.75_target0.85_mglm` | 333 | `p5_prev0.20_cstat0.75_target0.90_mglm` | 2,664 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.20_cstat0.75_target0.90_mglm` | 2,664 | `p5_prev0.20_cstat0.75_target0.95_mglm` | 10,656 | 4.00× higher | — | previous, next | next |
| Target slope: 0.9 → 0.95 | `p5_prev0.25_cstat0.70_target0.90_mglm` | 1,190 | `p5_prev0.25_cstat0.70_target0.95_mglm` | 6,752 | 5.67× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p5_prev0.25_cstat0.80_target0.90_mglm` | 742 | `p5_prev0.25_cstat0.80_target0.95_mglm` | 3,376 | 4.55× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.30_cstat0.65_target0.90_mglm` | 1,488 | `p5_prev0.30_cstat0.65_target0.95_mglm` | 5,632 | 3.78× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.30_cstat0.75_target0.90_mglm` | 697 | `p5_prev0.30_cstat0.75_target0.95_mglm` | 2,818 | 4.04× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.30_cstat0.80_target0.90_mglm` | 744 | `p5_prev0.30_cstat0.80_target0.95_mglm` | 2,816 | 3.78× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.30_cstat0.85_target0.90_mglm` | 558 | `p5_prev0.30_cstat0.85_target0.95_mglm` | 2,816 | 5.05× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.40_cstat0.60_target0.85_mglm` | 2,155 | `p5_prev0.40_cstat0.60_target0.90_mglm` | 17,792 | 8.26× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.40_cstat0.65_target0.90_mglm` | 1,798 | `p5_prev0.40_cstat0.65_target0.95_mglm` | 8,448 | 4.70× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.40_cstat0.70_target0.90_mglm` | 1,065 | `p5_prev0.40_cstat0.70_target0.95_mglm` | 3,712 | 3.49× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.40_cstat0.75_target0.90_mglm` | 751 | `p5_prev0.40_cstat0.75_target0.95_mglm` | 4,224 | 5.62× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.40_cstat0.85_target0.90_mglm` | 556 | `p5_prev0.40_cstat0.85_target0.95_mglm` | 1,722 | 3.10× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p5_prev0.50_cstat0.60_target0.80_mglm` | 1,152 | `p5_prev0.50_cstat0.60_target0.85_mglm` | 3,776 | 3.28× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.50_cstat0.65_target0.90_mglm` | 1,574 | `p5_prev0.50_cstat0.65_target0.95_mglm` | 8,486 | 5.39× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.50_cstat0.80_target0.90_mglm` | 367 | `p5_prev0.50_cstat0.80_target0.95_mglm` | 1,887 | 5.14× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.50_cstat0.85_target0.90_mglm` | 444 | `p5_prev0.50_cstat0.85_target0.95_mglm` | 1,680 | 3.78× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.05_cstat0.80_target0.90_mglm` | 19,095 | `p75_prev0.05_cstat0.80_target0.95_mglm` | 75,000 | 3.93× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.07_cstat0.70_target0.90_mglm` | 21,429 | `p75_prev0.07_cstat0.70_target0.95_mglm` | 69,863 | 3.26× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.07_cstat0.75_target0.90_mglm` | 26,666 | `p75_prev0.07_cstat0.75_target0.95_mglm` | 213,328 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.07_cstat0.80_target0.90_mglm` | 11,665 | `p75_prev0.07_cstat0.80_target0.95_mglm` | 37,862 | 3.25× higher | — | — | — |
| Target slope: 0.85 → 0.95 | `p75_prev0.10_cstat0.60_target0.85_mglm` | 54,341 | `p75_prev0.10_cstat0.60_target0.95_mglm` | 276,490 | 5.09× higher | Yes | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.10_cstat0.65_target0.90_mglm` | 38,842 | `p75_prev0.10_cstat0.65_target0.95_mglm` | 138,460 | 3.56× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p75_prev0.10_cstat0.80_target0.90_mglm` | 10,491 | `p75_prev0.10_cstat0.80_target0.95_mglm` | 37,500 | 3.57× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.20_cstat0.70_target0.90_mglm` | 8,035 | `p75_prev0.20_cstat0.70_target0.95_mglm` | 34,703 | 4.32× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.25_cstat0.80_target0.90_mglm` | 5,790 | `p75_prev0.25_cstat0.80_target0.95_mglm` | 50,528 | 8.73× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.30_cstat0.60_target0.90_mglm` | 39,032 | `p75_prev0.30_cstat0.60_target0.95_mglm` | 168,448 | 4.32× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.30_cstat0.80_target0.90_mglm` | 5,556 | `p75_prev0.30_cstat0.80_target0.95_mglm` | 21,056 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.40_cstat0.70_target0.90_mglm` | 8,685 | `p75_prev0.40_cstat0.70_target0.95_mglm` | 31,584 | 3.64× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.50_cstat0.60_target0.90_mglm` | 25,472 | `p75_prev0.50_cstat0.60_target0.95_mglm` | 113,610 | 4.46× higher | — | — | — |
| C-statistic: 0.6 → 0.7 | `p100_prev0.05_cstat0.60_target0.90_mlasso` | 99,214 | `p100_prev0.05_cstat0.70_target0.90_mlasso` | 21,428 | 4.63× lower | Yes | — | — |
| C-statistic: 0.65 → 0.7 | `p100_prev0.05_cstat0.65_target0.95_mlasso` | 184,616 | `p100_prev0.05_cstat0.70_target0.95_mlasso` | 42,857 | 4.31× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p100_prev0.07_cstat0.60_target0.95_mlasso` | 533,336 | `p100_prev0.07_cstat0.65_target0.95_mlasso` | 123,076 | 4.33× lower | — | previous, next | previous |
| C-statistic: 0.65 → 0.7 | `p100_prev0.07_cstat0.65_target0.95_mlasso` | 123,076 | `p100_prev0.07_cstat0.70_target0.95_mlasso` | 28,180 | 4.37× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p100_prev0.07_cstat0.70_target0.80_mlasso` | 7,142 | `p100_prev0.07_cstat0.75_target0.80_mlasso` | 2,215 | 3.22× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p100_prev0.07_cstat0.75_target0.95_mlasso` | 71,108 | `p100_prev0.07_cstat0.80_target0.95_mlasso` | 14,147 | 5.03× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p100_prev0.10_cstat0.70_target0.95_mlasso` | 85,714 | `p100_prev0.10_cstat0.75_target0.95_mlasso` | 11,570 | 7.41× lower | — | previous | — |
| C-statistic: 0.8 → 0.85 | `p100_prev0.10_cstat0.80_target0.95_mlasso` | 12,500 | `p100_prev0.10_cstat0.85_target0.95_mlasso` | 47,060 | 3.76× higher | — | next | — |
| C-statistic: 0.6 → 0.65 | `p100_prev0.15_cstat0.60_target0.80_mlasso` | 16,666 | `p100_prev0.15_cstat0.65_target0.80_mlasso` | 3,846 | 4.33× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p100_prev0.15_cstat0.60_target0.90_mlasso` | 34,926 | `p100_prev0.15_cstat0.65_target0.90_mlasso` | 7,489 | 4.66× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p100_prev0.15_cstat0.75_target0.95_mlasso` | 14,568 | `p100_prev0.15_cstat0.80_target0.95_mlasso` | 4,174 | 3.49× lower | — | next | next |
| C-statistic: 0.8 → 0.85 | `p100_prev0.15_cstat0.80_target0.95_mlasso` | 4,174 | `p100_prev0.15_cstat0.85_target0.95_mlasso` | 15,688 | 3.76× higher | — | previous, next | previous |
| C-statistic: 0.6 → 0.65 | `p100_prev0.20_cstat0.60_target0.80_mlasso` | 12,500 | `p100_prev0.20_cstat0.65_target0.80_mlasso` | 3,388 | 3.69× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p100_prev0.20_cstat0.60_target0.90_mlasso` | 50,000 | `p100_prev0.20_cstat0.65_target0.90_mlasso` | 5,724 | 8.74× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p100_prev0.20_cstat0.70_target0.85_mlasso` | 5,357 | `p100_prev0.20_cstat0.75_target0.85_mlasso` | 1,710 | 3.13× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p100_prev0.25_cstat0.60_target0.95_mlasso` | 176,420 | `p100_prev0.25_cstat0.65_target0.95_mlasso` | 21,418 | 8.24× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p100_prev0.30_cstat0.65_target0.80_mlasso` | 3,488 | `p100_prev0.30_cstat0.70_target0.80_mlasso` | 1,041 | 3.35× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p100_prev0.30_cstat0.65_target0.95_mlasso` | 56,128 | `p100_prev0.30_cstat0.70_target0.95_mlasso` | 6,018 | 9.33× lower | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p100_prev0.40_cstat0.60_target0.85_mlasso` | 11,764 | `p100_prev0.40_cstat0.65_target0.85_mlasso` | 3,818 | 3.08× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p100_prev0.40_cstat0.60_target0.95_mlasso` | 84,224 | `p100_prev0.40_cstat0.65_target0.95_mlasso` | 15,819 | 5.32× lower | — | previous | — |
| C-statistic: 0.8 → 0.85 | `p100_prev0.40_cstat0.80_target0.95_mlasso` | 2,632 | `p100_prev0.40_cstat0.85_target0.95_mlasso` | 10,528 | 4.00× higher | — | next | next |
| C-statistic: 0.6 → 0.65 | `p100_prev0.50_cstat0.60_target0.95_mlasso` | 67,360 | `p100_prev0.50_cstat0.65_target0.95_mlasso` | 17,312 | 3.89× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p10_prev0.05_cstat0.60_target0.80_mlasso` | 20,000 | `p10_prev0.05_cstat0.65_target0.80_mlasso` | 2,434 | 8.22× lower | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p10_prev0.05_cstat0.60_target0.95_mlasso` | 640,000 | `p10_prev0.05_cstat0.65_target0.95_mlasso` | 147,696 | 4.33× lower | — | previous, next | — |
| C-statistic: 0.65 → 0.7 | `p10_prev0.05_cstat0.65_target0.95_mlasso` | 147,696 | `p10_prev0.05_cstat0.70_target0.95_mlasso` | 31,977 | 4.62× lower | — | previous | — |
| C-statistic: 0.7 → 0.8 | `p10_prev0.05_cstat0.70_target0.90_mlasso` | 8,571 | `p10_prev0.05_cstat0.80_target0.90_mlasso` | 2,435 | 3.52× lower | Yes | — | — |
| C-statistic: 0.65 → 0.7 | `p10_prev0.07_cstat0.65_target0.95_mlasso` | 36,780 | `p10_prev0.07_cstat0.70_target0.95_mlasso` | 182,848 | 4.97× higher | — | next | next |
| C-statistic: 0.6 → 0.65 | `p10_prev0.10_cstat0.60_target0.90_mlasso` | 22,029 | `p10_prev0.10_cstat0.65_target0.90_mlasso` | 5,791 | 3.80× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p10_prev0.10_cstat0.70_target0.95_mlasso` | 34,289 | `p10_prev0.10_cstat0.75_target0.95_mlasso` | 10,664 | 3.22× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p10_prev0.20_cstat0.60_target0.90_mlasso` | 11,063 | `p10_prev0.20_cstat0.65_target0.90_mlasso` | 3,569 | 3.10× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p10_prev0.20_cstat0.65_target0.95_mlasso` | 73,856 | `p10_prev0.20_cstat0.70_target0.95_mlasso` | 8,818 | 8.38× lower | — | previous, next | — |
| C-statistic: 0.6 → 0.65 | `p10_prev0.25_cstat0.60_target0.95_mlasso` | 26,944 | `p10_prev0.25_cstat0.65_target0.95_mlasso` | 6,736 | 4.00× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p10_prev0.30_cstat0.60_target0.90_mlasso` | 6,917 | `p10_prev0.30_cstat0.65_target0.90_mlasso` | 1,396 | 4.95× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p10_prev0.30_cstat0.65_target0.95_mlasso` | 27,514 | `p10_prev0.30_cstat0.70_target0.95_mlasso` | 5,616 | 4.90× lower | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p10_prev0.40_cstat0.60_target0.80_mlasso` | 2,496 | `p10_prev0.40_cstat0.65_target0.80_mlasso` | 636 | 3.92× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p10_prev0.40_cstat0.60_target0.95_mlasso` | 33,664 | `p10_prev0.40_cstat0.65_target0.95_mlasso` | 8,416 | 4.00× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p10_prev0.40_cstat0.70_target0.95_mlasso` | 16,832 | `p10_prev0.40_cstat0.75_target0.95_mlasso` | 2,684 | 6.27× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p10_prev0.50_cstat0.60_target0.80_mlasso` | 2,000 | `p10_prev0.50_cstat0.65_target0.80_mlasso` | 645 | 3.10× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p15_prev0.05_cstat0.65_target0.90_mlasso` | 19,977 | `p15_prev0.05_cstat0.70_target0.90_mlasso` | 6,605 | 3.02× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.07_cstat0.60_target0.95_mlasso` | 160,000 | `p15_prev0.07_cstat0.65_target0.95_mlasso` | 36,918 | 4.33× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p15_prev0.07_cstat0.65_target0.80_mlasso` | 4,615 | `p15_prev0.07_cstat0.70_target0.80_mlasso` | 1,434 | 3.22× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p15_prev0.07_cstat0.70_target0.95_mlasso` | 76,273 | `p15_prev0.07_cstat0.75_target0.95_mlasso` | 15,330 | 4.98× lower | — | previous, next | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.15_cstat0.60_target0.90_mlasso` | 20,000 | `p15_prev0.15_cstat0.65_target0.90_mlasso` | 4,965 | 4.03× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p15_prev0.15_cstat0.70_target0.95_mlasso` | 34,288 | `p15_prev0.15_cstat0.75_target0.95_mlasso` | 5,100 | 6.72× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.20_cstat0.60_target0.85_mlasso` | 7,500 | `p15_prev0.20_cstat0.65_target0.85_mlasso` | 1,703 | 4.40× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.20_cstat0.60_target0.90_mlasso` | 15,000 | `p15_prev0.20_cstat0.65_target0.90_mlasso` | 4,142 | 3.62× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.25_cstat0.60_target0.85_mlasso` | 5,648 | `p15_prev0.25_cstat0.65_target0.85_mlasso` | 1,629 | 3.47× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.25_cstat0.60_target0.95_mlasso` | 40,448 | `p15_prev0.25_cstat0.65_target0.95_mlasso` | 12,495 | 3.24× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.40_cstat0.60_target0.80_mlasso` | 1,836 | `p15_prev0.40_cstat0.65_target0.80_mlasso` | 473 | 3.88× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p15_prev0.40_cstat0.65_target0.85_mlasso` | 1,764 | `p15_prev0.40_cstat0.70_target0.85_mlasso` | 441 | 4.00× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.50_cstat0.60_target0.85_mlasso` | 2,802 | `p15_prev0.50_cstat0.65_target0.85_mlasso` | 727 | 3.85× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p15_prev0.50_cstat0.65_target0.90_mlasso` | 5,328 | `p15_prev0.50_cstat0.70_target0.90_mlasso` | 1,488 | 3.58× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.05_cstat0.60_target0.80_mlasso` | 20,000 | `p20_prev0.05_cstat0.65_target0.80_mlasso` | 4,794 | 4.17× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.05_cstat0.60_target0.95_mlasso` | 160,000 | `p20_prev0.05_cstat0.65_target0.95_mlasso` | 36,924 | 4.33× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p20_prev0.05_cstat0.70_target0.90_mlasso` | 17,143 | `p20_prev0.05_cstat0.75_target0.90_mlasso` | 5,333 | 3.21× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.07_cstat0.60_target0.85_mlasso` | 14,596 | `p20_prev0.07_cstat0.65_target0.85_mlasso` | 4,454 | 3.28× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.07_cstat0.60_target0.90_mlasso` | 36,296 | `p20_prev0.07_cstat0.65_target0.90_mlasso` | 6,154 | 5.90× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p20_prev0.07_cstat0.70_target0.95_mlasso` | 45,716 | `p20_prev0.07_cstat0.75_target0.95_mlasso` | 14,224 | 3.21× lower | — | previous | — |
| C-statistic: 0.8 → 0.85 | `p20_prev0.07_cstat0.80_target0.95_mlasso` | 26,672 | `p20_prev0.07_cstat0.85_target0.95_mlasso` | 6,276 | 4.25× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.20_cstat0.60_target0.85_mlasso` | 10,000 | `p20_prev0.20_cstat0.65_target0.85_mlasso` | 2,307 | 4.33× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.20_cstat0.60_target0.95_mlasso` | 80,000 | `p20_prev0.20_cstat0.65_target0.95_mlasso` | 9,230 | 8.67× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.25_cstat0.60_target0.80_mlasso` | 4,000 | `p20_prev0.25_cstat0.65_target0.80_mlasso` | 992 | 4.03× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.25_cstat0.60_target0.90_mlasso` | 14,224 | `p20_prev0.25_cstat0.65_target0.90_mlasso` | 3,645 | 3.90× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.30_cstat0.60_target0.85_mlasso` | 6,280 | `p20_prev0.30_cstat0.65_target0.85_mlasso` | 1,514 | 4.15× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.30_cstat0.60_target0.95_mlasso` | 89,856 | `p20_prev0.30_cstat0.65_target0.95_mlasso` | 11,232 | 8.00× lower | — | previous | — |
| C-statistic: 0.8 → 0.85 | `p20_prev0.30_cstat0.80_target0.95_mlasso` | 11,232 | `p20_prev0.30_cstat0.85_target0.95_mlasso` | 2,669 | 4.21× lower | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p20_prev0.40_cstat0.60_target0.95_mlasso` | 50,418 | `p20_prev0.40_cstat0.65_target0.95_mlasso` | 10,568 | 4.77× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.50_cstat0.60_target0.95_mlasso` | 39,967 | `p20_prev0.50_cstat0.65_target0.95_mlasso` | 12,057 | 3.31× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p20_prev0.50_cstat0.70_target0.95_mlasso` | 13,472 | `p20_prev0.50_cstat0.75_target0.95_mlasso` | 2,596 | 5.19× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.05_cstat0.60_target0.85_mlasso` | 26,883 | `p25_prev0.05_cstat0.65_target0.85_mlasso` | 5,769 | 4.66× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.05_cstat0.60_target0.95_mlasso` | 159,197 | `p25_prev0.05_cstat0.65_target0.95_mlasso` | 35,294 | 4.51× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p25_prev0.05_cstat0.65_target0.90_mlasso` | 23,137 | `p25_prev0.05_cstat0.70_target0.90_mlasso` | 5,357 | 4.32× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p25_prev0.05_cstat0.70_target0.95_mlasso` | 85,716 | `p25_prev0.05_cstat0.75_target0.95_mlasso` | 20,670 | 4.15× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.07_cstat0.60_target0.90_mlasso` | 38,140 | `p25_prev0.07_cstat0.65_target0.90_mlasso` | 12,016 | 3.17× lower | — | — | — |
| C-statistic: 0.8 → 0.85 | `p25_prev0.07_cstat0.80_target0.80_mlasso` | 861 | `p25_prev0.07_cstat0.85_target0.80_mlasso` | 245 | 3.51× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.10_cstat0.60_target0.90_mlasso` | 25,000 | `p25_prev0.10_cstat0.65_target0.90_mlasso` | 5,769 | 4.33× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.15_cstat0.60_target0.95_mlasso` | 133,328 | `p25_prev0.15_cstat0.65_target0.95_mlasso` | 15,136 | 8.81× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.20_cstat0.60_target0.80_mlasso` | 3,459 | `p25_prev0.20_cstat0.65_target0.80_mlasso` | 1,085 | 3.19× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.20_cstat0.60_target0.95_mlasso` | 37,846 | `p25_prev0.20_cstat0.65_target0.95_mlasso` | 11,538 | 3.28× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.25_cstat0.60_target0.95_mlasso` | 67,392 | `p25_prev0.25_cstat0.65_target0.95_mlasso` | 16,228 | 4.15× lower | — | previous | — |
| C-statistic: 0.75 → 0.8 | `p25_prev0.25_cstat0.75_target0.95_mlasso` | 16,848 | `p25_prev0.25_cstat0.80_target0.95_mlasso` | 4,488 | 3.75× lower | — | previous | previous |
| C-statistic: 0.65 → 0.7 | `p25_prev0.30_cstat0.65_target0.95_mlasso` | 28,064 | `p25_prev0.30_cstat0.70_target0.95_mlasso` | 7,100 | 3.95× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.40_cstat0.60_target0.85_mlasso` | 5,880 | `p25_prev0.40_cstat0.65_target0.85_mlasso` | 1,470 | 4.00× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p30_prev0.05_cstat0.65_target0.90_mlasso` | 17,109 | `p30_prev0.05_cstat0.70_target0.90_mlasso` | 3,214 | 5.32× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p30_prev0.05_cstat0.65_target0.95_mlasso` | 443,072 | `p30_prev0.05_cstat0.70_target0.95_mlasso` | 50,774 | 8.73× lower | — | previous, next | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.07_cstat0.60_target0.90_mlasso` | 80,000 | `p30_prev0.07_cstat0.65_target0.90_mlasso` | 18,462 | 4.33× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p30_prev0.07_cstat0.65_target0.90_mlasso` | 18,462 | `p30_prev0.07_cstat0.70_target0.90_mlasso` | 6,146 | 3.00× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.10_cstat0.60_target0.80_mlasso` | 7,500 | `p30_prev0.10_cstat0.65_target0.80_mlasso` | 2,355 | 3.18× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.10_cstat0.60_target0.85_mlasso` | 10,177 | `p30_prev0.10_cstat0.65_target0.85_mlasso` | 3,071 | 3.31× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.10_cstat0.60_target0.90_mlasso` | 24,579 | `p30_prev0.10_cstat0.65_target0.90_mlasso` | 5,348 | 4.60× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p30_prev0.10_cstat0.65_target0.95_mlasso` | 74,299 | `p30_prev0.10_cstat0.70_target0.95_mlasso` | 19,986 | 3.72× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.15_cstat0.60_target0.90_mlasso` | 20,000 | `p30_prev0.15_cstat0.65_target0.90_mlasso` | 4,631 | 4.32× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.25_cstat0.60_target0.95_mlasso` | 241,151 | `p30_prev0.25_cstat0.65_target0.95_mlasso` | 40,416 | 5.97× lower | — | previous, next | previous |
| C-statistic: 0.65 → 0.7 | `p30_prev0.25_cstat0.65_target0.95_mlasso` | 40,416 | `p30_prev0.25_cstat0.70_target0.95_mlasso` | 9,585 | 4.22× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.30_cstat0.60_target0.95_mlasso` | 67,392 | `p30_prev0.30_cstat0.65_target0.95_mlasso` | 13,905 | 4.85× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.40_cstat0.60_target0.95_mlasso` | 50,496 | `p30_prev0.40_cstat0.65_target0.95_mlasso` | 14,463 | 3.49× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p30_prev0.50_cstat0.65_target0.80_mlasso` | 1,090 | `p30_prev0.50_cstat0.70_target0.80_mlasso` | 187 | 5.83× lower | — | previous, next | next |
| C-statistic: 0.6 → 0.65 | `p40_prev0.05_cstat0.60_target0.80_mlasso` | 20,000 | `p40_prev0.05_cstat0.65_target0.80_mlasso` | 5,111 | 3.91× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p40_prev0.05_cstat0.65_target0.90_mlasso` | 73,846 | `p40_prev0.05_cstat0.70_target0.90_mlasso` | 10,205 | 7.24× lower | — | previous | previous |
| C-statistic: 0.75 → 0.8 | `p40_prev0.05_cstat0.75_target0.95_mlasso` | 85,336 | `p40_prev0.05_cstat0.80_target0.95_mlasso` | 18,713 | 4.56× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.07_cstat0.60_target0.90_mlasso` | 40,679 | `p40_prev0.07_cstat0.65_target0.90_mlasso` | 13,452 | 3.02× lower | — | next | next |
| C-statistic: 0.65 → 0.7 | `p40_prev0.07_cstat0.65_target0.90_mlasso` | 13,452 | `p40_prev0.07_cstat0.70_target0.90_mlasso` | 45,714 | 3.40× higher | — | previous, next | previous, next |
| C-statistic: 0.7 → 0.75 | `p40_prev0.07_cstat0.70_target0.95_mlasso` | 91,428 | `p40_prev0.07_cstat0.75_target0.95_mlasso` | 24,671 | 3.71× lower | — | previous, next | previous |
| C-statistic: 0.8 → 0.85 | `p40_prev0.07_cstat0.80_target0.80_mlasso` | 1,666 | `p40_prev0.07_cstat0.85_target0.80_mlasso` | 392 | 4.25× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.10_cstat0.60_target0.85_mlasso` | 20,000 | `p40_prev0.10_cstat0.65_target0.85_mlasso` | 4,615 | 4.33× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.10_cstat0.60_target0.90_mlasso` | 40,000 | `p40_prev0.10_cstat0.65_target0.90_mlasso` | 9,231 | 4.33× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.10_cstat0.60_target0.95_mlasso` | 160,000 | `p40_prev0.10_cstat0.65_target0.95_mlasso` | 27,693 | 5.78× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p40_prev0.10_cstat0.70_target0.95_mlasso` | 68,572 | `p40_prev0.10_cstat0.75_target0.95_mlasso` | 15,039 | 4.56× lower | — | previous, next | previous |
| C-statistic: 0.8 → 0.85 | `p40_prev0.10_cstat0.80_target0.95_mlasso` | 20,000 | `p40_prev0.10_cstat0.85_target0.95_mlasso` | 4,706 | 4.25× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.15_cstat0.60_target0.90_mlasso` | 19,463 | `p40_prev0.15_cstat0.65_target0.90_mlasso` | 6,154 | 3.16× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.15_cstat0.60_target0.95_mlasso` | 426,656 | `p40_prev0.15_cstat0.65_target0.95_mlasso` | 49,232 | 8.67× lower | — | previous, next | — |
| C-statistic: 0.65 → 0.7 | `p40_prev0.15_cstat0.65_target0.95_mlasso` | 49,232 | `p40_prev0.15_cstat0.70_target0.95_mlasso` | 11,429 | 4.31× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.20_cstat0.60_target0.80_mlasso` | 5,001 | `p40_prev0.20_cstat0.65_target0.80_mlasso` | 1,521 | 3.29× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.25_cstat0.60_target0.90_mlasso` | 12,270 | `p40_prev0.25_cstat0.65_target0.90_mlasso` | 3,287 | 3.73× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.30_cstat0.60_target0.90_mlasso` | 11,848 | `p40_prev0.30_cstat0.65_target0.90_mlasso` | 3,735 | 3.17× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p40_prev0.30_cstat0.65_target0.95_mlasso` | 44,896 | `p40_prev0.30_cstat0.70_target0.95_mlasso` | 11,241 | 3.99× lower | — | previous | previous |
| C-statistic: 0.75 → 0.8 | `p40_prev0.50_cstat0.75_target0.90_mlasso` | 3,556 | `p40_prev0.50_cstat0.80_target0.90_mlasso` | 967 | 3.68× lower | — | previous | previous |
| C-statistic: 0.65 → 0.7 | `p50_prev0.07_cstat0.65_target0.80_mlasso` | 7,692 | `p50_prev0.07_cstat0.70_target0.80_mlasso` | 2,521 | 3.05× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p50_prev0.10_cstat0.70_target0.80_mlasso` | 2,415 | `p50_prev0.10_cstat0.75_target0.80_mlasso` | 762 | 3.17× lower | — | — | — |
| C-statistic: 0.8 → 0.85 | `p50_prev0.10_cstat0.80_target0.95_mlasso` | 12,864 | `p50_prev0.10_cstat0.85_target0.95_mlasso` | 4,282 | 3.00× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p50_prev0.20_cstat0.60_target0.85_mlasso` | 12,500 | `p50_prev0.20_cstat0.65_target0.85_mlasso` | 3,701 | 3.38× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p50_prev0.20_cstat0.65_target0.95_mlasso` | 46,152 | `p50_prev0.20_cstat0.70_target0.95_mlasso` | 11,284 | 4.09× lower | — | previous | — |
| C-statistic: 0.8 → 0.85 | `p50_prev0.20_cstat0.80_target0.90_mlasso` | 781 | `p50_prev0.20_cstat0.85_target0.90_mlasso` | 2,941 | 3.77× higher | — | — | — |
| C-statistic: 0.8 → 0.85 | `p50_prev0.20_cstat0.80_target0.95_mlasso` | 7,571 | `p50_prev0.20_cstat0.85_target0.95_mlasso` | 2,292 | 3.30× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p50_prev0.30_cstat0.60_target0.85_mlasso` | 7,844 | `p50_prev0.30_cstat0.65_target0.85_mlasso` | 981 | 8.00× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p50_prev0.40_cstat0.70_target0.80_mlasso` | 961 | `p50_prev0.40_cstat0.75_target0.80_mlasso` | 195 | 4.93× lower | — | next | next |
| C-statistic: 0.6 → 0.65 | `p50_prev0.50_cstat0.60_target0.80_mlasso` | 5,000 | `p50_prev0.50_cstat0.65_target0.80_mlasso` | 1,354 | 3.69× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p50_prev0.50_cstat0.60_target0.95_mlasso` | 52,574 | `p50_prev0.50_cstat0.65_target0.95_mlasso` | 16,538 | 3.18× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.05_cstat0.60_target0.95_mlasso` | 320,000 | `p5_prev0.05_cstat0.65_target0.95_mlasso` | 73,840 | 4.33× lower | — | previous, next | — |
| C-statistic: 0.65 → 0.7 | `p5_prev0.05_cstat0.65_target0.85_mlasso` | 9,230 | `p5_prev0.05_cstat0.70_target0.85_mlasso` | 2,236 | 4.13× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p5_prev0.05_cstat0.70_target0.95_mlasso` | 34,288 | `p5_prev0.05_cstat0.75_target0.95_mlasso` | 341,248 | 9.95× higher | — | previous, next | — |
| C-statistic: 0.8 → 0.85 | `p5_prev0.05_cstat0.80_target0.95_mlasso` | 5,000 | `p5_prev0.05_cstat0.85_target0.95_mlasso` | 18,816 | 3.76× higher | — | next | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.07_cstat0.60_target0.85_mlasso` | 13,332 | `p5_prev0.07_cstat0.65_target0.85_mlasso` | 3,157 | 4.22× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.07_cstat0.60_target0.95_mlasso` | 106,656 | `p5_prev0.07_cstat0.65_target0.95_mlasso` | 24,616 | 4.33× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p5_prev0.07_cstat0.65_target0.90_mlasso` | 6,154 | `p5_prev0.07_cstat0.70_target0.90_mlasso` | 45,712 | 7.43× higher | — | next | next |
| C-statistic: 0.6 → 0.65 | `p5_prev0.10_cstat0.60_target0.80_mlasso` | 10,000 | `p5_prev0.10_cstat0.65_target0.80_mlasso` | 1,714 | 5.83× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.10_cstat0.60_target0.95_mlasso` | 160,000 | `p5_prev0.10_cstat0.65_target0.95_mlasso` | 36,928 | 4.33× lower | — | next | next |
| C-statistic: 0.65 → 0.7 | `p5_prev0.10_cstat0.65_target0.95_mlasso` | 36,928 | `p5_prev0.10_cstat0.70_target0.95_mlasso` | 137,152 | 3.71× higher | — | previous, next | previous |
| C-statistic: 0.65 → 0.7 | `p5_prev0.20_cstat0.65_target0.95_mlasso` | 29,846 | `p5_prev0.20_cstat0.70_target0.95_mlasso` | 8,568 | 3.48× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p5_prev0.20_cstat0.70_target0.90_mlasso` | 4,284 | `p5_prev0.20_cstat0.75_target0.90_mlasso` | 1,332 | 3.22× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p5_prev0.25_cstat0.65_target0.90_mlasso` | 7,104 | `p5_prev0.25_cstat0.70_target0.90_mlasso` | 1,776 | 4.00× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p5_prev0.25_cstat0.65_target0.95_mlasso` | 27,008 | `p5_prev0.25_cstat0.70_target0.95_mlasso` | 6,752 | 4.00× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p5_prev0.30_cstat0.70_target0.95_mlasso` | 45,056 | `p5_prev0.30_cstat0.75_target0.95_mlasso` | 5,632 | 8.00× lower | — | previous, next | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.40_cstat0.60_target0.90_mlasso` | 8,896 | `p5_prev0.40_cstat0.65_target0.90_mlasso` | 2,627 | 3.39× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.50_cstat0.60_target0.80_mlasso` | 1,234 | `p5_prev0.50_cstat0.65_target0.80_mlasso` | 397 | 3.11× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.50_cstat0.60_target0.95_mlasso` | 26,880 | `p5_prev0.50_cstat0.65_target0.95_mlasso` | 6,720 | 4.00× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p5_prev0.50_cstat0.70_target0.95_mlasso` | 6,500 | `p5_prev0.50_cstat0.75_target0.95_mlasso` | 1,680 | 3.87× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p75_prev0.05_cstat0.60_target0.90_mlasso` | 91,658 | `p75_prev0.05_cstat0.65_target0.90_mlasso` | 22,812 | 4.02× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p75_prev0.05_cstat0.75_target0.95_mlasso` | 20,000 | `p75_prev0.05_cstat0.80_target0.95_mlasso` | 75,000 | 3.75× higher | — | next | next |
| C-statistic: 0.8 → 0.85 | `p75_prev0.05_cstat0.80_target0.95_mlasso` | 75,000 | `p75_prev0.05_cstat0.85_target0.95_mlasso` | 17,647 | 4.25× lower | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p75_prev0.07_cstat0.60_target0.90_mlasso` | 68,202 | `p75_prev0.07_cstat0.65_target0.90_mlasso` | 12,290 | 5.55× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p75_prev0.07_cstat0.70_target0.85_mlasso` | 10,714 | `p75_prev0.07_cstat0.75_target0.85_mlasso` | 3,490 | 3.07× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p75_prev0.10_cstat0.60_target0.80_mlasso` | 18,750 | `p75_prev0.10_cstat0.65_target0.80_mlasso` | 4,442 | 4.22× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p75_prev0.10_cstat0.60_target0.90_mlasso` | 28,106 | `p75_prev0.10_cstat0.65_target0.90_mlasso` | 8,558 | 3.28× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p75_prev0.10_cstat0.60_target0.95_mlasso` | 150,000 | `p75_prev0.10_cstat0.65_target0.95_mlasso` | 34,615 | 4.33× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p75_prev0.10_cstat0.70_target0.95_mlasso` | 64,286 | `p75_prev0.10_cstat0.75_target0.95_mlasso` | 10,000 | 6.43× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p75_prev0.15_cstat0.60_target0.85_mlasso` | 25,000 | `p75_prev0.15_cstat0.65_target0.85_mlasso` | 5,795 | 4.31× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p75_prev0.15_cstat0.65_target0.95_mlasso` | 34,254 | `p75_prev0.15_cstat0.70_target0.95_mlasso` | 10,714 | 3.20× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p75_prev0.20_cstat0.60_target0.90_mlasso` | 19,133 | `p75_prev0.20_cstat0.65_target0.90_mlasso` | 4,327 | 4.42× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p75_prev0.40_cstat0.60_target0.95_mlasso` | 63,168 | `p75_prev0.40_cstat0.65_target0.95_mlasso` | 11,314 | 5.58× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p100_prev0.05_cstat0.60_target0.95_mlasso` | 157,206 | `p100_prev0.07_cstat0.60_target0.95_mlasso` | 533,336 | 3.39× higher | — | next | next |
| Prevalence: 0.075 → 0.1 | `p100_prev0.07_cstat0.60_target0.95_mlasso` | 533,336 | `p100_prev0.10_cstat0.60_target0.95_mlasso` | 164,124 | 3.25× lower | — | previous | previous |
| Prevalence: 0.075 → 0.15 | `p100_prev0.07_cstat0.65_target0.95_mlasso` | 123,076 | `p100_prev0.15_cstat0.65_target0.95_mlasso` | 30,769 | 4.00× lower | Yes | previous | — |
| Prevalence: 0.075 → 0.1 | `p100_prev0.07_cstat0.70_target0.95_mlasso` | 28,180 | `p100_prev0.10_cstat0.70_target0.95_mlasso` | 85,714 | 3.04× higher | — | next | — |
| Prevalence: 0.075 → 0.1 | `p100_prev0.07_cstat0.75_target0.95_mlasso` | 71,108 | `p100_prev0.10_cstat0.75_target0.95_mlasso` | 11,570 | 6.15× lower | — | previous | — |
| Prevalence: 0.1 → 0.15 | `p100_prev0.10_cstat0.65_target0.80_mlasso` | 11,538 | `p100_prev0.15_cstat0.65_target0.80_mlasso` | 3,846 | 3.00× lower | — | — | — |
| Prevalence: 0.1 → 0.15 | `p100_prev0.10_cstat0.70_target0.95_mlasso` | 85,714 | `p100_prev0.15_cstat0.70_target0.95_mlasso` | 15,597 | 5.50× lower | — | previous | — |
| Prevalence: 0.25 → 0.3 | `p100_prev0.25_cstat0.80_target0.95_mlasso` | 16,844 | `p100_prev0.30_cstat0.80_target0.95_mlasso` | 4,241 | 3.97× lower | — | previous | previous |
| Prevalence: 0.3 → 0.4 | `p100_prev0.30_cstat0.65_target0.95_mlasso` | 56,128 | `p100_prev0.40_cstat0.65_target0.95_mlasso` | 15,819 | 3.55× lower | — | previous | previous |
| Prevalence: 0.3 → 0.4 | `p100_prev0.30_cstat0.85_target0.95_mlasso` | 3,508 | `p100_prev0.40_cstat0.85_target0.95_mlasso` | 10,528 | 3.00× higher | — | next | next |
| Prevalence: 0.4 → 0.5 | `p100_prev0.40_cstat0.85_target0.95_mlasso` | 10,528 | `p100_prev0.50_cstat0.85_target0.95_mlasso` | 3,164 | 3.33× lower | — | previous | previous |
| Prevalence: 0.05 → 0.075 | `p10_prev0.05_cstat0.65_target0.95_mlasso` | 147,696 | `p10_prev0.07_cstat0.65_target0.95_mlasso` | 36,780 | 4.02× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p10_prev0.05_cstat0.70_target0.95_mlasso` | 31,977 | `p10_prev0.07_cstat0.70_target0.95_mlasso` | 182,848 | 5.72× higher | — | next | next |
| Prevalence: 0.05 → 0.075 | `p10_prev0.05_cstat0.80_target0.95_mlasso` | 20,000 | `p10_prev0.07_cstat0.80_target0.95_mlasso` | 6,664 | 3.00× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p10_prev0.05_cstat0.85_target0.95_mlasso` | 9,412 | `p10_prev0.07_cstat0.85_target0.95_mlasso` | 3,136 | 3.00× lower | — | — | — |
| Prevalence: 0.075 → 0.1 | `p10_prev0.07_cstat0.70_target0.95_mlasso` | 182,848 | `p10_prev0.10_cstat0.70_target0.95_mlasso` | 34,289 | 5.33× lower | — | previous, next | previous |
| Prevalence: 0.15 → 0.2 | `p10_prev0.15_cstat0.65_target0.95_mlasso` | 24,616 | `p10_prev0.20_cstat0.65_target0.95_mlasso` | 73,856 | 3.00× higher | — | previous, next | — |
| Prevalence: 0.15 → 0.2 | `p10_prev0.15_cstat0.80_target0.95_mlasso` | 13,344 | `p10_prev0.20_cstat0.80_target0.95_mlasso` | 3,758 | 3.55× lower | — | previous | previous |
| Prevalence: 0.2 → 0.25 | `p10_prev0.20_cstat0.75_target0.95_mlasso` | 10,672 | `p10_prev0.25_cstat0.75_target0.95_mlasso` | 3,368 | 3.17× lower | — | previous | — |
| Prevalence: 0.2 → 0.25 | `p10_prev0.20_cstat0.85_target0.95_mlasso` | 9,408 | `p10_prev0.25_cstat0.85_target0.95_mlasso` | 2,320 | 4.06× lower | — | previous | — |
| Prevalence: 0.25 → 0.3 | `p10_prev0.25_cstat0.65_target0.95_mlasso` | 6,736 | `p10_prev0.30_cstat0.65_target0.95_mlasso` | 27,514 | 4.08× higher | — | next | next |
| Prevalence: 0.3 → 0.4 | `p10_prev0.30_cstat0.65_target0.95_mlasso` | 27,514 | `p10_prev0.40_cstat0.65_target0.95_mlasso` | 8,416 | 3.27× lower | — | previous | previous |
| Prevalence: 0.075 → 0.1 | `p15_prev0.07_cstat0.60_target0.95_mlasso` | 160,000 | `p15_prev0.10_cstat0.60_target0.95_mlasso` | 45,917 | 3.48× lower | — | next | next |
| Prevalence: 0.1 → 0.15 | `p15_prev0.10_cstat0.60_target0.95_mlasso` | 45,917 | `p15_prev0.15_cstat0.60_target0.95_mlasso` | 247,819 | 5.40× higher | — | previous, next | previous |
| Prevalence: 0.15 → 0.2 | `p15_prev0.15_cstat0.70_target0.95_mlasso` | 34,288 | `p15_prev0.20_cstat0.70_target0.95_mlasso` | 9,841 | 3.48× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p20_prev0.05_cstat0.60_target0.95_mlasso` | 160,000 | `p20_prev0.07_cstat0.60_target0.95_mlasso` | 853,312 | 5.33× higher | — | next | — |
| Prevalence: 0.05 → 0.075 | `p20_prev0.05_cstat0.70_target0.90_mlasso` | 17,143 | `p20_prev0.07_cstat0.70_target0.90_mlasso` | 5,714 | 3.00× lower | — | — | — |
| Prevalence: 0.05 → 0.075 | `p20_prev0.05_cstat0.85_target0.95_mlasso` | 37,648 | `p20_prev0.07_cstat0.85_target0.95_mlasso` | 6,276 | 6.00× lower | — | previous | — |
| Prevalence: 0.15 → 0.2 | `p20_prev0.15_cstat0.60_target0.95_mlasso` | 426,688 | `p20_prev0.20_cstat0.60_target0.95_mlasso` | 80,000 | 5.33× lower | — | previous, next | — |
| Prevalence: 0.25 → 0.3 | `p20_prev0.25_cstat0.60_target0.95_mlasso` | 26,818 | `p20_prev0.30_cstat0.60_target0.95_mlasso` | 89,856 | 3.35× higher | — | previous, next | previous |
| Prevalence: 0.25 → 0.3 | `p20_prev0.25_cstat0.70_target0.95_mlasso` | 13,472 | `p20_prev0.30_cstat0.70_target0.95_mlasso` | 4,418 | 3.05× lower | — | — | — |
| Prevalence: 0.25 → 0.3 | `p20_prev0.25_cstat0.80_target0.95_mlasso` | 3,642 | `p20_prev0.30_cstat0.80_target0.95_mlasso` | 11,232 | 3.08× higher | — | next | next |
| Prevalence: 0.3 → 0.4 | `p20_prev0.30_cstat0.80_target0.95_mlasso` | 11,232 | `p20_prev0.40_cstat0.80_target0.95_mlasso` | 3,157 | 3.56× lower | — | previous | previous |
| Prevalence: 0.05 → 0.075 | `p25_prev0.05_cstat0.70_target0.95_mlasso` | 85,716 | `p25_prev0.07_cstat0.70_target0.95_mlasso` | 27,866 | 3.08× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p25_prev0.05_cstat0.85_target0.80_mlasso` | 770 | `p25_prev0.07_cstat0.85_target0.80_mlasso` | 245 | 3.14× lower | — | — | — |
| Prevalence: 0.075 → 0.1 | `p25_prev0.07_cstat0.60_target0.85_mlasso` | 14,281 | `p25_prev0.10_cstat0.60_target0.85_mlasso` | 100,000 | 7.00× higher | — | next | next |
| Prevalence: 0.075 → 0.1 | `p25_prev0.07_cstat0.80_target0.85_mlasso` | 2,083 | `p25_prev0.10_cstat0.80_target0.85_mlasso` | 589 | 3.54× lower | — | — | — |
| Prevalence: 0.15 → 0.2 | `p25_prev0.15_cstat0.60_target0.95_mlasso` | 133,328 | `p25_prev0.20_cstat0.60_target0.95_mlasso` | 37,846 | 3.52× lower | — | previous | — |
| Prevalence: 0.2 → 0.25 | `p25_prev0.20_cstat0.75_target0.95_mlasso` | 5,024 | `p25_prev0.25_cstat0.75_target0.95_mlasso` | 16,848 | 3.35× higher | — | next | next |
| Prevalence: 0.25 → 0.3 | `p25_prev0.25_cstat0.60_target0.95_mlasso` | 67,392 | `p25_prev0.30_cstat0.60_target0.95_mlasso` | 449,024 | 6.66× higher | — | previous, next | next |
| Prevalence: 0.05 → 0.075 | `p30_prev0.05_cstat0.65_target0.95_mlasso` | 443,072 | `p30_prev0.07_cstat0.65_target0.95_mlasso` | 73,848 | 6.00× lower | — | previous | — |
| Prevalence: 0.075 → 0.1 | `p30_prev0.07_cstat0.60_target0.90_mlasso` | 80,000 | `p30_prev0.10_cstat0.60_target0.90_mlasso` | 24,579 | 3.25× lower | — | previous | — |
| Prevalence: 0.075 → 0.1 | `p30_prev0.07_cstat0.65_target0.90_mlasso` | 18,462 | `p30_prev0.10_cstat0.65_target0.90_mlasso` | 5,348 | 3.45× lower | — | — | — |
| Prevalence: 0.1 → 0.15 | `p30_prev0.10_cstat0.60_target0.95_mlasso` | 60,000 | `p30_prev0.15_cstat0.60_target0.95_mlasso` | 320,000 | 5.33× higher | — | next | — |
| Prevalence: 0.1 → 0.15 | `p30_prev0.10_cstat0.65_target0.95_mlasso` | 74,299 | `p30_prev0.15_cstat0.65_target0.95_mlasso` | 18,244 | 4.07× lower | — | previous | — |
| Prevalence: 0.2 → 0.25 | `p30_prev0.20_cstat0.60_target0.95_mlasso` | 30,000 | `p30_prev0.25_cstat0.60_target0.95_mlasso` | 241,151 | 8.04× higher | — | previous, next | previous, next |
| Prevalence: 0.25 → 0.3 | `p30_prev0.25_cstat0.60_target0.95_mlasso` | 241,151 | `p30_prev0.30_cstat0.60_target0.95_mlasso` | 67,392 | 3.58× lower | — | previous, next | previous |
| Prevalence: 0.4 → 0.5 | `p30_prev0.40_cstat0.70_target0.80_mlasso` | 938 | `p30_prev0.50_cstat0.70_target0.80_mlasso` | 187 | 5.02× lower | — | previous, next | next |
| Prevalence: 0.05 → 0.075 | `p40_prev0.05_cstat0.65_target0.90_mlasso` | 73,846 | `p40_prev0.07_cstat0.65_target0.90_mlasso` | 13,452 | 5.49× lower | — | previous, next | previous, next |
| Prevalence: 0.05 → 0.075 | `p40_prev0.05_cstat0.70_target0.90_mlasso` | 10,205 | `p40_prev0.07_cstat0.70_target0.90_mlasso` | 45,714 | 4.48× higher | — | next | next |
| Prevalence: 0.05 → 0.075 | `p40_prev0.05_cstat0.75_target0.95_mlasso` | 85,336 | `p40_prev0.07_cstat0.75_target0.95_mlasso` | 24,671 | 3.46× lower | — | previous, next | — |
| Prevalence: 0.05 → 0.075 | `p40_prev0.05_cstat0.85_target0.80_mlasso` | 1,177 | `p40_prev0.07_cstat0.85_target0.80_mlasso` | 392 | 3.00× lower | — | — | — |
| Prevalence: 0.075 → 0.1 | `p40_prev0.07_cstat0.70_target0.90_mlasso` | 45,714 | `p40_prev0.10_cstat0.70_target0.90_mlasso` | 5,600 | 8.16× lower | — | previous | previous |
| Prevalence: 0.1 → 0.15 | `p40_prev0.10_cstat0.70_target0.95_mlasso` | 68,572 | `p40_prev0.15_cstat0.70_target0.95_mlasso` | 11,429 | 6.00× lower | — | previous | previous |
| Prevalence: 0.15 → 0.2 | `p40_prev0.15_cstat0.60_target0.95_mlasso` | 426,656 | `p40_prev0.20_cstat0.60_target0.95_mlasso` | 50,005 | 8.53× lower | — | previous | — |
| Prevalence: 0.4 → 0.5 | `p40_prev0.40_cstat0.75_target0.90_mlasso` | 1,147 | `p40_prev0.50_cstat0.75_target0.90_mlasso` | 3,556 | 3.10× higher | — | next | next |
| Prevalence: 0.075 → 0.1 | `p50_prev0.07_cstat0.85_target0.95_mlasso` | 13,970 | `p50_prev0.10_cstat0.85_target0.95_mlasso` | 4,282 | 3.26× lower | — | previous | — |
| Prevalence: 0.1 → 0.15 | `p50_prev0.10_cstat0.70_target0.95_mlasso` | 19,009 | `p50_prev0.15_cstat0.70_target0.95_mlasso` | 57,144 | 3.01× higher | — | previous, next | previous, next |
| Prevalence: 0.15 → 0.2 | `p50_prev0.15_cstat0.70_target0.95_mlasso` | 57,144 | `p50_prev0.20_cstat0.70_target0.95_mlasso` | 11,284 | 5.06× lower | — | previous | previous |
| Prevalence: 0.2 → 0.25 | `p50_prev0.20_cstat0.60_target0.90_mlasso` | 16,524 | `p50_prev0.25_cstat0.60_target0.90_mlasso` | 4,219 | 3.92× lower | — | — | — |
| Prevalence: 0.25 → 0.3 | `p50_prev0.25_cstat0.65_target0.85_mlasso` | 3,024 | `p50_prev0.30_cstat0.65_target0.85_mlasso` | 981 | 3.08× lower | — | — | — |
| Prevalence: 0.3 → 0.4 | `p50_prev0.30_cstat0.75_target0.80_mlasso` | 764 | `p50_prev0.40_cstat0.75_target0.80_mlasso` | 195 | 3.92× lower | — | next | next |
| Prevalence: 0.05 → 0.075 | `p5_prev0.05_cstat0.60_target0.95_mlasso` | 320,000 | `p5_prev0.07_cstat0.60_target0.95_mlasso` | 106,656 | 3.00× lower | — | previous, next | — |
| Prevalence: 0.05 → 0.075 | `p5_prev0.05_cstat0.85_target0.95_mlasso` | 18,816 | `p5_prev0.07_cstat0.85_target0.95_mlasso` | 4,761 | 3.95× lower | — | previous | — |
| Prevalence: 0.075 → 0.1 | `p5_prev0.07_cstat0.70_target0.95_mlasso` | 22,856 | `p5_prev0.10_cstat0.70_target0.95_mlasso` | 137,152 | 6.00× higher | — | next | — |
| Prevalence: 0.075 → 0.1 | `p5_prev0.07_cstat0.75_target0.95_mlasso` | 19,776 | `p5_prev0.10_cstat0.75_target0.95_mlasso` | 85,376 | 4.32× higher | — | previous, next | — |
| Prevalence: 0.1 → 0.15 | `p5_prev0.10_cstat0.60_target0.80_mlasso` | 10,000 | `p5_prev0.15_cstat0.60_target0.80_mlasso` | 1,658 | 6.03× lower | — | previous | — |
| Prevalence: 0.1 → 0.15 | `p5_prev0.10_cstat0.60_target0.90_mlasso` | 161,251 | `p5_prev0.15_cstat0.60_target0.90_mlasso` | 53,344 | 3.02× lower | — | previous, next | — |
| Prevalence: 0.1 → 0.15 | `p5_prev0.10_cstat0.65_target0.95_mlasso` | 36,928 | `p5_prev0.15_cstat0.65_target0.95_mlasso` | 9,476 | 3.90× lower | — | previous, next | previous, next |
| Prevalence: 0.15 → 0.2 | `p5_prev0.15_cstat0.60_target0.90_mlasso` | 53,344 | `p5_prev0.20_cstat0.60_target0.90_mlasso` | 10,000 | 5.33× lower | — | previous | — |
| Prevalence: 0.15 → 0.2 | `p5_prev0.15_cstat0.65_target0.95_mlasso` | 9,476 | `p5_prev0.20_cstat0.65_target0.95_mlasso` | 29,846 | 3.15× higher | — | previous, next | previous |
| Prevalence: 0.2 → 0.25 | `p5_prev0.20_cstat0.80_target0.95_mlasso` | 5,089 | `p5_prev0.25_cstat0.80_target0.95_mlasso` | 1,688 | 3.01× lower | — | previous | — |
| Prevalence: 0.25 → 0.3 | `p5_prev0.25_cstat0.60_target0.95_mlasso` | 54,016 | `p5_prev0.30_cstat0.60_target0.95_mlasso` | 180,224 | 3.34× higher | — | previous, next | — |
| Prevalence: 0.25 → 0.3 | `p5_prev0.25_cstat0.70_target0.95_mlasso` | 6,752 | `p5_prev0.30_cstat0.70_target0.95_mlasso` | 45,056 | 6.67× higher | — | next | — |
| Prevalence: 0.3 → 0.4 | `p5_prev0.30_cstat0.60_target0.95_mlasso` | 180,224 | `p5_prev0.40_cstat0.60_target0.95_mlasso` | 26,589 | 6.78× lower | — | previous | — |
| Prevalence: 0.4 → 0.5 | `p5_prev0.40_cstat0.65_target0.95_mlasso` | 36,947 | `p5_prev0.50_cstat0.65_target0.95_mlasso` | 6,720 | 5.50× lower | — | previous | — |
| Prevalence: 0.05 → 0.1 | `p75_prev0.05_cstat0.65_target0.95_mlasso` | 138,462 | `p75_prev0.10_cstat0.65_target0.95_mlasso` | 34,615 | 4.00× lower | Yes | previous | — |
| Prevalence: 0.05 → 0.075 | `p75_prev0.05_cstat0.80_target0.95_mlasso` | 75,000 | `p75_prev0.07_cstat0.80_target0.95_mlasso` | 25,000 | 3.00× lower | — | previous, next | previous |
| Prevalence: 0.075 → 0.1 | `p75_prev0.07_cstat0.80_target0.95_mlasso` | 25,000 | `p75_prev0.10_cstat0.80_target0.95_mlasso` | 6,831 | 3.66× lower | — | previous | — |
| Prevalence: 0.1 → 0.15 | `p75_prev0.10_cstat0.70_target0.95_mlasso` | 64,286 | `p75_prev0.15_cstat0.70_target0.95_mlasso` | 10,714 | 6.00× lower | — | previous | — |
| Prevalence: 0.4 → 0.5 | `p75_prev0.40_cstat0.60_target0.95_mlasso` | 63,168 | `p75_prev0.50_cstat0.60_target0.95_mlasso` | 18,582 | 3.40× lower | — | previous, next | next |
| Predictors: 10 → 15 | `p10_prev0.05_cstat0.60_target0.80_mlasso` | 20,000 | `p15_prev0.05_cstat0.60_target0.80_mlasso` | 3,750 | 5.33× lower | — | previous, next | previous, next |
| Predictors: 10 → 15 | `p10_prev0.05_cstat0.60_target0.95_mlasso` | 640,000 | `p15_prev0.05_cstat0.60_target0.95_mlasso` | 179,983 | 3.56× lower | — | previous, next | — |
| Predictors: 10 → 15 | `p10_prev0.15_cstat0.60_target0.95_mlasso` | 53,328 | `p15_prev0.15_cstat0.60_target0.95_mlasso` | 247,819 | 4.65× higher | — | previous, next | — |
| Predictors: 10 → 15 | `p10_prev0.20_cstat0.65_target0.95_mlasso` | 73,856 | `p15_prev0.20_cstat0.65_target0.95_mlasso` | 13,848 | 5.33× lower | — | previous | — |
| Predictors: 10 → 15 | `p10_prev0.25_cstat0.80_target0.95_mlasso` | 1,684 | `p15_prev0.25_cstat0.80_target0.95_mlasso` | 5,056 | 3.00× higher | — | next | — |
| Predictors: 10 → 15 | `p10_prev0.30_cstat0.65_target0.90_mlasso` | 1,396 | `p15_prev0.30_cstat0.65_target0.90_mlasso` | 4,448 | 3.19× higher | — | — | — |
| Predictors: 10 → 15 | `p10_prev0.30_cstat0.65_target0.95_mlasso` | 27,514 | `p15_prev0.30_cstat0.65_target0.95_mlasso` | 8,416 | 3.27× lower | — | previous | previous |
| Predictors: 15 → 20 | `p15_prev0.05_cstat0.60_target0.80_mlasso` | 3,750 | `p20_prev0.05_cstat0.60_target0.80_mlasso` | 20,000 | 5.33× higher | — | previous, next | previous |
| Predictors: 15 → 20 | `p15_prev0.05_cstat0.60_target0.85_mlasso` | 16,617 | `p20_prev0.05_cstat0.60_target0.85_mlasso` | 160,000 | 9.63× higher | — | next | — |
| Predictors: 15 → 20 | `p15_prev0.07_cstat0.60_target0.95_mlasso` | 160,000 | `p20_prev0.07_cstat0.60_target0.95_mlasso` | 853,312 | 5.33× higher | — | next | — |
| Predictors: 15 → 20 | `p15_prev0.15_cstat0.70_target0.95_mlasso` | 34,288 | `p20_prev0.15_cstat0.70_target0.95_mlasso` | 11,428 | 3.00× lower | — | previous | — |
| Predictors: 15 → 20 | `p15_prev0.20_cstat0.60_target0.95_mlasso` | 22,111 | `p20_prev0.20_cstat0.60_target0.95_mlasso` | 80,000 | 3.62× higher | — | next | — |
| Predictors: 15 → 20 | `p15_prev0.30_cstat0.60_target0.95_mlasso` | 16,832 | `p20_prev0.30_cstat0.60_target0.95_mlasso` | 89,856 | 5.34× higher | — | next | — |
| Predictors: 15 → 20 | `p15_prev0.30_cstat0.80_target0.95_mlasso` | 3,519 | `p20_prev0.30_cstat0.80_target0.95_mlasso` | 11,232 | 3.19× higher | — | next | next |
| Predictors: 15 → 20 | `p15_prev0.40_cstat0.60_target0.95_mlasso` | 11,475 | `p20_prev0.40_cstat0.60_target0.95_mlasso` | 50,418 | 4.39× higher | — | previous, next | previous |
| Predictors: 20 → 25 | `p20_prev0.05_cstat0.60_target0.85_mlasso` | 160,000 | `p25_prev0.05_cstat0.60_target0.85_mlasso` | 26,883 | 5.95× lower | — | previous | — |
| Predictors: 20 → 25 | `p20_prev0.05_cstat0.70_target0.90_mlasso` | 17,143 | `p25_prev0.05_cstat0.70_target0.90_mlasso` | 5,357 | 3.20× lower | — | — | — |
| Predictors: 20 → 25 | `p20_prev0.05_cstat0.85_target0.95_mlasso` | 37,648 | `p25_prev0.05_cstat0.85_target0.95_mlasso` | 11,989 | 3.14× lower | — | previous | — |
| Predictors: 20 → 25 | `p20_prev0.15_cstat0.60_target0.95_mlasso` | 426,688 | `p25_prev0.15_cstat0.60_target0.95_mlasso` | 133,328 | 3.20× lower | — | previous, next | — |
| Predictors: 20 → 25 | `p20_prev0.30_cstat0.60_target0.95_mlasso` | 89,856 | `p25_prev0.30_cstat0.60_target0.95_mlasso` | 449,024 | 5.00× higher | — | previous, next | next |
| Predictors: 25 → 30 | `p25_prev0.10_cstat0.60_target0.85_mlasso` | 100,000 | `p30_prev0.10_cstat0.60_target0.85_mlasso` | 10,177 | 9.83× lower | — | previous | previous |
| Predictors: 25 → 30 | `p25_prev0.25_cstat0.60_target0.95_mlasso` | 67,392 | `p30_prev0.25_cstat0.60_target0.95_mlasso` | 241,151 | 3.58× higher | — | previous, next | next |
| Predictors: 25 → 30 | `p25_prev0.25_cstat0.75_target0.95_mlasso` | 16,848 | `p30_prev0.25_cstat0.75_target0.95_mlasso` | 3,921 | 4.30× lower | — | previous | previous |
| Predictors: 25 → 30 | `p25_prev0.30_cstat0.60_target0.95_mlasso` | 449,024 | `p30_prev0.30_cstat0.60_target0.95_mlasso` | 67,392 | 6.66× lower | — | previous, next | previous |
| Predictors: 30 → 40 | `p30_prev0.05_cstat0.65_target0.90_mlasso` | 17,109 | `p40_prev0.05_cstat0.65_target0.90_mlasso` | 73,846 | 4.32× higher | — | previous, next | next |
| Predictors: 30 → 40 | `p30_prev0.05_cstat0.65_target0.95_mlasso` | 443,072 | `p40_prev0.05_cstat0.65_target0.95_mlasso` | 73,846 | 6.00× lower | — | previous | — |
| Predictors: 30 → 40 | `p30_prev0.05_cstat0.70_target0.90_mlasso` | 3,214 | `p40_prev0.05_cstat0.70_target0.90_mlasso` | 10,205 | 3.18× higher | — | — | — |
| Predictors: 30 → 40 | `p30_prev0.05_cstat0.75_target0.95_mlasso` | 28,299 | `p40_prev0.05_cstat0.75_target0.95_mlasso` | 85,336 | 3.02× higher | — | previous, next | — |
| Predictors: 30 → 100 | `p30_prev0.07_cstat0.60_target0.95_mlasso` | 79,989 | `p100_prev0.07_cstat0.60_target0.95_mlasso` | 533,336 | 6.67× higher | Yes | next | next |
| Predictors: 30 → 40 | `p30_prev0.07_cstat0.70_target0.90_mlasso` | 6,146 | `p40_prev0.07_cstat0.70_target0.90_mlasso` | 45,714 | 7.44× higher | — | next | next |
| Predictors: 30 → 40 | `p30_prev0.07_cstat0.70_target0.95_mlasso` | 25,523 | `p40_prev0.07_cstat0.70_target0.95_mlasso` | 91,428 | 3.58× higher | — | next | next |
| Predictors: 30 → 40 | `p30_prev0.10_cstat0.70_target0.95_mlasso` | 19,986 | `p40_prev0.10_cstat0.70_target0.95_mlasso` | 68,572 | 3.43× higher | — | next | next |
| Predictors: 30 → 40 | `p30_prev0.25_cstat0.60_target0.95_mlasso` | 241,151 | `p40_prev0.25_cstat0.60_target0.95_mlasso` | 44,295 | 5.44× lower | — | previous | previous |
| Predictors: 30 → 40 | `p30_prev0.30_cstat0.65_target0.95_mlasso` | 13,905 | `p40_prev0.30_cstat0.65_target0.95_mlasso` | 44,896 | 3.23× higher | — | next | next |
| Predictors: 30 → 40 | `p30_prev0.50_cstat0.70_target0.80_mlasso` | 187 | `p40_prev0.50_cstat0.70_target0.80_mlasso` | 690 | 3.69× higher | — | previous | previous |
| Predictors: 40 → 50 | `p40_prev0.05_cstat0.65_target0.90_mlasso` | 73,846 | `p50_prev0.05_cstat0.65_target0.90_mlasso` | 23,077 | 3.20× lower | — | previous | previous |
| Predictors: 40 → 50 | `p40_prev0.07_cstat0.70_target0.90_mlasso` | 45,714 | `p50_prev0.07_cstat0.70_target0.90_mlasso` | 7,709 | 5.93× lower | — | previous | previous |
| Predictors: 40 → 50 | `p40_prev0.07_cstat0.70_target0.95_mlasso` | 91,428 | `p50_prev0.07_cstat0.70_target0.95_mlasso` | 22,834 | 4.00× lower | — | previous | previous |
| Predictors: 40 → 50 | `p40_prev0.10_cstat0.70_target0.95_mlasso` | 68,572 | `p50_prev0.10_cstat0.70_target0.95_mlasso` | 19,009 | 3.61× lower | — | previous, next | previous, next |
| Predictors: 40 → 50 | `p40_prev0.15_cstat0.60_target0.95_mlasso` | 426,656 | `p50_prev0.15_cstat0.60_target0.95_mlasso` | 133,336 | 3.20× lower | — | previous, next | — |
| Predictors: 40 → 50 | `p40_prev0.15_cstat0.70_target0.95_mlasso` | 11,429 | `p50_prev0.15_cstat0.70_target0.95_mlasso` | 57,144 | 5.00× higher | — | next | next |
| Predictors: 40 → 50 | `p40_prev0.30_cstat0.65_target0.95_mlasso` | 44,896 | `p50_prev0.30_cstat0.65_target0.95_mlasso` | 14,040 | 3.20× lower | — | previous | previous |
| Predictors: 40 → 50 | `p40_prev0.50_cstat0.75_target0.90_mlasso` | 3,556 | `p50_prev0.50_cstat0.75_target0.90_mlasso` | 1,111 | 3.20× lower | — | previous | previous |
| Predictors: 50 → 75 | `p50_prev0.05_cstat0.60_target0.80_mlasso` | 7,851 | `p75_prev0.05_cstat0.60_target0.80_mlasso` | 31,408 | 4.00× higher | — | previous | previous |
| Predictors: 50 → 75 | `p50_prev0.05_cstat0.70_target0.95_mlasso` | 42,857 | `p75_prev0.05_cstat0.70_target0.95_mlasso` | 257,144 | 6.00× higher | — | next | next |
| Predictors: 50 → 75 | `p50_prev0.05_cstat0.80_target0.95_mlasso` | 19,242 | `p75_prev0.05_cstat0.80_target0.95_mlasso` | 75,000 | 3.90× higher | — | next | next |
| Predictors: 50 → 75 | `p50_prev0.07_cstat0.70_target0.95_mlasso` | 22,834 | `p75_prev0.07_cstat0.70_target0.95_mlasso` | 171,428 | 7.51× higher | — | next | next |
| Predictors: 50 → 75 | `p50_prev0.10_cstat0.60_target0.80_mlasso` | 6,250 | `p75_prev0.10_cstat0.60_target0.80_mlasso` | 18,750 | 3.00× higher | — | — | — |
| Predictors: 50 → 75 | `p50_prev0.10_cstat0.70_target0.95_mlasso` | 19,009 | `p75_prev0.10_cstat0.70_target0.95_mlasso` | 64,286 | 3.38× higher | — | previous, next | previous |
| Predictors: 50 → 75 | `p50_prev0.10_cstat0.75_target0.80_mlasso` | 762 | `p75_prev0.10_cstat0.75_target0.80_mlasso` | 2,500 | 3.28× higher | — | — | — |
| Predictors: 50 → 75 | `p50_prev0.10_cstat0.85_target0.95_mlasso` | 4,282 | `p75_prev0.10_cstat0.85_target0.95_mlasso` | 17,648 | 4.12× higher | — | next | — |
| Predictors: 50 → 75 | `p50_prev0.15_cstat0.70_target0.95_mlasso` | 57,144 | `p75_prev0.15_cstat0.70_target0.95_mlasso` | 10,714 | 5.33× lower | — | previous | previous |
| Predictors: 50 → 75 | `p50_prev0.30_cstat0.65_target0.85_mlasso` | 981 | `p75_prev0.30_cstat0.65_target0.85_mlasso` | 3,950 | 4.03× higher | — | — | — |
| Predictors: 50 → 75 | `p50_prev0.40_cstat0.75_target0.80_mlasso` | 195 | `p75_prev0.40_cstat0.75_target0.80_mlasso` | 872 | 4.47× higher | — | previous | previous |
| Predictors: 5 → 10 | `p5_prev0.05_cstat0.60_target0.80_mlasso` | 5,000 | `p10_prev0.05_cstat0.60_target0.80_mlasso` | 20,000 | 4.00× higher | — | next | next |
| Predictors: 5 → 10 | `p5_prev0.05_cstat0.80_target0.95_mlasso` | 5,000 | `p10_prev0.05_cstat0.80_target0.95_mlasso` | 20,000 | 4.00× higher | — | next | — |
| Predictors: 5 → 10 | `p5_prev0.07_cstat0.70_target0.90_mlasso` | 45,712 | `p10_prev0.07_cstat0.70_target0.90_mlasso` | 5,714 | 8.00× lower | — | previous | previous |
| Predictors: 5 → 10 | `p5_prev0.07_cstat0.70_target0.95_mlasso` | 22,856 | `p10_prev0.07_cstat0.70_target0.95_mlasso` | 182,848 | 8.00× higher | — | next | next |
| Predictors: 5 → 10 | `p5_prev0.10_cstat0.60_target0.90_mlasso` | 161,251 | `p10_prev0.10_cstat0.60_target0.90_mlasso` | 22,029 | 7.32× lower | — | previous | — |
| Predictors: 5 → 10 | `p5_prev0.10_cstat0.70_target0.95_mlasso` | 137,152 | `p10_prev0.10_cstat0.70_target0.95_mlasso` | 34,289 | 4.00× lower | — | previous, next | — |
| Predictors: 5 → 10 | `p5_prev0.10_cstat0.75_target0.95_mlasso` | 85,376 | `p10_prev0.10_cstat0.75_target0.95_mlasso` | 10,664 | 8.01× lower | — | previous | — |
| Predictors: 5 → 10 | `p5_prev0.15_cstat0.60_target0.90_mlasso` | 53,344 | `p10_prev0.15_cstat0.60_target0.90_mlasso` | 9,182 | 5.81× lower | — | previous | — |
| Predictors: 5 → 10 | `p5_prev0.25_cstat0.65_target0.95_mlasso` | 27,008 | `p10_prev0.25_cstat0.65_target0.95_mlasso` | 6,736 | 4.01× lower | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.30_cstat0.60_target0.95_mlasso` | 180,224 | `p10_prev0.30_cstat0.60_target0.95_mlasso` | 44,928 | 4.01× lower | — | previous, next | — |
| Predictors: 5 → 10 | `p5_prev0.30_cstat0.70_target0.95_mlasso` | 45,056 | `p10_prev0.30_cstat0.70_target0.95_mlasso` | 5,616 | 8.02× lower | — | previous | — |
| Predictors: 5 → 10 | `p5_prev0.40_cstat0.65_target0.95_mlasso` | 36,947 | `p10_prev0.40_cstat0.65_target0.95_mlasso` | 8,416 | 4.39× lower | — | previous | — |
| Predictors: 5 → 10 | `p5_prev0.40_cstat0.70_target0.95_mlasso` | 3,554 | `p10_prev0.40_cstat0.70_target0.95_mlasso` | 16,832 | 4.74× higher | — | next | — |
| Predictors: 75 → 100 | `p75_prev0.05_cstat0.60_target0.80_mlasso` | 31,408 | `p100_prev0.05_cstat0.60_target0.80_mlasso` | 200,000 | 6.37× higher | — | next | — |
| Predictors: 75 → 100 | `p75_prev0.05_cstat0.70_target0.95_mlasso` | 257,144 | `p100_prev0.05_cstat0.70_target0.95_mlasso` | 42,857 | 6.00× lower | — | previous | previous |
| Predictors: 75 → 100 | `p75_prev0.05_cstat0.80_target0.95_mlasso` | 75,000 | `p100_prev0.05_cstat0.80_target0.95_mlasso` | 25,000 | 3.00× lower | — | previous | previous |
| Predictors: 75 → 100 | `p75_prev0.07_cstat0.70_target0.95_mlasso` | 171,428 | `p100_prev0.07_cstat0.70_target0.95_mlasso` | 28,180 | 6.08× lower | — | previous | previous |
| Predictors: 75 → 100 | `p75_prev0.07_cstat0.75_target0.95_mlasso` | 12,908 | `p100_prev0.07_cstat0.75_target0.95_mlasso` | 71,108 | 5.51× higher | — | next | — |
| Predictors: 75 → 100 | `p75_prev0.25_cstat0.60_target0.95_mlasso` | 50,528 | `p100_prev0.25_cstat0.60_target0.95_mlasso` | 176,420 | 3.49× higher | — | next | — |
| Predictors: 75 → 100 | `p75_prev0.40_cstat0.80_target0.95_mlasso` | 7,896 | `p100_prev0.40_cstat0.80_target0.95_mlasso` | 2,632 | 3.00× lower | — | previous | — |
| Predictors: 75 → 100 | `p75_prev0.40_cstat0.85_target0.95_mlasso` | 2,814 | `p100_prev0.40_cstat0.85_target0.95_mlasso` | 10,528 | 3.74× higher | — | next | next |
| Predictors: 75 → 100 | `p75_prev0.50_cstat0.60_target0.95_mlasso` | 18,582 | `p100_prev0.50_cstat0.60_target0.95_mlasso` | 67,360 | 3.63× higher | — | previous, next | previous |
| Predictors: 75 → 100 | `p75_prev0.50_cstat0.80_target0.95_mlasso` | 6,316 | `p100_prev0.50_cstat0.80_target0.95_mlasso` | 1,992 | 3.17× lower | — | — | — |
| Target slope: 0.8 → 0.85 | `p100_prev0.05_cstat0.60_target0.80_mlasso` | 200,000 | `p100_prev0.05_cstat0.60_target0.85_mlasso` | 52,119 | 3.84× lower | — | previous | — |
| Target slope: 0.85 → 0.95 | `p100_prev0.05_cstat0.65_target0.85_mlasso` | 23,222 | `p100_prev0.05_cstat0.65_target0.95_mlasso` | 184,616 | 7.95× higher | Yes | next | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.05_cstat0.80_target0.90_mlasso` | 6,753 | `p100_prev0.05_cstat0.80_target0.95_mlasso` | 25,000 | 3.70× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.07_cstat0.60_target0.90_mlasso` | 60,505 | `p100_prev0.07_cstat0.60_target0.95_mlasso` | 533,336 | 8.81× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p100_prev0.07_cstat0.65_target0.90_mlasso` | 20,756 | `p100_prev0.07_cstat0.65_target0.95_mlasso` | 123,076 | 5.93× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.07_cstat0.80_target0.90_mlasso` | 4,475 | `p100_prev0.07_cstat0.80_target0.95_mlasso` | 14,147 | 3.16× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.07_cstat0.85_target0.90_mlasso` | 3,921 | `p100_prev0.07_cstat0.85_target0.95_mlasso` | 31,372 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.10_cstat0.60_target0.90_mlasso` | 40,283 | `p100_prev0.10_cstat0.60_target0.95_mlasso` | 164,124 | 4.07× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.10_cstat0.70_target0.90_mlasso` | 10,714 | `p100_prev0.10_cstat0.70_target0.95_mlasso` | 85,714 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.10_cstat0.80_target0.90_mlasso` | 3,631 | `p100_prev0.10_cstat0.80_target0.95_mlasso` | 12,500 | 3.44× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.15_cstat0.65_target0.90_mlasso` | 7,489 | `p100_prev0.15_cstat0.65_target0.95_mlasso` | 30,769 | 4.11× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.15_cstat0.75_target0.90_mlasso` | 4,444 | `p100_prev0.15_cstat0.75_target0.95_mlasso` | 14,568 | 3.28× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.15_cstat0.85_target0.90_mlasso` | 2,154 | `p100_prev0.15_cstat0.85_target0.95_mlasso` | 15,688 | 7.28× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p100_prev0.20_cstat0.60_target0.85_mlasso` | 12,197 | `p100_prev0.20_cstat0.60_target0.90_mlasso` | 50,000 | 4.10× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.20_cstat0.65_target0.90_mlasso` | 5,724 | `p100_prev0.20_cstat0.65_target0.95_mlasso` | 28,399 | 4.96× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.20_cstat0.85_target0.90_mlasso` | 1,768 | `p100_prev0.20_cstat0.85_target0.95_mlasso` | 11,764 | 6.65× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.25_cstat0.60_target0.90_mlasso` | 17,776 | `p100_prev0.25_cstat0.60_target0.95_mlasso` | 176,420 | 9.92× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.25_cstat0.70_target0.90_mlasso` | 3,606 | `p100_prev0.25_cstat0.70_target0.95_mlasso` | 16,844 | 4.67× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.25_cstat0.80_target0.90_mlasso` | 2,222 | `p100_prev0.25_cstat0.80_target0.95_mlasso` | 16,844 | 7.58× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p100_prev0.25_cstat0.85_target0.90_mlasso` | 1,538 | `p100_prev0.25_cstat0.85_target0.95_mlasso` | 8,422 | 5.48× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.30_cstat0.60_target0.90_mlasso` | 15,322 | `p100_prev0.30_cstat0.60_target0.95_mlasso` | 112,256 | 7.33× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.30_cstat0.65_target0.90_mlasso` | 7,406 | `p100_prev0.30_cstat0.65_target0.95_mlasso` | 56,128 | 7.58× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p100_prev0.40_cstat0.60_target0.90_mlasso` | 13,213 | `p100_prev0.40_cstat0.60_target0.95_mlasso` | 84,224 | 6.37× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.40_cstat0.70_target0.90_mlasso` | 2,779 | `p100_prev0.40_cstat0.70_target0.95_mlasso` | 8,482 | 3.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.40_cstat0.85_target0.90_mlasso` | 1,071 | `p100_prev0.40_cstat0.85_target0.95_mlasso` | 10,528 | 9.83× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p100_prev0.50_cstat0.60_target0.90_mlasso` | 9,317 | `p100_prev0.50_cstat0.60_target0.95_mlasso` | 67,360 | 7.23× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p100_prev0.50_cstat0.65_target0.85_mlasso` | 1,747 | `p100_prev0.50_cstat0.65_target0.90_mlasso` | 5,788 | 3.31× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.05_cstat0.70_target0.90_mlasso` | 8,571 | `p10_prev0.05_cstat0.70_target0.95_mlasso` | 31,977 | 3.73× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.05_cstat0.80_target0.90_mlasso` | 2,435 | `p10_prev0.05_cstat0.80_target0.95_mlasso` | 20,000 | 8.21× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.05_cstat0.85_target0.90_mlasso` | 2,354 | `p10_prev0.05_cstat0.85_target0.95_mlasso` | 9,412 | 4.00× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p10_prev0.07_cstat0.60_target0.85_mlasso` | 6,667 | `p10_prev0.07_cstat0.60_target0.90_mlasso` | 27,647 | 4.15× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p10_prev0.07_cstat0.65_target0.85_mlasso` | 3,077 | `p10_prev0.07_cstat0.65_target0.90_mlasso` | 9,313 | 3.03× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.07_cstat0.65_target0.90_mlasso` | 9,313 | `p10_prev0.07_cstat0.65_target0.95_mlasso` | 36,780 | 3.95× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.07_cstat0.75_target0.90_mlasso` | 3,554 | `p10_prev0.07_cstat0.75_target0.95_mlasso` | 14,216 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.07_cstat0.80_target0.90_mlasso` | 2,155 | `p10_prev0.07_cstat0.80_target0.95_mlasso` | 6,664 | 3.09× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p10_prev0.10_cstat0.60_target0.85_mlasso` | 4,855 | `p10_prev0.10_cstat0.60_target0.90_mlasso` | 22,029 | 4.54× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.10_cstat0.60_target0.90_mlasso` | 22,029 | `p10_prev0.10_cstat0.60_target0.95_mlasso` | 80,000 | 3.63× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.10_cstat0.65_target0.90_mlasso` | 5,791 | `p10_prev0.10_cstat0.65_target0.95_mlasso` | 36,904 | 6.37× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.10_cstat0.70_target0.90_mlasso` | 4,284 | `p10_prev0.10_cstat0.70_target0.95_mlasso` | 34,289 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.10_cstat0.75_target0.90_mlasso` | 2,252 | `p10_prev0.10_cstat0.75_target0.95_mlasso` | 10,664 | 4.74× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.10_cstat0.85_target0.90_mlasso` | 1,176 | `p10_prev0.10_cstat0.85_target0.95_mlasso` | 5,501 | 4.68× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.15_cstat0.60_target0.90_mlasso` | 9,182 | `p10_prev0.15_cstat0.60_target0.95_mlasso` | 53,328 | 5.81× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.15_cstat0.65_target0.90_mlasso` | 4,469 | `p10_prev0.15_cstat0.65_target0.95_mlasso` | 24,616 | 5.51× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.15_cstat0.70_target0.90_mlasso` | 2,899 | `p10_prev0.15_cstat0.70_target0.95_mlasso` | 22,930 | 7.91× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.15_cstat0.75_target0.90_mlasso` | 1,778 | `p10_prev0.15_cstat0.75_target0.95_mlasso` | 9,016 | 5.07× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.15_cstat0.80_target0.90_mlasso` | 1,668 | `p10_prev0.15_cstat0.80_target0.95_mlasso` | 13,344 | 8.00× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p10_prev0.15_cstat0.85_target0.90_mlasso` | 993 | `p10_prev0.15_cstat0.85_target0.95_mlasso` | 6,280 | 6.32× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.20_cstat0.70_target0.90_mlasso` | 1,512 | `p10_prev0.20_cstat0.70_target0.95_mlasso` | 8,818 | 5.83× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.20_cstat0.75_target0.90_mlasso` | 1,337 | `p10_prev0.20_cstat0.75_target0.95_mlasso` | 10,672 | 7.98× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.20_cstat0.80_target0.90_mlasso` | 1,204 | `p10_prev0.20_cstat0.80_target0.95_mlasso` | 3,758 | 3.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.25_cstat0.60_target0.90_mlasso` | 7,219 | `p10_prev0.25_cstat0.60_target0.95_mlasso` | 26,944 | 3.73× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.30_cstat0.60_target0.90_mlasso` | 6,917 | `p10_prev0.30_cstat0.60_target0.95_mlasso` | 44,928 | 6.50× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.30_cstat0.70_target0.90_mlasso` | 1,728 | `p10_prev0.30_cstat0.70_target0.95_mlasso` | 5,616 | 3.25× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.30_cstat0.75_target0.90_mlasso` | 1,480 | `p10_prev0.30_cstat0.75_target0.95_mlasso` | 5,402 | 3.65× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.30_cstat0.80_target0.90_mlasso` | 915 | `p10_prev0.30_cstat0.80_target0.95_mlasso` | 3,703 | 4.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.30_cstat0.85_target0.90_mlasso` | 741 | `p10_prev0.30_cstat0.85_target0.95_mlasso` | 3,166 | 4.27× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.40_cstat0.60_target0.90_mlasso` | 6,514 | `p10_prev0.40_cstat0.60_target0.95_mlasso` | 33,664 | 5.17× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.40_cstat0.65_target0.90_mlasso` | 2,224 | `p10_prev0.40_cstat0.65_target0.95_mlasso` | 8,416 | 3.78× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.40_cstat0.80_target0.90_mlasso` | 552 | `p10_prev0.40_cstat0.80_target0.95_mlasso` | 4,208 | 7.62× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.40_cstat0.85_target0.90_mlasso` | 544 | `p10_prev0.40_cstat0.85_target0.95_mlasso` | 2,614 | 4.81× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.50_cstat0.60_target0.90_mlasso` | 4,597 | `p10_prev0.50_cstat0.60_target0.95_mlasso` | 40,108 | 8.72× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.50_cstat0.65_target0.90_mlasso` | 2,253 | `p10_prev0.50_cstat0.65_target0.95_mlasso` | 13,504 | 5.99× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.50_cstat0.70_target0.90_mlasso` | 1,776 | `p10_prev0.50_cstat0.70_target0.95_mlasso` | 6,910 | 3.89× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.50_cstat0.75_target0.90_mlasso` | 999 | `p10_prev0.50_cstat0.75_target0.95_mlasso` | 4,119 | 4.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.50_cstat0.80_target0.90_mlasso` | 707 | `p10_prev0.50_cstat0.80_target0.95_mlasso` | 2,956 | 4.18× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.50_cstat0.85_target0.90_mlasso` | 490 | `p10_prev0.50_cstat0.85_target0.95_mlasso` | 1,658 | 3.38× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p15_prev0.05_cstat0.60_target0.80_mlasso` | 3,750 | `p15_prev0.05_cstat0.60_target0.85_mlasso` | 16,617 | 4.43× higher | — | previous | previous |
| Target slope: 0.9 → 0.95 | `p15_prev0.05_cstat0.60_target0.90_mlasso` | 19,077 | `p15_prev0.05_cstat0.60_target0.95_mlasso` | 179,983 | 9.43× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.05_cstat0.70_target0.90_mlasso` | 6,605 | `p15_prev0.05_cstat0.70_target0.95_mlasso` | 25,714 | 3.89× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p15_prev0.05_cstat0.75_target0.85_mlasso` | 1,844 | `p15_prev0.05_cstat0.75_target0.90_mlasso` | 8,000 | 4.34× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.05_cstat0.80_target0.90_mlasso` | 3,578 | `p15_prev0.05_cstat0.80_target0.95_mlasso` | 30,000 | 8.38× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p15_prev0.05_cstat0.85_target0.85_mlasso` | 882 | `p15_prev0.05_cstat0.85_target0.90_mlasso` | 3,529 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.05_cstat0.85_target0.90_mlasso` | 3,529 | `p15_prev0.05_cstat0.85_target0.95_mlasso` | 14,116 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.07_cstat0.60_target0.90_mlasso` | 663,604 | `p15_prev0.07_cstat0.60_target0.95_mlasso` | 160,000 | 4.15× lower | — | previous | previous |
| Target slope: 0.9 → 0.95 | `p15_prev0.07_cstat0.65_target0.90_mlasso` | 9,226 | `p15_prev0.07_cstat0.65_target0.95_mlasso` | 36,918 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.07_cstat0.75_target0.90_mlasso` | 2,667 | `p15_prev0.07_cstat0.75_target0.95_mlasso` | 15,330 | 5.75× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.07_cstat0.80_target0.90_mlasso` | 2,125 | `p15_prev0.07_cstat0.80_target0.95_mlasso` | 10,004 | 4.71× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p15_prev0.07_cstat0.85_target0.80_mlasso` | 294 | `p15_prev0.07_cstat0.85_target0.85_mlasso` | 943 | 3.21× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.07_cstat0.85_target0.90_mlasso` | 1,244 | `p15_prev0.07_cstat0.85_target0.95_mlasso` | 6,885 | 5.53× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.10_cstat0.60_target0.90_mlasso` | 14,656 | `p15_prev0.10_cstat0.60_target0.95_mlasso` | 45,917 | 3.13× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p15_prev0.10_cstat0.65_target0.90_mlasso` | 8,179 | `p15_prev0.10_cstat0.65_target0.95_mlasso` | 27,692 | 3.39× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.10_cstat0.70_target0.90_mlasso` | 3,811 | `p15_prev0.10_cstat0.70_target0.95_mlasso` | 25,685 | 6.74× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.10_cstat0.75_target0.90_mlasso` | 2,271 | `p15_prev0.10_cstat0.75_target0.95_mlasso` | 10,489 | 4.62× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p15_prev0.10_cstat0.80_target0.80_mlasso` | 468 | `p15_prev0.10_cstat0.80_target0.85_mlasso` | 1,875 | 4.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.10_cstat0.80_target0.90_mlasso` | 2,159 | `p15_prev0.10_cstat0.80_target0.95_mlasso` | 7,544 | 3.49× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.10_cstat0.85_target0.90_mlasso` | 1,568 | `p15_prev0.10_cstat0.85_target0.95_mlasso` | 5,751 | 3.67× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p15_prev0.15_cstat0.60_target0.85_mlasso` | 4,796 | `p15_prev0.15_cstat0.60_target0.90_mlasso` | 20,000 | 4.17× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.15_cstat0.65_target0.90_mlasso` | 4,965 | `p15_prev0.15_cstat0.65_target0.95_mlasso` | 18,460 | 3.72× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.15_cstat0.70_target0.90_mlasso` | 4,286 | `p15_prev0.15_cstat0.70_target0.95_mlasso` | 34,288 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.15_cstat0.80_target0.90_mlasso` | 955 | `p15_prev0.15_cstat0.80_target0.95_mlasso` | 5,107 | 5.35× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.15_cstat0.85_target0.90_mlasso` | 1,193 | `p15_prev0.15_cstat0.85_target0.95_mlasso` | 4,704 | 3.94× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p15_prev0.20_cstat0.60_target0.80_mlasso` | 1,875 | `p15_prev0.20_cstat0.60_target0.85_mlasso` | 7,500 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.20_cstat0.65_target0.90_mlasso` | 4,142 | `p15_prev0.20_cstat0.65_target0.95_mlasso` | 13,848 | 3.34× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.20_cstat0.70_target0.90_mlasso` | 2,539 | `p15_prev0.20_cstat0.70_target0.95_mlasso` | 9,841 | 3.88× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.20_cstat0.75_target0.90_mlasso` | 1,889 | `p15_prev0.20_cstat0.75_target0.95_mlasso` | 7,234 | 3.83× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.20_cstat0.80_target0.90_mlasso` | 806 | `p15_prev0.20_cstat0.80_target0.95_mlasso` | 4,835 | 6.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.20_cstat0.85_target0.90_mlasso` | 900 | `p15_prev0.20_cstat0.85_target0.95_mlasso` | 3,837 | 4.26× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.25_cstat0.60_target0.90_mlasso` | 10,672 | `p15_prev0.25_cstat0.60_target0.95_mlasso` | 40,448 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.25_cstat0.65_target0.90_mlasso` | 4,156 | `p15_prev0.25_cstat0.65_target0.95_mlasso` | 12,495 | 3.01× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p15_prev0.25_cstat0.70_target0.85_mlasso` | 706 | `p15_prev0.25_cstat0.70_target0.90_mlasso` | 2,668 | 3.78× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.25_cstat0.80_target0.90_mlasso` | 974 | `p15_prev0.25_cstat0.80_target0.95_mlasso` | 5,056 | 5.19× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.30_cstat0.75_target0.90_mlasso` | 991 | `p15_prev0.30_cstat0.75_target0.95_mlasso` | 3,885 | 3.92× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.30_cstat0.80_target0.90_mlasso` | 1,112 | `p15_prev0.30_cstat0.80_target0.95_mlasso` | 3,519 | 3.16× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.30_cstat0.85_target0.90_mlasso` | 646 | `p15_prev0.30_cstat0.85_target0.95_mlasso` | 3,117 | 4.83× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p15_prev0.40_cstat0.65_target0.80_mlasso` | 473 | `p15_prev0.40_cstat0.65_target0.85_mlasso` | 1,764 | 3.73× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.40_cstat0.65_target0.90_mlasso` | 2,917 | `p15_prev0.40_cstat0.65_target0.95_mlasso` | 9,661 | 3.31× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p15_prev0.40_cstat0.70_target0.85_mlasso` | 441 | `p15_prev0.40_cstat0.70_target0.90_mlasso` | 1,721 | 3.90× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.40_cstat0.70_target0.90_mlasso` | 1,721 | `p15_prev0.40_cstat0.70_target0.95_mlasso` | 6,320 | 3.67× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p15_prev0.40_cstat0.75_target0.80_mlasso` | 237 | `p15_prev0.40_cstat0.75_target0.85_mlasso` | 882 | 3.72× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.40_cstat0.80_target0.90_mlasso` | 838 | `p15_prev0.40_cstat0.80_target0.95_mlasso` | 3,398 | 4.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.40_cstat0.85_target0.90_mlasso` | 702 | `p15_prev0.40_cstat0.85_target0.95_mlasso` | 3,160 | 4.50× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.50_cstat0.60_target0.90_mlasso` | 5,315 | `p15_prev0.50_cstat0.60_target0.95_mlasso` | 20,224 | 3.81× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p15_prev0.50_cstat0.65_target0.85_mlasso` | 727 | `p15_prev0.50_cstat0.65_target0.90_mlasso` | 5,328 | 7.33× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.50_cstat0.70_target0.90_mlasso` | 1,488 | `p15_prev0.50_cstat0.70_target0.95_mlasso` | 5,056 | 3.40× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.50_cstat0.75_target0.90_mlasso` | 1,011 | `p15_prev0.50_cstat0.75_target0.95_mlasso` | 3,475 | 3.44× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p15_prev0.50_cstat0.80_target0.85_mlasso` | 370 | `p15_prev0.50_cstat0.80_target0.90_mlasso` | 1,332 | 3.60× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.50_cstat0.85_target0.90_mlasso` | 674 | `p15_prev0.50_cstat0.85_target0.95_mlasso` | 2,528 | 3.75× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p20_prev0.05_cstat0.60_target0.80_mlasso` | 20,000 | `p20_prev0.05_cstat0.60_target0.85_mlasso` | 160,000 | 8.00× higher | — | previous, next | — |
| Target slope: 0.85 → 0.9 | `p20_prev0.05_cstat0.70_target0.85_mlasso` | 4,285 | `p20_prev0.05_cstat0.70_target0.90_mlasso` | 17,143 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.05_cstat0.75_target0.90_mlasso` | 5,333 | `p20_prev0.05_cstat0.75_target0.95_mlasso` | 42,664 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.05_cstat0.80_target0.90_mlasso` | 3,438 | `p20_prev0.05_cstat0.80_target0.95_mlasso` | 18,767 | 5.46× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.07_cstat0.65_target0.90_mlasso` | 6,154 | `p20_prev0.07_cstat0.65_target0.95_mlasso` | 49,232 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.07_cstat0.70_target0.90_mlasso` | 5,714 | `p20_prev0.07_cstat0.70_target0.95_mlasso` | 45,716 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.07_cstat0.75_target0.90_mlasso` | 3,435 | `p20_prev0.07_cstat0.75_target0.95_mlasso` | 14,224 | 4.14× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.07_cstat0.80_target0.90_mlasso` | 3,334 | `p20_prev0.07_cstat0.80_target0.95_mlasso` | 26,672 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.07_cstat0.85_target0.90_mlasso` | 1,826 | `p20_prev0.07_cstat0.85_target0.95_mlasso` | 6,276 | 3.44× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.10_cstat0.65_target0.90_mlasso` | 8,001 | `p20_prev0.10_cstat0.65_target0.95_mlasso` | 36,597 | 4.57× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.10_cstat0.70_target0.90_mlasso` | 4,300 | `p20_prev0.10_cstat0.70_target0.95_mlasso` | 17,142 | 3.99× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.10_cstat0.75_target0.90_mlasso` | 2,373 | `p20_prev0.10_cstat0.75_target0.95_mlasso` | 10,331 | 4.35× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.10_cstat0.80_target0.90_mlasso` | 1,839 | `p20_prev0.10_cstat0.80_target0.95_mlasso` | 10,000 | 5.44× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.10_cstat0.85_target0.90_mlasso` | 1,484 | `p20_prev0.10_cstat0.85_target0.95_mlasso` | 6,605 | 4.45× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.15_cstat0.65_target0.90_mlasso` | 6,159 | `p20_prev0.15_cstat0.65_target0.95_mlasso` | 24,616 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.15_cstat0.70_target0.90_mlasso` | 2,869 | `p20_prev0.15_cstat0.70_target0.95_mlasso` | 11,428 | 3.98× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.15_cstat0.75_target0.90_mlasso` | 1,743 | `p20_prev0.15_cstat0.75_target0.95_mlasso` | 6,663 | 3.82× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.15_cstat0.80_target0.90_mlasso` | 1,480 | `p20_prev0.15_cstat0.80_target0.95_mlasso` | 5,923 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.15_cstat0.85_target0.90_mlasso` | 1,050 | `p20_prev0.15_cstat0.85_target0.95_mlasso` | 4,510 | 4.30× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p20_prev0.20_cstat0.60_target0.80_mlasso` | 2,798 | `p20_prev0.20_cstat0.60_target0.85_mlasso` | 10,000 | 3.57× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.20_cstat0.60_target0.90_mlasso` | 10,000 | `p20_prev0.20_cstat0.60_target0.95_mlasso` | 80,000 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.20_cstat0.70_target0.90_mlasso` | 2,586 | `p20_prev0.20_cstat0.70_target0.95_mlasso` | 8,572 | 3.31× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.20_cstat0.75_target0.90_mlasso` | 1,269 | `p20_prev0.20_cstat0.75_target0.95_mlasso` | 5,088 | 4.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.20_cstat0.80_target0.90_mlasso` | 1,444 | `p20_prev0.20_cstat0.80_target0.95_mlasso` | 4,648 | 3.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.20_cstat0.85_target0.90_mlasso` | 1,176 | `p20_prev0.20_cstat0.85_target0.95_mlasso` | 4,704 | 4.00× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p20_prev0.25_cstat0.60_target0.85_mlasso` | 3,383 | `p20_prev0.25_cstat0.60_target0.90_mlasso` | 14,224 | 4.20× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.25_cstat0.65_target0.90_mlasso` | 3,645 | `p20_prev0.25_cstat0.65_target0.95_mlasso` | 13,472 | 3.70× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p20_prev0.25_cstat0.70_target0.85_mlasso` | 1,068 | `p20_prev0.25_cstat0.70_target0.90_mlasso` | 3,556 | 3.33× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.25_cstat0.70_target0.90_mlasso` | 3,556 | `p20_prev0.25_cstat0.70_target0.95_mlasso` | 13,472 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.25_cstat0.75_target0.90_mlasso` | 1,448 | `p20_prev0.25_cstat0.75_target0.95_mlasso` | 5,980 | 4.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.25_cstat0.80_target0.90_mlasso` | 751 | `p20_prev0.25_cstat0.80_target0.95_mlasso` | 3,642 | 4.85× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.25_cstat0.85_target0.90_mlasso` | 900 | `p20_prev0.25_cstat0.85_target0.95_mlasso` | 3,468 | 3.85× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p20_prev0.30_cstat0.60_target0.80_mlasso` | 1,838 | `p20_prev0.30_cstat0.60_target0.85_mlasso` | 6,280 | 3.42× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.30_cstat0.65_target0.90_mlasso` | 3,224 | `p20_prev0.30_cstat0.65_target0.95_mlasso` | 11,232 | 3.48× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.30_cstat0.75_target0.90_mlasso` | 1,482 | `p20_prev0.30_cstat0.75_target0.95_mlasso` | 4,634 | 3.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.30_cstat0.85_target0.90_mlasso` | 741 | `p20_prev0.30_cstat0.85_target0.95_mlasso` | 2,669 | 3.60× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p20_prev0.40_cstat0.60_target0.85_mlasso` | 2,942 | `p20_prev0.40_cstat0.60_target0.90_mlasso` | 8,896 | 3.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.40_cstat0.60_target0.90_mlasso` | 8,896 | `p20_prev0.40_cstat0.60_target0.95_mlasso` | 50,418 | 5.67× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.40_cstat0.65_target0.90_mlasso` | 3,087 | `p20_prev0.40_cstat0.65_target0.95_mlasso` | 10,568 | 3.42× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.40_cstat0.75_target0.90_mlasso` | 1,112 | `p20_prev0.40_cstat0.75_target0.95_mlasso` | 3,952 | 3.55× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.40_cstat0.80_target0.90_mlasso` | 857 | `p20_prev0.40_cstat0.80_target0.95_mlasso` | 3,157 | 3.68× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.40_cstat0.85_target0.90_mlasso` | 558 | `p20_prev0.40_cstat0.85_target0.95_mlasso` | 2,756 | 4.94× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.50_cstat0.65_target0.90_mlasso` | 3,260 | `p20_prev0.50_cstat0.65_target0.95_mlasso` | 12,057 | 3.70× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.50_cstat0.70_target0.90_mlasso` | 1,776 | `p20_prev0.50_cstat0.70_target0.95_mlasso` | 13,472 | 7.59× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.50_cstat0.80_target0.90_mlasso` | 785 | `p20_prev0.50_cstat0.80_target0.95_mlasso` | 3,368 | 4.29× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p20_prev0.50_cstat0.85_target0.85_mlasso` | 235 | `p20_prev0.50_cstat0.85_target0.90_mlasso` | 762 | 3.24× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.50_cstat0.85_target0.90_mlasso` | 762 | `p20_prev0.50_cstat0.85_target0.95_mlasso` | 2,450 | 3.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.05_cstat0.60_target0.90_mlasso` | 37,339 | `p25_prev0.05_cstat0.60_target0.95_mlasso` | 159,197 | 4.26× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_prev0.05_cstat0.65_target0.85_mlasso` | 5,769 | `p25_prev0.05_cstat0.65_target0.90_mlasso` | 23,137 | 4.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.05_cstat0.75_target0.90_mlasso` | 5,314 | `p25_prev0.05_cstat0.75_target0.95_mlasso` | 20,670 | 3.89× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.05_cstat0.80_target0.90_mlasso` | 3,750 | `p25_prev0.05_cstat0.80_target0.95_mlasso` | 12,500 | 3.33× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.05_cstat0.85_target0.90_mlasso` | 2,941 | `p25_prev0.05_cstat0.85_target0.95_mlasso` | 11,989 | 4.08× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.07_cstat0.65_target0.90_mlasso` | 12,016 | `p25_prev0.07_cstat0.65_target0.95_mlasso` | 61,540 | 5.12× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.07_cstat0.70_target0.90_mlasso` | 7,183 | `p25_prev0.07_cstat0.70_target0.95_mlasso` | 27,866 | 3.88× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.07_cstat0.75_target0.90_mlasso` | 3,935 | `p25_prev0.07_cstat0.75_target0.95_mlasso` | 18,661 | 4.74× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.07_cstat0.80_target0.90_mlasso` | 2,083 | `p25_prev0.07_cstat0.80_target0.95_mlasso` | 11,423 | 5.48× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p25_prev0.07_cstat0.85_target0.80_mlasso` | 245 | `p25_prev0.07_cstat0.85_target0.85_mlasso` | 938 | 3.83× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.07_cstat0.85_target0.90_mlasso` | 1,819 | `p25_prev0.07_cstat0.85_target0.95_mlasso` | 8,103 | 4.45× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_prev0.10_cstat0.60_target0.85_mlasso` | 100,000 | `p25_prev0.10_cstat0.60_target0.90_mlasso` | 25,000 | 4.00× lower | — | previous | previous |
| Target slope: 0.9 → 0.95 | `p25_prev0.10_cstat0.65_target0.90_mlasso` | 5,769 | `p25_prev0.10_cstat0.65_target0.95_mlasso` | 29,338 | 5.09× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.10_cstat0.75_target0.90_mlasso` | 3,203 | `p25_prev0.10_cstat0.75_target0.95_mlasso` | 13,332 | 4.16× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_prev0.10_cstat0.80_target0.85_mlasso` | 589 | `p25_prev0.10_cstat0.80_target0.90_mlasso` | 1,929 | 3.28× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.10_cstat0.80_target0.90_mlasso` | 1,929 | `p25_prev0.10_cstat0.80_target0.95_mlasso` | 12,500 | 6.48× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.10_cstat0.85_target0.90_mlasso` | 1,542 | `p25_prev0.10_cstat0.85_target0.95_mlasso` | 5,056 | 3.28× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.15_cstat0.60_target0.90_mlasso` | 16,691 | `p25_prev0.15_cstat0.60_target0.95_mlasso` | 133,328 | 7.99× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.15_cstat0.70_target0.90_mlasso` | 3,607 | `p25_prev0.15_cstat0.70_target0.95_mlasso` | 13,389 | 3.71× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.15_cstat0.75_target0.90_mlasso` | 2,233 | `p25_prev0.15_cstat0.75_target0.95_mlasso` | 8,892 | 3.98× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.15_cstat0.80_target0.90_mlasso` | 1,666 | `p25_prev0.15_cstat0.80_target0.95_mlasso` | 5,993 | 3.60× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.15_cstat0.85_target0.90_mlasso` | 980 | `p25_prev0.15_cstat0.85_target0.95_mlasso` | 4,051 | 4.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.20_cstat0.60_target0.90_mlasso` | 12,500 | `p25_prev0.20_cstat0.60_target0.95_mlasso` | 37,846 | 3.03× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_prev0.20_cstat0.70_target0.85_mlasso` | 1,333 | `p25_prev0.20_cstat0.70_target0.90_mlasso` | 5,357 | 4.02× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_prev0.20_cstat0.75_target0.85_mlasso` | 894 | `p25_prev0.20_cstat0.75_target0.90_mlasso` | 3,334 | 3.73× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.20_cstat0.80_target0.90_mlasso` | 1,676 | `p25_prev0.20_cstat0.80_target0.95_mlasso` | 5,545 | 3.31× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.20_cstat0.85_target0.90_mlasso` | 802 | `p25_prev0.20_cstat0.85_target0.95_mlasso` | 3,011 | 3.75× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.25_cstat0.60_target0.90_mlasso` | 8,750 | `p25_prev0.25_cstat0.60_target0.95_mlasso` | 67,392 | 7.70× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.25_cstat0.65_target0.90_mlasso` | 3,640 | `p25_prev0.25_cstat0.65_target0.95_mlasso` | 16,228 | 4.46× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.25_cstat0.70_target0.90_mlasso` | 2,160 | `p25_prev0.25_cstat0.70_target0.95_mlasso` | 8,273 | 3.83× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.25_cstat0.75_target0.90_mlasso` | 1,781 | `p25_prev0.25_cstat0.75_target0.95_mlasso` | 16,848 | 9.46× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p25_prev0.25_cstat0.80_target0.90_mlasso` | 1,114 | `p25_prev0.25_cstat0.80_target0.95_mlasso` | 4,488 | 4.03× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.25_cstat0.85_target0.90_mlasso` | 871 | `p25_prev0.25_cstat0.85_target0.95_mlasso` | 3,984 | 4.57× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.30_cstat0.65_target0.90_mlasso` | 3,384 | `p25_prev0.30_cstat0.65_target0.95_mlasso` | 28,064 | 8.29× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.30_cstat0.70_target0.90_mlasso` | 1,950 | `p25_prev0.30_cstat0.70_target0.95_mlasso` | 7,100 | 3.64× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.30_cstat0.75_target0.90_mlasso` | 931 | `p25_prev0.30_cstat0.75_target0.95_mlasso` | 7,016 | 7.54× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.30_cstat0.80_target0.90_mlasso` | 926 | `p25_prev0.30_cstat0.80_target0.95_mlasso` | 3,910 | 4.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.30_cstat0.85_target0.90_mlasso` | 926 | `p25_prev0.30_cstat0.85_target0.95_mlasso` | 3,508 | 3.79× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p25_prev0.40_cstat0.60_target0.80_mlasso` | 1,564 | `p25_prev0.40_cstat0.60_target0.85_mlasso` | 5,880 | 3.76× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.40_cstat0.65_target0.90_mlasso` | 2,973 | `p25_prev0.40_cstat0.65_target0.95_mlasso` | 9,935 | 3.34× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.40_cstat0.70_target0.90_mlasso` | 1,672 | `p25_prev0.40_cstat0.70_target0.95_mlasso` | 7,375 | 4.41× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.40_cstat0.75_target0.90_mlasso` | 1,299 | `p25_prev0.40_cstat0.75_target0.95_mlasso` | 5,272 | 4.06× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.40_cstat0.80_target0.90_mlasso` | 694 | `p25_prev0.40_cstat0.80_target0.95_mlasso` | 3,966 | 5.71× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.40_cstat0.85_target0.90_mlasso` | 694 | `p25_prev0.40_cstat0.85_target0.95_mlasso` | 2,692 | 3.88× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_prev0.50_cstat0.60_target0.85_mlasso` | 2,352 | `p25_prev0.50_cstat0.60_target0.90_mlasso` | 8,896 | 3.78× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.50_cstat0.70_target0.90_mlasso` | 1,436 | `p25_prev0.50_cstat0.70_target0.95_mlasso` | 7,141 | 4.97× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.50_cstat0.75_target0.90_mlasso` | 1,112 | `p25_prev0.50_cstat0.75_target0.95_mlasso` | 4,758 | 4.28× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.50_cstat0.80_target0.90_mlasso` | 939 | `p25_prev0.50_cstat0.80_target0.95_mlasso` | 3,485 | 3.71× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.50_cstat0.85_target0.90_mlasso` | 611 | `p25_prev0.50_cstat0.85_target0.95_mlasso` | 3,201 | 5.24× higher | — | next | — |
| Target slope: 0.8 → 0.9 | `p30_prev0.05_cstat0.60_target0.80_mlasso` | 8,352 | `p30_prev0.05_cstat0.60_target0.90_mlasso` | 41,593 | 4.98× higher | Yes | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.05_cstat0.75_target0.90_mlasso` | 4,266 | `p30_prev0.05_cstat0.75_target0.95_mlasso` | 28,299 | 6.63× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.05_cstat0.80_target0.90_mlasso` | 3,522 | `p30_prev0.05_cstat0.80_target0.95_mlasso` | 15,000 | 4.26× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.05_cstat0.85_target0.90_mlasso` | 3,221 | `p30_prev0.05_cstat0.85_target0.95_mlasso` | 14,173 | 4.40× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p30_prev0.07_cstat0.60_target0.85_mlasso` | 15,448 | `p30_prev0.07_cstat0.60_target0.90_mlasso` | 80,000 | 5.18× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.07_cstat0.65_target0.90_mlasso` | 18,462 | `p30_prev0.07_cstat0.65_target0.95_mlasso` | 73,848 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.07_cstat0.70_target0.90_mlasso` | 6,146 | `p30_prev0.07_cstat0.70_target0.95_mlasso` | 25,523 | 4.15× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.07_cstat0.75_target0.90_mlasso` | 5,333 | `p30_prev0.07_cstat0.75_target0.95_mlasso` | 21,569 | 4.04× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.07_cstat0.80_target0.90_mlasso` | 2,368 | `p30_prev0.07_cstat0.80_target0.95_mlasso` | 11,892 | 5.02× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.07_cstat0.85_target0.90_mlasso` | 2,353 | `p30_prev0.07_cstat0.85_target0.95_mlasso` | 9,196 | 3.91× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p30_prev0.10_cstat0.70_target0.85_mlasso` | 2,133 | `p30_prev0.10_cstat0.70_target0.90_mlasso` | 6,428 | 3.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.10_cstat0.70_target0.90_mlasso` | 6,428 | `p30_prev0.10_cstat0.70_target0.95_mlasso` | 19,986 | 3.11× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.10_cstat0.75_target0.90_mlasso` | 3,081 | `p30_prev0.10_cstat0.75_target0.95_mlasso` | 15,329 | 4.98× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p30_prev0.10_cstat0.80_target0.85_mlasso` | 1,139 | `p30_prev0.10_cstat0.80_target0.90_mlasso` | 3,750 | 3.29× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.10_cstat0.85_target0.90_mlasso` | 1,485 | `p30_prev0.10_cstat0.85_target0.95_mlasso` | 7,058 | 4.75× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.15_cstat0.65_target0.90_mlasso` | 4,631 | `p30_prev0.15_cstat0.65_target0.95_mlasso` | 18,244 | 3.94× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.15_cstat0.70_target0.90_mlasso` | 4,285 | `p30_prev0.15_cstat0.70_target0.95_mlasso` | 14,370 | 3.35× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.15_cstat0.75_target0.90_mlasso` | 2,198 | `p30_prev0.15_cstat0.75_target0.95_mlasso` | 10,751 | 4.89× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p30_prev0.15_cstat0.80_target0.85_mlasso` | 717 | `p30_prev0.15_cstat0.80_target0.90_mlasso` | 2,500 | 3.49× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.20_cstat0.65_target0.90_mlasso` | 5,022 | `p30_prev0.20_cstat0.65_target0.95_mlasso` | 22,586 | 4.50× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.20_cstat0.70_target0.90_mlasso` | 2,881 | `p30_prev0.20_cstat0.70_target0.95_mlasso` | 12,153 | 4.22× higher | — | — | — |
| Target slope: 0.8 → 0.9 | `p30_prev0.20_cstat0.75_target0.80_mlasso` | 500 | `p30_prev0.20_cstat0.75_target0.90_mlasso` | 2,000 | 4.00× higher | Yes | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.20_cstat0.80_target0.90_mlasso` | 1,875 | `p30_prev0.20_cstat0.80_target0.95_mlasso` | 7,500 | 4.00× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p30_prev0.20_cstat0.85_target0.85_mlasso` | 423 | `p30_prev0.20_cstat0.85_target0.90_mlasso` | 1,765 | 4.17× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.25_cstat0.70_target0.90_mlasso` | 2,771 | `p30_prev0.25_cstat0.70_target0.95_mlasso` | 9,585 | 3.46× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.25_cstat0.75_target0.90_mlasso` | 1,154 | `p30_prev0.25_cstat0.75_target0.95_mlasso` | 3,921 | 3.40× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.25_cstat0.80_target0.90_mlasso` | 1,343 | `p30_prev0.25_cstat0.80_target0.95_mlasso` | 5,074 | 3.78× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.25_cstat0.85_target0.90_mlasso` | 1,007 | `p30_prev0.25_cstat0.85_target0.95_mlasso` | 3,777 | 3.75× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.30_cstat0.60_target0.90_mlasso` | 8,888 | `p30_prev0.30_cstat0.60_target0.95_mlasso` | 67,392 | 7.58× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.30_cstat0.65_target0.90_mlasso` | 3,307 | `p30_prev0.30_cstat0.65_target0.95_mlasso` | 13,905 | 4.20× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.30_cstat0.70_target0.90_mlasso` | 2,222 | `p30_prev0.30_cstat0.70_target0.95_mlasso` | 8,424 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.30_cstat0.75_target0.90_mlasso` | 1,384 | `p30_prev0.30_cstat0.75_target0.95_mlasso` | 5,023 | 3.63× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.30_cstat0.80_target0.90_mlasso` | 1,111 | `p30_prev0.30_cstat0.80_target0.95_mlasso` | 4,865 | 4.38× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.30_cstat0.85_target0.90_mlasso` | 1,111 | `p30_prev0.30_cstat0.85_target0.95_mlasso` | 4,212 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.40_cstat0.60_target0.90_mlasso` | 6,320 | `p30_prev0.40_cstat0.60_target0.95_mlasso` | 50,496 | 7.99× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.40_cstat0.65_target0.90_mlasso` | 3,332 | `p30_prev0.40_cstat0.65_target0.95_mlasso` | 14,463 | 4.34× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.40_cstat0.70_target0.90_mlasso` | 1,975 | `p30_prev0.40_cstat0.70_target0.95_mlasso` | 7,734 | 3.92× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.40_cstat0.80_target0.90_mlasso` | 843 | `p30_prev0.40_cstat0.80_target0.95_mlasso` | 2,955 | 3.51× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.40_cstat0.85_target0.90_mlasso` | 600 | `p30_prev0.40_cstat0.85_target0.95_mlasso` | 3,380 | 5.63× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p30_prev0.50_cstat0.60_target0.80_mlasso` | 1,256 | `p30_prev0.50_cstat0.60_target0.85_mlasso` | 3,924 | 3.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.50_cstat0.60_target0.90_mlasso` | 5,179 | `p30_prev0.50_cstat0.60_target0.95_mlasso` | 40,438 | 7.81× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.50_cstat0.65_target0.90_mlasso` | 2,702 | `p30_prev0.50_cstat0.65_target0.95_mlasso` | 13,977 | 5.17× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p30_prev0.50_cstat0.70_target0.80_mlasso` | 187 | `p30_prev0.50_cstat0.70_target0.85_mlasso` | 921 | 4.93× higher | — | previous | previous |
| Target slope: 0.9 → 0.95 | `p30_prev0.50_cstat0.70_target0.90_mlasso` | 1,838 | `p30_prev0.50_cstat0.70_target0.95_mlasso` | 7,300 | 3.97× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.50_cstat0.75_target0.90_mlasso` | 1,213 | `p30_prev0.50_cstat0.75_target0.95_mlasso` | 4,244 | 3.50× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.50_cstat0.80_target0.90_mlasso` | 952 | `p30_prev0.50_cstat0.80_target0.95_mlasso` | 3,930 | 4.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.50_cstat0.85_target0.90_mlasso` | 802 | `p30_prev0.50_cstat0.85_target0.95_mlasso` | 3,287 | 4.10× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.05_cstat0.60_target0.90_mlasso` | 31,458 | `p40_prev0.05_cstat0.60_target0.95_mlasso` | 160,000 | 5.09× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p40_prev0.05_cstat0.65_target0.85_mlasso` | 9,582 | `p40_prev0.05_cstat0.65_target0.90_mlasso` | 73,846 | 7.71× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p40_prev0.05_cstat0.70_target0.90_mlasso` | 10,205 | `p40_prev0.05_cstat0.70_target0.95_mlasso` | 60,079 | 5.89× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.05_cstat0.80_target0.90_mlasso` | 5,000 | `p40_prev0.05_cstat0.80_target0.95_mlasso` | 18,713 | 3.74× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.05_cstat0.85_target0.90_mlasso` | 2,734 | `p40_prev0.05_cstat0.85_target0.95_mlasso` | 18,824 | 6.89× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.07_cstat0.65_target0.90_mlasso` | 13,452 | `p40_prev0.07_cstat0.65_target0.95_mlasso` | 46,929 | 3.49× higher | — | previous | previous |
| Target slope: 0.9 → 0.95 | `p40_prev0.07_cstat0.75_target0.90_mlasso` | 3,674 | `p40_prev0.07_cstat0.75_target0.95_mlasso` | 24,671 | 6.72× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.07_cstat0.80_target0.90_mlasso` | 2,778 | `p40_prev0.07_cstat0.80_target0.95_mlasso` | 26,664 | 9.60× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p40_prev0.07_cstat0.85_target0.80_mlasso` | 392 | `p40_prev0.07_cstat0.85_target0.85_mlasso` | 1,568 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.07_cstat0.85_target0.90_mlasso` | 2,091 | `p40_prev0.07_cstat0.85_target0.95_mlasso` | 9,877 | 4.72× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.10_cstat0.60_target0.90_mlasso` | 40,000 | `p40_prev0.10_cstat0.60_target0.95_mlasso` | 160,000 | 4.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.10_cstat0.65_target0.90_mlasso` | 9,231 | `p40_prev0.10_cstat0.65_target0.95_mlasso` | 27,693 | 3.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.10_cstat0.75_target0.90_mlasso` | 2,804 | `p40_prev0.10_cstat0.75_target0.95_mlasso` | 15,039 | 5.36× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.10_cstat0.80_target0.90_mlasso` | 2,500 | `p40_prev0.10_cstat0.80_target0.95_mlasso` | 20,000 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.15_cstat0.65_target0.90_mlasso` | 6,154 | `p40_prev0.15_cstat0.65_target0.95_mlasso` | 49,232 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.15_cstat0.75_target0.90_mlasso` | 1,666 | `p40_prev0.15_cstat0.75_target0.95_mlasso` | 10,334 | 6.20× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.15_cstat0.80_target0.90_mlasso` | 1,697 | `p40_prev0.15_cstat0.80_target0.95_mlasso` | 7,375 | 4.35× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.20_cstat0.60_target0.90_mlasso` | 14,960 | `p40_prev0.20_cstat0.60_target0.95_mlasso` | 50,005 | 3.34× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.20_cstat0.65_target0.90_mlasso` | 5,001 | `p40_prev0.20_cstat0.65_target0.95_mlasso` | 36,924 | 7.38× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.20_cstat0.70_target0.90_mlasso` | 4,285 | `p40_prev0.20_cstat0.70_target0.95_mlasso` | 17,142 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.20_cstat0.75_target0.90_mlasso` | 1,747 | `p40_prev0.20_cstat0.75_target0.95_mlasso` | 10,668 | 6.11× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p40_prev0.20_cstat0.80_target0.85_mlasso` | 410 | `p40_prev0.20_cstat0.80_target0.90_mlasso` | 1,521 | 3.71× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.20_cstat0.80_target0.90_mlasso` | 1,521 | `p40_prev0.20_cstat0.80_target0.95_mlasso` | 5,659 | 3.72× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.20_cstat0.85_target0.90_mlasso` | 1,073 | `p40_prev0.20_cstat0.85_target0.95_mlasso` | 5,148 | 4.80× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.25_cstat0.60_target0.90_mlasso` | 12,270 | `p40_prev0.25_cstat0.60_target0.95_mlasso` | 44,295 | 3.61× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.25_cstat0.65_target0.90_mlasso` | 3,287 | `p40_prev0.25_cstat0.65_target0.95_mlasso` | 17,515 | 5.33× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.25_cstat0.70_target0.90_mlasso` | 2,464 | `p40_prev0.25_cstat0.70_target0.95_mlasso` | 10,862 | 4.41× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.25_cstat0.75_target0.90_mlasso` | 1,564 | `p40_prev0.25_cstat0.75_target0.95_mlasso` | 6,182 | 3.95× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.25_cstat0.80_target0.90_mlasso` | 1,296 | `p40_prev0.25_cstat0.80_target0.95_mlasso` | 4,710 | 3.63× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.25_cstat0.85_target0.90_mlasso` | 1,030 | `p40_prev0.25_cstat0.85_target0.95_mlasso` | 6,736 | 6.54× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.30_cstat0.70_target0.90_mlasso` | 2,287 | `p40_prev0.30_cstat0.70_target0.95_mlasso` | 11,241 | 4.92× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.30_cstat0.75_target0.90_mlasso` | 1,570 | `p40_prev0.30_cstat0.75_target0.95_mlasso` | 6,578 | 4.19× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.30_cstat0.80_target0.90_mlasso` | 1,089 | `p40_prev0.30_cstat0.80_target0.95_mlasso` | 5,621 | 5.16× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.40_cstat0.65_target0.90_mlasso` | 3,496 | `p40_prev0.40_cstat0.65_target0.95_mlasso` | 16,848 | 4.82× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.40_cstat0.70_target0.90_mlasso` | 1,947 | `p40_prev0.40_cstat0.70_target0.95_mlasso` | 7,781 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.40_cstat0.75_target0.90_mlasso` | 1,147 | `p40_prev0.40_cstat0.75_target0.95_mlasso` | 5,128 | 4.47× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.40_cstat0.80_target0.90_mlasso` | 991 | `p40_prev0.40_cstat0.80_target0.95_mlasso` | 4,212 | 4.25× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.40_cstat0.85_target0.90_mlasso` | 915 | `p40_prev0.40_cstat0.85_target0.95_mlasso` | 4,128 | 4.51× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.50_cstat0.60_target0.90_mlasso` | 7,112 | `p40_prev0.50_cstat0.60_target0.95_mlasso` | 24,031 | 3.38× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.50_cstat0.65_target0.90_mlasso` | 3,721 | `p40_prev0.50_cstat0.65_target0.95_mlasso` | 13,203 | 3.55× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.50_cstat0.70_target0.90_mlasso` | 1,716 | `p40_prev0.50_cstat0.70_target0.95_mlasso` | 5,962 | 3.47× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p40_prev0.50_cstat0.75_target0.85_mlasso` | 941 | `p40_prev0.50_cstat0.75_target0.90_mlasso` | 3,556 | 3.78× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p40_prev0.50_cstat0.80_target0.90_mlasso` | 967 | `p40_prev0.50_cstat0.80_target0.95_mlasso` | 4,289 | 4.44× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.50_cstat0.85_target0.90_mlasso` | 889 | `p40_prev0.50_cstat0.85_target0.95_mlasso` | 3,665 | 4.12× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p50_prev0.05_cstat0.60_target0.80_mlasso` | 7,851 | `p50_prev0.05_cstat0.60_target0.85_mlasso` | 34,649 | 4.41× higher | — | previous | previous |
| Target slope: 0.9 → 0.95 | `p50_prev0.05_cstat0.65_target0.90_mlasso` | 23,077 | `p50_prev0.05_cstat0.65_target0.95_mlasso` | 92,308 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.05_cstat0.70_target0.90_mlasso` | 11,438 | `p50_prev0.05_cstat0.70_target0.95_mlasso` | 42,857 | 3.75× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.05_cstat0.75_target0.90_mlasso` | 6,493 | `p50_prev0.05_cstat0.75_target0.95_mlasso` | 35,122 | 5.41× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.05_cstat0.80_target0.90_mlasso` | 4,659 | `p50_prev0.05_cstat0.80_target0.95_mlasso` | 19,242 | 4.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.05_cstat0.85_target0.90_mlasso` | 3,374 | `p50_prev0.05_cstat0.85_target0.95_mlasso` | 11,765 | 3.49× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.07_cstat0.65_target0.90_mlasso` | 11,708 | `p50_prev0.07_cstat0.65_target0.95_mlasso` | 49,628 | 4.24× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.07_cstat0.75_target0.90_mlasso` | 3,890 | `p50_prev0.07_cstat0.75_target0.95_mlasso` | 35,556 | 9.14× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.07_cstat0.80_target0.90_mlasso` | 3,569 | `p50_prev0.07_cstat0.80_target0.95_mlasso` | 16,852 | 4.72× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.07_cstat0.85_target0.90_mlasso` | 2,389 | `p50_prev0.07_cstat0.85_target0.95_mlasso` | 13,970 | 5.85× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.10_cstat0.65_target0.90_mlasso` | 10,723 | `p50_prev0.10_cstat0.65_target0.95_mlasso` | 46,154 | 4.30× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.10_cstat0.70_target0.90_mlasso` | 5,755 | `p50_prev0.10_cstat0.70_target0.95_mlasso` | 19,009 | 3.30× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p50_prev0.10_cstat0.75_target0.90_mlasso` | 3,334 | `p50_prev0.10_cstat0.75_target0.95_mlasso` | 13,294 | 3.99× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.10_cstat0.80_target0.90_mlasso` | 2,069 | `p50_prev0.10_cstat0.80_target0.95_mlasso` | 12,864 | 6.22× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.15_cstat0.60_target0.90_mlasso` | 19,549 | `p50_prev0.15_cstat0.60_target0.95_mlasso` | 133,336 | 6.82× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.15_cstat0.65_target0.90_mlasso` | 7,877 | `p50_prev0.15_cstat0.65_target0.95_mlasso` | 59,404 | 7.54× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.15_cstat0.80_target0.90_mlasso` | 2,083 | `p50_prev0.15_cstat0.80_target0.95_mlasso` | 8,921 | 4.28× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.15_cstat0.85_target0.90_mlasso` | 1,483 | `p50_prev0.15_cstat0.85_target0.95_mlasso` | 5,677 | 3.83× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.20_cstat0.60_target0.90_mlasso` | 16,524 | `p50_prev0.20_cstat0.60_target0.95_mlasso` | 78,632 | 4.76× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.20_cstat0.65_target0.90_mlasso` | 5,774 | `p50_prev0.20_cstat0.65_target0.95_mlasso` | 46,152 | 7.99× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.20_cstat0.70_target0.90_mlasso` | 3,445 | `p50_prev0.20_cstat0.70_target0.95_mlasso` | 11,284 | 3.28× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.20_cstat0.80_target0.90_mlasso` | 781 | `p50_prev0.20_cstat0.80_target0.95_mlasso` | 7,571 | 9.69× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p50_prev0.20_cstat0.85_target0.85_mlasso` | 674 | `p50_prev0.20_cstat0.85_target0.90_mlasso` | 2,941 | 4.36× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.25_cstat0.60_target0.90_mlasso` | 4,219 | `p50_prev0.25_cstat0.60_target0.95_mlasso` | 33,626 | 7.97× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.25_cstat0.65_target0.90_mlasso` | 4,277 | `p50_prev0.25_cstat0.65_target0.95_mlasso` | 16,265 | 3.80× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.25_cstat0.70_target0.90_mlasso` | 2,689 | `p50_prev0.25_cstat0.70_target0.95_mlasso` | 13,276 | 4.94× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.25_cstat0.75_target0.90_mlasso` | 2,222 | `p50_prev0.25_cstat0.75_target0.95_mlasso` | 8,460 | 3.81× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.25_cstat0.80_target0.90_mlasso` | 1,099 | `p50_prev0.25_cstat0.80_target0.95_mlasso` | 8,420 | 7.66× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.25_cstat0.85_target0.90_mlasso` | 1,114 | `p50_prev0.25_cstat0.85_target0.95_mlasso` | 4,210 | 3.78× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.30_cstat0.60_target0.90_mlasso` | 7,408 | `p50_prev0.30_cstat0.60_target0.95_mlasso` | 28,080 | 3.79× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p50_prev0.30_cstat0.65_target0.85_mlasso` | 981 | `p50_prev0.30_cstat0.65_target0.90_mlasso` | 4,764 | 4.86× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.30_cstat0.70_target0.90_mlasso` | 2,933 | `p50_prev0.30_cstat0.70_target0.95_mlasso` | 14,040 | 4.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.30_cstat0.75_target0.90_mlasso` | 1,509 | `p50_prev0.30_cstat0.75_target0.95_mlasso` | 7,054 | 4.67× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.30_cstat0.85_target0.90_mlasso` | 957 | `p50_prev0.30_cstat0.85_target0.95_mlasso` | 3,499 | 3.66× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.40_cstat0.60_target0.90_mlasso` | 11,112 | `p50_prev0.40_cstat0.60_target0.95_mlasso` | 42,111 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.40_cstat0.65_target0.90_mlasso` | 3,724 | `p50_prev0.40_cstat0.65_target0.95_mlasso` | 21,056 | 5.65× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p50_prev0.40_cstat0.75_target0.80_mlasso` | 195 | `p50_prev0.40_cstat0.75_target0.85_mlasso` | 791 | 4.06× higher | — | previous | previous |
| Target slope: 0.9 → 0.95 | `p50_prev0.40_cstat0.75_target0.90_mlasso` | 1,287 | `p50_prev0.40_cstat0.75_target0.95_mlasso` | 6,731 | 5.23× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.40_cstat0.80_target0.90_mlasso` | 1,090 | `p50_prev0.40_cstat0.80_target0.95_mlasso` | 5,264 | 4.83× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.40_cstat0.85_target0.90_mlasso` | 1,389 | `p50_prev0.40_cstat0.85_target0.95_mlasso` | 5,264 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.50_cstat0.60_target0.90_mlasso` | 9,020 | `p50_prev0.50_cstat0.60_target0.95_mlasso` | 52,574 | 5.83× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.50_cstat0.65_target0.90_mlasso` | 3,795 | `p50_prev0.50_cstat0.65_target0.95_mlasso` | 16,538 | 4.36× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.50_cstat0.70_target0.90_mlasso` | 2,222 | `p50_prev0.50_cstat0.70_target0.95_mlasso` | 9,376 | 4.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.50_cstat0.75_target0.90_mlasso` | 1,111 | `p50_prev0.50_cstat0.75_target0.95_mlasso` | 4,375 | 3.94× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.50_cstat0.80_target0.90_mlasso` | 1,050 | `p50_prev0.50_cstat0.80_target0.95_mlasso` | 4,082 | 3.89× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.50_cstat0.85_target0.90_mlasso` | 909 | `p50_prev0.50_cstat0.85_target0.95_mlasso` | 3,318 | 3.65× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p5_prev0.05_cstat0.65_target0.80_mlasso` | 2,842 | `p5_prev0.05_cstat0.65_target0.85_mlasso` | 9,230 | 3.25× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.05_cstat0.65_target0.90_mlasso` | 8,968 | `p5_prev0.05_cstat0.65_target0.95_mlasso` | 73,840 | 8.23× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.05_cstat0.70_target0.90_mlasso` | 4,211 | `p5_prev0.05_cstat0.70_target0.95_mlasso` | 34,288 | 8.14× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.05_cstat0.75_target0.85_mlasso` | 1,646 | `p5_prev0.05_cstat0.75_target0.90_mlasso` | 5,332 | 3.24× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.05_cstat0.85_target0.90_mlasso` | 2,352 | `p5_prev0.05_cstat0.85_target0.95_mlasso` | 18,816 | 8.00× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p5_prev0.07_cstat0.60_target0.80_mlasso` | 4,268 | `p5_prev0.07_cstat0.60_target0.85_mlasso` | 13,332 | 3.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.07_cstat0.60_target0.90_mlasso` | 13,332 | `p5_prev0.07_cstat0.60_target0.95_mlasso` | 106,656 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.07_cstat0.65_target0.90_mlasso` | 6,154 | `p5_prev0.07_cstat0.65_target0.95_mlasso` | 24,616 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.07_cstat0.75_target0.90_mlasso` | 2,853 | `p5_prev0.07_cstat0.75_target0.95_mlasso` | 19,776 | 6.93× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.07_cstat0.80_target0.85_mlasso` | 885 | `p5_prev0.07_cstat0.80_target0.90_mlasso` | 3,336 | 3.77× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.07_cstat0.80_target0.90_mlasso` | 3,336 | `p5_prev0.07_cstat0.80_target0.95_mlasso` | 13,344 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.07_cstat0.85_target0.90_mlasso` | 1,326 | `p5_prev0.07_cstat0.85_target0.95_mlasso` | 4,761 | 3.59× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.10_cstat0.65_target0.90_mlasso` | 4,422 | `p5_prev0.10_cstat0.65_target0.95_mlasso` | 36,928 | 8.35× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p5_prev0.10_cstat0.80_target0.90_mlasso` | 1,565 | `p5_prev0.10_cstat0.80_target0.95_mlasso` | 8,011 | 5.12× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.10_cstat0.85_target0.90_mlasso` | 1,176 | `p5_prev0.10_cstat0.85_target0.95_mlasso` | 6,778 | 5.76× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.15_cstat0.65_target0.85_mlasso` | 1,538 | `p5_prev0.15_cstat0.65_target0.90_mlasso` | 4,976 | 3.24× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.15_cstat0.70_target0.90_mlasso` | 2,858 | `p5_prev0.15_cstat0.70_target0.95_mlasso` | 11,432 | 4.00× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.15_cstat0.75_target0.85_mlasso` | 444 | `p5_prev0.15_cstat0.75_target0.90_mlasso` | 1,566 | 3.53× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.15_cstat0.75_target0.90_mlasso` | 1,566 | `p5_prev0.15_cstat0.75_target0.95_mlasso` | 7,968 | 5.09× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.15_cstat0.80_target0.90_mlasso` | 1,437 | `p5_prev0.15_cstat0.80_target0.95_mlasso` | 6,821 | 4.75× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.15_cstat0.85_target0.90_mlasso` | 963 | `p5_prev0.15_cstat0.85_target0.95_mlasso` | 6,282 | 6.52× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.20_cstat0.60_target0.85_mlasso` | 2,977 | `p5_prev0.20_cstat0.60_target0.90_mlasso` | 10,000 | 3.36× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.20_cstat0.60_target0.90_mlasso` | 10,000 | `p5_prev0.20_cstat0.60_target0.95_mlasso` | 40,000 | 4.00× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.20_cstat0.65_target0.85_mlasso` | 1,143 | `p5_prev0.20_cstat0.65_target0.90_mlasso` | 3,912 | 3.42× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.20_cstat0.65_target0.90_mlasso` | 3,912 | `p5_prev0.20_cstat0.65_target0.95_mlasso` | 29,846 | 7.63× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.20_cstat0.70_target0.85_mlasso` | 1,071 | `p5_prev0.20_cstat0.70_target0.90_mlasso` | 4,284 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.20_cstat0.75_target0.90_mlasso` | 1,332 | `p5_prev0.20_cstat0.75_target0.95_mlasso` | 7,741 | 5.81× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.20_cstat0.80_target0.90_mlasso` | 624 | `p5_prev0.20_cstat0.80_target0.95_mlasso` | 5,089 | 8.16× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.20_cstat0.85_target0.90_mlasso` | 612 | `p5_prev0.20_cstat0.85_target0.95_mlasso` | 4,704 | 7.69× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.25_cstat0.65_target0.85_mlasso` | 967 | `p5_prev0.25_cstat0.65_target0.90_mlasso` | 7,104 | 7.35× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.25_cstat0.65_target0.90_mlasso` | 7,104 | `p5_prev0.25_cstat0.65_target0.95_mlasso` | 27,008 | 3.80× higher | — | previous | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.25_cstat0.70_target0.90_mlasso` | 1,776 | `p5_prev0.25_cstat0.70_target0.95_mlasso` | 6,752 | 3.80× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.25_cstat0.75_target0.90_mlasso` | 799 | `p5_prev0.25_cstat0.75_target0.95_mlasso` | 3,376 | 4.23× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.25_cstat0.85_target0.90_mlasso` | 914 | `p5_prev0.25_cstat0.85_target0.95_mlasso` | 3,526 | 3.86× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.30_cstat0.65_target0.90_mlasso` | 2,759 | `p5_prev0.30_cstat0.65_target0.95_mlasso` | 17,612 | 6.38× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.30_cstat0.75_target0.90_mlasso` | 1,071 | `p5_prev0.30_cstat0.75_target0.95_mlasso` | 5,632 | 5.26× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.30_cstat0.80_target0.90_mlasso` | 740 | `p5_prev0.30_cstat0.80_target0.95_mlasso` | 3,649 | 4.93× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.30_cstat0.85_target0.90_mlasso` | 709 | `p5_prev0.30_cstat0.85_target0.95_mlasso` | 2,747 | 3.87× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.40_cstat0.60_target0.85_mlasso` | 2,269 | `p5_prev0.40_cstat0.60_target0.90_mlasso` | 8,896 | 3.92× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.40_cstat0.70_target0.90_mlasso` | 1,112 | `p5_prev0.40_cstat0.70_target0.95_mlasso` | 3,554 | 3.20× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.40_cstat0.75_target0.90_mlasso` | 894 | `p5_prev0.40_cstat0.75_target0.95_mlasso` | 4,547 | 5.09× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.40_cstat0.80_target0.90_mlasso` | 717 | `p5_prev0.40_cstat0.80_target0.95_mlasso` | 3,497 | 4.88× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.40_cstat0.85_target0.90_mlasso` | 616 | `p5_prev0.40_cstat0.85_target0.95_mlasso` | 4,224 | 6.86× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.50_cstat0.60_target0.85_mlasso` | 1,511 | `p5_prev0.50_cstat0.60_target0.90_mlasso` | 5,524 | 3.66× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.50_cstat0.60_target0.90_mlasso` | 5,524 | `p5_prev0.50_cstat0.60_target0.95_mlasso` | 26,880 | 4.87× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.50_cstat0.65_target0.85_mlasso` | 962 | `p5_prev0.50_cstat0.65_target0.90_mlasso` | 3,552 | 3.69× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p5_prev0.50_cstat0.70_target0.80_mlasso` | 252 | `p5_prev0.50_cstat0.70_target0.85_mlasso` | 944 | 3.75× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.50_cstat0.70_target0.90_mlasso` | 1,776 | `p5_prev0.50_cstat0.70_target0.95_mlasso` | 6,500 | 3.66× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.50_cstat0.80_target0.85_mlasso` | 291 | `p5_prev0.50_cstat0.80_target0.90_mlasso` | 888 | 3.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.50_cstat0.85_target0.90_mlasso` | 615 | `p5_prev0.50_cstat0.85_target0.95_mlasso` | 3,360 | 5.46× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.05_cstat0.65_target0.90_mlasso` | 22,812 | `p75_prev0.05_cstat0.65_target0.95_mlasso` | 138,462 | 6.07× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.05_cstat0.85_target0.90_mlasso` | 4,236 | `p75_prev0.05_cstat0.85_target0.95_mlasso` | 17,647 | 4.17× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p75_prev0.07_cstat0.70_target0.80_mlasso` | 2,678 | `p75_prev0.07_cstat0.70_target0.85_mlasso` | 10,714 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.07_cstat0.80_target0.90_mlasso` | 4,222 | `p75_prev0.07_cstat0.80_target0.95_mlasso` | 25,000 | 5.92× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.07_cstat0.85_target0.90_mlasso` | 2,947 | `p75_prev0.07_cstat0.85_target0.95_mlasso` | 11,243 | 3.82× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.10_cstat0.60_target0.90_mlasso` | 28,106 | `p75_prev0.10_cstat0.60_target0.95_mlasso` | 150,000 | 5.34× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.10_cstat0.65_target0.90_mlasso` | 8,558 | `p75_prev0.10_cstat0.65_target0.95_mlasso` | 34,615 | 4.04× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.10_cstat0.85_target0.90_mlasso` | 2,279 | `p75_prev0.10_cstat0.85_target0.95_mlasso` | 17,648 | 7.74× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.15_cstat0.60_target0.90_mlasso` | 12,500 | `p75_prev0.15_cstat0.60_target0.95_mlasso` | 100,000 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.15_cstat0.65_target0.90_mlasso` | 8,662 | `p75_prev0.15_cstat0.65_target0.95_mlasso` | 34,254 | 3.95× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.15_cstat0.75_target0.90_mlasso` | 3,333 | `p75_prev0.15_cstat0.75_target0.95_mlasso` | 11,880 | 3.56× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.15_cstat0.85_target0.90_mlasso` | 1,468 | `p75_prev0.15_cstat0.85_target0.95_mlasso` | 6,378 | 4.34× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.20_cstat0.65_target0.90_mlasso` | 4,327 | `p75_prev0.20_cstat0.65_target0.95_mlasso` | 28,108 | 6.50× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.20_cstat0.70_target0.90_mlasso` | 4,123 | `p75_prev0.20_cstat0.70_target0.95_mlasso` | 16,071 | 3.90× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.20_cstat0.80_target0.90_mlasso` | 1,053 | `p75_prev0.20_cstat0.80_target0.95_mlasso` | 7,370 | 7.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.20_cstat0.85_target0.90_mlasso` | 1,103 | `p75_prev0.20_cstat0.85_target0.95_mlasso` | 6,873 | 6.23× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p75_prev0.25_cstat0.60_target0.80_mlasso` | 2,764 | `p75_prev0.25_cstat0.60_target0.85_mlasso` | 8,959 | 3.24× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.25_cstat0.60_target0.90_mlasso` | 12,167 | `p75_prev0.25_cstat0.60_target0.95_mlasso` | 50,528 | 4.15× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.25_cstat0.70_target0.90_mlasso` | 3,520 | `p75_prev0.25_cstat0.70_target0.95_mlasso` | 11,549 | 3.28× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.25_cstat0.75_target0.90_mlasso` | 2,251 | `p75_prev0.25_cstat0.75_target0.95_mlasso` | 12,632 | 5.61× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.25_cstat0.80_target0.90_mlasso` | 1,594 | `p75_prev0.25_cstat0.80_target0.95_mlasso` | 6,268 | 3.93× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.30_cstat0.60_target0.90_mlasso` | 12,436 | `p75_prev0.30_cstat0.60_target0.95_mlasso` | 61,348 | 4.93× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.30_cstat0.65_target0.90_mlasso` | 5,351 | `p75_prev0.30_cstat0.65_target0.95_mlasso` | 20,600 | 3.85× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.30_cstat0.75_target0.90_mlasso` | 2,222 | `p75_prev0.30_cstat0.75_target0.95_mlasso` | 6,936 | 3.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.30_cstat0.80_target0.90_mlasso` | 1,415 | `p75_prev0.30_cstat0.80_target0.95_mlasso` | 5,264 | 3.72× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.30_cstat0.85_target0.90_mlasso` | 886 | `p75_prev0.30_cstat0.85_target0.95_mlasso` | 5,679 | 6.41× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.40_cstat0.60_target0.90_mlasso` | 12,081 | `p75_prev0.40_cstat0.60_target0.95_mlasso` | 63,168 | 5.23× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.40_cstat0.80_target0.90_mlasso` | 1,352 | `p75_prev0.40_cstat0.80_target0.95_mlasso` | 7,896 | 5.84× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.50_cstat0.70_target0.90_mlasso` | 2,576 | `p75_prev0.50_cstat0.70_target0.95_mlasso` | 12,632 | 4.90× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.50_cstat0.75_target0.90_mlasso` | 1,818 | `p75_prev0.50_cstat0.75_target0.95_mlasso` | 5,522 | 3.04× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.50_cstat0.80_target0.90_mlasso` | 1,350 | `p75_prev0.50_cstat0.80_target0.95_mlasso` | 6,316 | 4.68× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.50_cstat0.85_target0.90_mlasso` | 948 | `p75_prev0.50_cstat0.85_target0.95_mlasso` | 6,316 | 6.66× higher | — | next | — |
| C-statistic: 0.7 → 0.75 | `p100_prev0.05_cstat0.70_target0.90_mridge` | 42,557 | `p100_prev0.05_cstat0.75_target0.90_mridge` | 10,000 | 4.26× lower | — | next | next |
| C-statistic: 0.75 → 0.8 | `p100_prev0.05_cstat0.75_target0.90_mridge` | 10,000 | `p100_prev0.05_cstat0.80_target0.90_mridge` | 50,000 | 5.00× higher | — | previous, next | previous |
| C-statistic: 0.75 → 0.8 | `p100_prev0.05_cstat0.75_target0.95_mridge` | 152,826 | `p100_prev0.05_cstat0.80_target0.95_mridge` | 50,000 | 3.06× lower | — | previous | — |
| C-statistic: 0.8 → 0.85 | `p100_prev0.05_cstat0.80_target0.85_mridge` | 25,000 | `p100_prev0.05_cstat0.85_target0.85_mridge` | 7,272 | 3.44× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p100_prev0.07_cstat0.65_target0.80_mridge` | 30,769 | `p100_prev0.07_cstat0.70_target0.80_mridge` | 7,226 | 4.26× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p100_prev0.07_cstat0.75_target0.90_mridge` | 71,108 | `p100_prev0.07_cstat0.80_target0.90_mridge` | 17,871 | 3.98× lower | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p100_prev0.10_cstat0.60_target0.85_mridge` | 50,000 | `p100_prev0.10_cstat0.65_target0.85_mridge` | 11,532 | 4.34× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p100_prev0.10_cstat0.70_target0.85_mridge` | 14,030 | `p100_prev0.10_cstat0.75_target0.85_mridge` | 3,218 | 4.36× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p100_prev0.10_cstat0.70_target0.95_mridge` | 21,028 | `p100_prev0.10_cstat0.75_target0.95_mridge` | 106,664 | 5.07× higher | — | next | next |
| C-statistic: 0.75 → 0.8 | `p100_prev0.10_cstat0.75_target0.95_mridge` | 106,664 | `p100_prev0.10_cstat0.80_target0.95_mridge` | 25,000 | 4.27× lower | — | previous | previous |
| C-statistic: 0.65 → 0.7 | `p100_prev0.20_cstat0.65_target0.80_mridge` | 6,446 | `p100_prev0.20_cstat0.70_target0.80_mridge` | 2,082 | 3.10× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p100_prev0.20_cstat0.65_target0.90_mridge` | 92,308 | `p100_prev0.20_cstat0.70_target0.90_mridge` | 21,429 | 4.31× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p100_prev0.25_cstat0.60_target0.80_mridge` | 20,000 | `p100_prev0.25_cstat0.65_target0.80_mridge` | 5,560 | 3.60× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p100_prev0.25_cstat0.70_target0.95_mridge` | 134,752 | `p100_prev0.25_cstat0.75_target0.95_mridge` | 28,985 | 4.65× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p100_prev0.30_cstat0.60_target0.80_mridge` | 16,664 | `p100_prev0.30_cstat0.65_target0.80_mridge` | 4,166 | 4.00× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p100_prev0.30_cstat0.60_target0.90_mridge` | 60,702 | `p100_prev0.30_cstat0.65_target0.90_mridge` | 14,512 | 4.18× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p100_prev0.40_cstat0.75_target0.90_mridge` | 22,224 | `p100_prev0.40_cstat0.80_target0.90_mridge` | 5,592 | 3.97× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p100_prev0.50_cstat0.60_target0.85_mridge` | 18,824 | `p100_prev0.50_cstat0.65_target0.85_mridge` | 4,706 | 4.00× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p100_prev0.50_cstat0.60_target0.95_mridge` | 196,585 | `p100_prev0.50_cstat0.65_target0.95_mridge` | 33,680 | 5.84× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p10_prev0.05_cstat0.60_target0.80_mridge` | 40,000 | `p10_prev0.05_cstat0.65_target0.80_mridge` | 6,743 | 5.93× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p10_prev0.05_cstat0.70_target0.90_mridge` | 34,284 | `p10_prev0.05_cstat0.75_target0.90_mridge` | 8,265 | 4.15× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p10_prev0.05_cstat0.70_target0.95_mridge` | 68,519 | `p10_prev0.05_cstat0.75_target0.95_mridge` | 20,770 | 3.30× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p10_prev0.05_cstat0.75_target0.85_mridge` | 4,784 | `p10_prev0.05_cstat0.80_target0.85_mridge` | 1,250 | 3.83× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p10_prev0.07_cstat0.70_target0.80_mridge` | 5,714 | `p10_prev0.07_cstat0.75_target0.80_mridge` | 1,777 | 3.22× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p10_prev0.07_cstat0.70_target0.90_mridge` | 22,856 | `p10_prev0.07_cstat0.75_target0.90_mridge` | 7,108 | 3.22× lower | — | previous | previous |
| C-statistic: 0.7 → 0.75 | `p10_prev0.07_cstat0.70_target0.95_mridge` | 65,945 | `p10_prev0.07_cstat0.75_target0.95_mridge` | 12,994 | 5.08× lower | — | previous, next | next |
| C-statistic: 0.75 → 0.8 | `p10_prev0.07_cstat0.75_target0.95_mridge` | 12,994 | `p10_prev0.07_cstat0.80_target0.95_mridge` | 82,409 | 6.34× higher | — | previous, next | previous, next |
| C-statistic: 0.8 → 0.85 | `p10_prev0.07_cstat0.80_target0.90_mridge` | 4,726 | `p10_prev0.07_cstat0.85_target0.90_mridge` | 1,171 | 4.04× lower | — | — | — |
| C-statistic: 0.8 → 0.85 | `p10_prev0.07_cstat0.80_target0.95_mridge` | 82,409 | `p10_prev0.07_cstat0.85_target0.95_mridge` | 18,916 | 4.36× lower | — | previous, next | previous |
| C-statistic: 0.7 → 0.75 | `p10_prev0.10_cstat0.70_target0.95_mridge` | 68,576 | `p10_prev0.10_cstat0.75_target0.95_mridge` | 21,328 | 3.22× lower | — | previous, next | next |
| C-statistic: 0.75 → 0.8 | `p10_prev0.10_cstat0.75_target0.95_mridge` | 21,328 | `p10_prev0.10_cstat0.80_target0.95_mridge` | 80,000 | 3.75× higher | — | previous, next | previous, next |
| C-statistic: 0.8 → 0.85 | `p10_prev0.10_cstat0.80_target0.95_mridge` | 80,000 | `p10_prev0.10_cstat0.85_target0.95_mridge` | 18,816 | 4.25× lower | — | previous, next | previous |
| C-statistic: 0.7 → 0.75 | `p10_prev0.15_cstat0.70_target0.95_mridge` | 45,712 | `p10_prev0.15_cstat0.75_target0.95_mridge` | 15,210 | 3.01× lower | — | previous, next | — |
| C-statistic: 0.75 → 0.8 | `p10_prev0.15_cstat0.75_target0.80_mridge` | 1,778 | `p10_prev0.15_cstat0.80_target0.80_mridge` | 417 | 4.26× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p10_prev0.20_cstat0.65_target0.85_mridge` | 9,232 | `p10_prev0.20_cstat0.70_target0.85_mridge` | 2,377 | 3.88× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p10_prev0.20_cstat0.65_target0.95_mridge` | 36,928 | `p10_prev0.20_cstat0.70_target0.95_mridge` | 11,757 | 3.14× lower | — | next | next |
| C-statistic: 0.8 → 0.85 | `p10_prev0.20_cstat0.80_target0.90_mridge` | 2,500 | `p10_prev0.20_cstat0.85_target0.90_mridge` | 9,408 | 3.76× higher | — | next | next |
| C-statistic: 0.6 → 0.65 | `p10_prev0.25_cstat0.60_target0.95_mridge` | 40,550 | `p10_prev0.25_cstat0.65_target0.95_mridge` | 12,466 | 3.25× lower | — | next | next |
| C-statistic: 0.65 → 0.7 | `p10_prev0.25_cstat0.65_target0.95_mridge` | 12,466 | `p10_prev0.25_cstat0.70_target0.95_mridge` | 53,888 | 4.32× higher | — | previous, next | previous, next |
| C-statistic: 0.7 → 0.75 | `p10_prev0.25_cstat0.70_target0.90_mridge` | 7,104 | `p10_prev0.25_cstat0.75_target0.90_mridge` | 2,317 | 3.07× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p10_prev0.25_cstat0.70_target0.95_mridge` | 53,888 | `p10_prev0.25_cstat0.75_target0.95_mridge` | 14,428 | 3.73× lower | — | previous, next | previous |
| C-statistic: 0.6 → 0.65 | `p10_prev0.30_cstat0.60_target0.95_mridge` | 44,928 | `p10_prev0.30_cstat0.65_target0.95_mridge` | 11,232 | 4.00× lower | — | previous | — |
| C-statistic: 0.8 → 0.85 | `p10_prev0.30_cstat0.80_target0.95_mridge` | 11,232 | `p10_prev0.30_cstat0.85_target0.95_mridge` | 2,808 | 4.00× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p10_prev0.40_cstat0.60_target0.80_mridge` | 4,992 | `p10_prev0.40_cstat0.65_target0.80_mridge` | 1,550 | 3.22× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p10_prev0.40_cstat0.60_target0.95_mridge` | 67,328 | `p10_prev0.40_cstat0.65_target0.95_mridge` | 12,624 | 5.33× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p10_prev0.50_cstat0.60_target0.95_mridge` | 13,504 | `p10_prev0.50_cstat0.65_target0.95_mridge` | 65,550 | 4.85× higher | — | next | next |
| C-statistic: 0.75 → 0.8 | `p10_prev0.50_cstat0.75_target0.95_mridge` | 6,664 | `p10_prev0.50_cstat0.80_target0.95_mridge` | 1,688 | 3.95× lower | — | next | next |
| C-statistic: 0.65 → 0.7 | `p15_prev0.05_cstat0.65_target0.85_mridge` | 27,692 | `p15_prev0.05_cstat0.70_target0.85_mridge` | 7,972 | 3.47× lower | — | — | — |
| C-statistic: 0.8 → 0.85 | `p15_prev0.05_cstat0.80_target0.95_mridge` | 240,000 | `p15_prev0.05_cstat0.85_target0.95_mridge` | 56,464 | 4.25× lower | — | previous, next | previous, next |
| C-statistic: 0.6 → 0.65 | `p15_prev0.07_cstat0.60_target0.85_mridge` | 80,000 | `p15_prev0.07_cstat0.65_target0.85_mridge` | 11,567 | 6.92× lower | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p15_prev0.07_cstat0.60_target0.90_mridge` | 74,865 | `p15_prev0.07_cstat0.65_target0.90_mridge` | 18,462 | 4.06× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p15_prev0.07_cstat0.70_target0.95_mridge` | 137,308 | `p15_prev0.07_cstat0.75_target0.95_mridge` | 31,130 | 4.41× lower | — | previous, next | — |
| C-statistic: 0.75 → 0.8 | `p15_prev0.07_cstat0.75_target0.85_mridge` | 10,668 | `p15_prev0.07_cstat0.80_target0.85_mridge` | 2,882 | 3.70× lower | — | previous | previous |
| C-statistic: 0.75 → 0.8 | `p15_prev0.07_cstat0.75_target0.95_mridge` | 31,130 | `p15_prev0.07_cstat0.80_target0.95_mridge` | 9,634 | 3.23× lower | — | previous, next | next |
| C-statistic: 0.8 → 0.85 | `p15_prev0.07_cstat0.80_target0.95_mridge` | 9,634 | `p15_prev0.07_cstat0.85_target0.95_mridge` | 37,648 | 3.91× higher | — | previous, next | previous |
| C-statistic: 0.6 → 0.65 | `p15_prev0.10_cstat0.60_target0.95_mridge` | 240,000 | `p15_prev0.10_cstat0.65_target0.95_mridge` | 27,692 | 8.67× lower | — | previous, next | next |
| C-statistic: 0.65 → 0.7 | `p15_prev0.10_cstat0.65_target0.95_mridge` | 27,692 | `p15_prev0.10_cstat0.70_target0.95_mridge` | 205,728 | 7.43× higher | — | previous, next | previous |
| C-statistic: 0.6 → 0.65 | `p15_prev0.15_cstat0.60_target0.85_mridge` | 40,000 | `p15_prev0.15_cstat0.65_target0.85_mridge` | 9,230 | 4.33× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.15_cstat0.60_target0.90_mridge` | 39,873 | `p15_prev0.15_cstat0.65_target0.90_mridge` | 9,947 | 4.01× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p15_prev0.15_cstat0.65_target0.95_mridge` | 29,787 | `p15_prev0.15_cstat0.70_target0.95_mridge` | 137,152 | 4.60× higher | — | next | — |
| C-statistic: 0.7 → 0.75 | `p15_prev0.15_cstat0.70_target0.85_mridge` | 4,286 | `p15_prev0.15_cstat0.75_target0.85_mridge` | 1,196 | 3.58× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p15_prev0.15_cstat0.70_target0.90_mridge` | 8,084 | `p15_prev0.15_cstat0.75_target0.90_mridge` | 2,588 | 3.12× lower | — | — | — |
| C-statistic: 0.8 → 0.85 | `p15_prev0.15_cstat0.80_target0.95_mridge` | 20,000 | `p15_prev0.15_cstat0.85_target0.95_mridge` | 3,861 | 5.18× lower | — | previous, next | next |
| C-statistic: 0.65 → 0.7 | `p15_prev0.20_cstat0.65_target0.95_mridge` | 55,392 | `p15_prev0.20_cstat0.70_target0.95_mridge` | 17,044 | 3.25× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p15_prev0.25_cstat0.60_target0.85_mridge` | 45,184 | `p15_prev0.25_cstat0.65_target0.85_mridge` | 5,648 | 8.00× lower | — | previous | previous |
| C-statistic: 0.65 → 0.7 | `p15_prev0.25_cstat0.65_target0.80_mridge` | 2,405 | `p15_prev0.25_cstat0.70_target0.80_mridge` | 709 | 3.39× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p15_prev0.25_cstat0.70_target0.95_mridge` | 40,448 | `p15_prev0.25_cstat0.75_target0.95_mridge` | 5,056 | 8.00× lower | — | previous | — |
| C-statistic: 0.8 → 0.85 | `p15_prev0.25_cstat0.80_target0.90_mridge` | 2,694 | `p15_prev0.25_cstat0.85_target0.90_mridge` | 10,672 | 3.96× higher | — | next | next |
| C-statistic: 0.6 → 0.65 | `p15_prev0.30_cstat0.60_target0.95_mridge` | 134,656 | `p15_prev0.30_cstat0.65_target0.95_mridge` | 31,104 | 4.33× lower | — | previous | — |
| C-statistic: 0.8 → 0.85 | `p15_prev0.30_cstat0.80_target0.95_mridge` | 8,416 | `p15_prev0.30_cstat0.85_target0.95_mridge` | 2,104 | 4.00× lower | — | next | next |
| C-statistic: 0.6 → 0.65 | `p15_prev0.40_cstat0.60_target0.95_mridge` | 202,240 | `p15_prev0.40_cstat0.65_target0.95_mridge` | 24,272 | 8.33× lower | — | previous | previous |
| C-statistic: 0.65 → 0.7 | `p15_prev0.40_cstat0.65_target0.85_mridge` | 7,056 | `p15_prev0.40_cstat0.70_target0.85_mridge` | 1,732 | 4.07× lower | — | previous | previous |
| C-statistic: 0.65 → 0.75 | `p15_prev0.50_cstat0.65_target0.90_mridge` | 10,656 | `p15_prev0.50_cstat0.75_target0.90_mridge` | 2,664 | 4.00× lower | Yes | — | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.05_cstat0.60_target0.80_mridge` | 40,000 | `p20_prev0.05_cstat0.65_target0.80_mridge` | 9,231 | 4.33× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.05_cstat0.60_target0.85_mridge` | 80,000 | `p20_prev0.05_cstat0.65_target0.85_mridge` | 21,908 | 3.65× lower | — | previous | previous |
| C-statistic: 0.6 → 0.7 | `p20_prev0.05_cstat0.60_target0.90_mridge` | 160,000 | `p20_prev0.05_cstat0.70_target0.90_mridge` | 17,143 | 9.33× lower | Yes | previous | — |
| C-statistic: 0.7 → 0.75 | `p20_prev0.05_cstat0.70_target0.95_mridge` | 34,286 | `p20_prev0.05_cstat0.75_target0.95_mridge` | 170,656 | 4.98× higher | — | next | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.07_cstat0.60_target0.80_mridge` | 26,666 | `p20_prev0.07_cstat0.65_target0.80_mridge` | 8,091 | 3.30× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p20_prev0.07_cstat0.75_target0.90_mridge` | 28,448 | `p20_prev0.07_cstat0.80_target0.90_mridge` | 6,668 | 4.27× lower | — | previous | previous |
| C-statistic: 0.75 → 0.8 | `p20_prev0.07_cstat0.75_target0.95_mridge` | 27,376 | `p20_prev0.07_cstat0.80_target0.95_mridge` | 106,688 | 3.90× higher | — | next | — |
| C-statistic: 0.65 → 0.7 | `p20_prev0.10_cstat0.65_target0.95_mridge` | 73,848 | `p20_prev0.10_cstat0.70_target0.95_mridge` | 17,142 | 4.31× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.15_cstat0.60_target0.85_mridge` | 26,668 | `p20_prev0.15_cstat0.65_target0.85_mridge` | 7,070 | 3.77× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.15_cstat0.60_target0.90_mridge` | 53,336 | `p20_prev0.15_cstat0.65_target0.90_mridge` | 11,856 | 4.50× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.15_cstat0.60_target0.95_mridge` | 38,008 | `p20_prev0.15_cstat0.65_target0.95_mridge` | 256,742 | 6.75× higher | — | next | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.20_cstat0.60_target0.90_mridge` | 23,782 | `p20_prev0.20_cstat0.65_target0.90_mridge` | 4,615 | 5.15× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.25_cstat0.60_target0.80_mridge` | 8,000 | `p20_prev0.25_cstat0.65_target0.80_mridge` | 2,441 | 3.28× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p20_prev0.25_cstat0.65_target0.95_mridge` | 107,776 | `p20_prev0.25_cstat0.70_target0.95_mridge` | 18,327 | 5.88× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.30_cstat0.60_target0.80_mridge` | 6,672 | `p20_prev0.30_cstat0.65_target0.80_mridge` | 1,246 | 5.35× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p20_prev0.30_cstat0.65_target0.85_mridge` | 6,280 | `p20_prev0.30_cstat0.70_target0.85_mridge` | 1,570 | 4.00× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p20_prev0.30_cstat0.65_target0.95_mridge` | 44,928 | `p20_prev0.30_cstat0.70_target0.95_mridge` | 10,893 | 4.12× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.40_cstat0.60_target0.80_mridge` | 5,179 | `p20_prev0.40_cstat0.65_target0.80_mridge` | 1,250 | 4.14× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.40_cstat0.60_target0.85_mridge` | 9,408 | `p20_prev0.40_cstat0.65_target0.85_mridge` | 2,318 | 4.06× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p20_prev0.40_cstat0.60_target0.90_mridge` | 35,584 | `p20_prev0.40_cstat0.65_target0.90_mridge` | 7,932 | 4.49× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p20_prev0.50_cstat0.65_target0.90_mridge` | 14,208 | `p20_prev0.50_cstat0.70_target0.90_mridge` | 4,012 | 3.54× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p20_prev0.50_cstat0.65_target0.95_mridge` | 24,598 | `p20_prev0.50_cstat0.70_target0.95_mridge` | 6,559 | 3.75× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.05_cstat0.60_target0.80_mridge` | 50,000 | `p25_prev0.05_cstat0.65_target0.80_mridge` | 13,207 | 3.79× lower | — | — | — |
| C-statistic: 0.6 → 0.7 | `p25_prev0.05_cstat0.60_target0.90_mridge` | 149,087 | `p25_prev0.05_cstat0.70_target0.90_mridge` | 28,455 | 5.24× lower | Yes | previous | — |
| C-statistic: 0.7 → 0.75 | `p25_prev0.05_cstat0.70_target0.80_mridge` | 10,714 | `p25_prev0.05_cstat0.75_target0.80_mridge` | 3,561 | 3.01× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p25_prev0.05_cstat0.70_target0.85_mridge` | 21,429 | `p25_prev0.05_cstat0.75_target0.85_mridge` | 6,900 | 3.11× lower | — | — | — |
| C-statistic: 0.8 → 0.85 | `p25_prev0.05_cstat0.80_target0.95_mridge` | 50,000 | `p25_prev0.05_cstat0.85_target0.95_mridge` | 11,555 | 4.33× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.07_cstat0.60_target0.90_mridge` | 533,344 | `p25_prev0.07_cstat0.65_target0.90_mridge` | 61,540 | 8.67× lower | — | previous | previous |
| C-statistic: 0.65 → 0.7 | `p25_prev0.07_cstat0.65_target0.90_mridge` | 61,540 | `p25_prev0.07_cstat0.70_target0.90_mridge` | 14,396 | 4.27× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p25_prev0.07_cstat0.75_target0.90_mridge` | 35,552 | `p25_prev0.07_cstat0.80_target0.90_mridge` | 3,989 | 8.91× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p25_prev0.10_cstat0.65_target0.80_mridge` | 11,538 | `p25_prev0.10_cstat0.70_target0.80_mridge` | 2,678 | 4.31× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p25_prev0.10_cstat0.65_target0.90_mridge` | 46,152 | `p25_prev0.10_cstat0.70_target0.90_mridge` | 11,713 | 3.94× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p25_prev0.10_cstat0.70_target0.85_mridge` | 10,714 | `p25_prev0.10_cstat0.75_target0.85_mridge` | 3,460 | 3.10× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p25_prev0.10_cstat0.70_target0.95_mridge` | 42,314 | `p25_prev0.10_cstat0.75_target0.95_mridge` | 13,332 | 3.17× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p25_prev0.10_cstat0.75_target0.80_mridge` | 2,426 | `p25_prev0.10_cstat0.80_target0.80_mridge` | 606 | 4.00× lower | — | — | — |
| C-statistic: 0.8 → 0.85 | `p25_prev0.10_cstat0.80_target0.95_mridge` | 19,509 | `p25_prev0.10_cstat0.85_target0.95_mridge` | 5,882 | 3.32× lower | — | next | next |
| C-statistic: 0.6 → 0.65 | `p25_prev0.15_cstat0.60_target0.85_mridge` | 33,332 | `p25_prev0.15_cstat0.65_target0.85_mridge` | 5,225 | 6.38× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.15_cstat0.60_target0.90_mridge` | 52,057 | `p25_prev0.15_cstat0.65_target0.90_mridge` | 15,384 | 3.38× lower | — | — | — |
| C-statistic: 0.8 → 0.85 | `p25_prev0.15_cstat0.80_target0.95_mridge` | 10,295 | `p25_prev0.15_cstat0.85_target0.95_mridge` | 31,376 | 3.05× higher | — | next | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.20_cstat0.60_target0.80_mridge` | 6,250 | `p25_prev0.20_cstat0.65_target0.80_mridge` | 1,442 | 4.33× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.20_cstat0.60_target0.95_mridge` | 400,000 | `p25_prev0.20_cstat0.65_target0.95_mridge` | 46,152 | 8.67× lower | — | previous | previous |
| C-statistic: 0.7 → 0.75 | `p25_prev0.20_cstat0.70_target0.90_mridge` | 21,428 | `p25_prev0.20_cstat0.75_target0.90_mridge` | 3,334 | 6.43× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p25_prev0.20_cstat0.70_target0.95_mridge` | 85,712 | `p25_prev0.20_cstat0.75_target0.95_mridge` | 16,315 | 5.25× lower | — | previous | previous |
| C-statistic: 0.75 → 0.8 | `p25_prev0.20_cstat0.75_target0.95_mridge` | 16,315 | `p25_prev0.20_cstat0.80_target0.95_mridge` | 49,984 | 3.06× higher | — | next | next |
| C-statistic: 0.65 → 0.7 | `p25_prev0.25_cstat0.65_target0.95_mridge` | 134,784 | `p25_prev0.25_cstat0.70_target0.95_mridge` | 22,118 | 6.09× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.30_cstat0.60_target0.85_mridge` | 15,680 | `p25_prev0.30_cstat0.65_target0.85_mridge` | 5,093 | 3.08× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p25_prev0.30_cstat0.60_target0.95_mridge` | 112,256 | `p25_prev0.30_cstat0.65_target0.95_mridge` | 28,064 | 4.00× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p25_prev0.30_cstat0.65_target0.80_mridge` | 4,043 | `p25_prev0.30_cstat0.70_target0.80_mridge` | 1,088 | 3.72× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p25_prev0.30_cstat0.75_target0.95_mridge` | 56,128 | `p25_prev0.30_cstat0.80_target0.95_mridge` | 7,016 | 8.00× lower | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p25_prev0.40_cstat0.60_target0.95_mridge` | 336,896 | `p25_prev0.40_cstat0.65_target0.95_mridge` | 42,112 | 8.00× lower | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p25_prev0.50_cstat0.60_target0.95_mridge` | 134,656 | `p25_prev0.50_cstat0.65_target0.95_mridge` | 23,488 | 5.73× lower | — | previous | — |
| C-statistic: 0.75 → 0.8 | `p25_prev0.50_cstat0.75_target0.95_mridge` | 9,655 | `p25_prev0.50_cstat0.80_target0.95_mridge` | 34,030 | 3.52× higher | — | next | next |
| C-statistic: 0.8 → 0.85 | `p25_prev0.50_cstat0.80_target0.95_mridge` | 34,030 | `p25_prev0.50_cstat0.85_target0.95_mridge` | 11,026 | 3.09× lower | — | previous, next | previous |
| C-statistic: 0.6 → 0.65 | `p30_prev0.05_cstat0.60_target0.80_mridge` | 39,169 | `p30_prev0.05_cstat0.65_target0.80_mridge` | 12,185 | 3.21× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.07_cstat0.60_target0.80_mridge` | 40,000 | `p30_prev0.07_cstat0.65_target0.80_mridge` | 8,715 | 4.59× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.07_cstat0.60_target0.85_mridge` | 80,000 | `p30_prev0.07_cstat0.65_target0.85_mridge` | 9,018 | 8.87× lower | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p30_prev0.07_cstat0.60_target0.95_mridge` | 320,000 | `p30_prev0.07_cstat0.65_target0.95_mridge` | 70,909 | 4.51× lower | — | previous | previous |
| C-statistic: 0.65 → 0.7 | `p30_prev0.07_cstat0.65_target0.90_mridge` | 73,848 | `p30_prev0.07_cstat0.70_target0.90_mridge` | 18,934 | 3.90× lower | — | previous | — |
| C-statistic: 0.75 → 0.8 | `p30_prev0.07_cstat0.75_target0.95_mridge` | 10,666 | `p30_prev0.07_cstat0.80_target0.95_mridge` | 40,000 | 3.75× higher | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p30_prev0.10_cstat0.60_target0.80_mridge` | 16,911 | `p30_prev0.10_cstat0.65_target0.80_mridge` | 3,153 | 5.36× lower | — | previous, next | next |
| C-statistic: 0.6 → 0.65 | `p30_prev0.10_cstat0.60_target0.95_mridge` | 120,000 | `p30_prev0.10_cstat0.65_target0.95_mridge` | 37,806 | 3.17× lower | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p30_prev0.15_cstat0.60_target0.80_mridge` | 20,000 | `p30_prev0.15_cstat0.65_target0.80_mridge` | 4,626 | 4.32× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p30_prev0.15_cstat0.65_target0.95_mridge` | 147,696 | `p30_prev0.15_cstat0.70_target0.95_mridge` | 34,284 | 4.31× lower | — | previous | previous |
| C-statistic: 0.7 → 0.75 | `p30_prev0.15_cstat0.70_target0.80_mridge` | 4,285 | `p30_prev0.15_cstat0.75_target0.80_mridge` | 1,338 | 3.20× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.20_cstat0.60_target0.95_mridge` | 480,000 | `p30_prev0.20_cstat0.65_target0.95_mridge` | 55,384 | 8.67× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p30_prev0.20_cstat0.70_target0.85_mridge` | 6,429 | `p30_prev0.20_cstat0.75_target0.85_mridge` | 1,854 | 3.47× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p30_prev0.20_cstat0.70_target0.90_mridge` | 12,858 | `p30_prev0.20_cstat0.75_target0.90_mridge` | 4,000 | 3.21× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.25_cstat0.60_target0.95_mridge` | 323,328 | `p30_prev0.25_cstat0.65_target0.95_mridge` | 37,567 | 8.61× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p30_prev0.25_cstat0.70_target0.95_mridge` | 40,416 | `p30_prev0.25_cstat0.75_target0.95_mridge` | 9,806 | 4.12× lower | — | previous | — |
| C-statistic: 0.8 → 0.85 | `p30_prev0.25_cstat0.80_target0.95_mridge` | 20,208 | `p30_prev0.25_cstat0.85_target0.95_mridge` | 5,052 | 4.00× lower | — | previous, next | next |
| C-statistic: 0.6 → 0.65 | `p30_prev0.30_cstat0.60_target0.80_mridge` | 10,000 | `p30_prev0.30_cstat0.65_target0.80_mridge` | 3,098 | 3.23× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p30_prev0.30_cstat0.65_target0.85_mridge` | 9,408 | `p30_prev0.30_cstat0.70_target0.85_mridge` | 2,380 | 3.95× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p30_prev0.30_cstat0.65_target0.95_mridge` | 134,784 | `p30_prev0.30_cstat0.70_target0.95_mridge` | 15,899 | 8.48× lower | — | previous | — |
| C-statistic: 0.75 → 0.8 | `p30_prev0.30_cstat0.75_target0.90_mridge` | 8,888 | `p30_prev0.30_cstat0.80_target0.90_mridge` | 2,223 | 4.00× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p30_prev0.40_cstat0.60_target0.95_mridge` | 50,114 | `p30_prev0.40_cstat0.65_target0.95_mridge` | 201,984 | 4.03× higher | — | previous, next | previous, next |
| C-statistic: 0.65 → 0.7 | `p30_prev0.40_cstat0.65_target0.80_mridge` | 3,031 | `p30_prev0.40_cstat0.70_target0.80_mridge` | 939 | 3.23× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.05_cstat0.60_target0.85_mridge` | 38,776 | `p40_prev0.05_cstat0.65_target0.85_mridge` | 147,692 | 3.81× higher | — | next | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.05_cstat0.60_target0.90_mridge` | 153,338 | `p40_prev0.05_cstat0.65_target0.90_mridge` | 36,580 | 4.19× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p40_prev0.05_cstat0.70_target0.80_mridge` | 17,143 | `p40_prev0.05_cstat0.75_target0.80_mridge` | 5,396 | 3.18× lower | — | previous | — |
| C-statistic: 0.75 → 0.8 | `p40_prev0.05_cstat0.75_target0.90_mridge` | 42,668 | `p40_prev0.05_cstat0.80_target0.90_mridge` | 9,744 | 4.38× lower | — | — | — |
| C-statistic: 0.8 → 0.85 | `p40_prev0.05_cstat0.80_target0.95_mridge` | 80,000 | `p40_prev0.05_cstat0.85_target0.95_mridge` | 18,321 | 4.37× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.07_cstat0.60_target0.85_mridge` | 64,623 | `p40_prev0.07_cstat0.65_target0.85_mridge` | 12,070 | 5.35× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p40_prev0.07_cstat0.65_target0.80_mridge` | 11,002 | `p40_prev0.07_cstat0.70_target0.80_mridge` | 2,803 | 3.93× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p40_prev0.10_cstat0.70_target0.95_mridge` | 66,704 | `p40_prev0.10_cstat0.75_target0.95_mridge` | 21,306 | 3.13× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.15_cstat0.60_target0.85_mridge` | 53,332 | `p40_prev0.15_cstat0.65_target0.85_mridge` | 9,977 | 5.35× lower | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p40_prev0.15_cstat0.60_target0.90_mridge` | 52,171 | `p40_prev0.15_cstat0.65_target0.90_mridge` | 12,308 | 4.24× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p40_prev0.15_cstat0.65_target0.95_mridge` | 196,928 | `p40_prev0.15_cstat0.70_target0.95_mridge` | 42,076 | 4.68× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p40_prev0.15_cstat0.70_target0.85_mridge` | 5,932 | `p40_prev0.15_cstat0.75_target0.85_mridge` | 1,844 | 3.22× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p40_prev0.15_cstat0.75_target0.95_mridge` | 28,830 | `p40_prev0.15_cstat0.80_target0.95_mridge` | 106,688 | 3.70× higher | — | next | — |
| C-statistic: 0.8 → 0.85 | `p40_prev0.15_cstat0.80_target0.95_mridge` | 106,688 | `p40_prev0.15_cstat0.85_target0.95_mridge` | 12,552 | 8.50× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.20_cstat0.60_target0.95_mridge` | 71,028 | `p40_prev0.20_cstat0.65_target0.95_mridge` | 295,392 | 4.16× higher | — | next | next |
| C-statistic: 0.65 → 0.7 | `p40_prev0.20_cstat0.65_target0.90_mridge` | 73,848 | `p40_prev0.20_cstat0.70_target0.90_mridge` | 8,801 | 8.39× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p40_prev0.20_cstat0.65_target0.95_mridge` | 295,392 | `p40_prev0.20_cstat0.70_target0.95_mridge` | 33,848 | 8.73× lower | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p40_prev0.25_cstat0.60_target0.85_mridge` | 30,112 | `p40_prev0.25_cstat0.65_target0.85_mridge` | 3,764 | 8.00× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.25_cstat0.60_target0.95_mridge` | 107,737 | `p40_prev0.25_cstat0.65_target0.95_mridge` | 13,008 | 8.28× lower | — | previous, next | next |
| C-statistic: 0.65 → 0.7 | `p40_prev0.25_cstat0.65_target0.90_mridge` | 28,448 | `p40_prev0.25_cstat0.70_target0.90_mridge` | 8,705 | 3.27× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.30_cstat0.60_target0.80_mridge` | 13,328 | `p40_prev0.30_cstat0.65_target0.80_mridge` | 3,356 | 3.97× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.40_cstat0.60_target0.90_mridge` | 24,429 | `p40_prev0.40_cstat0.65_target0.90_mridge` | 6,745 | 3.62× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.40_cstat0.60_target0.95_mridge` | 344,162 | `p40_prev0.40_cstat0.65_target0.95_mridge` | 41,623 | 8.27× lower | — | previous, next | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.50_cstat0.60_target0.80_mridge` | 7,475 | `p40_prev0.50_cstat0.65_target0.80_mridge` | 1,998 | 3.74× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p40_prev0.50_cstat0.60_target0.85_mridge` | 15,056 | `p40_prev0.50_cstat0.65_target0.85_mridge` | 4,996 | 3.01× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p40_prev0.50_cstat0.65_target0.95_mridge` | 40,169 | `p40_prev0.50_cstat0.70_target0.95_mridge` | 11,972 | 3.36× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p50_prev0.05_cstat0.65_target0.85_mridge` | 11,342 | `p50_prev0.05_cstat0.70_target0.85_mridge` | 42,857 | 3.78× higher | — | next | next |
| C-statistic: 0.7 → 0.75 | `p50_prev0.05_cstat0.70_target0.85_mridge` | 42,857 | `p50_prev0.05_cstat0.75_target0.85_mridge` | 11,649 | 3.68× lower | — | previous | previous |
| C-statistic: 0.8 → 0.85 | `p50_prev0.05_cstat0.80_target0.95_mridge` | 200,000 | `p50_prev0.05_cstat0.85_target0.95_mridge` | 40,100 | 4.99× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p50_prev0.07_cstat0.70_target0.80_mridge` | 14,285 | `p50_prev0.07_cstat0.75_target0.80_mridge` | 4,219 | 3.39× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p50_prev0.10_cstat0.65_target0.95_mridge` | 135,980 | `p50_prev0.10_cstat0.70_target0.95_mridge` | 20,763 | 6.55× lower | — | previous, next | next |
| C-statistic: 0.7 → 0.75 | `p50_prev0.10_cstat0.70_target0.85_mridge` | 21,429 | `p50_prev0.10_cstat0.75_target0.85_mridge` | 5,719 | 3.75× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p50_prev0.10_cstat0.70_target0.90_mridge` | 42,858 | `p50_prev0.10_cstat0.75_target0.90_mridge` | 13,334 | 3.21× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p50_prev0.15_cstat0.65_target0.85_mridge` | 13,610 | `p50_prev0.15_cstat0.70_target0.85_mridge` | 3,571 | 3.81× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p50_prev0.15_cstat0.65_target0.95_mridge` | 101,272 | `p50_prev0.15_cstat0.70_target0.95_mridge` | 28,429 | 3.56× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p50_prev0.20_cstat0.65_target0.80_mridge` | 11,538 | `p50_prev0.20_cstat0.70_target0.80_mridge` | 3,134 | 3.68× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p50_prev0.20_cstat0.65_target0.90_mridge` | 23,581 | `p50_prev0.20_cstat0.70_target0.90_mridge` | 5,222 | 4.52× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p50_prev0.20_cstat0.70_target0.95_mridge` | 85,712 | `p50_prev0.20_cstat0.75_target0.95_mridge` | 24,793 | 3.46× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p50_prev0.25_cstat0.60_target0.80_mridge` | 9,017 | `p50_prev0.25_cstat0.65_target0.80_mridge` | 2,414 | 3.74× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p50_prev0.25_cstat0.70_target0.80_mridge` | 5,000 | `p50_prev0.25_cstat0.75_target0.80_mridge` | 1,647 | 3.04× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p50_prev0.30_cstat0.70_target0.95_mridge` | 112,320 | `p50_prev0.30_cstat0.75_target0.95_mridge` | 14,040 | 8.00× lower | — | previous | previous |
| C-statistic: 0.6 → 0.7 | `p50_prev0.40_cstat0.60_target0.95_mridge` | 84,224 | `p50_prev0.40_cstat0.70_target0.95_mridge` | 24,516 | 3.44× lower | Yes | — | — |
| C-statistic: 0.6 → 0.65 | `p50_prev0.50_cstat0.60_target0.90_mridge` | 36,308 | `p50_prev0.50_cstat0.65_target0.90_mridge` | 8,281 | 4.38× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p50_prev0.50_cstat0.65_target0.85_mridge` | 9,408 | `p50_prev0.50_cstat0.70_target0.85_mridge` | 2,940 | 3.20× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p5_prev0.05_cstat0.70_target0.85_mridge` | 3,210 | `p5_prev0.05_cstat0.75_target0.85_mridge` | 21,328 | 6.64× higher | — | next | next |
| C-statistic: 0.75 → 0.8 | `p5_prev0.05_cstat0.75_target0.80_mridge` | 1,975 | `p5_prev0.05_cstat0.80_target0.80_mridge` | 625 | 3.16× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p5_prev0.05_cstat0.75_target0.85_mridge` | 21,328 | `p5_prev0.05_cstat0.80_target0.85_mridge` | 2,500 | 8.53× lower | — | previous | previous |
| C-statistic: 0.75 → 0.8 | `p5_prev0.05_cstat0.75_target0.95_mridge` | 79,130 | `p5_prev0.05_cstat0.80_target0.95_mridge` | 17,372 | 4.56× lower | — | previous | — |
| C-statistic: 0.8 → 0.85 | `p5_prev0.05_cstat0.80_target0.85_mridge` | 2,500 | `p5_prev0.05_cstat0.85_target0.85_mridge` | 588 | 4.25× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.07_cstat0.60_target0.85_mridge` | 13,332 | `p5_prev0.07_cstat0.65_target0.85_mridge` | 3,077 | 4.33× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p5_prev0.07_cstat0.65_target0.95_mridge` | 196,928 | `p5_prev0.07_cstat0.70_target0.95_mridge` | 22,856 | 8.62× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p5_prev0.07_cstat0.70_target0.85_mridge` | 5,714 | `p5_prev0.07_cstat0.75_target0.85_mridge` | 1,778 | 3.21× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p5_prev0.07_cstat0.75_target0.90_mridge` | 14,224 | `p5_prev0.07_cstat0.80_target0.90_mridge` | 53,376 | 3.75× higher | — | previous, next | next |
| C-statistic: 0.75 → 0.8 | `p5_prev0.07_cstat0.75_target0.95_mridge` | 14,224 | `p5_prev0.07_cstat0.80_target0.95_mridge` | 53,376 | 3.75× higher | — | next | next |
| C-statistic: 0.8 → 0.85 | `p5_prev0.07_cstat0.80_target0.95_mridge` | 53,376 | `p5_prev0.07_cstat0.85_target0.95_mridge` | 6,280 | 8.50× lower | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p5_prev0.10_cstat0.60_target0.95_mridge` | 320,000 | `p5_prev0.10_cstat0.65_target0.95_mridge` | 36,928 | 8.67× lower | — | previous | — |
| C-statistic: 0.7 → 0.75 | `p5_prev0.10_cstat0.70_target0.85_mridge` | 4,286 | `p5_prev0.10_cstat0.75_target0.85_mridge` | 1,256 | 3.41× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p5_prev0.10_cstat0.70_target0.90_mridge` | 8,572 | `p5_prev0.10_cstat0.75_target0.90_mridge` | 2,034 | 4.21× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p5_prev0.15_cstat0.65_target0.80_mridge` | 3,076 | `p5_prev0.15_cstat0.70_target0.80_mridge` | 680 | 4.52× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.20_cstat0.60_target0.85_mridge` | 10,000 | `p5_prev0.20_cstat0.65_target0.85_mridge` | 2,421 | 4.13× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p5_prev0.20_cstat0.60_target0.90_mridge` | 15,251 | `p5_prev0.20_cstat0.65_target0.90_mridge` | 73,856 | 4.84× higher | — | next | next |
| C-statistic: 0.7 → 0.75 | `p5_prev0.20_cstat0.70_target0.80_mridge` | 1,071 | `p5_prev0.20_cstat0.75_target0.80_mridge` | 333 | 3.22× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p5_prev0.20_cstat0.75_target0.85_mridge` | 952 | `p5_prev0.20_cstat0.80_target0.85_mridge` | 312 | 3.05× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p5_prev0.20_cstat0.75_target0.95_mridge` | 5,328 | `p5_prev0.20_cstat0.80_target0.95_mridge` | 26,299 | 4.94× higher | — | next | next |
| C-statistic: 0.8 → 0.85 | `p5_prev0.20_cstat0.80_target0.90_mridge` | 4,992 | `p5_prev0.20_cstat0.85_target0.90_mridge` | 1,176 | 4.24× lower | — | previous | — |
| C-statistic: 0.8 → 0.85 | `p5_prev0.20_cstat0.80_target0.95_mridge` | 26,299 | `p5_prev0.20_cstat0.85_target0.95_mridge` | 7,304 | 3.60× lower | — | previous, next | previous |
| C-statistic: 0.65 → 0.7 | `p5_prev0.25_cstat0.65_target0.90_mridge` | 7,104 | `p5_prev0.25_cstat0.70_target0.90_mridge` | 2,186 | 3.25× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p5_prev0.25_cstat0.65_target0.95_mridge` | 27,008 | `p5_prev0.25_cstat0.70_target0.95_mridge` | 6,752 | 4.00× lower | — | — | — |
| C-statistic: 0.8 → 0.85 | `p5_prev0.25_cstat0.80_target0.90_mridge` | 1,776 | `p5_prev0.25_cstat0.85_target0.90_mridge` | 7,075 | 3.98× higher | — | next | — |
| C-statistic: 0.65 → 0.7 | `p5_prev0.30_cstat0.65_target0.95_mridge` | 22,528 | `p5_prev0.30_cstat0.70_target0.95_mridge` | 4,568 | 4.93× lower | — | previous | — |
| C-statistic: 0.75 → 0.8 | `p5_prev0.30_cstat0.75_target0.95_mridge` | 11,264 | `p5_prev0.30_cstat0.80_target0.95_mridge` | 2,816 | 4.00× lower | — | next | next |
| C-statistic: 0.8 → 0.85 | `p5_prev0.30_cstat0.80_target0.95_mridge` | 2,816 | `p5_prev0.30_cstat0.85_target0.95_mridge` | 8,903 | 3.16× higher | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p5_prev0.40_cstat0.60_target0.95_mridge` | 135,168 | `p5_prev0.40_cstat0.65_target0.95_mridge` | 25,049 | 5.40× lower | — | previous, next | — |
| C-statistic: 0.75 → 0.8 | `p5_prev0.40_cstat0.75_target0.95_mridge` | 12,863 | `p5_prev0.40_cstat0.80_target0.95_mridge` | 3,216 | 4.00× lower | — | previous, next | next |
| C-statistic: 0.8 → 0.85 | `p5_prev0.40_cstat0.80_target0.95_mridge` | 3,216 | `p5_prev0.40_cstat0.85_target0.95_mridge` | 16,896 | 5.25× higher | — | previous, next | previous |
| C-statistic: 0.6 → 0.65 | `p5_prev0.50_cstat0.60_target0.95_mridge` | 107,520 | `p5_prev0.50_cstat0.65_target0.95_mridge` | 20,164 | 5.33× lower | — | previous, next | — |
| C-statistic: 0.65 → 0.7 | `p5_prev0.50_cstat0.65_target0.95_mridge` | 20,164 | `p5_prev0.50_cstat0.70_target0.95_mridge` | 3,360 | 6.00× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p75_prev0.05_cstat0.65_target0.90_mridge` | 53,047 | `p75_prev0.05_cstat0.70_target0.90_mridge` | 16,016 | 3.31× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p75_prev0.05_cstat0.70_target0.85_mridge` | 32,143 | `p75_prev0.05_cstat0.75_target0.85_mridge` | 10,000 | 3.21× lower | — | previous | — |
| C-statistic: 0.75 → 0.8 | `p75_prev0.05_cstat0.75_target0.95_mridge` | 119,559 | `p75_prev0.05_cstat0.80_target0.95_mridge` | 35,535 | 3.36× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p75_prev0.07_cstat0.60_target0.85_mridge` | 100,000 | `p75_prev0.07_cstat0.65_target0.85_mridge` | 17,725 | 5.64× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p75_prev0.07_cstat0.65_target0.85_mridge` | 17,725 | `p75_prev0.07_cstat0.70_target0.85_mridge` | 5,357 | 3.31× lower | — | — | — |
| C-statistic: 0.7 → 0.75 | `p75_prev0.07_cstat0.70_target0.80_mridge` | 10,714 | `p75_prev0.07_cstat0.75_target0.80_mridge` | 3,222 | 3.33× lower | — | — | — |
| C-statistic: 0.75 → 0.8 | `p75_prev0.07_cstat0.75_target0.85_mridge` | 13,333 | `p75_prev0.07_cstat0.80_target0.85_mridge` | 3,125 | 4.27× lower | — | next | next |
| C-statistic: 0.8 → 0.85 | `p75_prev0.07_cstat0.80_target0.85_mridge` | 3,125 | `p75_prev0.07_cstat0.85_target0.85_mridge` | 11,765 | 3.76× higher | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p75_prev0.10_cstat0.60_target0.80_mridge` | 37,500 | `p75_prev0.10_cstat0.65_target0.80_mridge` | 10,456 | 3.59× lower | — | previous | — |
| C-statistic: 0.6 → 0.65 | `p75_prev0.10_cstat0.60_target0.90_mridge` | 151,570 | `p75_prev0.10_cstat0.65_target0.90_mridge` | 29,936 | 5.06× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p75_prev0.10_cstat0.65_target0.85_mridge` | 69,230 | `p75_prev0.10_cstat0.70_target0.85_mridge` | 11,894 | 5.82× lower | — | previous | previous |
| C-statistic: 0.7 → 0.75 | `p75_prev0.10_cstat0.70_target0.95_mridge` | 58,267 | `p75_prev0.10_cstat0.75_target0.95_mridge` | 9,945 | 5.86× lower | — | previous, next | next |
| C-statistic: 0.75 → 0.8 | `p75_prev0.10_cstat0.75_target0.95_mridge` | 9,945 | `p75_prev0.10_cstat0.80_target0.95_mridge` | 38,918 | 3.91× higher | — | previous | previous |
| C-statistic: 0.6 → 0.65 | `p75_prev0.15_cstat0.60_target0.90_mridge` | 93,491 | `p75_prev0.15_cstat0.65_target0.90_mridge` | 31,163 | 3.00× lower | — | previous | — |
| C-statistic: 0.65 → 0.7 | `p75_prev0.15_cstat0.65_target0.80_mridge` | 11,538 | `p75_prev0.15_cstat0.70_target0.80_mridge` | 2,678 | 4.31× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p75_prev0.20_cstat0.60_target0.90_mridge` | 76,183 | `p75_prev0.20_cstat0.65_target0.90_mridge` | 17,308 | 4.40× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p75_prev0.20_cstat0.65_target0.80_mridge` | 6,479 | `p75_prev0.20_cstat0.70_target0.80_mridge` | 1,895 | 3.42× lower | — | — | — |
| C-statistic: 0.6 → 0.65 | `p75_prev0.25_cstat0.60_target0.90_mridge` | 106,656 | `p75_prev0.25_cstat0.65_target0.90_mridge` | 15,046 | 7.09× lower | — | previous | previous |
| C-statistic: 0.65 → 0.7 | `p75_prev0.40_cstat0.65_target0.85_mridge` | 17,648 | `p75_prev0.40_cstat0.70_target0.85_mridge` | 3,985 | 4.43× lower | — | — | — |
| C-statistic: 0.65 → 0.7 | `p75_prev0.40_cstat0.65_target0.95_mridge` | 15,763 | `p75_prev0.40_cstat0.70_target0.95_mridge` | 63,168 | 4.01× higher | — | next | next |
| C-statistic: 0.7 → 0.75 | `p75_prev0.40_cstat0.70_target0.95_mridge` | 63,168 | `p75_prev0.40_cstat0.75_target0.95_mridge` | 14,895 | 4.24× lower | — | previous | previous |
| C-statistic: 0.65 → 0.7 | `p75_prev0.50_cstat0.65_target0.80_mridge` | 7,500 | `p75_prev0.50_cstat0.70_target0.80_mridge` | 2,003 | 3.74× lower | — | — | — |
| Prevalence: 0.05 → 0.075 | `p100_prev0.05_cstat0.65_target0.85_mridge` | 46,154 | `p100_prev0.07_cstat0.65_target0.85_mridge` | 14,338 | 3.22× lower | — | — | — |
| Prevalence: 0.05 → 0.1 | `p100_prev0.05_cstat0.70_target0.95_mridge` | 85,714 | `p100_prev0.10_cstat0.70_target0.95_mridge` | 21,028 | 4.08× lower | Yes | — | — |
| Prevalence: 0.05 → 0.075 | `p100_prev0.05_cstat0.75_target0.90_mridge` | 10,000 | `p100_prev0.07_cstat0.75_target0.90_mridge` | 71,108 | 7.11× higher | — | previous, next | previous, next |
| Prevalence: 0.05 → 0.075 | `p100_prev0.05_cstat0.80_target0.85_mridge` | 25,000 | `p100_prev0.07_cstat0.80_target0.85_mridge` | 7,714 | 3.24× lower | — | — | — |
| Prevalence: 0.075 → 0.1 | `p100_prev0.07_cstat0.60_target0.80_mridge` | 42,093 | `p100_prev0.10_cstat0.60_target0.80_mridge` | 5,824 | 7.23× lower | — | previous | — |
| Prevalence: 0.075 → 0.1 | `p100_prev0.07_cstat0.75_target0.85_mridge` | 10,558 | `p100_prev0.10_cstat0.75_target0.85_mridge` | 3,218 | 3.28× lower | — | — | — |
| Prevalence: 0.075 → 0.1 | `p100_prev0.07_cstat0.75_target0.90_mridge` | 71,108 | `p100_prev0.10_cstat0.75_target0.90_mridge` | 13,333 | 5.33× lower | — | previous | previous |
| Prevalence: 0.1 → 0.15 | `p100_prev0.10_cstat0.60_target0.80_mridge` | 5,824 | `p100_prev0.15_cstat0.60_target0.80_mridge` | 18,717 | 3.21× higher | — | next | — |
| Prevalence: 0.1 → 0.15 | `p100_prev0.10_cstat0.75_target0.95_mridge` | 106,664 | `p100_prev0.15_cstat0.75_target0.95_mridge` | 32,254 | 3.31× lower | — | previous | previous |
| Prevalence: 0.15 → 0.2 | `p100_prev0.15_cstat0.60_target0.80_mridge` | 18,717 | `p100_prev0.20_cstat0.60_target0.80_mridge` | 2,585 | 7.24× lower | — | previous, next | next |
| Prevalence: 0.2 → 0.25 | `p100_prev0.20_cstat0.60_target0.80_mridge` | 2,585 | `p100_prev0.25_cstat0.60_target0.80_mridge` | 20,000 | 7.74× higher | — | previous, next | previous |
| Prevalence: 0.2 → 0.25 | `p100_prev0.20_cstat0.65_target0.90_mridge` | 92,308 | `p100_prev0.25_cstat0.65_target0.90_mridge` | 27,628 | 3.34× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p10_prev0.05_cstat0.60_target0.80_mridge` | 40,000 | `p10_prev0.07_cstat0.60_target0.80_mridge` | 9,985 | 4.01× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p10_prev0.05_cstat0.60_target0.95_mridge` | 320,000 | `p10_prev0.07_cstat0.60_target0.95_mridge` | 53,336 | 6.00× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p10_prev0.05_cstat0.65_target0.85_mridge` | 18,462 | `p10_prev0.07_cstat0.65_target0.85_mridge` | 6,102 | 3.03× lower | — | — | — |
| Prevalence: 0.05 → 0.075 | `p10_prev0.05_cstat0.65_target0.90_mridge` | 36,924 | `p10_prev0.07_cstat0.65_target0.90_mridge` | 12,308 | 3.00× lower | — | — | — |
| Prevalence: 0.05 → 0.075 | `p10_prev0.05_cstat0.65_target0.95_mridge` | 147,696 | `p10_prev0.07_cstat0.65_target0.95_mridge` | 24,616 | 6.00× lower | — | previous, next | next |
| Prevalence: 0.05 → 0.075 | `p10_prev0.05_cstat0.80_target0.95_mridge` | 12,409 | `p10_prev0.07_cstat0.80_target0.95_mridge` | 82,409 | 6.64× higher | — | next | next |
| Prevalence: 0.05 → 0.075 | `p10_prev0.05_cstat0.85_target0.90_mridge` | 3,791 | `p10_prev0.07_cstat0.85_target0.90_mridge` | 1,171 | 3.24× lower | — | — | — |
| Prevalence: 0.1 → 0.15 | `p10_prev0.10_cstat0.65_target0.90_mridge` | 18,460 | `p10_prev0.15_cstat0.65_target0.90_mridge` | 5,732 | 3.22× lower | — | — | — |
| Prevalence: 0.1 → 0.15 | `p10_prev0.10_cstat0.80_target0.95_mridge` | 80,000 | `p10_prev0.15_cstat0.80_target0.95_mridge` | 13,344 | 6.00× lower | — | previous, next | previous |
| Prevalence: 0.15 → 0.2 | `p10_prev0.15_cstat0.70_target0.95_mridge` | 45,712 | `p10_prev0.20_cstat0.70_target0.95_mridge` | 11,757 | 3.89× lower | — | previous, next | next |
| Prevalence: 0.15 → 0.2 | `p10_prev0.15_cstat0.85_target0.90_mridge` | 1,534 | `p10_prev0.20_cstat0.85_target0.90_mridge` | 9,408 | 6.13× higher | — | next | next |
| Prevalence: 0.2 → 0.25 | `p10_prev0.20_cstat0.70_target0.95_mridge` | 11,757 | `p10_prev0.25_cstat0.70_target0.95_mridge` | 53,888 | 4.58× higher | — | previous, next | previous, next |
| Prevalence: 0.2 → 0.25 | `p10_prev0.20_cstat0.85_target0.90_mridge` | 9,408 | `p10_prev0.25_cstat0.85_target0.90_mridge` | 1,288 | 7.30× lower | — | previous, next | previous, next |
| Prevalence: 0.25 → 0.3 | `p10_prev0.25_cstat0.70_target0.95_mridge` | 53,888 | `p10_prev0.30_cstat0.70_target0.95_mridge` | 7,375 | 7.31× lower | — | previous | previous |
| Prevalence: 0.25 → 0.3 | `p10_prev0.25_cstat0.75_target0.95_mridge` | 14,428 | `p10_prev0.30_cstat0.75_target0.95_mridge` | 4,574 | 3.15× lower | — | previous | — |
| Prevalence: 0.25 → 0.3 | `p10_prev0.25_cstat0.85_target0.90_mridge` | 1,288 | `p10_prev0.30_cstat0.85_target0.90_mridge` | 5,919 | 4.60× higher | — | previous, next | previous |
| Prevalence: 0.3 → 0.4 | `p10_prev0.30_cstat0.60_target0.80_mridge` | 1,605 | `p10_prev0.40_cstat0.60_target0.80_mridge` | 4,992 | 3.11× higher | — | — | — |
| Prevalence: 0.4 → 0.5 | `p10_prev0.40_cstat0.60_target0.95_mridge` | 67,328 | `p10_prev0.50_cstat0.60_target0.95_mridge` | 13,504 | 4.99× lower | — | previous | — |
| Prevalence: 0.4 → 0.5 | `p10_prev0.40_cstat0.65_target0.95_mridge` | 12,624 | `p10_prev0.50_cstat0.65_target0.95_mridge` | 65,550 | 5.19× higher | — | next | next |
| Prevalence: 0.4 → 0.5 | `p10_prev0.40_cstat0.80_target0.95_mridge` | 8,416 | `p10_prev0.50_cstat0.80_target0.95_mridge` | 1,688 | 4.99× lower | — | previous, next | next |
| Prevalence: 0.4 → 0.5 | `p10_prev0.40_cstat0.85_target0.90_mridge` | 3,353 | `p10_prev0.50_cstat0.85_target0.90_mridge` | 14,204 | 4.24× higher | — | previous, next | — |
| Prevalence: 0.05 → 0.075 | `p15_prev0.05_cstat0.60_target0.80_mridge` | 30,000 | `p15_prev0.07_cstat0.60_target0.80_mridge` | 10,000 | 3.00× lower | — | — | — |
| Prevalence: 0.05 → 0.075 | `p15_prev0.05_cstat0.65_target0.80_mridge` | 13,846 | `p15_prev0.07_cstat0.65_target0.80_mridge` | 4,519 | 3.06× lower | — | — | — |
| Prevalence: 0.05 → 0.075 | `p15_prev0.05_cstat0.75_target0.95_mridge` | 128,000 | `p15_prev0.07_cstat0.75_target0.95_mridge` | 31,130 | 4.11× lower | — | previous, next | — |
| Prevalence: 0.075 → 0.1 | `p15_prev0.07_cstat0.60_target0.85_mridge` | 80,000 | `p15_prev0.10_cstat0.60_target0.85_mridge` | 22,636 | 3.53× lower | — | previous | previous |
| Prevalence: 0.075 → 0.1 | `p15_prev0.07_cstat0.60_target0.90_mridge` | 74,865 | `p15_prev0.10_cstat0.60_target0.90_mridge` | 480,000 | 6.41× higher | — | next | — |
| Prevalence: 0.1 → 0.15 | `p15_prev0.10_cstat0.60_target0.95_mridge` | 240,000 | `p15_prev0.15_cstat0.60_target0.95_mridge` | 80,000 | 3.00× lower | — | previous | — |
| Prevalence: 0.1 → 0.15 | `p15_prev0.10_cstat0.75_target0.85_mridge` | 4,000 | `p15_prev0.15_cstat0.75_target0.85_mridge` | 1,196 | 3.34× lower | — | — | — |
| Prevalence: 0.1 → 0.15 | `p15_prev0.10_cstat0.85_target0.95_mridge` | 14,120 | `p15_prev0.15_cstat0.85_target0.95_mridge` | 3,861 | 3.66× lower | — | next | next |
| Prevalence: 0.15 → 0.2 | `p15_prev0.15_cstat0.60_target0.85_mridge` | 40,000 | `p15_prev0.20_cstat0.60_target0.85_mridge` | 12,236 | 3.27× lower | — | next | next |
| Prevalence: 0.15 → 0.2 | `p15_prev0.15_cstat0.70_target0.95_mridge` | 137,152 | `p15_prev0.20_cstat0.70_target0.95_mridge` | 17,044 | 8.05× lower | — | previous | — |
| Prevalence: 0.15 → 0.2 | `p15_prev0.15_cstat0.85_target0.95_mridge` | 3,861 | `p15_prev0.20_cstat0.85_target0.95_mridge` | 14,112 | 3.66× higher | — | previous, next | previous |
| Prevalence: 0.2 → 0.25 | `p15_prev0.20_cstat0.60_target0.85_mridge` | 12,236 | `p15_prev0.25_cstat0.60_target0.85_mridge` | 45,184 | 3.69× higher | — | previous, next | previous, next |
| Prevalence: 0.2 → 0.25 | `p15_prev0.20_cstat0.60_target0.95_mridge` | 118,676 | `p15_prev0.25_cstat0.60_target0.95_mridge` | 33,827 | 3.51× lower | — | previous, next | next |
| Prevalence: 0.2 → 0.25 | `p15_prev0.20_cstat0.75_target0.95_mridge` | 16,000 | `p15_prev0.25_cstat0.75_target0.95_mridge` | 5,056 | 3.16× lower | — | — | — |
| Prevalence: 0.2 → 0.25 | `p15_prev0.20_cstat0.85_target0.90_mridge` | 1,767 | `p15_prev0.25_cstat0.85_target0.90_mridge` | 10,672 | 6.04× higher | — | next | next |
| Prevalence: 0.25 → 0.3 | `p15_prev0.25_cstat0.60_target0.85_mridge` | 45,184 | `p15_prev0.30_cstat0.60_target0.85_mridge` | 9,408 | 4.80× lower | — | previous | previous |
| Prevalence: 0.25 → 0.3 | `p15_prev0.25_cstat0.60_target0.95_mridge` | 33,827 | `p15_prev0.30_cstat0.60_target0.95_mridge` | 134,656 | 3.98× higher | — | previous, next | previous |
| Prevalence: 0.25 → 0.3 | `p15_prev0.25_cstat0.85_target0.90_mridge` | 10,672 | `p15_prev0.30_cstat0.85_target0.90_mridge` | 1,522 | 7.01× lower | — | previous | previous |
| Prevalence: 0.25 → 0.3 | `p15_prev0.25_cstat0.85_target0.95_mridge` | 10,112 | `p15_prev0.30_cstat0.85_target0.95_mridge` | 2,104 | 4.81× lower | — | next | next |
| Prevalence: 0.3 → 0.4 | `p15_prev0.30_cstat0.85_target0.95_mridge` | 2,104 | `p15_prev0.40_cstat0.85_target0.95_mridge` | 12,640 | 6.01× higher | — | previous, next | previous |
| Prevalence: 0.05 → 0.075 | `p20_prev0.05_cstat0.60_target0.85_mridge` | 80,000 | `p20_prev0.07_cstat0.60_target0.85_mridge` | 19,086 | 4.19× lower | — | previous | previous |
| Prevalence: 0.05 → 0.075 | `p20_prev0.05_cstat0.60_target0.90_mridge` | 160,000 | `p20_prev0.07_cstat0.60_target0.90_mridge` | 53,332 | 3.00× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p20_prev0.05_cstat0.75_target0.95_mridge` | 170,656 | `p20_prev0.07_cstat0.75_target0.95_mridge` | 27,376 | 6.23× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p20_prev0.05_cstat0.85_target0.95_mridge` | 14,088 | `p20_prev0.07_cstat0.85_target0.95_mridge` | 50,208 | 3.56× higher | — | next | — |
| Prevalence: 0.075 → 0.1 | `p20_prev0.07_cstat0.75_target0.90_mridge` | 28,448 | `p20_prev0.10_cstat0.75_target0.90_mridge` | 8,017 | 3.55× lower | — | previous | previous |
| Prevalence: 0.075 → 0.1 | `p20_prev0.07_cstat0.80_target0.95_mridge` | 106,688 | `p20_prev0.10_cstat0.80_target0.95_mridge` | 18,351 | 5.81× lower | — | previous | — |
| Prevalence: 0.1 → 0.15 | `p20_prev0.10_cstat0.60_target0.95_mridge` | 148,216 | `p20_prev0.15_cstat0.60_target0.95_mridge` | 38,008 | 3.90× lower | — | — | — |
| Prevalence: 0.1 → 0.15 | `p20_prev0.10_cstat0.65_target0.95_mridge` | 73,848 | `p20_prev0.15_cstat0.65_target0.95_mridge` | 256,742 | 3.48× higher | — | next | — |
| Prevalence: 0.1 → 0.15 | `p20_prev0.10_cstat0.85_target0.95_mridge` | 37,648 | `p20_prev0.15_cstat0.85_target0.95_mridge` | 12,544 | 3.00× lower | — | previous | — |
| Prevalence: 0.15 → 0.2 | `p20_prev0.15_cstat0.60_target0.85_mridge` | 26,668 | `p20_prev0.20_cstat0.60_target0.85_mridge` | 8,476 | 3.15× lower | — | — | — |
| Prevalence: 0.15 → 0.2 | `p20_prev0.15_cstat0.60_target0.95_mridge` | 38,008 | `p20_prev0.20_cstat0.60_target0.95_mridge` | 320,000 | 8.42× higher | — | next | — |
| Prevalence: 0.2 → 0.25 | `p20_prev0.20_cstat0.65_target0.95_mridge` | 18,460 | `p20_prev0.25_cstat0.65_target0.95_mridge` | 107,776 | 5.84× higher | — | next | — |
| Prevalence: 0.25 → 0.3 | `p20_prev0.25_cstat0.60_target0.95_mridge` | 107,776 | `p20_prev0.30_cstat0.60_target0.95_mridge` | 22,464 | 4.80× lower | — | next | next |
| Prevalence: 0.05 → 0.075 | `p25_prev0.05_cstat0.60_target0.80_mridge` | 50,000 | `p25_prev0.07_cstat0.60_target0.80_mridge` | 15,724 | 3.18× lower | — | — | — |
| Prevalence: 0.05 → 0.075 | `p25_prev0.05_cstat0.60_target0.90_mridge` | 149,087 | `p25_prev0.07_cstat0.60_target0.90_mridge` | 533,344 | 3.58× higher | — | previous, next | next |
| Prevalence: 0.05 → 0.075 | `p25_prev0.05_cstat0.70_target0.85_mridge` | 21,429 | `p25_prev0.07_cstat0.70_target0.85_mridge` | 6,798 | 3.15× lower | — | — | — |
| Prevalence: 0.075 → 0.1 | `p25_prev0.07_cstat0.75_target0.90_mridge` | 35,552 | `p25_prev0.10_cstat0.75_target0.90_mridge` | 6,401 | 5.55× lower | — | previous | — |
| Prevalence: 0.075 → 0.1 | `p25_prev0.07_cstat0.80_target0.80_mridge` | 4,166 | `p25_prev0.10_cstat0.80_target0.80_mridge` | 606 | 6.87× lower | — | previous | — |
| Prevalence: 0.075 → 0.1 | `p25_prev0.07_cstat0.85_target0.95_mridge` | 31,368 | `p25_prev0.10_cstat0.85_target0.95_mridge` | 5,882 | 5.33× lower | — | previous, next | next |
| Prevalence: 0.1 → 0.15 | `p25_prev0.10_cstat0.65_target0.90_mridge` | 46,152 | `p25_prev0.15_cstat0.65_target0.90_mridge` | 15,384 | 3.00× lower | — | — | — |
| Prevalence: 0.1 → 0.15 | `p25_prev0.10_cstat0.85_target0.95_mridge` | 5,882 | `p25_prev0.15_cstat0.85_target0.95_mridge` | 31,376 | 5.33× higher | — | previous, next | previous |
| Prevalence: 0.15 → 0.2 | `p25_prev0.15_cstat0.60_target0.95_mridge` | 65,214 | `p25_prev0.20_cstat0.60_target0.95_mridge` | 400,000 | 6.13× higher | — | next | next |
| Prevalence: 0.15 → 0.2 | `p25_prev0.15_cstat0.65_target0.80_mridge` | 4,398 | `p25_prev0.20_cstat0.65_target0.80_mridge` | 1,442 | 3.05× lower | — | — | — |
| Prevalence: 0.15 → 0.2 | `p25_prev0.15_cstat0.70_target0.95_mridge` | 28,434 | `p25_prev0.20_cstat0.70_target0.95_mridge` | 85,712 | 3.01× higher | — | next | next |
| Prevalence: 0.15 → 0.2 | `p25_prev0.15_cstat0.80_target0.95_mridge` | 10,295 | `p25_prev0.20_cstat0.80_target0.95_mridge` | 49,984 | 4.86× higher | — | next | next |
| Prevalence: 0.2 → 0.25 | `p25_prev0.20_cstat0.60_target0.85_mridge` | 12,500 | `p25_prev0.25_cstat0.60_target0.85_mridge` | 2,923 | 4.28× lower | — | next | next |
| Prevalence: 0.2 → 0.25 | `p25_prev0.20_cstat0.60_target0.95_mridge` | 400,000 | `p25_prev0.25_cstat0.60_target0.95_mridge` | 62,323 | 6.42× lower | — | previous | previous |
| Prevalence: 0.2 → 0.25 | `p25_prev0.20_cstat0.70_target0.95_mridge` | 85,712 | `p25_prev0.25_cstat0.70_target0.95_mridge` | 22,118 | 3.88× lower | — | previous | previous |
| Prevalence: 0.2 → 0.25 | `p25_prev0.20_cstat0.80_target0.95_mridge` | 49,984 | `p25_prev0.25_cstat0.80_target0.95_mridge` | 8,115 | 6.16× lower | — | previous | previous |
| Prevalence: 0.25 → 0.3 | `p25_prev0.25_cstat0.60_target0.85_mridge` | 2,923 | `p25_prev0.30_cstat0.60_target0.85_mridge` | 15,680 | 5.36× higher | — | previous, next | previous |
| Prevalence: 0.25 → 0.3 | `p25_prev0.25_cstat0.65_target0.95_mridge` | 134,784 | `p25_prev0.30_cstat0.65_target0.95_mridge` | 28,064 | 4.80× lower | — | previous | — |
| Prevalence: 0.25 → 0.3 | `p25_prev0.25_cstat0.75_target0.95_mridge` | 8,424 | `p25_prev0.30_cstat0.75_target0.95_mridge` | 56,128 | 6.66× higher | — | next | next |
| Prevalence: 0.3 → 0.4 | `p25_prev0.30_cstat0.60_target0.95_mridge` | 112,256 | `p25_prev0.40_cstat0.60_target0.95_mridge` | 336,896 | 3.00× higher | — | previous, next | next |
| Prevalence: 0.3 → 0.4 | `p25_prev0.30_cstat0.75_target0.95_mridge` | 56,128 | `p25_prev0.40_cstat0.75_target0.95_mridge` | 9,515 | 5.90× lower | — | previous | previous |
| Prevalence: 0.4 → 0.5 | `p25_prev0.40_cstat0.80_target0.95_mridge` | 7,027 | `p25_prev0.50_cstat0.80_target0.95_mridge` | 34,030 | 4.84× higher | — | next | next |
| Prevalence: 0.05 → 0.075 | `p30_prev0.05_cstat0.70_target0.90_mridge` | 6,017 | `p30_prev0.07_cstat0.70_target0.90_mridge` | 18,934 | 3.15× higher | — | previous | previous |
| Prevalence: 0.05 → 0.075 | `p30_prev0.05_cstat0.75_target0.95_mridge` | 64,000 | `p30_prev0.07_cstat0.75_target0.95_mridge` | 10,666 | 6.00× lower | — | previous, next | next |
| Prevalence: 0.05 → 0.075 | `p30_prev0.05_cstat0.85_target0.95_mridge` | 56,472 | `p30_prev0.07_cstat0.85_target0.95_mridge` | 17,365 | 3.25× lower | — | previous | previous |
| Prevalence: 0.075 → 0.15 | `p30_prev0.07_cstat0.60_target0.85_mridge` | 80,000 | `p30_prev0.15_cstat0.60_target0.85_mridge` | 18,996 | 4.21× lower | Yes | previous | previous |
| Prevalence: 0.075 → 0.1 | `p30_prev0.07_cstat0.75_target0.95_mridge` | 10,666 | `p30_prev0.10_cstat0.75_target0.95_mridge` | 32,000 | 3.00× higher | — | previous | previous |
| Prevalence: 0.1 → 0.2 | `p30_prev0.10_cstat0.60_target0.95_mridge` | 120,000 | `p30_prev0.20_cstat0.60_target0.95_mridge` | 480,000 | 4.00× higher | Yes | previous, next | previous |
| Prevalence: 0.1 → 0.15 | `p30_prev0.10_cstat0.65_target0.95_mridge` | 37,806 | `p30_prev0.15_cstat0.65_target0.95_mridge` | 147,696 | 3.91× higher | — | next | next |
| Prevalence: 0.1 → 0.15 | `p30_prev0.10_cstat0.85_target0.80_mridge` | 1,764 | `p30_prev0.15_cstat0.85_target0.80_mridge` | 561 | 3.14× lower | — | — | — |
| Prevalence: 0.2 → 0.25 | `p30_prev0.20_cstat0.85_target0.95_mridge` | 28,240 | `p30_prev0.25_cstat0.85_target0.95_mridge` | 5,052 | 5.59× lower | — | previous, next | next |
| Prevalence: 0.25 → 0.3 | `p30_prev0.25_cstat0.65_target0.95_mridge` | 37,567 | `p30_prev0.30_cstat0.65_target0.95_mridge` | 134,784 | 3.59× higher | — | next | — |
| Prevalence: 0.3 → 0.4 | `p30_prev0.30_cstat0.60_target0.95_mridge` | 269,568 | `p30_prev0.40_cstat0.60_target0.95_mridge` | 50,114 | 5.38× lower | — | previous, next | next |
| Prevalence: 0.3 → 0.4 | `p30_prev0.30_cstat0.75_target0.90_mridge` | 8,888 | `p30_prev0.40_cstat0.75_target0.90_mridge` | 2,938 | 3.03× lower | — | — | — |
| Prevalence: 0.4 → 0.5 | `p30_prev0.40_cstat0.65_target0.95_mridge` | 201,984 | `p30_prev0.50_cstat0.65_target0.95_mridge` | 36,569 | 5.52× lower | — | previous | previous |
| Prevalence: 0.4 → 0.5 | `p30_prev0.40_cstat0.70_target0.80_mridge` | 939 | `p30_prev0.50_cstat0.70_target0.80_mridge` | 3,000 | 3.19× higher | — | — | — |
| Prevalence: 0.05 → 0.1 | `p40_prev0.05_cstat0.60_target0.90_mridge` | 153,338 | `p40_prev0.10_cstat0.60_target0.90_mridge` | 35,100 | 4.37× lower | Yes | — | — |
| Prevalence: 0.05 → 0.075 | `p40_prev0.05_cstat0.70_target0.80_mridge` | 17,143 | `p40_prev0.07_cstat0.70_target0.80_mridge` | 2,803 | 6.12× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p40_prev0.05_cstat0.80_target0.95_mridge` | 80,000 | `p40_prev0.07_cstat0.80_target0.95_mridge` | 23,024 | 3.47× lower | — | previous | — |
| Prevalence: 0.075 → 0.1 | `p40_prev0.07_cstat0.60_target0.85_mridge` | 64,623 | `p40_prev0.10_cstat0.60_target0.85_mridge` | 20,000 | 3.23× lower | — | previous | — |
| Prevalence: 0.075 → 0.1 | `p40_prev0.07_cstat0.65_target0.95_mridge` | 98,460 | `p40_prev0.10_cstat0.65_target0.95_mridge` | 27,755 | 3.55× lower | — | — | — |
| Prevalence: 0.1 → 0.15 | `p40_prev0.10_cstat0.65_target0.95_mridge` | 27,755 | `p40_prev0.15_cstat0.65_target0.95_mridge` | 196,928 | 7.10× higher | — | next | — |
| Prevalence: 0.1 → 0.15 | `p40_prev0.10_cstat0.80_target0.95_mridge` | 27,275 | `p40_prev0.15_cstat0.80_target0.95_mridge` | 106,688 | 3.91× higher | — | next | — |
| Prevalence: 0.15 → 0.2 | `p40_prev0.15_cstat0.60_target0.95_mridge` | 313,459 | `p40_prev0.20_cstat0.60_target0.95_mridge` | 71,028 | 4.41× lower | — | previous | previous |
| Prevalence: 0.15 → 0.2 | `p40_prev0.15_cstat0.65_target0.90_mridge` | 12,308 | `p40_prev0.20_cstat0.65_target0.90_mridge` | 73,848 | 6.00× higher | — | next | — |
| Prevalence: 0.15 → 0.2 | `p40_prev0.15_cstat0.80_target0.95_mridge` | 106,688 | `p40_prev0.20_cstat0.80_target0.95_mridge` | 16,016 | 6.66× lower | — | previous | — |
| Prevalence: 0.25 → 0.3 | `p40_prev0.25_cstat0.60_target0.85_mridge` | 30,112 | `p40_prev0.30_cstat0.60_target0.85_mridge` | 9,273 | 3.25× lower | — | previous | — |
| Prevalence: 0.25 → 0.3 | `p40_prev0.25_cstat0.65_target0.95_mridge` | 13,008 | `p40_prev0.30_cstat0.65_target0.95_mridge` | 52,497 | 4.04× higher | — | previous | previous |
| Prevalence: 0.3 → 0.4 | `p40_prev0.30_cstat0.60_target0.95_mridge` | 42,121 | `p40_prev0.40_cstat0.60_target0.95_mridge` | 344,162 | 8.17× higher | — | next | — |
| Prevalence: 0.4 → 0.5 | `p40_prev0.40_cstat0.60_target0.95_mridge` | 344,162 | `p40_prev0.50_cstat0.60_target0.95_mridge` | 107,776 | 3.19× lower | — | previous, next | next |
| Prevalence: 0.05 → 0.075 | `p50_prev0.05_cstat0.70_target0.85_mridge` | 42,857 | `p50_prev0.07_cstat0.70_target0.85_mridge` | 14,285 | 3.00× lower | — | previous | previous |
| Prevalence: 0.05 → 0.075 | `p50_prev0.05_cstat0.80_target0.95_mridge` | 200,000 | `p50_prev0.07_cstat0.80_target0.95_mridge` | 30,072 | 6.65× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p50_prev0.05_cstat0.85_target0.90_mridge` | 23,530 | `p50_prev0.07_cstat0.85_target0.90_mridge` | 3,922 | 6.00× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p50_prev0.05_cstat0.85_target0.95_mridge` | 40,100 | `p50_prev0.07_cstat0.85_target0.95_mridge` | 12,227 | 3.28× lower | — | — | — |
| Prevalence: 0.1 → 0.15 | `p50_prev0.10_cstat0.70_target0.85_mridge` | 21,429 | `p50_prev0.15_cstat0.70_target0.85_mridge` | 3,571 | 6.00× lower | — | previous | — |
| Prevalence: 0.15 → 0.2 | `p50_prev0.15_cstat0.70_target0.95_mridge` | 28,429 | `p50_prev0.20_cstat0.70_target0.95_mridge` | 85,712 | 3.01× higher | — | next | — |
| Prevalence: 0.15 → 0.2 | `p50_prev0.15_cstat0.85_target0.95_mridge` | 11,478 | `p50_prev0.20_cstat0.85_target0.95_mridge` | 47,056 | 4.10× higher | — | next | next |
| Prevalence: 0.2 → 0.25 | `p50_prev0.20_cstat0.65_target0.80_mridge` | 11,538 | `p50_prev0.25_cstat0.65_target0.80_mridge` | 2,414 | 4.78× lower | — | — | — |
| Prevalence: 0.2 → 0.25 | `p50_prev0.20_cstat0.85_target0.95_mridge` | 47,056 | `p50_prev0.25_cstat0.85_target0.95_mridge` | 8,095 | 5.81× lower | — | previous | previous |
| Prevalence: 0.25 → 0.3 | `p50_prev0.25_cstat0.60_target0.90_mridge` | 34,036 | `p50_prev0.30_cstat0.60_target0.90_mridge` | 118,528 | 3.48× higher | — | next | next |
| Prevalence: 0.25 → 0.3 | `p50_prev0.25_cstat0.70_target0.95_mridge` | 34,883 | `p50_prev0.30_cstat0.70_target0.95_mridge` | 112,320 | 3.22× higher | — | next | next |
| Prevalence: 0.25 → 0.3 | `p50_prev0.25_cstat0.85_target0.95_mridge` | 8,095 | `p50_prev0.30_cstat0.85_target0.95_mridge` | 28,080 | 3.47× higher | — | next | next |
| Prevalence: 0.3 → 0.4 | `p50_prev0.30_cstat0.60_target0.90_mridge` | 118,528 | `p50_prev0.40_cstat0.60_target0.90_mridge` | 22,224 | 5.33× lower | — | previous | previous |
| Prevalence: 0.3 → 0.4 | `p50_prev0.30_cstat0.70_target0.95_mridge` | 112,320 | `p50_prev0.40_cstat0.70_target0.95_mridge` | 24,516 | 4.58× lower | — | previous | previous |
| Prevalence: 0.4 → 0.5 | `p50_prev0.40_cstat0.85_target0.95_mridge` | 9,506 | `p50_prev0.50_cstat0.85_target0.95_mridge` | 33,696 | 3.54× higher | — | previous, next | previous |
| Prevalence: 0.05 → 0.075 | `p5_prev0.05_cstat0.65_target0.85_mridge` | 9,461 | `p5_prev0.07_cstat0.65_target0.85_mridge` | 3,077 | 3.07× lower | — | — | — |
| Prevalence: 0.05 → 0.075 | `p5_prev0.05_cstat0.75_target0.95_mridge` | 79,130 | `p5_prev0.07_cstat0.75_target0.95_mridge` | 14,224 | 5.56× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p5_prev0.05_cstat0.80_target0.95_mridge` | 17,372 | `p5_prev0.07_cstat0.80_target0.95_mridge` | 53,376 | 3.07× higher | — | next | next |
| Prevalence: 0.05 → 0.075 | `p5_prev0.05_cstat0.85_target0.90_mridge` | 9,408 | `p5_prev0.07_cstat0.85_target0.90_mridge` | 50,240 | 5.34× higher | — | previous, next | — |
| Prevalence: 0.075 → 0.1 | `p5_prev0.07_cstat0.60_target0.95_mridge` | 87,921 | `p5_prev0.10_cstat0.60_target0.95_mridge` | 320,000 | 3.64× higher | — | next | — |
| Prevalence: 0.075 → 0.1 | `p5_prev0.07_cstat0.65_target0.95_mridge` | 196,928 | `p5_prev0.10_cstat0.65_target0.95_mridge` | 36,928 | 5.33× lower | — | previous | — |
| Prevalence: 0.075 → 0.1 | `p5_prev0.07_cstat0.75_target0.90_mridge` | 14,224 | `p5_prev0.10_cstat0.75_target0.90_mridge` | 2,034 | 6.99× lower | — | previous | — |
| Prevalence: 0.075 → 0.1 | `p5_prev0.07_cstat0.80_target0.95_mridge` | 53,376 | `p5_prev0.10_cstat0.80_target0.95_mridge` | 15,295 | 3.49× lower | — | previous, next | previous |
| Prevalence: 0.1 → 0.15 | `p5_prev0.10_cstat0.60_target0.85_mridge` | 10,555 | `p5_prev0.15_cstat0.60_target0.85_mridge` | 3,334 | 3.17× lower | — | next | next |
| Prevalence: 0.1 → 0.15 | `p5_prev0.10_cstat0.65_target0.95_mridge` | 36,928 | `p5_prev0.15_cstat0.65_target0.95_mridge` | 12,304 | 3.00× lower | — | next | next |
| Prevalence: 0.15 → 0.2 | `p5_prev0.15_cstat0.65_target0.95_mridge` | 12,304 | `p5_prev0.20_cstat0.65_target0.95_mridge` | 36,928 | 3.00× higher | — | previous | previous |
| Prevalence: 0.15 → 0.2 | `p5_prev0.15_cstat0.70_target0.95_mridge` | 11,432 | `p5_prev0.20_cstat0.70_target0.95_mridge` | 54,438 | 4.76× higher | — | next | — |
| Prevalence: 0.15 → 0.2 | `p5_prev0.15_cstat0.75_target0.95_mridge` | 28,416 | `p5_prev0.20_cstat0.75_target0.95_mridge` | 5,328 | 5.33× lower | — | previous | — |
| Prevalence: 0.15 → 0.2 | `p5_prev0.15_cstat0.80_target0.90_mridge` | 1,664 | `p5_prev0.20_cstat0.80_target0.90_mridge` | 4,992 | 3.00× higher | — | next | — |
| Prevalence: 0.2 → 0.25 | `p5_prev0.20_cstat0.70_target0.95_mridge` | 54,438 | `p5_prev0.25_cstat0.70_target0.95_mridge` | 6,752 | 8.06× lower | — | previous | — |
| Prevalence: 0.2 → 0.25 | `p5_prev0.20_cstat0.80_target0.95_mridge` | 26,299 | `p5_prev0.25_cstat0.80_target0.95_mridge` | 3,376 | 7.79× lower | — | previous | previous |
| Prevalence: 0.2 → 0.25 | `p5_prev0.20_cstat0.85_target0.90_mridge` | 1,176 | `p5_prev0.25_cstat0.85_target0.90_mridge` | 7,075 | 6.02× higher | — | next | — |
| Prevalence: 0.25 → 0.3 | `p5_prev0.25_cstat0.80_target0.90_mridge` | 1,776 | `p5_prev0.30_cstat0.80_target0.90_mridge` | 372 | 4.77× lower | — | — | — |
| Prevalence: 0.3 → 0.4 | `p5_prev0.30_cstat0.60_target0.95_mridge` | 22,528 | `p5_prev0.40_cstat0.60_target0.95_mridge` | 135,168 | 6.00× higher | — | next | — |
| Prevalence: 0.3 → 0.4 | `p5_prev0.30_cstat0.85_target0.90_mridge` | 71,614 | `p5_prev0.40_cstat0.85_target0.90_mridge` | 17,792 | 4.03× lower | — | previous, next | — |
| Prevalence: 0.4 → 0.5 | `p5_prev0.40_cstat0.80_target0.95_mridge` | 3,216 | `p5_prev0.50_cstat0.80_target0.95_mridge` | 13,440 | 4.18× higher | — | previous, next | previous |
| Prevalence: 0.05 → 0.075 | `p75_prev0.05_cstat0.60_target0.80_mridge` | 150,000 | `p75_prev0.07_cstat0.60_target0.80_mridge` | 50,000 | 3.00× lower | — | previous | — |
| Prevalence: 0.05 → 0.2 | `p75_prev0.05_cstat0.65_target0.95_mridge` | 276,924 | `p75_prev0.20_cstat0.65_target0.95_mridge` | 69,232 | 4.00× lower | Yes | previous | — |
| Prevalence: 0.05 → 0.075 | `p75_prev0.05_cstat0.70_target0.85_mridge` | 32,143 | `p75_prev0.07_cstat0.70_target0.85_mridge` | 5,357 | 6.00× lower | — | previous | — |
| Prevalence: 0.05 → 0.075 | `p75_prev0.05_cstat0.80_target0.85_mridge` | 9,649 | `p75_prev0.07_cstat0.80_target0.85_mridge` | 3,125 | 3.09× lower | — | next | next |
| Prevalence: 0.075 → 0.1 | `p75_prev0.07_cstat0.60_target0.95_mridge` | 199,854 | `p75_prev0.10_cstat0.60_target0.95_mridge` | 37,500 | 5.33× lower | — | previous | — |
| Prevalence: 0.075 → 0.1 | `p75_prev0.07_cstat0.65_target0.85_mridge` | 17,725 | `p75_prev0.10_cstat0.65_target0.85_mridge` | 69,230 | 3.91× higher | — | next | next |
| Prevalence: 0.075 → 0.1 | `p75_prev0.07_cstat0.75_target0.95_mridge` | 81,490 | `p75_prev0.10_cstat0.75_target0.95_mridge` | 9,945 | 8.19× lower | — | previous, next | next |
| Prevalence: 0.1 → 0.15 | `p75_prev0.10_cstat0.60_target0.80_mridge` | 37,500 | `p75_prev0.15_cstat0.60_target0.80_mridge` | 4,884 | 7.68× lower | — | previous, next | next |
| Prevalence: 0.1 → 0.15 | `p75_prev0.10_cstat0.60_target0.85_mridge` | 75,000 | `p75_prev0.15_cstat0.60_target0.85_mridge` | 12,500 | 6.00× lower | — | previous, next | next |
| Prevalence: 0.1 → 0.15 | `p75_prev0.10_cstat0.65_target0.85_mridge` | 69,230 | `p75_prev0.15_cstat0.65_target0.85_mridge` | 11,538 | 6.00× lower | — | previous | previous |
| Prevalence: 0.1 → 0.15 | `p75_prev0.10_cstat0.75_target0.90_mridge` | 20,000 | `p75_prev0.15_cstat0.75_target0.90_mridge` | 6,554 | 3.05× lower | — | — | — |
| Prevalence: 0.1 → 0.15 | `p75_prev0.10_cstat0.75_target0.95_mridge` | 9,945 | `p75_prev0.15_cstat0.75_target0.95_mridge` | 38,257 | 3.85× higher | — | previous, next | previous |
| Prevalence: 0.15 → 0.2 | `p75_prev0.15_cstat0.85_target0.95_mridge` | 94,112 | `p75_prev0.20_cstat0.85_target0.95_mridge` | 15,806 | 5.95× lower | — | previous | — |
| Prevalence: 0.25 → 0.3 | `p75_prev0.25_cstat0.60_target0.80_mridge` | 11,533 | `p75_prev0.30_cstat0.60_target0.80_mridge` | 3,083 | 3.74× lower | — | next | next |
| Prevalence: 0.3 → 0.4 | `p75_prev0.30_cstat0.65_target0.95_mridge` | 84,866 | `p75_prev0.40_cstat0.65_target0.95_mridge` | 15,763 | 5.38× lower | — | previous | — |
| Prevalence: 0.4 → 0.5 | `p75_prev0.40_cstat0.60_target0.90_mridge` | 33,217 | `p75_prev0.50_cstat0.60_target0.90_mridge` | 6,668 | 4.98× lower | — | next | next |
| Prevalence: 0.4 → 0.5 | `p75_prev0.40_cstat0.85_target0.95_mridge` | 7,750 | `p75_prev0.50_cstat0.85_target0.95_mridge` | 25,264 | 3.26× higher | — | next | — |
| Predictors: 10 → 20 | `p10_prev0.05_cstat0.60_target0.90_mridge` | 20,000 | `p20_prev0.05_cstat0.60_target0.90_mridge` | 160,000 | 8.00× higher | Yes | next | — |
| Predictors: 10 → 15 | `p10_prev0.05_cstat0.60_target0.95_mridge` | 320,000 | `p15_prev0.05_cstat0.60_target0.95_mridge` | 60,000 | 5.33× lower | — | previous | — |
| Predictors: 10 → 15 | `p10_prev0.05_cstat0.75_target0.95_mridge` | 20,770 | `p15_prev0.05_cstat0.75_target0.95_mridge` | 128,000 | 6.16× higher | — | next | — |
| Predictors: 10 → 15 | `p10_prev0.05_cstat0.80_target0.85_mridge` | 1,250 | `p15_prev0.05_cstat0.80_target0.85_mridge` | 3,750 | 3.00× higher | — | — | — |
| Predictors: 10 → 15 | `p10_prev0.07_cstat0.60_target0.85_mridge` | 13,334 | `p15_prev0.07_cstat0.60_target0.85_mridge` | 80,000 | 6.00× higher | — | next | next |
| Predictors: 10 → 15 | `p10_prev0.07_cstat0.75_target0.85_mridge` | 2,650 | `p15_prev0.07_cstat0.75_target0.85_mridge` | 10,668 | 4.03× higher | — | next | next |
| Predictors: 10 → 15 | `p10_prev0.07_cstat0.80_target0.95_mridge` | 82,409 | `p15_prev0.07_cstat0.80_target0.95_mridge` | 9,634 | 8.55× lower | — | previous, next | previous, next |
| Predictors: 10 → 15 | `p10_prev0.07_cstat0.85_target0.90_mridge` | 1,171 | `p15_prev0.07_cstat0.85_target0.90_mridge` | 4,706 | 4.02× higher | — | — | — |
| Predictors: 10 → 15 | `p10_prev0.10_cstat0.60_target0.95_mridge` | 80,000 | `p15_prev0.10_cstat0.60_target0.95_mridge` | 240,000 | 3.00× higher | — | previous, next | previous |
| Predictors: 10 → 15 | `p10_prev0.10_cstat0.70_target0.95_mridge` | 68,576 | `p15_prev0.10_cstat0.70_target0.95_mridge` | 205,728 | 3.00× higher | — | previous, next | — |
| Predictors: 10 → 15 | `p10_prev0.10_cstat0.80_target0.95_mridge` | 80,000 | `p15_prev0.10_cstat0.80_target0.95_mridge` | 15,701 | 5.10× lower | — | previous | previous |
| Predictors: 10 → 15 | `p10_prev0.15_cstat0.60_target0.85_mridge` | 13,332 | `p15_prev0.15_cstat0.60_target0.85_mridge` | 40,000 | 3.00× higher | — | — | — |
| Predictors: 10 → 15 | `p10_prev0.15_cstat0.60_target0.90_mridge` | 13,226 | `p15_prev0.15_cstat0.60_target0.90_mridge` | 39,873 | 3.01× higher | — | — | — |
| Predictors: 10 → 15 | `p10_prev0.15_cstat0.70_target0.95_mridge` | 45,712 | `p15_prev0.15_cstat0.70_target0.95_mridge` | 137,152 | 3.00× higher | — | previous, next | — |
| Predictors: 10 → 15 | `p10_prev0.15_cstat0.85_target0.80_mridge` | 392 | `p15_prev0.15_cstat0.85_target0.80_mridge` | 1,176 | 3.00× higher | — | — | — |
| Predictors: 10 → 15 | `p10_prev0.20_cstat0.85_target0.90_mridge` | 9,408 | `p15_prev0.20_cstat0.85_target0.90_mridge` | 1,767 | 5.32× lower | — | previous | previous |
| Predictors: 10 → 15 | `p10_prev0.25_cstat0.60_target0.85_mridge` | 7,536 | `p15_prev0.25_cstat0.60_target0.85_mridge` | 45,184 | 6.00× higher | — | next | next |
| Predictors: 10 → 15 | `p10_prev0.25_cstat0.65_target0.95_mridge` | 12,466 | `p15_prev0.25_cstat0.65_target0.95_mridge` | 80,896 | 6.49× higher | — | previous, next | previous |
| Predictors: 10 → 15 | `p10_prev0.25_cstat0.85_target0.90_mridge` | 1,288 | `p15_prev0.25_cstat0.85_target0.90_mridge` | 10,672 | 8.29× higher | — | previous, next | previous, next |
| Predictors: 10 → 15 | `p10_prev0.30_cstat0.85_target0.90_mridge` | 5,919 | `p15_prev0.30_cstat0.85_target0.90_mridge` | 1,522 | 3.89× lower | — | previous | — |
| Predictors: 10 → 15 | `p10_prev0.40_cstat0.60_target0.95_mridge` | 67,328 | `p15_prev0.40_cstat0.60_target0.95_mridge` | 202,240 | 3.00× higher | — | previous, next | next |
| Predictors: 10 → 15 | `p10_prev0.40_cstat0.65_target0.85_mridge` | 2,352 | `p15_prev0.40_cstat0.65_target0.85_mridge` | 7,056 | 3.00× higher | — | next | next |
| Predictors: 10 → 15 | `p10_prev0.40_cstat0.85_target0.95_mridge` | 3,246 | `p15_prev0.40_cstat0.85_target0.95_mridge` | 12,640 | 3.89× higher | — | previous, next | previous |
| Predictors: 10 → 15 | `p10_prev0.50_cstat0.65_target0.95_mridge` | 65,550 | `p15_prev0.50_cstat0.65_target0.95_mridge` | 20,616 | 3.18× lower | — | previous | previous |
| Predictors: 10 → 15 | `p10_prev0.50_cstat0.80_target0.95_mridge` | 1,688 | `p15_prev0.50_cstat0.80_target0.95_mridge` | 10,112 | 5.99× higher | — | previous, next | previous |
| Predictors: 10 → 15 | `p10_prev0.50_cstat0.85_target0.95_mridge` | 1,688 | `p15_prev0.50_cstat0.85_target0.95_mridge` | 12,549 | 7.43× higher | — | previous, next | previous |
| Predictors: 15 → 20 | `p15_prev0.05_cstat0.85_target0.95_mridge` | 56,464 | `p20_prev0.05_cstat0.85_target0.95_mridge` | 14,088 | 4.01× lower | — | previous | previous |
| Predictors: 15 → 20 | `p15_prev0.07_cstat0.60_target0.85_mridge` | 80,000 | `p20_prev0.07_cstat0.60_target0.85_mridge` | 19,086 | 4.19× lower | — | previous | previous |
| Predictors: 15 → 30 | `p15_prev0.07_cstat0.60_target0.95_mridge` | 101,621 | `p30_prev0.07_cstat0.60_target0.95_mridge` | 320,000 | 3.15× higher | Yes | next | next |
| Predictors: 15 → 20 | `p15_prev0.07_cstat0.65_target0.95_mridge` | 295,392 | `p20_prev0.07_cstat0.65_target0.95_mridge` | 49,232 | 6.00× lower | — | previous | — |
| Predictors: 15 → 20 | `p15_prev0.07_cstat0.70_target0.95_mridge` | 137,308 | `p20_prev0.07_cstat0.70_target0.95_mridge` | 42,321 | 3.24× lower | — | previous | — |
| Predictors: 15 → 20 | `p15_prev0.07_cstat0.75_target0.85_mridge` | 10,668 | `p20_prev0.07_cstat0.75_target0.85_mridge` | 3,556 | 3.00× lower | — | previous | previous |
| Predictors: 15 → 20 | `p15_prev0.07_cstat0.75_target0.90_mridge` | 5,232 | `p20_prev0.07_cstat0.75_target0.90_mridge` | 28,448 | 5.44× higher | — | next | next |
| Predictors: 15 → 20 | `p15_prev0.15_cstat0.65_target0.95_mridge` | 29,787 | `p20_prev0.15_cstat0.65_target0.95_mridge` | 256,742 | 8.62× higher | — | next | — |
| Predictors: 15 → 20 | `p15_prev0.15_cstat0.70_target0.95_mridge` | 137,152 | `p20_prev0.15_cstat0.70_target0.95_mridge` | 16,405 | 8.36× lower | — | previous | — |
| Predictors: 15 → 20 | `p15_prev0.15_cstat0.85_target0.95_mridge` | 3,861 | `p20_prev0.15_cstat0.85_target0.95_mridge` | 12,544 | 3.25× higher | — | previous | previous |
| Predictors: 15 → 20 | `p15_prev0.20_cstat0.65_target0.90_mridge` | 13,848 | `p20_prev0.20_cstat0.65_target0.90_mridge` | 4,615 | 3.00× lower | — | — | — |
| Predictors: 15 → 20 | `p15_prev0.20_cstat0.65_target0.95_mridge` | 55,392 | `p20_prev0.20_cstat0.65_target0.95_mridge` | 18,460 | 3.00× lower | — | — | — |
| Predictors: 15 → 20 | `p15_prev0.25_cstat0.60_target0.85_mridge` | 45,184 | `p20_prev0.25_cstat0.60_target0.85_mridge` | 14,607 | 3.09× lower | — | previous | previous |
| Predictors: 15 → 20 | `p15_prev0.25_cstat0.60_target0.95_mridge` | 33,827 | `p20_prev0.25_cstat0.60_target0.95_mridge` | 107,776 | 3.19× higher | — | previous | previous |
| Predictors: 15 → 20 | `p15_prev0.25_cstat0.85_target0.90_mridge` | 10,672 | `p20_prev0.25_cstat0.85_target0.90_mridge` | 1,764 | 6.05× lower | — | previous | previous |
| Predictors: 15 → 20 | `p15_prev0.30_cstat0.60_target0.95_mridge` | 134,656 | `p20_prev0.30_cstat0.60_target0.95_mridge` | 22,464 | 5.99× lower | — | previous, next | next |
| Predictors: 15 → 20 | `p15_prev0.30_cstat0.85_target0.95_mridge` | 2,104 | `p20_prev0.30_cstat0.85_target0.95_mridge` | 11,232 | 5.34× higher | — | previous, next | previous |
| Predictors: 15 → 20 | `p15_prev0.40_cstat0.60_target0.95_mridge` | 202,240 | `p20_prev0.40_cstat0.60_target0.95_mridge` | 67,328 | 3.00× lower | — | previous | previous |
| Predictors: 15 → 20 | `p15_prev0.40_cstat0.65_target0.85_mridge` | 7,056 | `p20_prev0.40_cstat0.65_target0.85_mridge` | 2,318 | 3.04× lower | — | previous | previous |
| Predictors: 15 → 20 | `p15_prev0.50_cstat0.60_target0.95_mridge` | 323,584 | `p20_prev0.50_cstat0.60_target0.95_mridge` | 38,604 | 8.38× lower | — | previous | — |
| Predictors: 20 → 25 | `p20_prev0.05_cstat0.60_target0.85_mridge` | 80,000 | `p25_prev0.05_cstat0.60_target0.85_mridge` | 24,395 | 3.28× lower | — | previous | previous |
| Predictors: 20 → 25 | `p20_prev0.05_cstat0.75_target0.95_mridge` | 170,656 | `p25_prev0.05_cstat0.75_target0.95_mridge` | 20,719 | 8.24× lower | — | previous | — |
| Predictors: 20 → 25 | `p20_prev0.05_cstat0.80_target0.95_mridge` | 9,345 | `p25_prev0.05_cstat0.80_target0.95_mridge` | 50,000 | 5.35× higher | — | next | — |
| Predictors: 20 → 25 | `p20_prev0.07_cstat0.80_target0.95_mridge` | 106,688 | `p25_prev0.07_cstat0.80_target0.95_mridge` | 21,693 | 4.92× lower | — | previous, next | — |
| Predictors: 20 → 25 | `p20_prev0.10_cstat0.85_target0.95_mridge` | 37,648 | `p25_prev0.10_cstat0.85_target0.95_mridge` | 5,882 | 6.40× lower | — | previous, next | next |
| Predictors: 20 → 25 | `p20_prev0.15_cstat0.65_target0.95_mridge` | 256,742 | `p25_prev0.15_cstat0.65_target0.95_mridge` | 57,362 | 4.48× lower | — | previous | — |
| Predictors: 20 → 25 | `p20_prev0.20_cstat0.65_target0.80_mridge` | 4,615 | `p25_prev0.20_cstat0.65_target0.80_mridge` | 1,442 | 3.20× lower | — | — | — |
| Predictors: 20 → 25 | `p20_prev0.20_cstat0.70_target0.95_mridge` | 24,197 | `p25_prev0.20_cstat0.70_target0.95_mridge` | 85,712 | 3.54× higher | — | next | next |
| Predictors: 20 → 25 | `p20_prev0.25_cstat0.60_target0.85_mridge` | 14,607 | `p25_prev0.25_cstat0.60_target0.85_mridge` | 2,923 | 5.00× lower | — | next | next |
| Predictors: 20 → 25 | `p20_prev0.30_cstat0.60_target0.95_mridge` | 22,464 | `p25_prev0.30_cstat0.60_target0.95_mridge` | 112,256 | 5.00× higher | — | previous, next | previous |
| Predictors: 20 → 25 | `p20_prev0.30_cstat0.65_target0.80_mridge` | 1,246 | `p25_prev0.30_cstat0.65_target0.80_mridge` | 4,043 | 3.24× higher | — | — | — |
| Predictors: 20 → 25 | `p20_prev0.30_cstat0.75_target0.95_mridge` | 11,259 | `p25_prev0.30_cstat0.75_target0.95_mridge` | 56,128 | 4.99× higher | — | next | next |
| Predictors: 20 → 25 | `p20_prev0.40_cstat0.60_target0.95_mridge` | 67,328 | `p25_prev0.40_cstat0.60_target0.95_mridge` | 336,896 | 5.00× higher | — | next | next |
| Predictors: 20 → 25 | `p20_prev0.50_cstat0.60_target0.95_mridge` | 38,604 | `p25_prev0.50_cstat0.60_target0.95_mridge` | 134,656 | 3.49× higher | — | next | — |
| Predictors: 20 → 25 | `p20_prev0.50_cstat0.80_target0.95_mridge` | 6,736 | `p25_prev0.50_cstat0.80_target0.95_mridge` | 34,030 | 5.05× higher | — | next | next |
| Predictors: 25 → 30 | `p25_prev0.05_cstat0.60_target0.90_mridge` | 149,087 | `p30_prev0.05_cstat0.60_target0.90_mridge` | 43,545 | 3.42× lower | — | previous, next | next |
| Predictors: 25 → 75 | `p25_prev0.05_cstat0.65_target0.95_mridge` | 92,308 | `p75_prev0.05_cstat0.65_target0.95_mridge` | 276,924 | 3.00× higher | Yes | next | — |
| Predictors: 25 → 30 | `p25_prev0.05_cstat0.70_target0.90_mridge` | 28,455 | `p30_prev0.05_cstat0.70_target0.90_mridge` | 6,017 | 4.73× lower | — | next | next |
| Predictors: 25 → 30 | `p25_prev0.05_cstat0.75_target0.95_mridge` | 20,719 | `p30_prev0.05_cstat0.75_target0.95_mridge` | 64,000 | 3.09× higher | — | next | — |
| Predictors: 25 → 30 | `p25_prev0.05_cstat0.85_target0.95_mridge` | 11,555 | `p30_prev0.05_cstat0.85_target0.95_mridge` | 56,472 | 4.89× higher | — | next | next |
| Predictors: 25 → 30 | `p25_prev0.07_cstat0.60_target0.85_mridge` | 14,069 | `p30_prev0.07_cstat0.60_target0.85_mridge` | 80,000 | 5.69× higher | — | next | next |
| Predictors: 25 → 30 | `p25_prev0.07_cstat0.60_target0.90_mridge` | 533,344 | `p30_prev0.07_cstat0.60_target0.90_mridge` | 80,000 | 6.67× lower | — | previous | previous |
| Predictors: 25 → 30 | `p25_prev0.07_cstat0.75_target0.95_mridge` | 35,458 | `p30_prev0.07_cstat0.75_target0.95_mridge` | 10,666 | 3.32× lower | — | next | next |
| Predictors: 25 → 30 | `p25_prev0.10_cstat0.65_target0.80_mridge` | 11,538 | `p30_prev0.10_cstat0.65_target0.80_mridge` | 3,153 | 3.66× lower | — | next | next |
| Predictors: 25 → 30 | `p25_prev0.10_cstat0.80_target0.80_mridge` | 606 | `p30_prev0.10_cstat0.80_target0.80_mridge` | 1,932 | 3.19× higher | — | — | — |
| Predictors: 25 → 30 | `p25_prev0.10_cstat0.85_target0.95_mridge` | 5,882 | `p30_prev0.10_cstat0.85_target0.95_mridge` | 32,205 | 5.48× higher | — | previous, next | previous |
| Predictors: 25 → 40 | `p25_prev0.15_cstat0.60_target0.95_mridge` | 65,214 | `p40_prev0.15_cstat0.60_target0.95_mridge` | 313,459 | 4.81× higher | Yes | next | next |
| Predictors: 25 → 30 | `p25_prev0.20_cstat0.80_target0.95_mridge` | 49,984 | `p30_prev0.20_cstat0.80_target0.95_mridge` | 12,187 | 4.10× lower | — | previous | previous |
| Predictors: 25 → 30 | `p25_prev0.25_cstat0.60_target0.95_mridge` | 62,323 | `p30_prev0.25_cstat0.60_target0.95_mridge` | 323,328 | 5.19× higher | — | next | — |
| Predictors: 25 → 30 | `p25_prev0.25_cstat0.65_target0.95_mridge` | 134,784 | `p30_prev0.25_cstat0.65_target0.95_mridge` | 37,567 | 3.59× lower | — | previous | — |
| Predictors: 25 → 30 | `p25_prev0.25_cstat0.85_target0.95_mridge` | 16,848 | `p30_prev0.25_cstat0.85_target0.95_mridge` | 5,052 | 3.33× lower | — | previous, next | next |
| Predictors: 25 → 30 | `p25_prev0.30_cstat0.65_target0.95_mridge` | 28,064 | `p30_prev0.30_cstat0.65_target0.95_mridge` | 134,784 | 4.80× higher | — | next | — |
| Predictors: 25 → 30 | `p25_prev0.30_cstat0.75_target0.95_mridge` | 56,128 | `p30_prev0.30_cstat0.75_target0.95_mridge` | 8,102 | 6.93× lower | — | previous | previous |
| Predictors: 25 → 30 | `p25_prev0.40_cstat0.60_target0.95_mridge` | 336,896 | `p30_prev0.40_cstat0.60_target0.95_mridge` | 50,114 | 6.72× lower | — | previous, next | previous, next |
| Predictors: 25 → 30 | `p25_prev0.40_cstat0.65_target0.95_mridge` | 42,112 | `p30_prev0.40_cstat0.65_target0.95_mridge` | 201,984 | 4.80× higher | — | next | next |
| Predictors: 25 → 30 | `p25_prev0.50_cstat0.60_target0.95_mridge` | 134,656 | `p30_prev0.50_cstat0.60_target0.95_mridge` | 20,224 | 6.66× lower | — | previous, next | next |
| Predictors: 25 → 30 | `p25_prev0.50_cstat0.80_target0.95_mridge` | 34,030 | `p30_prev0.50_cstat0.80_target0.95_mridge` | 8,014 | 4.25× lower | — | previous | previous |
| Predictors: 30 → 40 | `p30_prev0.05_cstat0.60_target0.90_mridge` | 43,545 | `p40_prev0.05_cstat0.60_target0.90_mridge` | 153,338 | 3.52× higher | — | previous | previous |
| Predictors: 30 → 40 | `p30_prev0.05_cstat0.65_target0.85_mridge` | 26,021 | `p40_prev0.05_cstat0.65_target0.85_mridge` | 147,692 | 5.68× higher | — | next | — |
| Predictors: 30 → 40 | `p30_prev0.05_cstat0.65_target0.90_mridge` | 110,768 | `p40_prev0.05_cstat0.65_target0.90_mridge` | 36,580 | 3.03× lower | — | previous | — |
| Predictors: 30 → 40 | `p30_prev0.05_cstat0.70_target0.85_mridge` | 13,233 | `p40_prev0.05_cstat0.70_target0.85_mridge` | 4,263 | 3.10× lower | — | — | — |
| Predictors: 30 → 40 | `p30_prev0.05_cstat0.70_target0.90_mridge` | 6,017 | `p40_prev0.05_cstat0.70_target0.90_mridge` | 31,547 | 5.24× higher | — | previous, next | previous |
| Predictors: 30 → 40 | `p30_prev0.05_cstat0.85_target0.95_mridge` | 56,472 | `p40_prev0.05_cstat0.85_target0.95_mridge` | 18,321 | 3.08× lower | — | previous | previous |
| Predictors: 30 → 50 | `p30_prev0.07_cstat0.60_target0.95_mridge` | 320,000 | `p50_prev0.07_cstat0.60_target0.95_mridge` | 105,914 | 3.02× lower | Yes | previous | previous |
| Predictors: 30 → 40 | `p30_prev0.07_cstat0.65_target0.90_mridge` | 73,848 | `p40_prev0.07_cstat0.65_target0.90_mridge` | 24,615 | 3.00× lower | — | previous | — |
| Predictors: 30 → 40 | `p30_prev0.07_cstat0.70_target0.95_mridge` | 137,144 | `p40_prev0.07_cstat0.70_target0.95_mridge` | 45,714 | 3.00× lower | — | previous | — |
| Predictors: 30 → 40 | `p30_prev0.07_cstat0.75_target0.95_mridge` | 10,666 | `p40_prev0.07_cstat0.75_target0.95_mridge` | 54,074 | 5.07× higher | — | previous, next | previous |
| Predictors: 30 → 75 | `p30_prev0.10_cstat0.60_target0.95_mridge` | 120,000 | `p75_prev0.10_cstat0.60_target0.95_mridge` | 37,500 | 3.20× lower | Yes | previous | previous |
| Predictors: 30 → 40 | `p30_prev0.15_cstat0.80_target0.95_mridge` | 15,710 | `p40_prev0.15_cstat0.80_target0.95_mridge` | 106,688 | 6.79× higher | — | next | — |
| Predictors: 30 → 40 | `p30_prev0.20_cstat0.60_target0.95_mridge` | 480,000 | `p40_prev0.20_cstat0.60_target0.95_mridge` | 71,028 | 6.76× lower | — | previous | — |
| Predictors: 30 → 40 | `p30_prev0.20_cstat0.65_target0.90_mridge` | 13,846 | `p40_prev0.20_cstat0.65_target0.90_mridge` | 73,848 | 5.33× higher | — | next | — |
| Predictors: 30 → 40 | `p30_prev0.20_cstat0.65_target0.95_mridge` | 55,384 | `p40_prev0.20_cstat0.65_target0.95_mridge` | 295,392 | 5.33× higher | — | next | next |
| Predictors: 30 → 40 | `p30_prev0.20_cstat0.85_target0.95_mridge` | 28,240 | `p40_prev0.20_cstat0.85_target0.95_mridge` | 9,412 | 3.00× lower | — | previous | — |
| Predictors: 30 → 40 | `p30_prev0.25_cstat0.60_target0.85_mridge` | 8,358 | `p40_prev0.25_cstat0.60_target0.85_mridge` | 30,112 | 3.60× higher | — | next | — |
| Predictors: 30 → 40 | `p30_prev0.25_cstat0.60_target0.95_mridge` | 323,328 | `p40_prev0.25_cstat0.60_target0.95_mridge` | 107,737 | 3.00× lower | — | previous, next | — |
| Predictors: 30 → 40 | `p30_prev0.25_cstat0.70_target0.95_mridge` | 40,416 | `p40_prev0.25_cstat0.70_target0.95_mridge` | 13,225 | 3.06× lower | — | previous | — |
| Predictors: 30 → 40 | `p30_prev0.25_cstat0.85_target0.95_mridge` | 5,052 | `p40_prev0.25_cstat0.85_target0.95_mridge` | 26,944 | 5.33× higher | — | previous, next | previous |
| Predictors: 30 → 40 | `p30_prev0.30_cstat0.60_target0.95_mridge` | 269,568 | `p40_prev0.30_cstat0.60_target0.95_mridge` | 42,121 | 6.40× lower | — | previous | — |
| Predictors: 30 → 40 | `p30_prev0.30_cstat0.75_target0.90_mridge` | 8,888 | `p40_prev0.30_cstat0.75_target0.90_mridge` | 2,855 | 3.11× lower | — | — | — |
| Predictors: 30 → 40 | `p30_prev0.40_cstat0.60_target0.95_mridge` | 50,114 | `p40_prev0.40_cstat0.60_target0.95_mridge` | 344,162 | 6.87× higher | — | previous, next | previous |
| Predictors: 30 → 40 | `p30_prev0.40_cstat0.65_target0.95_mridge` | 201,984 | `p40_prev0.40_cstat0.65_target0.95_mridge` | 41,623 | 4.85× lower | — | previous, next | previous |
| Predictors: 30 → 40 | `p30_prev0.50_cstat0.60_target0.95_mridge` | 20,224 | `p40_prev0.50_cstat0.60_target0.95_mridge` | 107,776 | 5.33× higher | — | previous, next | previous, next |
| Predictors: 40 → 75 | `p40_prev0.05_cstat0.60_target0.80_mridge` | 24,314 | `p75_prev0.05_cstat0.60_target0.80_mridge` | 150,000 | 6.17× higher | Yes | next | — |
| Predictors: 40 → 50 | `p40_prev0.05_cstat0.85_target0.85_mridge` | 3,726 | `p50_prev0.05_cstat0.85_target0.85_mridge` | 11,765 | 3.16× higher | — | — | — |
| Predictors: 40 → 50 | `p40_prev0.07_cstat0.60_target0.85_mridge` | 64,623 | `p50_prev0.07_cstat0.60_target0.85_mridge` | 16,298 | 3.97× lower | — | previous, next | next |
| Predictors: 40 → 50 | `p40_prev0.07_cstat0.70_target0.80_mridge` | 2,803 | `p50_prev0.07_cstat0.70_target0.80_mridge` | 14,285 | 5.10× higher | — | next | — |
| Predictors: 40 → 50 | `p40_prev0.10_cstat0.65_target0.95_mridge` | 27,755 | `p50_prev0.10_cstat0.65_target0.95_mridge` | 135,980 | 4.90× higher | — | next | — |
| Predictors: 40 → 50 | `p40_prev0.10_cstat0.70_target0.95_mridge` | 66,704 | `p50_prev0.10_cstat0.70_target0.95_mridge` | 20,763 | 3.21× lower | — | next | next |
| Predictors: 40 → 75 | `p40_prev0.15_cstat0.60_target0.85_mridge` | 53,332 | `p75_prev0.15_cstat0.60_target0.85_mridge` | 12,500 | 4.27× lower | Yes | previous, next | previous, next |
| Predictors: 40 → 75 | `p40_prev0.15_cstat0.60_target0.95_mridge` | 313,459 | `p75_prev0.15_cstat0.60_target0.95_mridge` | 100,000 | 3.13× lower | Yes | previous | previous |
| Predictors: 40 → 50 | `p40_prev0.15_cstat0.80_target0.95_mridge` | 106,688 | `p50_prev0.15_cstat0.80_target0.95_mridge` | 25,730 | 4.15× lower | — | previous, next | — |
| Predictors: 40 → 50 | `p40_prev0.20_cstat0.65_target0.90_mridge` | 73,848 | `p50_prev0.20_cstat0.65_target0.90_mridge` | 23,581 | 3.13× lower | — | previous | — |
| Predictors: 40 → 50 | `p40_prev0.20_cstat0.65_target0.95_mridge` | 295,392 | `p50_prev0.20_cstat0.65_target0.95_mridge` | 43,737 | 6.75× lower | — | previous | previous |
| Predictors: 40 → 50 | `p40_prev0.20_cstat0.85_target0.95_mridge` | 9,412 | `p50_prev0.20_cstat0.85_target0.95_mridge` | 47,056 | 5.00× higher | — | next | next |
| Predictors: 40 → 50 | `p40_prev0.25_cstat0.65_target0.95_mridge` | 13,008 | `p50_prev0.25_cstat0.65_target0.95_mridge` | 54,320 | 4.18× higher | — | previous | previous |
| Predictors: 40 → 50 | `p40_prev0.25_cstat0.85_target0.95_mridge` | 26,944 | `p50_prev0.25_cstat0.85_target0.95_mridge` | 8,095 | 3.33× lower | — | previous | — |
| Predictors: 40 → 50 | `p40_prev0.30_cstat0.60_target0.90_mridge` | 18,174 | `p50_prev0.30_cstat0.60_target0.90_mridge` | 118,528 | 6.52× higher | — | next | next |
| Predictors: 40 → 50 | `p40_prev0.30_cstat0.70_target0.95_mridge` | 22,448 | `p50_prev0.30_cstat0.70_target0.95_mridge` | 112,320 | 5.00× higher | — | next | next |
| Predictors: 40 → 50 | `p40_prev0.40_cstat0.60_target0.95_mridge` | 344,162 | `p50_prev0.40_cstat0.60_target0.95_mridge` | 84,224 | 4.09× lower | — | previous | — |
| Predictors: 40 → 50 | `p40_prev0.50_cstat0.60_target0.95_mridge` | 107,776 | `p50_prev0.50_cstat0.60_target0.95_mridge` | 30,971 | 3.48× lower | — | previous | previous |
| Predictors: 40 → 50 | `p40_prev0.50_cstat0.85_target0.95_mridge` | 10,287 | `p50_prev0.50_cstat0.85_target0.95_mridge` | 33,696 | 3.28× higher | — | next | — |
| Predictors: 50 → 75 | `p50_prev0.05_cstat0.80_target0.95_mridge` | 200,000 | `p75_prev0.05_cstat0.80_target0.95_mridge` | 35,535 | 5.63× lower | — | previous | — |
| Predictors: 50 → 75 | `p50_prev0.07_cstat0.60_target0.80_mridge` | 16,666 | `p75_prev0.07_cstat0.60_target0.80_mridge` | 50,000 | 3.00× higher | — | — | — |
| Predictors: 50 → 75 | `p50_prev0.07_cstat0.60_target0.85_mridge` | 16,298 | `p75_prev0.07_cstat0.60_target0.85_mridge` | 100,000 | 6.14× higher | — | previous, next | previous |
| Predictors: 50 → 75 | `p50_prev0.07_cstat0.85_target0.95_mridge` | 12,227 | `p75_prev0.07_cstat0.85_target0.95_mridge` | 37,153 | 3.04× higher | — | — | — |
| Predictors: 50 → 75 | `p50_prev0.10_cstat0.60_target0.80_mridge` | 12,412 | `p75_prev0.10_cstat0.60_target0.80_mridge` | 37,500 | 3.02× higher | — | next | — |
| Predictors: 50 → 75 | `p50_prev0.10_cstat0.60_target0.85_mridge` | 23,954 | `p75_prev0.10_cstat0.60_target0.85_mridge` | 75,000 | 3.13× higher | — | next | — |
| Predictors: 50 → 75 | `p50_prev0.10_cstat0.60_target0.90_mridge` | 50,000 | `p75_prev0.10_cstat0.60_target0.90_mridge` | 151,570 | 3.03× higher | — | next | — |
| Predictors: 50 → 75 | `p50_prev0.10_cstat0.65_target0.85_mridge` | 9,584 | `p75_prev0.10_cstat0.65_target0.85_mridge` | 69,230 | 7.22× higher | — | next | next |
| Predictors: 50 → 75 | `p50_prev0.10_cstat0.75_target0.95_mridge` | 44,136 | `p75_prev0.10_cstat0.75_target0.95_mridge` | 9,945 | 4.44× lower | — | next | next |
| Predictors: 50 → 75 | `p50_prev0.15_cstat0.60_target0.80_mridge` | 17,298 | `p75_prev0.15_cstat0.60_target0.80_mridge` | 4,884 | 3.54× lower | — | next | next |
| Predictors: 50 → 75 | `p50_prev0.15_cstat0.85_target0.95_mridge` | 11,478 | `p75_prev0.15_cstat0.85_target0.95_mridge` | 94,112 | 8.20× higher | — | next | — |
| Predictors: 50 → 75 | `p50_prev0.25_cstat0.60_target0.90_mridge` | 34,036 | `p75_prev0.25_cstat0.60_target0.90_mridge` | 106,656 | 3.13× higher | — | next | next |
| Predictors: 50 → 75 | `p50_prev0.30_cstat0.60_target0.95_mridge` | 46,270 | `p75_prev0.30_cstat0.60_target0.95_mridge` | 161,611 | 3.49× higher | — | — | — |
| Predictors: 50 → 75 | `p50_prev0.30_cstat0.65_target0.90_mridge` | 7,265 | `p75_prev0.30_cstat0.65_target0.90_mridge` | 22,224 | 3.06× higher | — | — | — |
| Predictors: 50 → 75 | `p50_prev0.30_cstat0.70_target0.95_mridge` | 112,320 | `p75_prev0.30_cstat0.70_target0.95_mridge` | 37,152 | 3.02× lower | — | previous | previous |
| Predictors: 50 → 75 | `p50_prev0.50_cstat0.60_target0.90_mridge` | 36,308 | `p75_prev0.50_cstat0.60_target0.90_mridge` | 6,668 | 5.45× lower | — | previous, next | next |
| Predictors: 50 → 100 | `p50_prev0.50_cstat0.60_target0.95_mridge` | 30,971 | `p100_prev0.50_cstat0.60_target0.95_mridge` | 196,585 | 6.35× higher | Yes | next | — |
| Predictors: 5 → 10 | `p5_prev0.05_cstat0.70_target0.90_mridge` | 9,999 | `p10_prev0.05_cstat0.70_target0.90_mridge` | 34,284 | 3.43× higher | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.05_cstat0.75_target0.85_mridge` | 21,328 | `p10_prev0.05_cstat0.75_target0.85_mridge` | 4,784 | 4.46× lower | — | previous | previous |
| Predictors: 5 → 10 | `p5_prev0.05_cstat0.75_target0.95_mridge` | 79,130 | `p10_prev0.05_cstat0.75_target0.95_mridge` | 20,770 | 3.81× lower | — | previous | — |
| Predictors: 5 → 10 | `p5_prev0.05_cstat0.85_target0.85_mridge` | 588 | `p10_prev0.05_cstat0.85_target0.85_mridge` | 2,476 | 4.21× higher | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.07_cstat0.65_target0.95_mridge` | 196,928 | `p10_prev0.07_cstat0.65_target0.95_mridge` | 24,616 | 8.00× lower | — | previous, next | next |
| Predictors: 5 → 10 | `p5_prev0.07_cstat0.70_target0.90_mridge` | 6,414 | `p10_prev0.07_cstat0.70_target0.90_mridge` | 22,856 | 3.56× higher | — | next | next |
| Predictors: 5 → 10 | `p5_prev0.07_cstat0.85_target0.95_mridge` | 6,280 | `p10_prev0.07_cstat0.85_target0.95_mridge` | 18,916 | 3.01× higher | — | next | — |
| Predictors: 5 → 10 | `p5_prev0.10_cstat0.60_target0.90_mridge` | 138,663 | `p10_prev0.10_cstat0.60_target0.90_mridge` | 29,006 | 4.78× lower | — | previous | — |
| Predictors: 5 → 10 | `p5_prev0.10_cstat0.60_target0.95_mridge` | 320,000 | `p10_prev0.10_cstat0.60_target0.95_mridge` | 80,000 | 4.00× lower | — | previous, next | next |
| Predictors: 5 → 10 | `p5_prev0.10_cstat0.70_target0.95_mridge` | 15,718 | `p10_prev0.10_cstat0.70_target0.95_mridge` | 68,576 | 4.36× higher | — | next | — |
| Predictors: 5 → 10 | `p5_prev0.10_cstat0.80_target0.95_mridge` | 15,295 | `p10_prev0.10_cstat0.80_target0.95_mridge` | 80,000 | 5.23× higher | — | previous, next | next |
| Predictors: 5 → 10 | `p5_prev0.15_cstat0.60_target0.85_mridge` | 3,334 | `p10_prev0.15_cstat0.60_target0.85_mridge` | 13,332 | 4.00× higher | — | previous | previous |
| Predictors: 5 → 10 | `p5_prev0.15_cstat0.70_target0.80_mridge` | 680 | `p10_prev0.15_cstat0.70_target0.80_mridge` | 2,857 | 4.20× higher | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.15_cstat0.70_target0.95_mridge` | 11,432 | `p10_prev0.15_cstat0.70_target0.95_mridge` | 45,712 | 4.00× higher | — | next | — |
| Predictors: 5 → 10 | `p5_prev0.20_cstat0.65_target0.85_mridge` | 2,421 | `p10_prev0.20_cstat0.65_target0.85_mridge` | 9,232 | 3.81× higher | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.20_cstat0.65_target0.90_mridge` | 73,856 | `p10_prev0.20_cstat0.65_target0.90_mridge` | 9,232 | 8.00× lower | — | previous | previous |
| Predictors: 5 → 10 | `p5_prev0.20_cstat0.70_target0.95_mridge` | 54,438 | `p10_prev0.20_cstat0.70_target0.95_mridge` | 11,757 | 4.63× lower | — | previous, next | next |
| Predictors: 5 → 10 | `p5_prev0.20_cstat0.80_target0.85_mridge` | 312 | `p10_prev0.20_cstat0.80_target0.85_mridge` | 1,250 | 4.01× higher | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.20_cstat0.85_target0.90_mridge` | 1,176 | `p10_prev0.20_cstat0.85_target0.90_mridge` | 9,408 | 8.00× higher | — | next | next |
| Predictors: 5 → 10 | `p5_prev0.25_cstat0.70_target0.90_mridge` | 2,186 | `p10_prev0.25_cstat0.70_target0.90_mridge` | 7,104 | 3.25× higher | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.25_cstat0.70_target0.95_mridge` | 6,752 | `p10_prev0.25_cstat0.70_target0.95_mridge` | 53,888 | 7.98× higher | — | next | next |
| Predictors: 5 → 10 | `p5_prev0.25_cstat0.85_target0.90_mridge` | 7,075 | `p10_prev0.25_cstat0.85_target0.90_mridge` | 1,288 | 5.49× lower | — | previous, next | next |
| Predictors: 5 → 10 | `p5_prev0.30_cstat0.70_target0.90_mridge` | 1,488 | `p10_prev0.30_cstat0.70_target0.90_mridge` | 5,920 | 3.98× higher | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.30_cstat0.75_target0.90_mridge` | 23,808 | `p10_prev0.30_cstat0.75_target0.90_mridge` | 2,960 | 8.04× lower | — | previous | previous |
| Predictors: 5 → 10 | `p5_prev0.30_cstat0.80_target0.90_mridge` | 372 | `p10_prev0.30_cstat0.80_target0.90_mridge` | 2,220 | 5.97× higher | — | next | — |
| Predictors: 5 → 10 | `p5_prev0.30_cstat0.80_target0.95_mridge` | 2,816 | `p10_prev0.30_cstat0.80_target0.95_mridge` | 11,232 | 3.99× higher | — | previous, next | previous |
| Predictors: 5 → 10 | `p5_prev0.30_cstat0.85_target0.95_mridge` | 8,903 | `p10_prev0.30_cstat0.85_target0.95_mridge` | 2,808 | 3.17× lower | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.40_cstat0.85_target0.90_mridge` | 17,792 | `p10_prev0.40_cstat0.85_target0.90_mridge` | 3,353 | 5.31× lower | — | previous, next | — |
| Predictors: 5 → 10 | `p5_prev0.40_cstat0.85_target0.95_mridge` | 16,896 | `p10_prev0.40_cstat0.85_target0.95_mridge` | 3,246 | 5.21× lower | — | previous, next | next |
| Predictors: 5 → 10 | `p5_prev0.50_cstat0.60_target0.80_mridge` | 1,000 | `p10_prev0.50_cstat0.60_target0.80_mridge` | 4,000 | 4.00× higher | — | — | — |
| Predictors: 5 → 10 | `p5_prev0.50_cstat0.60_target0.95_mridge` | 107,520 | `p10_prev0.50_cstat0.60_target0.95_mridge` | 13,504 | 7.96× lower | — | previous | — |
| Predictors: 5 → 10 | `p5_prev0.50_cstat0.65_target0.95_mridge` | 20,164 | `p10_prev0.50_cstat0.65_target0.95_mridge` | 65,550 | 3.25× higher | — | previous, next | next |
| Predictors: 5 → 10 | `p5_prev0.50_cstat0.80_target0.95_mridge` | 13,440 | `p10_prev0.50_cstat0.80_target0.95_mridge` | 1,688 | 7.96× lower | — | previous, next | next |
| Predictors: 5 → 10 | `p5_prev0.50_cstat0.85_target0.95_mridge` | 13,440 | `p10_prev0.50_cstat0.85_target0.95_mridge` | 1,688 | 7.96× lower | — | previous, next | next |
| Predictors: 75 → 100 | `p75_prev0.07_cstat0.60_target0.85_mridge` | 100,000 | `p100_prev0.07_cstat0.60_target0.85_mridge` | 32,608 | 3.07× lower | — | previous | — |
| Predictors: 75 → 100 | `p75_prev0.07_cstat0.70_target0.85_mridge` | 5,357 | `p100_prev0.07_cstat0.70_target0.85_mridge` | 17,433 | 3.25× higher | — | — | — |
| Predictors: 75 → 100 | `p75_prev0.07_cstat0.75_target0.90_mridge` | 21,244 | `p100_prev0.07_cstat0.75_target0.90_mridge` | 71,108 | 3.35× higher | — | next | next |
| Predictors: 75 → 100 | `p75_prev0.10_cstat0.60_target0.80_mridge` | 37,500 | `p100_prev0.10_cstat0.60_target0.80_mridge` | 5,824 | 6.44× lower | — | previous | — |
| Predictors: 75 → 100 | `p75_prev0.10_cstat0.65_target0.85_mridge` | 69,230 | `p100_prev0.10_cstat0.65_target0.85_mridge` | 11,532 | 6.00× lower | — | previous | previous |
| Predictors: 75 → 100 | `p75_prev0.15_cstat0.60_target0.80_mridge` | 4,884 | `p100_prev0.15_cstat0.60_target0.80_mridge` | 18,717 | 3.83× higher | — | previous, next | previous |
| Predictors: 75 → 100 | `p75_prev0.15_cstat0.60_target0.85_mridge` | 12,500 | `p100_prev0.15_cstat0.60_target0.85_mridge` | 43,882 | 3.51× higher | — | previous | previous |
| Predictors: 75 → 100 | `p75_prev0.15_cstat0.85_target0.95_mridge` | 94,112 | `p100_prev0.15_cstat0.85_target0.95_mridge` | 23,476 | 4.01× lower | — | previous | — |
| Predictors: 75 → 100 | `p75_prev0.20_cstat0.60_target0.80_mridge` | 8,900 | `p100_prev0.20_cstat0.60_target0.80_mridge` | 2,585 | 3.44× lower | — | next | next |
| Predictors: 75 → 100 | `p75_prev0.20_cstat0.65_target0.90_mridge` | 17,308 | `p100_prev0.20_cstat0.65_target0.90_mridge` | 92,308 | 5.33× higher | — | next | — |
| Predictors: 75 → 100 | `p75_prev0.25_cstat0.60_target0.90_mridge` | 106,656 | `p100_prev0.25_cstat0.60_target0.90_mridge` | 35,552 | 3.00× lower | — | previous | previous |
| Predictors: 75 → 100 | `p75_prev0.25_cstat0.70_target0.95_mridge` | 25,264 | `p100_prev0.25_cstat0.70_target0.95_mridge` | 134,752 | 5.33× higher | — | next | — |
| Predictors: 75 → 100 | `p75_prev0.30_cstat0.60_target0.80_mridge` | 3,083 | `p100_prev0.30_cstat0.60_target0.80_mridge` | 16,664 | 5.41× higher | — | previous, next | previous |
| Predictors: 75 → 100 | `p75_prev0.30_cstat0.65_target0.95_mridge` | 84,866 | `p100_prev0.30_cstat0.65_target0.95_mridge` | 27,531 | 3.08× lower | — | previous | — |
| Predictors: 75 → 100 | `p75_prev0.40_cstat0.75_target0.90_mridge` | 6,536 | `p100_prev0.40_cstat0.75_target0.90_mridge` | 22,224 | 3.40× higher | — | next | — |
| Predictors: 75 → 100 | `p75_prev0.50_cstat0.60_target0.90_mridge` | 6,668 | `p100_prev0.50_cstat0.60_target0.90_mridge` | 27,106 | 4.07× higher | — | previous | previous |
| Predictors: 75 → 100 | `p75_prev0.50_cstat0.70_target0.95_mridge` | 50,528 | `p100_prev0.50_cstat0.70_target0.95_mridge` | 16,840 | 3.00× lower | — | previous | — |
| Target slope: 0.8 → 0.85 | `p100_prev0.05_cstat0.75_target0.80_mridge` | 6,437 | `p100_prev0.05_cstat0.75_target0.85_mridge` | 26,667 | 4.14× higher | — | next | next |
| Target slope: 0.8 → 0.85 | `p100_prev0.05_cstat0.80_target0.80_mridge` | 6,250 | `p100_prev0.05_cstat0.80_target0.85_mridge` | 25,000 | 4.00× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p100_prev0.05_cstat0.85_target0.85_mridge` | 7,272 | `p100_prev0.05_cstat0.85_target0.90_mridge` | 23,529 | 3.24× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p100_prev0.07_cstat0.65_target0.85_mridge` | 14,338 | `p100_prev0.07_cstat0.65_target0.90_mridge` | 61,538 | 4.29× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p100_prev0.07_cstat0.75_target0.85_mridge` | 10,558 | `p100_prev0.07_cstat0.75_target0.90_mridge` | 71,108 | 6.73× higher | — | next | next |
| Target slope: 0.8 → 0.85 | `p100_prev0.10_cstat0.60_target0.80_mridge` | 5,824 | `p100_prev0.10_cstat0.60_target0.85_mridge` | 50,000 | 8.59× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p100_prev0.10_cstat0.75_target0.85_mridge` | 3,218 | `p100_prev0.10_cstat0.75_target0.90_mridge` | 13,333 | 4.14× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.10_cstat0.75_target0.90_mridge` | 13,333 | `p100_prev0.10_cstat0.75_target0.95_mridge` | 106,664 | 8.00× higher | — | next | next |
| Target slope: 0.8 → 0.85 | `p100_prev0.10_cstat0.85_target0.80_mridge` | 1,253 | `p100_prev0.10_cstat0.85_target0.85_mridge` | 5,882 | 4.69× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p100_prev0.15_cstat0.75_target0.85_mridge` | 4,279 | `p100_prev0.15_cstat0.75_target0.90_mridge` | 13,074 | 3.06× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.15_cstat0.80_target0.90_mridge` | 8,006 | `p100_prev0.15_cstat0.80_target0.95_mridge` | 33,309 | 4.16× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p100_prev0.20_cstat0.65_target0.85_mridge` | 13,060 | `p100_prev0.20_cstat0.65_target0.90_mridge` | 92,308 | 7.07× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p100_prev0.20_cstat0.70_target0.80_mridge` | 2,082 | `p100_prev0.20_cstat0.70_target0.85_mridge` | 7,360 | 3.54× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.20_cstat0.75_target0.90_mridge` | 11,643 | `p100_prev0.20_cstat0.75_target0.95_mridge` | 53,336 | 4.58× higher | — | — | — |
| Target slope: 0.85 → 0.95 | `p100_prev0.20_cstat0.80_target0.85_mridge` | 3,983 | `p100_prev0.20_cstat0.80_target0.95_mridge` | 25,000 | 6.28× higher | Yes | next | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.20_cstat0.85_target0.90_mridge` | 6,062 | `p100_prev0.20_cstat0.85_target0.95_mridge` | 19,376 | 3.20× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.25_cstat0.70_target0.90_mridge` | 14,115 | `p100_prev0.25_cstat0.70_target0.95_mridge` | 134,752 | 9.55× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.25_cstat0.75_target0.90_mridge` | 9,050 | `p100_prev0.25_cstat0.75_target0.95_mridge` | 28,985 | 3.20× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.25_cstat0.85_target0.90_mridge` | 4,444 | `p100_prev0.25_cstat0.85_target0.95_mridge` | 16,844 | 3.79× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p100_prev0.30_cstat0.60_target0.85_mridge` | 15,389 | `p100_prev0.30_cstat0.60_target0.90_mridge` | 60,702 | 3.94× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p100_prev0.30_cstat0.65_target0.80_mridge` | 4,166 | `p100_prev0.30_cstat0.65_target0.85_mridge` | 15,684 | 3.76× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.30_cstat0.70_target0.90_mridge` | 13,956 | `p100_prev0.30_cstat0.70_target0.95_mridge` | 45,540 | 3.26× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.30_cstat0.75_target0.90_mridge` | 8,167 | `p100_prev0.30_cstat0.75_target0.95_mridge` | 26,852 | 3.29× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p100_prev0.30_cstat0.80_target0.85_mridge` | 1,960 | `p100_prev0.30_cstat0.80_target0.90_mridge` | 6,979 | 3.56× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.40_cstat0.70_target0.90_mridge` | 12,469 | `p100_prev0.40_cstat0.70_target0.95_mridge` | 42,112 | 3.38× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p100_prev0.40_cstat0.75_target0.85_mridge` | 2,941 | `p100_prev0.40_cstat0.75_target0.90_mridge` | 22,224 | 7.56× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.40_cstat0.80_target0.90_mridge` | 5,592 | `p100_prev0.40_cstat0.80_target0.95_mridge` | 21,056 | 3.77× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p100_prev0.40_cstat0.85_target0.85_mridge` | 1,472 | `p100_prev0.40_cstat0.85_target0.90_mridge` | 4,916 | 3.34× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.50_cstat0.60_target0.90_mridge` | 27,106 | `p100_prev0.50_cstat0.60_target0.95_mridge` | 196,585 | 7.25× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p100_prev0.50_cstat0.65_target0.85_mridge` | 4,706 | `p100_prev0.50_cstat0.65_target0.90_mridge` | 19,078 | 4.05× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p100_prev0.50_cstat0.70_target0.80_mridge` | 2,866 | `p100_prev0.50_cstat0.70_target0.85_mridge` | 9,412 | 3.28× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p100_prev0.50_cstat0.75_target0.80_mridge` | 1,247 | `p100_prev0.50_cstat0.75_target0.85_mridge` | 4,706 | 3.77× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.50_cstat0.75_target0.90_mridge` | 8,888 | `p100_prev0.50_cstat0.75_target0.95_mridge` | 26,782 | 3.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_prev0.50_cstat0.85_target0.90_mridge` | 4,511 | `p100_prev0.50_cstat0.85_target0.95_mridge` | 33,680 | 7.47× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.05_cstat0.65_target0.90_mridge` | 36,924 | `p10_prev0.05_cstat0.65_target0.95_mridge` | 147,696 | 4.00× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p10_prev0.05_cstat0.70_target0.85_mridge` | 8,596 | `p10_prev0.05_cstat0.70_target0.90_mridge` | 34,284 | 3.99× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p10_prev0.05_cstat0.80_target0.85_mridge` | 1,250 | `p10_prev0.05_cstat0.80_target0.90_mridge` | 7,500 | 6.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.05_cstat0.85_target0.90_mridge` | 3,791 | `p10_prev0.05_cstat0.85_target0.95_mridge` | 18,824 | 4.97× higher | — | — | — |
| Target slope: 0.85 → 0.95 | `p10_prev0.07_cstat0.60_target0.85_mridge` | 13,334 | `p10_prev0.07_cstat0.60_target0.95_mridge` | 53,336 | 4.00× higher | Yes | — | — |
| Target slope: 0.85 → 0.9 | `p10_prev0.07_cstat0.70_target0.85_mridge` | 5,714 | `p10_prev0.07_cstat0.70_target0.90_mridge` | 22,856 | 4.00× higher | — | next | next |
| Target slope: 0.85 → 0.9 | `p10_prev0.07_cstat0.80_target0.85_mridge` | 1,247 | `p10_prev0.07_cstat0.80_target0.90_mridge` | 4,726 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.10_cstat0.70_target0.90_mridge` | 8,572 | `p10_prev0.10_cstat0.70_target0.95_mridge` | 68,576 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.10_cstat0.75_target0.90_mridge` | 5,068 | `p10_prev0.10_cstat0.75_target0.95_mridge` | 21,328 | 4.21× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p10_prev0.10_cstat0.85_target0.90_mridge` | 2,373 | `p10_prev0.10_cstat0.85_target0.95_mridge` | 18,816 | 7.93× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.15_cstat0.60_target0.90_mridge` | 13,226 | `p10_prev0.15_cstat0.60_target0.95_mridge` | 53,328 | 4.03× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.15_cstat0.65_target0.90_mridge` | 5,732 | `p10_prev0.15_cstat0.65_target0.95_mridge` | 24,616 | 4.29× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.15_cstat0.75_target0.90_mridge` | 1,789 | `p10_prev0.15_cstat0.75_target0.95_mridge` | 15,210 | 8.50× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p10_prev0.15_cstat0.80_target0.80_mridge` | 417 | `p10_prev0.15_cstat0.80_target0.85_mridge` | 1,355 | 3.25× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.15_cstat0.80_target0.90_mridge` | 2,491 | `p10_prev0.15_cstat0.80_target0.95_mridge` | 13,344 | 5.36× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.15_cstat0.85_target0.90_mridge` | 1,534 | `p10_prev0.15_cstat0.85_target0.95_mridge` | 6,280 | 4.09× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p10_prev0.20_cstat0.60_target0.85_mridge` | 4,888 | `p10_prev0.20_cstat0.60_target0.90_mridge` | 19,645 | 4.02× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p10_prev0.20_cstat0.65_target0.80_mridge` | 2,437 | `p10_prev0.20_cstat0.65_target0.85_mridge` | 9,232 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.20_cstat0.65_target0.90_mridge` | 9,232 | `p10_prev0.20_cstat0.65_target0.95_mridge` | 36,928 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.20_cstat0.70_target0.90_mridge` | 3,132 | `p10_prev0.20_cstat0.70_target0.95_mridge` | 11,757 | 3.75× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p10_prev0.20_cstat0.75_target0.90_mridge` | 2,668 | `p10_prev0.20_cstat0.75_target0.95_mridge` | 10,672 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.20_cstat0.80_target0.90_mridge` | 2,500 | `p10_prev0.20_cstat0.80_target0.95_mridge` | 10,000 | 4.00× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p10_prev0.25_cstat0.60_target0.80_mridge` | 2,000 | `p10_prev0.25_cstat0.60_target0.85_mridge` | 7,536 | 3.77× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.25_cstat0.60_target0.90_mridge` | 10,564 | `p10_prev0.25_cstat0.60_target0.95_mridge` | 40,550 | 3.84× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p10_prev0.25_cstat0.70_target0.85_mridge` | 1,886 | `p10_prev0.25_cstat0.70_target0.90_mridge` | 7,104 | 3.77× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.25_cstat0.70_target0.90_mridge` | 7,104 | `p10_prev0.25_cstat0.70_target0.95_mridge` | 53,888 | 7.59× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p10_prev0.25_cstat0.75_target0.90_mridge` | 2,317 | `p10_prev0.25_cstat0.75_target0.95_mridge` | 14,428 | 6.23× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.25_cstat0.80_target0.90_mridge` | 1,776 | `p10_prev0.25_cstat0.80_target0.95_mridge` | 6,736 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.25_cstat0.85_target0.90_mridge` | 1,288 | `p10_prev0.25_cstat0.85_target0.95_mridge` | 6,736 | 5.23× higher | — | previous, next | previous |
| Target slope: 0.8 → 0.85 | `p10_prev0.30_cstat0.60_target0.80_mridge` | 1,605 | `p10_prev0.30_cstat0.60_target0.85_mridge` | 6,272 | 3.91× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.30_cstat0.60_target0.90_mridge` | 5,809 | `p10_prev0.30_cstat0.60_target0.95_mridge` | 44,928 | 7.73× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p10_prev0.30_cstat0.70_target0.85_mridge` | 1,671 | `p10_prev0.30_cstat0.70_target0.90_mridge` | 5,920 | 3.54× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p10_prev0.30_cstat0.80_target0.85_mridge` | 713 | `p10_prev0.30_cstat0.80_target0.90_mridge` | 2,220 | 3.11× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.30_cstat0.80_target0.90_mridge` | 2,220 | `p10_prev0.30_cstat0.80_target0.95_mridge` | 11,232 | 5.06× higher | — | previous, next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.40_cstat0.60_target0.90_mridge` | 11,238 | `p10_prev0.40_cstat0.60_target0.95_mridge` | 67,328 | 5.99× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.40_cstat0.65_target0.90_mridge` | 4,170 | `p10_prev0.40_cstat0.65_target0.95_mridge` | 12,624 | 3.03× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.40_cstat0.70_target0.90_mridge` | 2,810 | `p10_prev0.40_cstat0.70_target0.95_mridge` | 9,802 | 3.49× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.40_cstat0.80_target0.90_mridge` | 1,364 | `p10_prev0.40_cstat0.80_target0.95_mridge` | 8,416 | 6.17× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p10_prev0.40_cstat0.85_target0.85_mridge` | 575 | `p10_prev0.40_cstat0.85_target0.90_mridge` | 3,353 | 5.83× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.50_cstat0.75_target0.90_mridge` | 1,924 | `p10_prev0.50_cstat0.75_target0.95_mridge` | 6,664 | 3.46× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_prev0.50_cstat0.85_target0.90_mridge` | 14,204 | `p10_prev0.50_cstat0.85_target0.95_mridge` | 1,688 | 8.41× lower | — | previous, next | next |
| Target slope: 0.9 → 0.95 | `p15_prev0.05_cstat0.70_target0.90_mridge` | 19,200 | `p15_prev0.05_cstat0.70_target0.95_mridge` | 102,856 | 5.36× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p15_prev0.05_cstat0.75_target0.80_mridge` | 2,000 | `p15_prev0.05_cstat0.75_target0.85_mridge` | 8,000 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.05_cstat0.85_target0.90_mridge` | 6,447 | `p15_prev0.05_cstat0.85_target0.95_mridge` | 56,464 | 8.76× higher | — | next | next |
| Target slope: 0.8 → 0.85 | `p15_prev0.07_cstat0.60_target0.80_mridge` | 10,000 | `p15_prev0.07_cstat0.60_target0.85_mridge` | 80,000 | 8.00× higher | — | next | next |
| Target slope: 0.8 → 0.85 | `p15_prev0.07_cstat0.75_target0.80_mridge` | 2,667 | `p15_prev0.07_cstat0.75_target0.85_mridge` | 10,668 | 4.00× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p15_prev0.07_cstat0.75_target0.90_mridge` | 5,232 | `p15_prev0.07_cstat0.75_target0.95_mridge` | 31,130 | 5.95× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.07_cstat0.85_target0.90_mridge` | 4,706 | `p15_prev0.07_cstat0.85_target0.95_mridge` | 37,648 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.10_cstat0.75_target0.90_mridge` | 4,182 | `p15_prev0.10_cstat0.75_target0.95_mridge` | 16,000 | 3.83× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.10_cstat0.80_target0.90_mridge` | 3,750 | `p15_prev0.10_cstat0.80_target0.95_mridge` | 15,701 | 4.19× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.10_cstat0.85_target0.90_mridge` | 3,245 | `p15_prev0.10_cstat0.85_target0.95_mridge` | 14,120 | 4.35× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p15_prev0.15_cstat0.60_target0.80_mridge` | 10,000 | `p15_prev0.15_cstat0.60_target0.85_mridge` | 40,000 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.15_cstat0.75_target0.90_mridge` | 2,588 | `p15_prev0.15_cstat0.75_target0.95_mridge` | 10,664 | 4.12× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p15_prev0.15_cstat0.80_target0.85_mridge` | 1,251 | `p15_prev0.15_cstat0.80_target0.90_mridge` | 5,000 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.15_cstat0.80_target0.90_mridge` | 5,000 | `p15_prev0.15_cstat0.80_target0.95_mridge` | 20,000 | 4.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.20_cstat0.60_target0.90_mridge` | 21,517 | `p15_prev0.20_cstat0.60_target0.95_mridge` | 118,676 | 5.52× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.20_cstat0.65_target0.90_mridge` | 13,848 | `p15_prev0.20_cstat0.65_target0.95_mridge` | 55,392 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.20_cstat0.75_target0.90_mridge` | 3,994 | `p15_prev0.20_cstat0.75_target0.95_mridge` | 16,000 | 4.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.20_cstat0.80_target0.90_mridge` | 1,829 | `p15_prev0.20_cstat0.80_target0.95_mridge` | 15,008 | 8.21× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.20_cstat0.85_target0.90_mridge` | 1,767 | `p15_prev0.20_cstat0.85_target0.95_mridge` | 14,112 | 7.99× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p15_prev0.25_cstat0.60_target0.80_mridge` | 5,372 | `p15_prev0.25_cstat0.60_target0.85_mridge` | 45,184 | 8.41× higher | — | next | next |
| Target slope: 0.85 → 0.9 | `p15_prev0.25_cstat0.60_target0.85_mridge` | 45,184 | `p15_prev0.25_cstat0.60_target0.90_mridge` | 341,504 | 7.56× higher | — | previous, next | previous, next |
| Target slope: 0.9 → 0.95 | `p15_prev0.25_cstat0.70_target0.90_mridge` | 4,564 | `p15_prev0.25_cstat0.70_target0.95_mridge` | 40,448 | 8.86× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.30_cstat0.65_target0.90_mridge` | 8,896 | `p15_prev0.30_cstat0.65_target0.95_mridge` | 31,104 | 3.50× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.30_cstat0.70_target0.90_mridge` | 4,516 | `p15_prev0.30_cstat0.70_target0.95_mridge` | 16,832 | 3.73× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.30_cstat0.80_target0.90_mridge` | 2,242 | `p15_prev0.30_cstat0.80_target0.95_mridge` | 8,416 | 3.75× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p15_prev0.40_cstat0.65_target0.80_mridge` | 1,876 | `p15_prev0.40_cstat0.65_target0.85_mridge` | 7,056 | 3.76× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p15_prev0.40_cstat0.65_target0.90_mridge` | 6,339 | `p15_prev0.40_cstat0.65_target0.95_mridge` | 24,272 | 3.83× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.40_cstat0.70_target0.90_mridge` | 3,172 | `p15_prev0.40_cstat0.70_target0.95_mridge` | 12,392 | 3.91× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.40_cstat0.80_target0.90_mridge` | 1,813 | `p15_prev0.40_cstat0.80_target0.95_mridge` | 9,669 | 5.33× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p15_prev0.40_cstat0.85_target0.85_mridge` | 849 | `p15_prev0.40_cstat0.85_target0.90_mridge` | 3,336 | 3.93× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.40_cstat0.85_target0.90_mridge` | 3,336 | `p15_prev0.40_cstat0.85_target0.95_mridge` | 12,640 | 3.79× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p15_prev0.50_cstat0.65_target0.85_mridge` | 2,898 | `p15_prev0.50_cstat0.65_target0.90_mridge` | 10,656 | 3.68× higher | — | — | — |
| Target slope: 0.85 → 0.95 | `p15_prev0.50_cstat0.70_target0.85_mridge` | 1,351 | `p15_prev0.50_cstat0.70_target0.95_mridge` | 10,996 | 8.14× higher | Yes | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.50_cstat0.75_target0.90_mridge` | 2,664 | `p15_prev0.50_cstat0.75_target0.95_mridge` | 8,063 | 3.03× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.50_cstat0.80_target0.90_mridge` | 1,796 | `p15_prev0.50_cstat0.80_target0.95_mridge` | 10,112 | 5.63× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_prev0.50_cstat0.85_target0.90_mridge` | 1,335 | `p15_prev0.50_cstat0.85_target0.95_mridge` | 12,549 | 9.40× higher | — | next | — |
| Target slope: 0.85 → 0.95 | `p20_prev0.05_cstat0.65_target0.85_mridge` | 21,908 | `p20_prev0.05_cstat0.65_target0.95_mridge` | 72,568 | 3.31× higher | Yes | — | — |
| Target slope: 0.85 → 0.9 | `p20_prev0.07_cstat0.75_target0.85_mridge` | 3,556 | `p20_prev0.07_cstat0.75_target0.90_mridge` | 28,448 | 8.00× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p20_prev0.10_cstat0.60_target0.90_mridge` | 46,608 | `p20_prev0.10_cstat0.60_target0.95_mridge` | 148,216 | 3.18× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.10_cstat0.65_target0.90_mridge` | 21,146 | `p20_prev0.10_cstat0.65_target0.95_mridge` | 73,848 | 3.49× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p20_prev0.10_cstat0.75_target0.85_mridge` | 2,667 | `p20_prev0.10_cstat0.75_target0.90_mridge` | 8,017 | 3.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.10_cstat0.80_target0.90_mridge` | 3,870 | `p20_prev0.10_cstat0.80_target0.95_mridge` | 18,351 | 4.74× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.10_cstat0.85_target0.90_mridge` | 4,201 | `p20_prev0.10_cstat0.85_target0.95_mridge` | 37,648 | 8.96× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p20_prev0.15_cstat0.60_target0.80_mridge` | 6,877 | `p20_prev0.15_cstat0.60_target0.85_mridge` | 26,668 | 3.88× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.15_cstat0.80_target0.90_mridge` | 3,701 | `p20_prev0.15_cstat0.80_target0.95_mridge` | 13,156 | 3.55× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p20_prev0.15_cstat0.85_target0.85_mridge` | 1,003 | `p20_prev0.15_cstat0.85_target0.90_mridge` | 3,136 | 3.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.15_cstat0.85_target0.90_mridge` | 3,136 | `p20_prev0.15_cstat0.85_target0.95_mridge` | 12,544 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.20_cstat0.65_target0.90_mridge` | 4,615 | `p20_prev0.20_cstat0.65_target0.95_mridge` | 18,460 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.20_cstat0.70_target0.90_mridge` | 7,666 | `p20_prev0.20_cstat0.70_target0.95_mridge` | 24,197 | 3.16× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.20_cstat0.80_target0.90_mridge` | 3,041 | `p20_prev0.20_cstat0.80_target0.95_mridge` | 20,000 | 6.58× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.20_cstat0.85_target0.90_mridge` | 2,230 | `p20_prev0.20_cstat0.85_target0.95_mridge` | 18,816 | 8.44× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.25_cstat0.60_target0.90_mridge` | 25,232 | `p20_prev0.25_cstat0.60_target0.95_mridge` | 107,776 | 4.27× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p20_prev0.25_cstat0.65_target0.80_mridge` | 2,441 | `p20_prev0.25_cstat0.65_target0.85_mridge` | 7,528 | 3.08× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.25_cstat0.65_target0.90_mridge` | 11,158 | `p20_prev0.25_cstat0.65_target0.95_mridge` | 107,776 | 9.66× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.25_cstat0.70_target0.90_mridge` | 3,972 | `p20_prev0.25_cstat0.70_target0.95_mridge` | 18,327 | 4.61× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p20_prev0.25_cstat0.80_target0.85_mridge` | 941 | `p20_prev0.25_cstat0.80_target0.90_mridge` | 2,890 | 3.07× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.25_cstat0.85_target0.90_mridge` | 1,764 | `p20_prev0.25_cstat0.85_target0.95_mridge` | 6,736 | 3.82× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p20_prev0.30_cstat0.60_target0.85_mridge` | 5,755 | `p20_prev0.30_cstat0.60_target0.90_mridge` | 21,514 | 3.74× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p20_prev0.30_cstat0.65_target0.80_mridge` | 1,246 | `p20_prev0.30_cstat0.65_target0.85_mridge` | 6,280 | 5.04× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.30_cstat0.65_target0.90_mridge` | 8,808 | `p20_prev0.30_cstat0.65_target0.95_mridge` | 44,928 | 5.10× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p20_prev0.30_cstat0.70_target0.85_mridge` | 1,570 | `p20_prev0.30_cstat0.70_target0.90_mridge` | 5,928 | 3.78× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.30_cstat0.75_target0.90_mridge` | 3,390 | `p20_prev0.30_cstat0.75_target0.95_mridge` | 11,259 | 3.32× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.30_cstat0.85_target0.90_mridge` | 1,482 | `p20_prev0.30_cstat0.85_target0.95_mridge` | 11,232 | 7.58× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p20_prev0.40_cstat0.60_target0.85_mridge` | 9,408 | `p20_prev0.40_cstat0.60_target0.90_mridge` | 35,584 | 3.78× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p20_prev0.40_cstat0.65_target0.85_mridge` | 2,318 | `p20_prev0.40_cstat0.65_target0.90_mridge` | 7,932 | 3.42× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.40_cstat0.70_target0.90_mridge` | 2,811 | `p20_prev0.40_cstat0.70_target0.95_mridge` | 14,451 | 5.14× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.40_cstat0.85_target0.90_mridge` | 1,113 | `p20_prev0.40_cstat0.85_target0.95_mridge` | 8,416 | 7.56× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p20_prev0.50_cstat0.65_target0.85_mridge` | 3,791 | `p20_prev0.50_cstat0.65_target0.90_mridge` | 14,208 | 3.75× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.50_cstat0.75_target0.90_mridge` | 2,907 | `p20_prev0.50_cstat0.75_target0.95_mridge` | 9,082 | 3.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.50_cstat0.80_target0.90_mridge` | 1,776 | `p20_prev0.50_cstat0.80_target0.95_mridge` | 6,736 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_prev0.50_cstat0.85_target0.90_mridge` | 1,776 | `p20_prev0.50_cstat0.85_target0.95_mridge` | 6,736 | 3.79× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_prev0.05_cstat0.60_target0.85_mridge` | 24,395 | `p25_prev0.05_cstat0.60_target0.90_mridge` | 149,087 | 6.11× higher | — | next | — |
| Target slope: 0.85 → 0.95 | `p25_prev0.05_cstat0.65_target0.85_mridge` | 21,232 | `p25_prev0.05_cstat0.65_target0.95_mridge` | 92,308 | 4.35× higher | Yes | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.05_cstat0.80_target0.90_mridge` | 11,266 | `p25_prev0.05_cstat0.80_target0.95_mridge` | 50,000 | 4.44× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p25_prev0.07_cstat0.65_target0.85_mridge` | 15,385 | `p25_prev0.07_cstat0.65_target0.90_mridge` | 61,540 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.07_cstat0.70_target0.90_mridge` | 14,396 | `p25_prev0.07_cstat0.70_target0.95_mridge` | 59,489 | 4.13× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p25_prev0.07_cstat0.75_target0.80_mridge` | 2,939 | `p25_prev0.07_cstat0.75_target0.85_mridge` | 8,888 | 3.02× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_prev0.07_cstat0.75_target0.85_mridge` | 8,888 | `p25_prev0.07_cstat0.75_target0.90_mridge` | 35,552 | 4.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.07_cstat0.80_target0.90_mridge` | 3,989 | `p25_prev0.07_cstat0.80_target0.95_mridge` | 21,693 | 5.44× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.07_cstat0.85_target0.90_mridge` | 3,908 | `p25_prev0.07_cstat0.85_target0.95_mridge` | 31,368 | 8.03× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p25_prev0.10_cstat0.65_target0.85_mridge` | 12,109 | `p25_prev0.10_cstat0.65_target0.90_mridge` | 46,152 | 3.81× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p25_prev0.10_cstat0.70_target0.80_mridge` | 2,678 | `p25_prev0.10_cstat0.70_target0.85_mridge` | 10,714 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.10_cstat0.70_target0.90_mridge` | 11,713 | `p25_prev0.10_cstat0.70_target0.95_mridge` | 42,314 | 3.61× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p25_prev0.10_cstat0.80_target0.80_mridge` | 606 | `p25_prev0.10_cstat0.80_target0.85_mridge` | 2,991 | 4.94× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.10_cstat0.80_target0.90_mridge` | 6,284 | `p25_prev0.10_cstat0.80_target0.95_mridge` | 19,509 | 3.10× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p25_prev0.15_cstat0.60_target0.80_mridge` | 10,325 | `p25_prev0.15_cstat0.60_target0.85_mridge` | 33,332 | 3.23× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.15_cstat0.65_target0.90_mridge` | 15,384 | `p25_prev0.15_cstat0.65_target0.95_mridge` | 57,362 | 3.73× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.15_cstat0.75_target0.90_mridge` | 6,496 | `p25_prev0.15_cstat0.75_target0.95_mridge` | 19,790 | 3.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.15_cstat0.85_target0.90_mridge` | 3,922 | `p25_prev0.15_cstat0.85_target0.95_mridge` | 31,376 | 8.00× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p25_prev0.20_cstat0.65_target0.80_mridge` | 1,442 | `p25_prev0.20_cstat0.65_target0.85_mridge` | 6,739 | 4.67× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.20_cstat0.65_target0.90_mridge` | 12,622 | `p25_prev0.20_cstat0.65_target0.95_mridge` | 46,152 | 3.66× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_prev0.20_cstat0.70_target0.85_mridge` | 5,357 | `p25_prev0.20_cstat0.70_target0.90_mridge` | 21,428 | 4.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.20_cstat0.70_target0.90_mridge` | 21,428 | `p25_prev0.20_cstat0.70_target0.95_mridge` | 85,712 | 4.00× higher | — | previous, next | next |
| Target slope: 0.9 → 0.95 | `p25_prev0.20_cstat0.75_target0.90_mridge` | 3,334 | `p25_prev0.20_cstat0.75_target0.95_mridge` | 16,315 | 4.89× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.20_cstat0.85_target0.90_mridge` | 2,942 | `p25_prev0.20_cstat0.85_target0.95_mridge` | 23,536 | 8.00× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p25_prev0.25_cstat0.65_target0.85_mridge` | 5,170 | `p25_prev0.25_cstat0.65_target0.90_mridge` | 17,776 | 3.44× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.25_cstat0.65_target0.90_mridge` | 17,776 | `p25_prev0.25_cstat0.65_target0.95_mridge` | 134,784 | 7.58× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.25_cstat0.85_target0.90_mridge` | 2,442 | `p25_prev0.25_cstat0.85_target0.95_mridge` | 16,848 | 6.90× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.30_cstat0.60_target0.90_mridge` | 13,859 | `p25_prev0.30_cstat0.60_target0.95_mridge` | 112,256 | 8.10× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.30_cstat0.85_target0.90_mridge` | 1,866 | `p25_prev0.30_cstat0.85_target0.95_mridge` | 14,032 | 7.52× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.40_cstat0.65_target0.90_mridge` | 10,209 | `p25_prev0.40_cstat0.65_target0.95_mridge` | 42,112 | 4.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.40_cstat0.70_target0.90_mridge` | 5,552 | `p25_prev0.40_cstat0.70_target0.95_mridge` | 16,657 | 3.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.40_cstat0.85_target0.90_mridge` | 2,776 | `p25_prev0.40_cstat0.85_target0.95_mridge` | 10,528 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.50_cstat0.60_target0.90_mridge` | 17,471 | `p25_prev0.50_cstat0.60_target0.95_mridge` | 134,656 | 7.71× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.50_cstat0.70_target0.90_mridge` | 3,493 | `p25_prev0.50_cstat0.70_target0.95_mridge` | 15,669 | 4.49× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_prev0.50_cstat0.85_target0.90_mridge` | 1,956 | `p25_prev0.50_cstat0.85_target0.95_mridge` | 11,026 | 5.64× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.05_cstat0.60_target0.90_mridge` | 43,545 | `p30_prev0.05_cstat0.60_target0.95_mridge` | 240,000 | 5.51× higher | — | previous, next | previous |
| Target slope: 0.85 → 0.9 | `p30_prev0.05_cstat0.65_target0.85_mridge` | 26,021 | `p30_prev0.05_cstat0.65_target0.90_mridge` | 110,768 | 4.26× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.05_cstat0.75_target0.90_mridge` | 15,676 | `p30_prev0.05_cstat0.75_target0.95_mridge` | 64,000 | 4.08× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.05_cstat0.85_target0.90_mridge` | 8,949 | `p30_prev0.05_cstat0.85_target0.95_mridge` | 56,472 | 6.31× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p30_prev0.07_cstat0.60_target0.90_mridge` | 80,000 | `p30_prev0.07_cstat0.60_target0.95_mridge` | 320,000 | 4.00× higher | — | next | next |
| Target slope: 0.85 → 0.9 | `p30_prev0.07_cstat0.65_target0.85_mridge` | 9,018 | `p30_prev0.07_cstat0.65_target0.90_mridge` | 73,848 | 8.19× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.07_cstat0.70_target0.90_mridge` | 18,934 | `p30_prev0.07_cstat0.70_target0.95_mridge` | 137,144 | 7.24× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.07_cstat0.80_target0.90_mridge` | 9,487 | `p30_prev0.07_cstat0.80_target0.95_mridge` | 40,000 | 4.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.10_cstat0.60_target0.90_mridge` | 27,494 | `p30_prev0.10_cstat0.60_target0.95_mridge` | 120,000 | 4.36× higher | — | next | next |
| Target slope: 0.8 → 0.85 | `p30_prev0.10_cstat0.65_target0.80_mridge` | 3,153 | `p30_prev0.10_cstat0.65_target0.85_mridge` | 14,003 | 4.44× higher | — | previous | previous |
| Target slope: 0.9 → 0.95 | `p30_prev0.10_cstat0.70_target0.90_mridge` | 10,715 | `p30_prev0.10_cstat0.70_target0.95_mridge` | 49,452 | 4.62× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.10_cstat0.75_target0.90_mridge` | 8,000 | `p30_prev0.10_cstat0.75_target0.95_mridge` | 32,000 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.10_cstat0.85_target0.90_mridge` | 7,058 | `p30_prev0.10_cstat0.85_target0.95_mridge` | 32,205 | 4.56× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.15_cstat0.65_target0.90_mridge` | 19,043 | `p30_prev0.15_cstat0.65_target0.95_mridge` | 147,696 | 7.76× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p30_prev0.15_cstat0.70_target0.90_mridge` | 10,160 | `p30_prev0.15_cstat0.70_target0.95_mridge` | 34,284 | 3.37× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.15_cstat0.80_target0.90_mridge` | 4,918 | `p30_prev0.15_cstat0.80_target0.95_mridge` | 15,710 | 3.19× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p30_prev0.15_cstat0.85_target0.80_mridge` | 561 | `p30_prev0.15_cstat0.85_target0.85_mridge` | 2,353 | 4.19× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.15_cstat0.85_target0.90_mridge` | 4,000 | `p30_prev0.15_cstat0.85_target0.95_mridge` | 37,648 | 9.41× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.20_cstat0.65_target0.90_mridge` | 13,846 | `p30_prev0.20_cstat0.65_target0.95_mridge` | 55,384 | 4.00× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p30_prev0.20_cstat0.70_target0.80_mridge` | 1,607 | `p30_prev0.20_cstat0.70_target0.85_mridge` | 6,429 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.20_cstat0.75_target0.90_mridge` | 4,000 | `p30_prev0.20_cstat0.75_target0.95_mridge` | 16,140 | 4.04× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.20_cstat0.80_target0.90_mridge` | 3,653 | `p30_prev0.20_cstat0.80_target0.95_mridge` | 12,187 | 3.34× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.20_cstat0.85_target0.90_mridge` | 2,991 | `p30_prev0.20_cstat0.85_target0.95_mridge` | 28,240 | 9.44× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.25_cstat0.65_target0.90_mridge` | 10,433 | `p30_prev0.25_cstat0.65_target0.95_mridge` | 37,567 | 3.60× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p30_prev0.25_cstat0.70_target0.85_mridge` | 2,221 | `p30_prev0.25_cstat0.70_target0.90_mridge` | 7,412 | 3.34× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.25_cstat0.70_target0.90_mridge` | 7,412 | `p30_prev0.25_cstat0.70_target0.95_mridge` | 40,416 | 5.45× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.25_cstat0.80_target0.90_mridge` | 3,660 | `p30_prev0.25_cstat0.80_target0.95_mridge` | 20,208 | 5.52× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p30_prev0.30_cstat0.65_target0.80_mridge` | 3,098 | `p30_prev0.30_cstat0.65_target0.85_mridge` | 9,408 | 3.04× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.30_cstat0.65_target0.90_mridge` | 17,776 | `p30_prev0.30_cstat0.65_target0.95_mridge` | 134,784 | 7.58× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p30_prev0.30_cstat0.70_target0.85_mridge` | 2,380 | `p30_prev0.30_cstat0.70_target0.90_mridge` | 8,888 | 3.73× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p30_prev0.30_cstat0.75_target0.85_mridge` | 2,032 | `p30_prev0.30_cstat0.75_target0.90_mridge` | 8,888 | 4.37× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.30_cstat0.80_target0.90_mridge` | 2,223 | `p30_prev0.30_cstat0.80_target0.95_mridge` | 8,424 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.30_cstat0.85_target0.90_mridge` | 2,183 | `p30_prev0.30_cstat0.85_target0.95_mridge` | 8,424 | 3.86× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.40_cstat0.60_target0.90_mridge` | 13,328 | `p30_prev0.40_cstat0.60_target0.95_mridge` | 50,114 | 3.76× higher | — | next | next |
| Target slope: 0.8 → 0.85 | `p30_prev0.40_cstat0.70_target0.80_mridge` | 939 | `p30_prev0.40_cstat0.70_target0.85_mridge` | 2,818 | 3.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.40_cstat0.75_target0.90_mridge` | 2,938 | `p30_prev0.40_cstat0.75_target0.95_mridge` | 11,512 | 3.92× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.40_cstat0.85_target0.90_mridge` | 2,355 | `p30_prev0.40_cstat0.85_target0.95_mridge` | 12,624 | 5.36× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.50_cstat0.65_target0.90_mridge` | 9,942 | `p30_prev0.50_cstat0.65_target0.95_mridge` | 36,569 | 3.68× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.50_cstat0.70_target0.90_mridge` | 5,336 | `p30_prev0.50_cstat0.70_target0.95_mridge` | 20,224 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.50_cstat0.75_target0.90_mridge` | 2,572 | `p30_prev0.50_cstat0.75_target0.95_mridge` | 12,166 | 4.73× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.50_cstat0.80_target0.90_mridge` | 2,168 | `p30_prev0.50_cstat0.80_target0.95_mridge` | 8,014 | 3.70× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p30_prev0.50_cstat0.85_target0.80_mridge` | 368 | `p30_prev0.50_cstat0.85_target0.85_mridge` | 1,243 | 3.38× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_prev0.50_cstat0.85_target0.90_mridge` | 2,453 | `p30_prev0.50_cstat0.85_target0.95_mridge` | 20,224 | 8.24× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p40_prev0.05_cstat0.60_target0.85_mridge` | 38,776 | `p40_prev0.05_cstat0.60_target0.90_mridge` | 153,338 | 3.95× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p40_prev0.05_cstat0.65_target0.80_mridge` | 18,708 | `p40_prev0.05_cstat0.65_target0.85_mridge` | 147,692 | 7.89× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p40_prev0.05_cstat0.65_target0.85_mridge` | 147,692 | `p40_prev0.05_cstat0.65_target0.90_mridge` | 36,580 | 4.04× lower | — | previous | — |
| Target slope: 0.8 → 0.85 | `p40_prev0.05_cstat0.70_target0.80_mridge` | 17,143 | `p40_prev0.05_cstat0.70_target0.85_mridge` | 4,263 | 4.02× lower | — | previous | — |
| Target slope: 0.85 → 0.9 | `p40_prev0.05_cstat0.70_target0.85_mridge` | 4,263 | `p40_prev0.05_cstat0.70_target0.90_mridge` | 31,547 | 7.40× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.05_cstat0.70_target0.90_mridge` | 31,547 | `p40_prev0.05_cstat0.70_target0.95_mridge` | 130,334 | 4.13× higher | — | previous | — |
| Target slope: 0.85 → 0.9 | `p40_prev0.05_cstat0.75_target0.85_mridge` | 10,667 | `p40_prev0.05_cstat0.75_target0.90_mridge` | 42,668 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.05_cstat0.80_target0.90_mridge` | 9,744 | `p40_prev0.05_cstat0.80_target0.95_mridge` | 80,000 | 8.21× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p40_prev0.05_cstat0.85_target0.85_mridge` | 3,726 | `p40_prev0.05_cstat0.85_target0.90_mridge` | 18,824 | 5.05× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.07_cstat0.65_target0.90_mridge` | 24,615 | `p40_prev0.07_cstat0.65_target0.95_mridge` | 98,460 | 4.00× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p40_prev0.07_cstat0.70_target0.80_mridge` | 2,803 | `p40_prev0.07_cstat0.70_target0.85_mridge` | 11,428 | 4.08× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p40_prev0.07_cstat0.75_target0.85_mridge` | 6,420 | `p40_prev0.07_cstat0.75_target0.90_mridge` | 28,444 | 4.43× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p40_prev0.10_cstat0.65_target0.85_mridge` | 9,231 | `p40_prev0.10_cstat0.65_target0.90_mridge` | 34,098 | 3.69× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.10_cstat0.70_target0.90_mridge` | 17,147 | `p40_prev0.10_cstat0.70_target0.95_mridge` | 66,704 | 3.89× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.10_cstat0.80_target0.90_mridge` | 7,559 | `p40_prev0.10_cstat0.80_target0.95_mridge` | 27,275 | 3.61× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.10_cstat0.85_target0.90_mridge` | 4,706 | `p40_prev0.10_cstat0.85_target0.95_mridge` | 14,368 | 3.05× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p40_prev0.15_cstat0.60_target0.80_mridge` | 12,992 | `p40_prev0.15_cstat0.60_target0.85_mridge` | 53,332 | 4.10× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p40_prev0.15_cstat0.60_target0.90_mridge` | 52,171 | `p40_prev0.15_cstat0.60_target0.95_mridge` | 313,459 | 6.01× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p40_prev0.15_cstat0.70_target0.90_mridge` | 9,887 | `p40_prev0.15_cstat0.70_target0.95_mridge` | 42,076 | 4.26× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p40_prev0.15_cstat0.75_target0.85_mridge` | 1,844 | `p40_prev0.15_cstat0.75_target0.90_mridge` | 6,694 | 3.63× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.15_cstat0.75_target0.90_mridge` | 6,694 | `p40_prev0.15_cstat0.75_target0.95_mridge` | 28,830 | 4.31× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.15_cstat0.85_target0.90_mridge` | 3,596 | `p40_prev0.15_cstat0.85_target0.95_mridge` | 12,552 | 3.49× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p40_prev0.20_cstat0.65_target0.85_mridge` | 8,443 | `p40_prev0.20_cstat0.65_target0.90_mridge` | 73,848 | 8.75× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.20_cstat0.65_target0.90_mridge` | 73,848 | `p40_prev0.20_cstat0.65_target0.95_mridge` | 295,392 | 4.00× higher | — | previous, next | next |
| Target slope: 0.9 → 0.95 | `p40_prev0.20_cstat0.70_target0.90_mridge` | 8,801 | `p40_prev0.20_cstat0.70_target0.95_mridge` | 33,848 | 3.85× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.20_cstat0.75_target0.90_mridge` | 6,169 | `p40_prev0.20_cstat0.75_target0.95_mridge` | 18,709 | 3.03× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.20_cstat0.80_target0.90_mridge` | 4,358 | `p40_prev0.20_cstat0.80_target0.95_mridge` | 16,016 | 3.68× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p40_prev0.25_cstat0.60_target0.80_mridge` | 7,325 | `p40_prev0.25_cstat0.60_target0.85_mridge` | 30,112 | 4.11× higher | — | next | — |
| Target slope: 0.85 → 0.95 | `p40_prev0.25_cstat0.60_target0.85_mridge` | 30,112 | `p40_prev0.25_cstat0.60_target0.95_mridge` | 107,737 | 3.58× higher | Yes | previous, next | — |
| Target slope: 0.85 → 0.9 | `p40_prev0.25_cstat0.65_target0.85_mridge` | 3,764 | `p40_prev0.25_cstat0.65_target0.90_mridge` | 28,448 | 7.56× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.25_cstat0.75_target0.90_mridge` | 5,873 | `p40_prev0.25_cstat0.75_target0.95_mridge` | 18,277 | 3.11× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.25_cstat0.80_target0.90_mridge` | 4,434 | `p40_prev0.25_cstat0.80_target0.95_mridge` | 14,675 | 3.31× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.25_cstat0.85_target0.90_mridge` | 3,584 | `p40_prev0.25_cstat0.85_target0.95_mridge` | 26,944 | 7.52× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.30_cstat0.65_target0.90_mridge` | 14,376 | `p40_prev0.30_cstat0.65_target0.95_mridge` | 52,497 | 3.65× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.30_cstat0.75_target0.90_mridge` | 2,855 | `p40_prev0.30_cstat0.75_target0.95_mridge` | 22,448 | 7.86× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.30_cstat0.80_target0.90_mridge` | 3,738 | `p40_prev0.30_cstat0.80_target0.95_mridge` | 11,423 | 3.06× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.30_cstat0.85_target0.90_mridge` | 2,962 | `p40_prev0.30_cstat0.85_target0.95_mridge` | 22,448 | 7.58× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p40_prev0.40_cstat0.60_target0.85_mridge` | 8,015 | `p40_prev0.40_cstat0.60_target0.90_mridge` | 24,429 | 3.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.40_cstat0.65_target0.90_mridge` | 6,745 | `p40_prev0.40_cstat0.65_target0.95_mridge` | 41,623 | 6.17× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p40_prev0.40_cstat0.70_target0.85_mridge` | 2,235 | `p40_prev0.40_cstat0.70_target0.90_mridge` | 8,888 | 3.98× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.40_cstat0.75_target0.90_mridge` | 4,301 | `p40_prev0.40_cstat0.75_target0.95_mridge` | 15,544 | 3.61× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p40_prev0.40_cstat0.80_target0.85_mridge` | 1,176 | `p40_prev0.40_cstat0.80_target0.90_mridge` | 4,444 | 3.78× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.40_cstat0.85_target0.90_mridge` | 2,637 | `p40_prev0.40_cstat0.85_target0.95_mridge` | 16,848 | 6.39× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.50_cstat0.60_target0.90_mridge` | 13,946 | `p40_prev0.50_cstat0.60_target0.95_mridge` | 107,776 | 7.73× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p40_prev0.50_cstat0.65_target0.90_mridge` | 11,376 | `p40_prev0.50_cstat0.65_target0.95_mridge` | 40,169 | 3.53× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.50_cstat0.75_target0.90_mridge` | 3,330 | `p40_prev0.50_cstat0.75_target0.95_mridge` | 13,472 | 4.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_prev0.50_cstat0.85_target0.90_mridge` | 2,461 | `p40_prev0.50_cstat0.85_target0.95_mridge` | 10,287 | 4.18× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p50_prev0.05_cstat0.65_target0.85_mridge` | 11,342 | `p50_prev0.05_cstat0.65_target0.90_mridge` | 65,039 | 5.73× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p50_prev0.05_cstat0.70_target0.80_mridge` | 10,714 | `p50_prev0.05_cstat0.70_target0.85_mridge` | 42,857 | 4.00× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p50_prev0.05_cstat0.75_target0.90_mridge` | 26,724 | `p50_prev0.05_cstat0.75_target0.95_mridge` | 104,743 | 3.92× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p50_prev0.05_cstat0.80_target0.85_mridge` | 3,930 | `p50_prev0.05_cstat0.80_target0.90_mridge` | 25,000 | 6.36× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.05_cstat0.80_target0.90_mridge` | 25,000 | `p50_prev0.05_cstat0.80_target0.95_mridge` | 200,000 | 8.00× higher | — | previous, next | — |
| Target slope: 0.8 → 0.85 | `p50_prev0.05_cstat0.85_target0.80_mridge` | 2,944 | `p50_prev0.05_cstat0.85_target0.85_mridge` | 11,765 | 4.00× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p50_prev0.07_cstat0.60_target0.85_mridge` | 16,298 | `p50_prev0.07_cstat0.60_target0.90_mridge` | 66,666 | 4.09× higher | — | previous | previous |
| Target slope: 0.9 → 0.95 | `p50_prev0.07_cstat0.80_target0.90_mridge` | 9,222 | `p50_prev0.07_cstat0.80_target0.95_mridge` | 30,072 | 3.26× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.07_cstat0.85_target0.90_mridge` | 3,922 | `p50_prev0.07_cstat0.85_target0.95_mridge` | 12,227 | 3.12× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p50_prev0.10_cstat0.65_target0.85_mridge` | 9,584 | `p50_prev0.10_cstat0.65_target0.90_mridge` | 38,514 | 4.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.10_cstat0.65_target0.90_mridge` | 38,514 | `p50_prev0.10_cstat0.65_target0.95_mridge` | 135,980 | 3.53× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p50_prev0.10_cstat0.70_target0.80_mridge` | 5,458 | `p50_prev0.10_cstat0.70_target0.85_mridge` | 21,429 | 3.93× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.10_cstat0.75_target0.90_mridge` | 13,334 | `p50_prev0.10_cstat0.75_target0.95_mridge` | 44,136 | 3.31× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p50_prev0.10_cstat0.80_target0.85_mridge` | 2,363 | `p50_prev0.10_cstat0.80_target0.90_mridge` | 9,165 | 3.88× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.15_cstat0.65_target0.90_mridge` | 23,057 | `p50_prev0.15_cstat0.65_target0.95_mridge` | 101,272 | 4.39× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p50_prev0.15_cstat0.70_target0.85_mridge` | 3,571 | `p50_prev0.15_cstat0.70_target0.90_mridge` | 14,546 | 4.07× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.15_cstat0.75_target0.90_mridge` | 8,208 | `p50_prev0.15_cstat0.75_target0.95_mridge` | 30,585 | 3.73× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.15_cstat0.80_target0.90_mridge` | 4,086 | `p50_prev0.15_cstat0.80_target0.95_mridge` | 25,730 | 6.30× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.20_cstat0.75_target0.90_mridge` | 6,666 | `p50_prev0.20_cstat0.75_target0.95_mridge` | 24,793 | 3.72× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.20_cstat0.80_target0.90_mridge` | 6,250 | `p50_prev0.20_cstat0.80_target0.95_mridge` | 25,000 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.25_cstat0.65_target0.90_mridge` | 17,476 | `p50_prev0.25_cstat0.65_target0.95_mridge` | 54,320 | 3.11× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.25_cstat0.70_target0.90_mridge` | 9,382 | `p50_prev0.25_cstat0.70_target0.95_mridge` | 34,883 | 3.72× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p50_prev0.25_cstat0.75_target0.85_mridge` | 2,903 | `p50_prev0.25_cstat0.75_target0.90_mridge` | 8,888 | 3.06× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.25_cstat0.80_target0.90_mridge` | 4,444 | `p50_prev0.25_cstat0.80_target0.95_mridge` | 15,704 | 3.53× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p50_prev0.30_cstat0.60_target0.85_mridge` | 12,835 | `p50_prev0.30_cstat0.60_target0.90_mridge` | 118,528 | 9.23× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p50_prev0.30_cstat0.65_target0.90_mridge` | 7,265 | `p50_prev0.30_cstat0.65_target0.95_mridge` | 56,242 | 7.74× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.30_cstat0.80_target0.90_mridge` | 4,010 | `p50_prev0.30_cstat0.80_target0.95_mridge` | 28,080 | 7.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.30_cstat0.85_target0.90_mridge` | 3,301 | `p50_prev0.30_cstat0.85_target0.95_mridge` | 28,080 | 8.51× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p50_prev0.40_cstat0.60_target0.90_mridge` | 22,224 | `p50_prev0.40_cstat0.60_target0.95_mridge` | 84,224 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.40_cstat0.70_target0.90_mridge` | 5,514 | `p50_prev0.40_cstat0.70_target0.95_mridge` | 24,516 | 4.45× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.40_cstat0.85_target0.90_mridge` | 2,933 | `p50_prev0.40_cstat0.85_target0.95_mridge` | 9,506 | 3.24× higher | — | next | next |
| Target slope: 0.8 → 0.85 | `p50_prev0.50_cstat0.65_target0.80_mridge` | 3,001 | `p50_prev0.50_cstat0.65_target0.85_mridge` | 9,408 | 3.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.50_cstat0.65_target0.90_mridge` | 8,281 | `p50_prev0.50_cstat0.65_target0.95_mridge` | 67,392 | 8.14× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.50_cstat0.70_target0.90_mridge` | 4,381 | `p50_prev0.50_cstat0.70_target0.95_mridge` | 25,873 | 5.91× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_prev0.50_cstat0.75_target0.90_mridge` | 4,554 | `p50_prev0.50_cstat0.75_target0.95_mridge` | 16,852 | 3.70× higher | — | — | — |
| Target slope: 0.85 → 0.95 | `p5_prev0.05_cstat0.60_target0.85_mridge` | 22,504 | `p5_prev0.05_cstat0.60_target0.95_mridge` | 160,000 | 7.11× higher | Yes | next | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.05_cstat0.65_target0.85_mridge` | 9,461 | `p5_prev0.05_cstat0.65_target0.90_mridge` | 28,738 | 3.04× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.05_cstat0.65_target0.90_mridge` | 28,738 | `p5_prev0.05_cstat0.65_target0.95_mridge` | 147,680 | 5.14× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.05_cstat0.70_target0.85_mridge` | 3,210 | `p5_prev0.05_cstat0.70_target0.90_mridge` | 9,999 | 3.11× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.05_cstat0.70_target0.90_mridge` | 9,999 | `p5_prev0.05_cstat0.70_target0.95_mridge` | 56,033 | 5.60× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.05_cstat0.75_target0.90_mridge` | 10,664 | `p5_prev0.05_cstat0.75_target0.95_mridge` | 79,130 | 7.42× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p5_prev0.05_cstat0.80_target0.80_mridge` | 625 | `p5_prev0.05_cstat0.80_target0.85_mridge` | 2,500 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.05_cstat0.80_target0.90_mridge` | 3,876 | `p5_prev0.05_cstat0.80_target0.95_mridge` | 17,372 | 4.48× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.07_cstat0.65_target0.85_mridge` | 3,077 | `p5_prev0.07_cstat0.65_target0.90_mridge` | 10,131 | 3.29× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.07_cstat0.70_target0.90_mridge` | 6,414 | `p5_prev0.07_cstat0.70_target0.95_mridge` | 22,856 | 3.56× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.07_cstat0.75_target0.85_mridge` | 1,778 | `p5_prev0.07_cstat0.75_target0.90_mridge` | 14,224 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.07_cstat0.85_target0.90_mridge` | 50,240 | `p5_prev0.07_cstat0.85_target0.95_mridge` | 6,280 | 8.00× lower | — | previous | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.10_cstat0.65_target0.90_mridge` | 295,424 | `p5_prev0.10_cstat0.65_target0.95_mridge` | 36,928 | 8.00× lower | — | previous | previous |
| Target slope: 0.9 → 0.95 | `p5_prev0.10_cstat0.80_target0.90_mridge` | 2,807 | `p5_prev0.10_cstat0.80_target0.95_mridge` | 15,295 | 5.45× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.10_cstat0.85_target0.85_mridge` | 810 | `p5_prev0.10_cstat0.85_target0.90_mridge` | 4,700 | 5.80× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.10_cstat0.85_target0.90_mridge` | 4,700 | `p5_prev0.10_cstat0.85_target0.95_mridge` | 18,816 | 4.00× higher | — | previous | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.15_cstat0.60_target0.85_mridge` | 3,334 | `p5_prev0.15_cstat0.60_target0.90_mridge` | 10,003 | 3.00× higher | — | previous | previous |
| Target slope: 0.8 → 0.85 | `p5_prev0.15_cstat0.70_target0.80_mridge` | 680 | `p5_prev0.15_cstat0.70_target0.85_mridge` | 2,858 | 4.20× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.15_cstat0.70_target0.90_mridge` | 3,325 | `p5_prev0.15_cstat0.70_target0.95_mridge` | 11,432 | 3.44× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.15_cstat0.80_target0.90_mridge` | 1,664 | `p5_prev0.15_cstat0.80_target0.95_mridge` | 9,708 | 5.83× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.15_cstat0.85_target0.85_mridge` | 661 | `p5_prev0.15_cstat0.85_target0.90_mridge` | 3,136 | 4.74× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.15_cstat0.85_target0.90_mridge` | 3,136 | `p5_prev0.15_cstat0.85_target0.95_mridge` | 12,544 | 4.00× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p5_prev0.20_cstat0.60_target0.80_mridge` | 3,055 | `p5_prev0.20_cstat0.60_target0.85_mridge` | 10,000 | 3.27× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.20_cstat0.60_target0.90_mridge` | 15,251 | `p5_prev0.20_cstat0.60_target0.95_mridge` | 80,000 | 5.25× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.20_cstat0.80_target0.90_mridge` | 4,992 | `p5_prev0.20_cstat0.80_target0.95_mridge` | 26,299 | 5.27× higher | — | previous, next | next |
| Target slope: 0.9 → 0.95 | `p5_prev0.20_cstat0.85_target0.90_mridge` | 1,176 | `p5_prev0.20_cstat0.85_target0.95_mridge` | 7,304 | 6.21× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.25_cstat0.60_target0.90_mridge` | 7,104 | `p5_prev0.25_cstat0.60_target0.95_mridge` | 54,016 | 7.60× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.25_cstat0.65_target0.85_mridge` | 1,993 | `p5_prev0.25_cstat0.65_target0.90_mridge` | 7,104 | 3.56× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.25_cstat0.65_target0.90_mridge` | 7,104 | `p5_prev0.25_cstat0.65_target0.95_mridge` | 27,008 | 3.80× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.25_cstat0.70_target0.90_mridge` | 2,186 | `p5_prev0.25_cstat0.70_target0.95_mridge` | 6,752 | 3.09× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.25_cstat0.75_target0.90_mridge` | 1,776 | `p5_prev0.25_cstat0.75_target0.95_mridge` | 6,752 | 3.80× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.30_cstat0.60_target0.90_mridge` | 5,952 | `p5_prev0.30_cstat0.60_target0.95_mridge` | 22,528 | 3.78× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p5_prev0.30_cstat0.65_target0.80_mridge` | 711 | `p5_prev0.30_cstat0.65_target0.85_mridge` | 3,136 | 4.41× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.30_cstat0.65_target0.90_mridge` | 3,736 | `p5_prev0.30_cstat0.65_target0.95_mridge` | 22,528 | 6.03× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.30_cstat0.70_target0.90_mridge` | 1,488 | `p5_prev0.30_cstat0.70_target0.95_mridge` | 4,568 | 3.07× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.30_cstat0.80_target0.90_mridge` | 372 | `p5_prev0.30_cstat0.80_target0.95_mridge` | 2,816 | 7.57× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p5_prev0.30_cstat0.85_target0.90_mridge` | 71,614 | `p5_prev0.30_cstat0.85_target0.95_mridge` | 8,903 | 8.04× lower | — | previous | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.40_cstat0.65_target0.90_mridge` | 3,335 | `p5_prev0.40_cstat0.65_target0.95_mridge` | 25,049 | 7.51× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.40_cstat0.70_target0.90_mridge` | 1,934 | `p5_prev0.40_cstat0.70_target0.95_mridge` | 8,448 | 4.37× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.40_cstat0.80_target0.90_mridge` | 17,792 | `p5_prev0.40_cstat0.80_target0.95_mridge` | 3,216 | 5.53× lower | — | previous, next | next |
| Target slope: 0.8 → 0.85 | `p5_prev0.50_cstat0.60_target0.80_mridge` | 1,000 | `p5_prev0.50_cstat0.60_target0.85_mridge` | 3,036 | 3.04× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.50_cstat0.65_target0.90_mridge` | 3,552 | `p5_prev0.50_cstat0.65_target0.95_mridge` | 20,164 | 5.68× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.50_cstat0.70_target0.85_mridge` | 1,019 | `p5_prev0.50_cstat0.70_target0.90_mridge` | 3,552 | 3.49× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_prev0.50_cstat0.75_target0.90_mridge` | 1,209 | `p5_prev0.50_cstat0.75_target0.95_mridge` | 8,277 | 6.85× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_prev0.50_cstat0.85_target0.85_mridge` | 944 | `p5_prev0.50_cstat0.85_target0.90_mridge` | 8,991 | 9.52× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p75_prev0.05_cstat0.60_target0.80_mridge` | 150,000 | `p75_prev0.05_cstat0.60_target0.85_mridge` | 35,982 | 4.17× lower | — | previous, next | next |
| Target slope: 0.85 → 0.95 | `p75_prev0.05_cstat0.60_target0.85_mridge` | 35,982 | `p75_prev0.05_cstat0.60_target0.95_mridge` | 131,882 | 3.67× higher | Yes | previous | previous |
| Target slope: 0.85 → 0.9 | `p75_prev0.05_cstat0.65_target0.85_mridge` | 17,307 | `p75_prev0.05_cstat0.65_target0.90_mridge` | 53,047 | 3.07× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.05_cstat0.65_target0.90_mridge` | 53,047 | `p75_prev0.05_cstat0.65_target0.95_mridge` | 276,924 | 5.22× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.05_cstat0.75_target0.90_mridge` | 29,323 | `p75_prev0.05_cstat0.75_target0.95_mridge` | 119,559 | 4.08× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p75_prev0.05_cstat0.85_target0.85_mridge` | 4,411 | `p75_prev0.05_cstat0.85_target0.90_mridge` | 17,559 | 3.98× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.05_cstat0.85_target0.90_mridge` | 17,559 | `p75_prev0.05_cstat0.85_target0.95_mridge` | 53,708 | 3.06× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p75_prev0.07_cstat0.70_target0.85_mridge` | 5,357 | `p75_prev0.07_cstat0.70_target0.90_mridge` | 42,857 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.07_cstat0.70_target0.90_mridge` | 42,857 | `p75_prev0.07_cstat0.70_target0.95_mridge` | 147,739 | 3.45× higher | — | previous | — |
| Target slope: 0.8 → 0.85 | `p75_prev0.07_cstat0.75_target0.80_mridge` | 3,222 | `p75_prev0.07_cstat0.75_target0.85_mridge` | 13,333 | 4.14× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.07_cstat0.75_target0.90_mridge` | 21,244 | `p75_prev0.07_cstat0.75_target0.95_mridge` | 81,490 | 3.84× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p75_prev0.07_cstat0.80_target0.85_mridge` | 3,125 | `p75_prev0.07_cstat0.80_target0.90_mridge` | 12,779 | 4.09× higher | — | previous | previous |
| Target slope: 0.9 → 0.95 | `p75_prev0.07_cstat0.80_target0.90_mridge` | 12,779 | `p75_prev0.07_cstat0.80_target0.95_mridge` | 51,137 | 4.00× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p75_prev0.07_cstat0.85_target0.80_mridge` | 2,828 | `p75_prev0.07_cstat0.85_target0.85_mridge` | 11,765 | 4.16× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.07_cstat0.85_target0.90_mridge` | 9,092 | `p75_prev0.07_cstat0.85_target0.95_mridge` | 37,153 | 4.09× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.10_cstat0.60_target0.90_mridge` | 151,570 | `p75_prev0.10_cstat0.60_target0.95_mridge` | 37,500 | 4.04× lower | — | previous | — |
| Target slope: 0.8 → 0.85 | `p75_prev0.10_cstat0.65_target0.80_mridge` | 10,456 | `p75_prev0.10_cstat0.65_target0.85_mridge` | 69,230 | 6.62× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p75_prev0.10_cstat0.70_target0.90_mridge` | 15,109 | `p75_prev0.10_cstat0.70_target0.95_mridge` | 58,267 | 3.86× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.10_cstat0.80_target0.90_mridge` | 11,615 | `p75_prev0.10_cstat0.80_target0.95_mridge` | 38,918 | 3.35× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.10_cstat0.85_target0.90_mridge` | 8,824 | `p75_prev0.10_cstat0.85_target0.95_mridge` | 35,296 | 4.00× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p75_prev0.15_cstat0.60_target0.85_mridge` | 12,500 | `p75_prev0.15_cstat0.60_target0.90_mridge` | 93,491 | 7.48× higher | — | previous, next | previous |
| Target slope: 0.85 → 0.9 | `p75_prev0.15_cstat0.70_target0.85_mridge` | 5,209 | `p75_prev0.15_cstat0.70_target0.90_mridge` | 18,555 | 3.56× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.15_cstat0.70_target0.90_mridge` | 18,555 | `p75_prev0.15_cstat0.70_target0.95_mridge` | 67,296 | 3.63× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.15_cstat0.75_target0.90_mridge` | 6,554 | `p75_prev0.15_cstat0.75_target0.95_mridge` | 38,257 | 5.84× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.15_cstat0.80_target0.90_mridge` | 8,876 | `p75_prev0.15_cstat0.80_target0.95_mridge` | 50,000 | 5.63× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p75_prev0.20_cstat0.60_target0.85_mridge` | 18,750 | `p75_prev0.20_cstat0.60_target0.90_mridge` | 76,183 | 4.06× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.20_cstat0.65_target0.90_mridge` | 17,308 | `p75_prev0.20_cstat0.65_target0.95_mridge` | 69,232 | 4.00× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p75_prev0.20_cstat0.70_target0.80_mridge` | 1,895 | `p75_prev0.20_cstat0.70_target0.85_mridge` | 5,798 | 3.06× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.20_cstat0.70_target0.90_mridge` | 13,843 | `p75_prev0.20_cstat0.70_target0.95_mridge` | 57,560 | 4.16× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.20_cstat0.75_target0.90_mridge` | 10,074 | `p75_prev0.20_cstat0.75_target0.95_mridge` | 31,991 | 3.18× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.20_cstat0.80_target0.90_mridge` | 4,213 | `p75_prev0.20_cstat0.80_target0.95_mridge` | 21,353 | 5.07× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p75_prev0.20_cstat0.85_target0.85_mridge` | 2,726 | `p75_prev0.20_cstat0.85_target0.90_mridge` | 8,824 | 3.24× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p75_prev0.25_cstat0.60_target0.85_mridge` | 13,150 | `p75_prev0.25_cstat0.60_target0.90_mridge` | 106,656 | 8.11× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p75_prev0.25_cstat0.65_target0.90_mridge` | 15,046 | `p75_prev0.25_cstat0.65_target0.95_mridge` | 50,528 | 3.36× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p75_prev0.25_cstat0.70_target0.85_mridge` | 3,773 | `p75_prev0.25_cstat0.70_target0.90_mridge` | 13,379 | 3.55× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.25_cstat0.75_target0.90_mridge` | 8,910 | `p75_prev0.25_cstat0.75_target0.95_mridge` | 29,468 | 3.31× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.25_cstat0.80_target0.90_mridge` | 6,281 | `p75_prev0.25_cstat0.80_target0.95_mridge` | 25,264 | 4.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.25_cstat0.85_target0.90_mridge` | 4,634 | `p75_prev0.25_cstat0.85_target0.95_mridge` | 14,761 | 3.19× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p75_prev0.30_cstat0.60_target0.80_mridge` | 3,083 | `p75_prev0.30_cstat0.60_target0.85_mridge` | 12,043 | 3.91× higher | — | previous | previous |
| Target slope: 0.85 → 0.9 | `p75_prev0.30_cstat0.60_target0.85_mridge` | 12,043 | `p75_prev0.30_cstat0.60_target0.90_mridge` | 44,448 | 3.69× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.30_cstat0.60_target0.90_mridge` | 44,448 | `p75_prev0.30_cstat0.60_target0.95_mridge` | 161,611 | 3.64× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.30_cstat0.65_target0.90_mridge` | 22,224 | `p75_prev0.30_cstat0.65_target0.95_mridge` | 84,866 | 3.82× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.30_cstat0.70_target0.90_mridge` | 11,575 | `p75_prev0.30_cstat0.70_target0.95_mridge` | 37,152 | 3.21× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p75_prev0.40_cstat0.65_target0.80_mridge` | 4,469 | `p75_prev0.40_cstat0.65_target0.85_mridge` | 17,648 | 3.95× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.40_cstat0.70_target0.90_mridge` | 10,451 | `p75_prev0.40_cstat0.70_target0.95_mridge` | 63,168 | 6.04× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p75_prev0.40_cstat0.80_target0.90_mridge` | 4,909 | `p75_prev0.40_cstat0.80_target0.95_mridge` | 15,792 | 3.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.50_cstat0.70_target0.90_mridge` | 6,668 | `p75_prev0.50_cstat0.70_target0.95_mridge` | 50,528 | 7.58× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.50_cstat0.80_target0.90_mridge` | 4,613 | `p75_prev0.50_cstat0.80_target0.95_mridge` | 15,634 | 3.39× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_prev0.50_cstat0.85_target0.90_mridge` | 4,325 | `p75_prev0.50_cstat0.85_target0.95_mridge` | 25,264 | 5.84× higher | — | next | — |

## Continuous

| Swept input | Previous cell | Previous N | Next cell | Next N | Next vs previous | Gap | Excluded endpoints | Earlier isolated endpoints |
|---|---|---:|---|---:|---|---|---|---|
| R²: 0.1 → 0.2 | `p100_r20.10_target0.95_mlasso` | 8,972 | `p100_r20.20_target0.95_mlasso` | 2,290 | 3.92× lower | — | — | — |
| R²: 0.3 → 0.4 | `p100_r20.30_target0.95_mlasso` | 2,122 | `p100_r20.40_target0.95_mlasso` | 630 | 3.37× lower | — | — | — |
| R²: 0.1 → 0.2 | `p10_r20.10_target0.80_mlasso` | 576 | `p10_r20.20_target0.80_mlasso` | 186 | 3.10× lower | — | — | — |
| R²: 0.1 → 0.2 | `p10_r20.10_target0.90_mlasso` | 2,112 | `p10_r20.20_target0.90_mlasso` | 668 | 3.16× lower | — | — | — |
| R²: 0.2 → 0.3 | `p25_r20.20_target0.80_mlasso` | 360 | `p25_r20.30_target0.80_mlasso` | 89 | 4.04× lower | — | — | — |
| R²: 0.1 → 0.2 | `p30_r20.10_target0.80_mlasso` | 864 | `p30_r20.20_target0.80_mlasso` | 286 | 3.02× lower | — | — | — |
| R²: 0.1 → 0.2 | `p40_r20.10_target0.95_mlasso` | 8,064 | `p40_r20.20_target0.95_mlasso` | 1,926 | 4.19× lower | — | — | — |
| R²: 0.2 → 0.3 | `p40_r20.20_target0.90_mlasso` | 931 | `p40_r20.30_target0.90_mlasso` | 253 | 3.68× lower | — | — | — |
| R²: 0.4 → 0.5 | `p5_r20.40_target0.85_mlasso` | 102 | `p5_r20.50_target0.85_mlasso` | 34 | 3.00× lower | — | — | — |
| Predictors: 10 → 15 | `p10_r20.10_target0.95_mlasso` | 32,768 | `p15_r20.10_target0.95_mlasso` | 6,402 | 5.12× lower | — | previous | previous |
| Predictors: 5 → 10 | `p5_r20.10_target0.95_mlasso` | 4,861 | `p10_r20.10_target0.95_mlasso` | 32,768 | 6.74× higher | — | next | next |
| Predictors: 5 → 10 | `p5_r20.50_target0.85_mlasso` | 34 | `p10_r20.50_target0.85_mlasso` | 106 | 3.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_r20.10_target0.90_mlasso` | 2,640 | `p100_r20.10_target0.95_mlasso` | 8,972 | 3.40× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p10_r20.10_target0.85_mlasso` | 700 | `p10_r20.10_target0.90_mlasso` | 2,112 | 3.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_r20.20_target0.90_mlasso` | 668 | `p10_r20.20_target0.95_mlasso` | 2,506 | 3.75× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_r20.50_target0.90_mlasso` | 168 | `p10_r20.50_target0.95_mlasso` | 633 | 3.77× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_r20.60_target0.90_mlasso` | 134 | `p10_r20.60_target0.95_mlasso` | 433 | 3.23× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_r20.10_target0.90_mlasso` | 1,600 | `p15_r20.10_target0.95_mlasso` | 6,402 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_r20.20_target0.90_mlasso` | 617 | `p15_r20.20_target0.95_mlasso` | 3,008 | 4.88× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_r20.30_target0.90_mlasso` | 426 | `p15_r20.30_target0.95_mlasso` | 1,659 | 3.89× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_r20.40_target0.90_mlasso` | 295 | `p15_r20.40_target0.95_mlasso` | 934 | 3.17× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_r20.50_target0.90_mlasso` | 190 | `p15_r20.50_target0.95_mlasso` | 705 | 3.71× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_r20.60_target0.90_mlasso` | 146 | `p15_r20.60_target0.95_mlasso` | 482 | 3.30× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_r20.10_target0.90_mlasso` | 2,112 | `p20_r20.10_target0.95_mlasso` | 8,071 | 3.82× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_r20.20_target0.90_mlasso` | 818 | `p20_r20.20_target0.95_mlasso` | 2,963 | 3.62× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_r20.30_target0.90_mlasso` | 428 | `p20_r20.30_target0.95_mlasso` | 2,016 | 4.71× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_r20.40_target0.90_mlasso` | 264 | `p20_r20.40_target0.95_mlasso` | 1,115 | 4.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_r20.50_target0.90_mlasso` | 157 | `p20_r20.50_target0.95_mlasso` | 1,008 | 6.42× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_r20.60_target0.90_mlasso` | 139 | `p20_r20.60_target0.95_mlasso` | 504 | 3.63× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_r20.30_target0.90_mlasso` | 472 | `p25_r20.30_target0.95_mlasso` | 2,528 | 5.36× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_r20.40_target0.90_mlasso` | 330 | `p25_r20.40_target0.95_mlasso` | 1,088 | 3.30× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_r20.10_target0.90_mlasso` | 1,941 | `p30_r20.10_target0.95_mlasso` | 7,256 | 3.74× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_r20.20_target0.90_mlasso` | 801 | `p30_r20.20_target0.95_mlasso` | 3,074 | 3.84× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_r20.30_target0.90_mlasso` | 369 | `p30_r20.30_target0.95_mlasso` | 1,692 | 4.59× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_r20.40_target0.90_mlasso` | 396 | `p30_r20.40_target0.95_mlasso` | 1,195 | 3.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_r20.50_target0.90_mlasso` | 202 | `p30_r20.50_target0.95_mlasso` | 792 | 3.92× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_r20.10_target0.90_mlasso` | 2,112 | `p40_r20.10_target0.95_mlasso` | 8,064 | 3.82× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_r20.30_target0.90_mlasso` | 253 | `p40_r20.30_target0.95_mlasso` | 1,892 | 7.48× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_r20.50_target0.90_mlasso` | 252 | `p40_r20.50_target0.95_mlasso` | 1,008 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_r20.30_target0.90_mlasso` | 661 | `p50_r20.30_target0.95_mlasso` | 2,528 | 3.82× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_r20.40_target0.90_mlasso` | 332 | `p50_r20.40_target0.95_mlasso` | 998 | 3.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_r20.10_target0.90_mlasso` | 1,425 | `p5_r20.10_target0.95_mlasso` | 4,861 | 3.41× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_r20.20_target0.90_mlasso` | 512 | `p5_r20.20_target0.95_mlasso` | 1,953 | 3.81× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_r20.30_target0.90_mlasso` | 243 | `p5_r20.30_target0.95_mlasso` | 993 | 4.09× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_r20.40_target0.90_mlasso` | 198 | `p5_r20.40_target0.95_mlasso` | 736 | 3.72× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_r20.50_target0.85_mlasso` | 34 | `p5_r20.50_target0.90_mlasso` | 128 | 3.76× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_r20.50_target0.90_mlasso` | 128 | `p5_r20.50_target0.95_mlasso` | 532 | 4.16× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p5_r20.60_target0.80_mlasso` | 18 | `p5_r20.60_target0.85_mlasso` | 61 | 3.39× higher | — | — | — |
| R²: 0.1 → 0.2 | `p10_r20.10_target0.95_mlm` | 8,192 | `p10_r20.20_target0.95_mlm` | 2,048 | 4.00× lower | — | previous | — |
| R²: 0.5 → 0.6 | `p10_r20.50_target0.95_mlm` | 521 | `p10_r20.60_target0.95_mlm` | 128 | 4.07× lower | — | — | — |
| R²: 0.2 → 0.3 | `p15_r20.20_target0.95_mlm` | 6,016 | `p15_r20.30_target0.95_mlm` | 1,430 | 4.21× lower | — | previous | — |
| R²: 0.1 → 0.2 | `p30_r20.10_target0.95_mlm` | 9,393 | `p30_r20.20_target0.95_mlm` | 3,008 | 3.12× lower | — | — | — |
| R²: 0.1 → 0.2 | `p40_r20.10_target0.95_mlm` | 12,212 | `p40_r20.20_target0.95_mlm` | 4,032 | 3.03× lower | — | — | — |
| R²: 0.1 → 0.2 | `p50_r20.10_target0.95_mlm` | 20,224 | `p50_r20.20_target0.95_mlm` | 5,056 | 4.00× lower | — | — | — |
| R²: 0.1 → 0.2 | `p5_r20.10_target0.95_mlm` | 16,384 | `p5_r20.20_target0.95_mlm` | 2,048 | 8.00× lower | — | previous | — |
| R²: 0.1 → 0.2 | `p75_r20.10_target0.95_mlm` | 30,208 | `p75_r20.20_target0.95_mlm` | 7,552 | 4.00× lower | — | — | — |
| Predictors: 10 → 15 | `p10_r20.60_target0.95_mlm` | 128 | `p15_r20.60_target0.95_mlm` | 437 | 3.41× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_r20.10_target0.90_mlm` | 1,057 | `p10_r20.10_target0.95_mlm` | 8,192 | 7.75× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_r20.20_target0.90_mlm` | 523 | `p10_r20.20_target0.95_mlm` | 2,048 | 3.92× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_r20.10_target0.90_mlm` | 1,923 | `p15_r20.10_target0.95_mlm` | 6,461 | 3.36× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_r20.20_target0.90_mlm` | 801 | `p15_r20.20_target0.95_mlm` | 6,016 | 7.51× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_r20.30_target0.90_mlm` | 400 | `p15_r20.30_target0.95_mlm` | 1,430 | 3.58× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_r20.10_target0.90_mlm` | 2,498 | `p20_r20.10_target0.95_mlm` | 8,064 | 3.23× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_r20.30_target0.90_mlm` | 721 | `p30_r20.30_target0.95_mlm` | 2,312 | 3.21× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_r20.50_target0.90_mlm` | 448 | `p30_r20.50_target0.95_mlm` | 1,504 | 3.36× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_r20.10_target0.90_mlm` | 5,703 | `p50_r20.10_target0.95_mlm` | 20,224 | 3.55× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_r20.20_target0.90_mlm` | 512 | `p5_r20.20_target0.95_mlm` | 2,048 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_r20.40_target0.90_mlm` | 129 | `p5_r20.40_target0.95_mlm` | 654 | 5.07× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_r20.50_target0.90_mlm` | 128 | `p5_r20.50_target0.95_mlm` | 512 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_r20.10_target0.90_mlm` | 7,657 | `p75_r20.10_target0.95_mlm` | 30,208 | 3.95× higher | — | — | — |
| R²: 0.1 → 0.2 | `p15_r20.10_target0.90_mridge` | 3,293 | `p15_r20.20_target0.90_mridge` | 722 | 4.56× lower | — | — | — |
| R²: 0.1 → 0.2 | `p20_r20.10_target0.95_mridge` | 32,256 | `p20_r20.20_target0.95_mridge` | 6,796 | 4.75× lower | — | previous | — |
| R²: 0.4 → 0.5 | `p20_r20.40_target0.95_mridge` | 4,032 | `p20_r20.50_target0.95_mridge` | 949 | 4.25× lower | — | — | — |
| R²: 0.1 → 0.2 | `p25_r20.10_target0.95_mridge` | 17,538 | `p25_r20.20_target0.95_mridge` | 5,441 | 3.22× lower | — | — | — |
| R²: 0.2 → 0.3 | `p25_r20.20_target0.85_mridge` | 1,376 | `p25_r20.30_target0.85_mridge` | 321 | 4.29× lower | — | — | — |
| R²: 0.3 → 0.4 | `p25_r20.30_target0.95_mridge` | 4,358 | `p25_r20.40_target0.95_mridge` | 1,264 | 3.45× lower | — | — | — |
| R²: 0.1 → 0.2 | `p30_r20.10_target0.80_mridge` | 1,611 | `p30_r20.20_target0.80_mridge` | 428 | 3.76× lower | — | — | — |
| R²: 0.1 → 0.2 | `p30_r20.10_target0.85_mridge` | 3,296 | `p30_r20.20_target0.85_mridge` | 967 | 3.41× lower | — | — | — |
| R²: 0.1 → 0.2 | `p40_r20.10_target0.85_mridge` | 4,416 | `p40_r20.20_target0.85_mridge` | 1,362 | 3.24× lower | — | — | — |
| R²: 0.1 → 0.2 | `p5_r20.10_target0.95_mridge` | 8,192 | `p5_r20.20_target0.95_mridge` | 1,955 | 4.19× lower | — | — | — |
| R²: 0.1 → 0.2 | `p75_r20.10_target0.80_mridge` | 1,959 | `p75_r20.20_target0.80_mridge` | 540 | 3.63× lower | — | — | — |
| Predictors: 15 → 20 | `p15_r20.10_target0.95_mridge` | 6,016 | `p20_r20.10_target0.95_mridge` | 32,256 | 5.36× higher | — | next | — |
| Predictors: 20 → 25 | `p20_r20.40_target0.95_mridge` | 4,032 | `p25_r20.40_target0.95_mridge` | 1,264 | 3.19× lower | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_r20.30_target0.90_mridge` | 2,641 | `p100_r20.30_target0.95_mridge` | 10,080 | 3.82× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_r20.40_target0.90_mridge` | 991 | `p100_r20.40_target0.95_mridge` | 5,522 | 5.57× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p100_r20.50_target0.85_mridge` | 345 | `p100_r20.50_target0.90_mridge` | 1,199 | 3.48× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_r20.60_target0.90_mridge` | 825 | `p100_r20.60_target0.95_mridge` | 2,532 | 3.07× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_r20.10_target0.90_mridge` | 2,482 | `p10_r20.10_target0.95_mridge` | 8,498 | 3.42× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_r20.20_target0.90_mridge` | 1,343 | `p10_r20.20_target0.95_mridge` | 4,096 | 3.05× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p10_r20.30_target0.85_mridge` | 272 | `p10_r20.30_target0.90_mridge` | 1,056 | 3.88× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_r20.40_target0.90_mridge` | 522 | `p10_r20.40_target0.95_mridge` | 1,621 | 3.11× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_r20.50_target0.90_mridge` | 318 | `p10_r20.50_target0.95_mridge` | 1,024 | 3.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_r20.20_target0.90_mridge` | 722 | `p15_r20.20_target0.95_mridge` | 5,277 | 7.31× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_r20.30_target0.90_mridge` | 750 | `p15_r20.30_target0.95_mridge` | 2,741 | 3.65× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_r20.40_target0.90_mridge` | 683 | `p15_r20.40_target0.95_mridge` | 3,008 | 4.40× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_r20.10_target0.90_mridge` | 4,370 | `p20_r20.10_target0.95_mridge` | 32,256 | 7.38× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_r20.20_target0.90_mridge` | 2,113 | `p20_r20.20_target0.95_mridge` | 6,796 | 3.22× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p20_r20.40_target0.85_mridge` | 338 | `p20_r20.40_target0.90_mridge` | 1,056 | 3.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_r20.40_target0.90_mridge` | 1,056 | `p20_r20.40_target0.95_mridge` | 4,032 | 3.82× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_r20.60_target0.90_mridge` | 354 | `p20_r20.60_target0.95_mridge` | 1,116 | 3.15× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_r20.10_target0.90_mridge` | 5,066 | `p25_r20.10_target0.95_mridge` | 17,538 | 3.46× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_r20.30_target0.85_mridge` | 321 | `p25_r20.30_target0.90_mridge` | 1,404 | 4.37× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_r20.30_target0.90_mridge` | 1,404 | `p25_r20.30_target0.95_mridge` | 4,358 | 3.10× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_r20.50_target0.90_mridge` | 565 | `p25_r20.50_target0.95_mridge` | 1,784 | 3.16× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_r20.60_target0.90_mridge` | 328 | `p25_r20.60_target0.95_mridge` | 1,275 | 3.89× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_r20.10_target0.90_mridge` | 6,336 | `p30_r20.10_target0.95_mridge` | 20,118 | 3.18× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_r20.20_target0.90_mridge` | 2,727 | `p30_r20.20_target0.95_mridge` | 8,338 | 3.06× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_r20.50_target0.90_mridge` | 550 | `p30_r20.50_target0.95_mridge` | 1,898 | 3.45× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_r20.60_target0.90_mridge` | 411 | `p30_r20.60_target0.95_mridge` | 1,234 | 3.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_r20.10_target0.90_mridge` | 7,191 | `p40_r20.10_target0.95_mridge` | 22,502 | 3.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_r20.20_target0.90_mridge` | 2,600 | `p40_r20.20_target0.95_mridge` | 8,064 | 3.10× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p40_r20.50_target0.80_mridge` | 144 | `p40_r20.50_target0.85_mridge` | 552 | 3.83× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_r20.50_target0.90_mridge` | 726 | `p40_r20.50_target0.95_mridge` | 2,323 | 3.20× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_r20.30_target0.90_mridge` | 1,946 | `p50_r20.30_target0.95_mridge` | 6,023 | 3.10× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_r20.40_target0.90_mridge` | 1,312 | `p50_r20.40_target0.95_mridge` | 5,056 | 3.85× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_r20.50_target0.90_mridge` | 787 | `p50_r20.50_target0.95_mridge` | 2,480 | 3.15× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_r20.10_target0.90_mridge` | 2,059 | `p5_r20.10_target0.95_mridge` | 8,192 | 3.98× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_r20.50_target0.90_mridge` | 128 | `p5_r20.50_target0.95_mridge` | 760 | 5.94× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_r20.60_target0.90_mridge` | 138 | `p5_r20.60_target0.95_mridge` | 447 | 3.24× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_r20.10_target0.90_mridge` | 9,868 | `p75_r20.10_target0.95_mridge` | 29,751 | 3.01× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p75_r20.20_target0.80_mridge` | 540 | `p75_r20.20_target0.85_mridge` | 2,072 | 3.84× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_r20.30_target0.90_mridge` | 1,955 | `p75_r20.30_target0.95_mridge` | 7,552 | 3.86× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_r20.60_target0.90_mridge` | 482 | `p75_r20.60_target0.95_mridge` | 1,888 | 3.92× higher | — | — | — |

## Survival

| Swept input | Previous cell | Previous N | Next cell | Next N | Next vs previous | Gap | Excluded endpoints | Earlier isolated endpoints |
|---|---|---:|---|---:|---|---|---|---|
| Baseline hazard: 0.5 → 1 | `p10_cindex0.60_hazard0.50_censor0.50_target0.95_mcoxph` | 21,312 | `p10_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 3,946 | 5.40× lower | — | previous | — |
| Baseline hazard: 1 → 2 | `p10_cindex0.65_hazard1.00_censor0.30_target0.95_mcoxph` | 7,040 | `p10_cindex0.65_hazard2.00_censor0.30_target0.95_mcoxph` | 1,760 | 4.00× lower | — | previous | — |
| Baseline hazard: 1 → 2 | `p10_cindex0.70_hazard1.00_censor0.50_target0.95_mcoxph` | 4,576 | `p10_cindex0.70_hazard2.00_censor0.50_target0.95_mcoxph` | 1,144 | 4.00× lower | — | previous, next | next |
| Baseline hazard: 0.5 → 1 | `p15_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 4,388 | `p15_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 17,792 | 4.05× higher | — | next | next |
| Baseline hazard: 0.5 → 1 | `p15_cindex0.60_hazard0.50_censor0.30_target0.95_mcoxph` | 10,969 | `p15_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 91,392 | 8.33× higher | — | next | next |
| Baseline hazard: 1 → 2 | `p15_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 17,792 | `p15_cindex0.60_hazard2.00_censor0.10_target0.95_mcoxph` | 5,930 | 3.00× lower | — | previous | previous |
| Baseline hazard: 1 → 2 | `p15_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 91,392 | `p15_cindex0.60_hazard2.00_censor0.30_target0.95_mcoxph` | 731,136 | 8.00× higher | — | previous, next | previous, next |
| Baseline hazard: 0.5 → 1 | `p20_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 23,680 | `p20_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 2,960 | 8.00× lower | — | previous | — |
| Baseline hazard: 0.5 → 1 | `p20_cindex0.60_hazard0.50_censor0.30_target0.95_mcoxph` | 4,763 | `p20_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 15,264 | 3.20× higher | — | next | — |
| Baseline hazard: 0.5 → 1 | `p20_cindex0.60_hazard0.50_censor0.50_target0.95_mcoxph` | 5,336 | `p20_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 17,020 | 3.19× higher | — | — | — |
| Baseline hazard: 0.5 → 1 | `p20_cindex0.80_hazard0.50_censor0.10_target0.95_mcoxph` | 2,224 | `p20_cindex0.80_hazard1.00_censor0.10_target0.95_mcoxph` | 556 | 4.00× lower | — | — | — |
| Baseline hazard: 0.5 → 1 | `p25_cindex0.60_hazard0.50_censor0.50_target0.95_mcoxph` | 13,328 | `p25_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 106,624 | 8.00× higher | — | next | next |
| Baseline hazard: 1 → 2 | `p25_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 106,624 | `p25_cindex0.60_hazard2.00_censor0.50_target0.95_mcoxph` | 13,328 | 8.00× lower | — | previous | previous |
| Baseline hazard: 0.5 → 1 | `p30_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 35,520 | `p30_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 10,063 | 3.53× lower | — | previous | — |
| Baseline hazard: 1 → 2 | `p30_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 256,000 | `p30_cindex0.60_hazard2.00_censor0.50_target0.95_mcoxph` | 32,000 | 8.00× lower | — | previous | — |
| Baseline hazard: 1 → 2 | `p40_cindex0.65_hazard1.00_censor0.50_target0.95_mcoxph` | 9,319 | `p40_cindex0.65_hazard2.00_censor0.50_target0.95_mcoxph` | 39,392 | 4.23× higher | — | next | — |
| Baseline hazard: 0.5 → 1 | `p40_cindex0.75_hazard0.50_censor0.50_target0.95_mcoxph` | 17,072 | `p40_cindex0.75_hazard1.00_censor0.50_target0.95_mcoxph` | 3,850 | 4.43× lower | — | previous | — |
| Baseline hazard: 0.5 → 1 | `p5_cindex0.60_hazard0.50_censor0.30_target0.95_mcoxph` | 15,104 | `p5_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 1,888 | 8.00× lower | — | previous | — |
| Baseline hazard: 0.5 → 1 | `p5_cindex0.60_hazard0.50_censor0.50_target0.95_mcoxph` | 42,752 | `p5_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 5,985 | 7.14× lower | — | previous | — |
| Baseline hazard: 1 → 2 | `p5_cindex0.60_hazard1.00_censor0.10_target0.90_mcoxph` | 372 | `p5_cindex0.60_hazard2.00_censor0.10_target0.90_mcoxph` | 1,488 | 4.00× higher | — | — | — |
| Baseline hazard: 1 → 2 | `p5_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 1,888 | `p5_cindex0.60_hazard2.00_censor0.30_target0.95_mcoxph` | 7,552 | 4.00× higher | — | next | — |
| Baseline hazard: 1 → 2 | `p5_cindex0.65_hazard1.00_censor0.50_target0.95_mcoxph` | 19,712 | `p5_cindex0.65_hazard2.00_censor0.50_target0.95_mcoxph` | 4,928 | 4.00× lower | — | previous, next | — |
| Baseline hazard: 1 → 2 | `p5_cindex0.70_hazard1.00_censor0.10_target0.95_mcoxph` | 628 | `p5_cindex0.70_hazard2.00_censor0.10_target0.95_mcoxph` | 2,560 | 4.08× higher | — | next | — |
| Baseline hazard: 1 → 2 | `p5_cindex0.75_hazard1.00_censor0.10_target0.95_mcoxph` | 1,200 | `p5_cindex0.75_hazard2.00_censor0.10_target0.95_mcoxph` | 390 | 3.08× lower | — | previous | — |
| Baseline hazard: 0.5 → 1 | `p75_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 23,689 | `p75_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 177,664 | 7.50× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p100_cindex0.60_hazard1.00_censor0.30_target0.90_mcoxph` | 14,133 | `p100_cindex0.60_hazard1.00_censor0.50_target0.90_mcoxph` | 53,328 | 3.77× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p10_cindex0.70_hazard1.00_censor0.30_target0.95_mcoxph` | 1,167 | `p10_cindex0.70_hazard1.00_censor0.50_target0.95_mcoxph` | 4,576 | 3.92× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p10_cindex0.75_hazard2.00_censor0.30_target0.95_mcoxph` | 1,114 | `p10_cindex0.75_hazard2.00_censor0.50_target0.95_mcoxph` | 4,272 | 3.83× higher | — | next | next |
| Censoring: 0.1 → 0.3 | `p15_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 17,792 | `p15_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 91,392 | 5.14× higher | — | previous, next | previous, next |
| Censoring: 0.3 → 0.5 | `p15_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 91,392 | `p15_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 16,000 | 5.71× lower | — | previous | previous |
| Censoring: 0.1 → 0.3 | `p15_cindex0.60_hazard2.00_censor0.10_target0.90_mcoxph` | 1,692 | `p15_cindex0.60_hazard2.00_censor0.30_target0.90_mcoxph` | 5,712 | 3.38× higher | — | — | — |
| Censoring: 0.3 → 0.5 | `p15_cindex0.70_hazard0.50_censor0.30_target0.95_mcoxph` | 2,079 | `p15_cindex0.70_hazard0.50_censor0.50_target0.95_mcoxph` | 6,864 | 3.30× higher | — | next | — |
| Censoring: 0.1 → 0.3 | `p15_cindex0.70_hazard2.00_censor0.10_target0.95_mcoxph` | 3,824 | `p15_cindex0.70_hazard2.00_censor0.30_target0.95_mcoxph` | 1,224 | 3.12× lower | — | previous | — |
| Censoring: 0.1 → 0.3 | `p20_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 23,680 | `p20_cindex0.60_hazard0.50_censor0.30_target0.95_mcoxph` | 4,763 | 4.97× lower | — | previous | — |
| Censoring: 0.1 → 0.3 | `p20_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 2,960 | `p20_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 15,264 | 5.16× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p20_cindex0.65_hazard0.50_censor0.30_target0.95_mcoxph` | 2,706 | `p20_cindex0.65_hazard0.50_censor0.50_target0.95_mcoxph` | 9,840 | 3.64× higher | — | — | — |
| Censoring: 0.1 → 0.3 | `p30_cindex0.70_hazard2.00_censor0.10_target0.95_mcoxph` | 7,616 | `p30_cindex0.70_hazard2.00_censor0.30_target0.95_mcoxph` | 2,452 | 3.11× lower | — | previous | — |
| Censoring: 0.1 → 0.3 | `p30_cindex0.85_hazard1.00_censor0.10_target0.95_mcoxph` | 1,310 | `p30_cindex0.85_hazard1.00_censor0.30_target0.95_mcoxph` | 4,040 | 3.08× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p40_cindex0.65_hazard2.00_censor0.30_target0.95_mcoxph` | 5,412 | `p40_cindex0.65_hazard2.00_censor0.50_target0.95_mcoxph` | 39,392 | 7.28× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p40_cindex0.75_hazard0.50_censor0.30_target0.95_mcoxph` | 3,190 | `p40_cindex0.75_hazard0.50_censor0.50_target0.95_mcoxph` | 17,072 | 5.35× higher | — | next | — |
| Censoring: 0.1 → 0.3 | `p5_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 2,778 | `p5_cindex0.60_hazard0.50_censor0.30_target0.95_mcoxph` | 15,104 | 5.44× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p5_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 1,888 | `p5_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 5,985 | 3.17× higher | — | — | — |
| Censoring: 0.1 → 0.3 | `p5_cindex0.60_hazard2.00_censor0.10_target0.95_mcoxph` | 1,488 | `p5_cindex0.60_hazard2.00_censor0.30_target0.95_mcoxph` | 7,552 | 5.08× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p5_cindex0.60_hazard2.00_censor0.30_target0.90_mcoxph` | 1,275 | `p5_cindex0.60_hazard2.00_censor0.50_target0.90_mcoxph` | 5,344 | 4.19× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p5_cindex0.65_hazard0.50_censor0.30_target0.95_mcoxph` | 2,281 | `p5_cindex0.65_hazard0.50_censor0.50_target0.95_mcoxph` | 616 | 3.70× lower | — | previous | — |
| Censoring: 0.3 → 0.5 | `p5_cindex0.65_hazard2.00_censor0.30_target0.95_mcoxph` | 872 | `p5_cindex0.65_hazard2.00_censor0.50_target0.95_mcoxph` | 4,928 | 5.65× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p5_cindex0.70_hazard2.00_censor0.30_target0.95_mcoxph` | 3,232 | `p5_cindex0.70_hazard2.00_censor0.50_target0.95_mcoxph` | 420 | 7.70× lower | — | previous | previous |
| Censoring: 0.1 → 0.3 | `p5_cindex0.85_hazard0.50_censor0.10_target0.95_mcoxph` | 194 | `p5_cindex0.85_hazard0.50_censor0.30_target0.95_mcoxph` | 672 | 3.46× higher | — | — | — |
| Censoring: 0.1 → 0.3 | `p75_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 177,664 | `p75_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 27,793 | 6.39× lower | — | previous | — |
| Censoring: 0.1 → 0.5 | `p75_cindex0.60_hazard2.00_censor0.10_target0.95_mcoxph` | 177,664 | `p75_cindex0.60_hazard2.00_censor0.50_target0.95_mcoxph` | 19,451 | 9.13× lower | Yes | previous | — |
| C-index: 0.6 → 0.65 | `p100_cindex0.60_hazard1.00_censor0.50_target0.90_mcoxph` | 53,328 | `p100_cindex0.65_hazard1.00_censor0.50_target0.90_mcoxph` | 8,503 | 6.27× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p100_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 106,656 | `p100_cindex0.65_hazard1.00_censor0.50_target0.95_mcoxph` | 24,616 | 4.33× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p100_cindex0.60_hazard2.00_censor0.50_target0.95_mcoxph` | 51,605 | `p100_cindex0.65_hazard2.00_censor0.50_target0.95_mcoxph` | 12,135 | 4.25× lower | — | — | — |
| C-index: 0.75 → 0.8 | `p100_cindex0.75_hazard0.50_censor0.50_target0.95_mcoxph` | 21,336 | `p100_cindex0.80_hazard0.50_censor0.50_target0.95_mcoxph` | 6,316 | 3.38× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p10_cindex0.60_hazard0.50_censor0.50_target0.95_mcoxph` | 21,312 | `p10_cindex0.65_hazard0.50_censor0.50_target0.95_mcoxph` | 2,415 | 8.82× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p10_cindex0.60_hazard1.00_censor0.50_target0.85_mcoxph` | 2,664 | `p10_cindex0.65_hazard1.00_censor0.50_target0.85_mcoxph` | 708 | 3.76× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p10_cindex0.60_hazard2.00_censor0.50_target0.85_mcoxph` | 2,664 | `p10_cindex0.65_hazard2.00_censor0.50_target0.85_mcoxph` | 604 | 4.41× lower | — | — | — |
| C-index: 0.6 → 0.7 | `p10_cindex0.60_hazard2.00_censor0.50_target0.90_mcoxph` | 2,664 | `p10_cindex0.70_hazard2.00_censor0.50_target0.90_mcoxph` | 557 | 4.78× lower | Yes | — | — |
| C-index: 0.65 → 0.7 | `p10_cindex0.65_hazard1.00_censor0.30_target0.95_mcoxph` | 7,040 | `p10_cindex0.70_hazard1.00_censor0.30_target0.95_mcoxph` | 1,167 | 6.03× lower | — | previous | — |
| C-index: 0.65 → 0.7 | `p10_cindex0.65_hazard2.00_censor0.50_target0.95_mcoxph` | 4,928 | `p10_cindex0.70_hazard2.00_censor0.50_target0.95_mcoxph` | 1,144 | 4.31× lower | — | previous, next | next |
| C-index: 0.7 → 0.75 | `p10_cindex0.70_hazard1.00_censor0.50_target0.95_mcoxph` | 4,576 | `p10_cindex0.75_hazard1.00_censor0.50_target0.95_mcoxph` | 1,431 | 3.20× lower | — | previous | — |
| C-index: 0.7 → 0.75 | `p10_cindex0.70_hazard2.00_censor0.50_target0.95_mcoxph` | 1,144 | `p10_cindex0.75_hazard2.00_censor0.50_target0.95_mcoxph` | 4,272 | 3.73× higher | — | previous, next | previous, next |
| C-index: 0.75 → 0.8 | `p10_cindex0.75_hazard2.00_censor0.50_target0.95_mcoxph` | 4,272 | `p10_cindex0.80_hazard2.00_censor0.50_target0.95_mcoxph` | 1,062 | 4.02× lower | — | previous | previous |
| C-index: 0.6 → 0.65 | `p15_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 17,792 | `p15_cindex0.65_hazard1.00_censor0.10_target0.95_mcoxph` | 2,200 | 8.09× lower | — | previous | previous |
| C-index: 0.6 → 0.65 | `p15_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 16,000 | `p15_cindex0.65_hazard1.00_censor0.50_target0.95_mcoxph` | 3,696 | 4.33× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p15_cindex0.60_hazard2.00_censor0.30_target0.90_mcoxph` | 5,712 | `p15_cindex0.65_hazard2.00_censor0.30_target0.90_mcoxph` | 1,424 | 4.01× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p15_cindex0.65_hazard2.00_censor0.30_target0.95_mcoxph` | 5,264 | `p15_cindex0.70_hazard2.00_censor0.30_target0.95_mcoxph` | 1,224 | 4.30× lower | — | — | — |
| C-index: 0.7 → 0.75 | `p15_cindex0.70_hazard0.50_censor0.50_target0.95_mcoxph` | 6,864 | `p15_cindex0.75_hazard0.50_censor0.50_target0.95_mcoxph` | 1,835 | 3.74× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 23,680 | `p20_cindex0.65_hazard0.50_censor0.10_target0.95_mcoxph` | 3,624 | 6.53× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard0.50_censor0.30_target0.80_mcoxph` | 1,908 | `p20_cindex0.65_hazard0.50_censor0.30_target0.80_mcoxph` | 442 | 4.32× lower | — | — | — |
| C-index: 0.6 → 0.7 | `p20_cindex0.60_hazard0.50_censor0.50_target0.85_mcoxph` | 2,668 | `p20_cindex0.70_hazard0.50_censor0.50_target0.85_mcoxph` | 757 | 3.52× lower | Yes | — | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 15,264 | `p20_cindex0.65_hazard1.00_censor0.30_target0.95_mcoxph` | 4,138 | 3.69× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard1.00_censor0.50_target0.80_mcoxph` | 2,668 | `p20_cindex0.65_hazard1.00_censor0.50_target0.80_mcoxph` | 854 | 3.12× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 17,020 | `p20_cindex0.65_hazard1.00_censor0.50_target0.95_mcoxph` | 4,687 | 3.63× lower | — | — | — |
| C-index: 0.6 → 0.7 | `p25_cindex0.60_hazard0.50_censor0.50_target0.80_mcoxph` | 2,202 | `p25_cindex0.70_hazard0.50_censor0.50_target0.80_mcoxph` | 610 | 3.61× lower | Yes | — | — |
| C-index: 0.6 → 0.65 | `p25_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 9,209 | `p25_cindex0.65_hazard1.00_censor0.10_target0.95_mcoxph` | 2,996 | 3.07× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p25_cindex0.65_hazard2.00_censor0.30_target0.95_mcoxph` | 8,784 | `p25_cindex0.70_hazard2.00_censor0.30_target0.95_mcoxph` | 2,751 | 3.19× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p30_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 35,520 | `p30_cindex0.65_hazard0.50_censor0.10_target0.95_mcoxph` | 3,937 | 9.02× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p30_cindex0.60_hazard0.50_censor0.30_target0.95_mcoxph` | 16,451 | `p30_cindex0.65_hazard0.50_censor0.30_target0.95_mcoxph` | 5,137 | 3.20× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p30_cindex0.60_hazard2.00_censor0.30_target0.95_mcoxph` | 22,880 | `p30_cindex0.65_hazard2.00_censor0.30_target0.95_mcoxph` | 5,156 | 4.44× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p30_cindex0.60_hazard2.00_censor0.50_target0.95_mcoxph` | 32,000 | `p30_cindex0.65_hazard2.00_censor0.50_target0.95_mcoxph` | 7,572 | 4.23× lower | — | — | — |
| C-index: 0.7 → 0.75 | `p30_cindex0.70_hazard2.00_censor0.10_target0.95_mcoxph` | 7,616 | `p30_cindex0.75_hazard2.00_censor0.10_target0.95_mcoxph` | 1,941 | 3.92× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p40_cindex0.60_hazard2.00_censor0.10_target0.90_mcoxph` | 11,840 | `p40_cindex0.65_hazard2.00_censor0.10_target0.90_mcoxph` | 2,509 | 4.72× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p40_cindex0.60_hazard2.00_censor0.30_target0.95_mcoxph` | 20,096 | `p40_cindex0.65_hazard2.00_censor0.30_target0.95_mcoxph` | 5,412 | 3.71× lower | — | — | — |
| C-index: 0.6 → 0.7 | `p40_cindex0.60_hazard2.00_censor0.50_target0.90_mcoxph` | 9,261 | `p40_cindex0.70_hazard2.00_censor0.50_target0.90_mcoxph` | 2,273 | 4.07× lower | Yes | — | — |
| C-index: 0.65 → 0.7 | `p40_cindex0.65_hazard2.00_censor0.50_target0.95_mcoxph` | 39,392 | `p40_cindex0.70_hazard2.00_censor0.50_target0.95_mcoxph` | 4,277 | 9.21× lower | — | previous | — |
| C-index: 0.7 → 0.75 | `p40_cindex0.70_hazard1.00_censor0.30_target0.95_mcoxph` | 4,588 | `p40_cindex0.75_hazard1.00_censor0.30_target0.95_mcoxph` | 1,522 | 3.01× lower | — | — | — |
| C-index: 0.75 → 0.8 | `p40_cindex0.75_hazard0.50_censor0.50_target0.95_mcoxph` | 17,072 | `p40_cindex0.80_hazard0.50_censor0.50_target0.95_mcoxph` | 3,140 | 5.44× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p50_cindex0.60_hazard1.00_censor0.50_target0.80_mcoxph` | 6,668 | `p50_cindex0.65_hazard1.00_censor0.50_target0.80_mcoxph` | 1,961 | 3.40× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p50_cindex0.60_hazard2.00_censor0.10_target0.90_mcoxph` | 7,416 | `p50_cindex0.65_hazard2.00_censor0.10_target0.90_mcoxph` | 1,711 | 4.33× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p50_cindex0.65_hazard0.50_censor0.30_target0.95_mcoxph` | 17,568 | `p50_cindex0.70_hazard0.50_censor0.30_target0.95_mcoxph` | 4,080 | 4.31× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p5_cindex0.60_hazard0.50_censor0.30_target0.95_mcoxph` | 15,104 | `p5_cindex0.65_hazard0.50_censor0.30_target0.95_mcoxph` | 2,281 | 6.62× lower | — | previous, next | — |
| C-index: 0.6 → 0.65 | `p5_cindex0.60_hazard1.00_censor0.50_target0.90_mcoxph` | 1,852 | `p5_cindex0.65_hazard1.00_censor0.50_target0.90_mcoxph` | 611 | 3.03× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p5_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 5,985 | `p5_cindex0.65_hazard1.00_censor0.50_target0.95_mcoxph` | 19,712 | 3.29× higher | — | next | — |
| C-index: 0.6 → 0.65 | `p5_cindex0.60_hazard2.00_censor0.30_target0.95_mcoxph` | 7,552 | `p5_cindex0.65_hazard2.00_censor0.30_target0.95_mcoxph` | 872 | 8.66× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p5_cindex0.60_hazard2.00_censor0.50_target0.90_mcoxph` | 5,344 | `p5_cindex0.65_hazard2.00_censor0.50_target0.90_mcoxph` | 616 | 8.68× lower | — | previous | — |
| C-index: 0.65 → 0.7 | `p5_cindex0.65_hazard2.00_censor0.10_target0.95_mcoxph` | 688 | `p5_cindex0.70_hazard2.00_censor0.10_target0.95_mcoxph` | 2,560 | 3.72× higher | — | next | — |
| C-index: 0.65 → 0.7 | `p5_cindex0.65_hazard2.00_censor0.30_target0.95_mcoxph` | 872 | `p5_cindex0.70_hazard2.00_censor0.30_target0.95_mcoxph` | 3,232 | 3.71× higher | — | next | next |
| C-index: 0.7 → 0.75 | `p5_cindex0.70_hazard2.00_censor0.10_target0.95_mcoxph` | 2,560 | `p5_cindex0.75_hazard2.00_censor0.10_target0.95_mcoxph` | 390 | 6.56× lower | — | previous | — |
| C-index: 0.7 → 0.75 | `p5_cindex0.70_hazard2.00_censor0.30_target0.95_mcoxph` | 3,232 | `p5_cindex0.75_hazard2.00_censor0.30_target0.95_mcoxph` | 685 | 4.72× lower | — | previous | previous |
| C-index: 0.8 → 0.85 | `p5_cindex0.80_hazard0.50_censor0.50_target0.90_mcoxph` | 500 | `p5_cindex0.85_hazard0.50_censor0.50_target0.90_mcoxph` | 118 | 4.24× lower | — | previous | — |
| Predictors: 10 → 15 | `p10_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 5,334 | `p15_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 17,792 | 3.34× higher | — | next | next |
| Predictors: 10 → 15 | `p10_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 3,946 | `p15_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 16,000 | 4.05× higher | — | — | — |
| Predictors: 10 → 15 | `p10_cindex0.70_hazard0.50_censor0.50_target0.95_mcoxph` | 2,083 | `p15_cindex0.70_hazard0.50_censor0.50_target0.95_mcoxph` | 6,864 | 3.30× higher | — | next | — |
| Predictors: 10 → 15 | `p10_cindex0.75_hazard2.00_censor0.10_target0.95_mcoxph` | 1,184 | `p15_cindex0.75_hazard2.00_censor0.10_target0.95_mcoxph` | 3,568 | 3.01× higher | — | next | — |
| Predictors: 10 → 15 | `p10_cindex0.85_hazard0.50_censor0.30_target0.95_mcoxph` | 336 | `p15_cindex0.85_hazard0.50_censor0.30_target0.95_mcoxph` | 1,013 | 3.01× higher | — | — | — |
| Predictors: 15 → 20 | `p15_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 4,388 | `p20_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 23,680 | 5.40× higher | — | next | — |
| Predictors: 15 → 20 | `p15_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 17,792 | `p20_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 2,960 | 6.01× lower | — | previous | previous |
| Predictors: 15 → 20 | `p15_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 91,392 | `p20_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 15,264 | 5.99× lower | — | previous, next | previous |
| Predictors: 20 → 25 | `p20_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 23,680 | `p25_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 7,024 | 3.37× lower | — | previous, next | next |
| Predictors: 20 → 25 | `p20_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 2,960 | `p25_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 9,209 | 3.11× higher | — | — | — |
| Predictors: 20 → 25 | `p20_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 17,020 | `p25_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 106,624 | 6.26× higher | — | next | next |
| Predictors: 20 → 25 | `p20_cindex0.65_hazard2.00_censor0.50_target0.95_mcoxph` | 1,882 | `p25_cindex0.65_hazard2.00_censor0.50_target0.95_mcoxph` | 7,625 | 4.05× higher | — | — | — |
| Predictors: 25 → 30 | `p25_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 7,024 | `p30_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 35,520 | 5.06× higher | — | previous, next | previous |
| Predictors: 25 → 30 | `p25_cindex0.70_hazard2.00_censor0.10_target0.95_mcoxph` | 1,532 | `p30_cindex0.70_hazard2.00_censor0.10_target0.95_mcoxph` | 7,616 | 4.97× higher | — | next | — |
| Predictors: 25 → 30 | `p25_cindex0.85_hazard1.00_censor0.30_target0.95_mcoxph` | 1,340 | `p30_cindex0.85_hazard1.00_censor0.30_target0.95_mcoxph` | 4,040 | 3.01× higher | — | next | — |
| Predictors: 30 → 50 | `p30_cindex0.60_hazard2.00_censor0.50_target0.95_mcoxph` | 32,000 | `p50_cindex0.60_hazard2.00_censor0.50_target0.95_mcoxph` | 213,376 | 6.67× higher | Yes | next | — |
| Predictors: 30 → 40 | `p30_cindex0.65_hazard2.00_censor0.50_target0.95_mcoxph` | 7,572 | `p40_cindex0.65_hazard2.00_censor0.50_target0.95_mcoxph` | 39,392 | 5.20× higher | — | next | — |
| Predictors: 30 → 40 | `p30_cindex0.75_hazard0.50_censor0.50_target0.95_mcoxph` | 2,916 | `p40_cindex0.75_hazard0.50_censor0.50_target0.95_mcoxph` | 17,072 | 5.85× higher | — | next | — |
| Predictors: 40 → 50 | `p40_cindex0.65_hazard2.00_censor0.50_target0.95_mcoxph` | 39,392 | `p50_cindex0.65_hazard2.00_censor0.50_target0.95_mcoxph` | 12,304 | 3.20× lower | — | previous | — |
| Predictors: 40 → 50 | `p40_cindex0.75_hazard0.50_censor0.50_target0.95_mcoxph` | 17,072 | `p50_cindex0.75_hazard0.50_censor0.50_target0.95_mcoxph` | 4,821 | 3.54× lower | — | previous | — |
| Predictors: 50 → 100 | `p50_cindex0.60_hazard0.50_censor0.50_target0.95_mcoxph` | 426,752 | `p100_cindex0.60_hazard0.50_censor0.50_target0.95_mcoxph` | 53,328 | 8.00× lower | Yes | previous | — |
| Predictors: 50 → 100 | `p50_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 13,336 | `p100_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 106,656 | 8.00× higher | Yes | next | — |
| Predictors: 5 → 10 | `p5_cindex0.60_hazard1.00_censor0.10_target0.90_mcoxph` | 372 | `p10_cindex0.60_hazard1.00_censor0.10_target0.90_mcoxph` | 1,657 | 4.45× higher | — | — | — |
| Predictors: 5 → 10 | `p5_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 1,488 | `p10_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 5,334 | 3.58× higher | — | — | — |
| Predictors: 5 → 10 | `p5_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 1,888 | `p10_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 5,797 | 3.07× higher | — | — | — |
| Predictors: 5 → 10 | `p5_cindex0.60_hazard1.00_censor0.50_target0.85_mcoxph` | 668 | `p10_cindex0.60_hazard1.00_censor0.50_target0.85_mcoxph` | 2,664 | 3.99× higher | — | — | — |
| Predictors: 5 → 10 | `p5_cindex0.60_hazard2.00_censor0.10_target0.95_mcoxph` | 1,488 | `p10_cindex0.60_hazard2.00_censor0.10_target0.95_mcoxph` | 5,620 | 3.78× higher | — | — | — |
| Predictors: 5 → 10 | `p5_cindex0.65_hazard0.50_censor0.50_target0.95_mcoxph` | 616 | `p10_cindex0.65_hazard0.50_censor0.50_target0.95_mcoxph` | 2,415 | 3.92× higher | — | — | — |
| Predictors: 5 → 10 | `p5_cindex0.65_hazard1.00_censor0.30_target0.95_mcoxph` | 1,948 | `p10_cindex0.65_hazard1.00_censor0.30_target0.95_mcoxph` | 7,040 | 3.61× higher | — | next | — |
| Predictors: 5 → 10 | `p5_cindex0.65_hazard1.00_censor0.50_target0.95_mcoxph` | 19,712 | `p10_cindex0.65_hazard1.00_censor0.50_target0.95_mcoxph` | 2,464 | 8.00× lower | — | previous | — |
| Predictors: 5 → 10 | `p5_cindex0.65_hazard2.00_censor0.10_target0.95_mcoxph` | 688 | `p10_cindex0.65_hazard2.00_censor0.10_target0.95_mcoxph` | 2,736 | 3.98× higher | — | — | — |
| Predictors: 5 → 15 | `p5_cindex0.65_hazard2.00_censor0.50_target0.90_mcoxph` | 616 | `p15_cindex0.65_hazard2.00_censor0.50_target0.90_mcoxph` | 1,848 | 3.00× higher | Yes | — | — |
| Predictors: 5 → 10 | `p5_cindex0.70_hazard1.00_censor0.50_target0.95_mcoxph` | 1,057 | `p10_cindex0.70_hazard1.00_censor0.50_target0.95_mcoxph` | 4,576 | 4.33× higher | — | next | — |
| Predictors: 5 → 10 | `p5_cindex0.75_hazard2.00_censor0.10_target0.95_mcoxph` | 390 | `p10_cindex0.75_hazard2.00_censor0.10_target0.95_mcoxph` | 1,184 | 3.04× higher | — | — | — |
| Predictors: 5 → 10 | `p5_cindex0.75_hazard2.00_censor0.50_target0.95_mcoxph` | 1,076 | `p10_cindex0.75_hazard2.00_censor0.50_target0.95_mcoxph` | 4,272 | 3.97× higher | — | next | next |
| Predictors: 5 → 10 | `p5_cindex0.85_hazard0.50_censor0.50_target0.90_mcoxph` | 118 | `p10_cindex0.85_hazard0.50_censor0.50_target0.90_mcoxph` | 359 | 3.04× higher | — | — | — |
| Predictors: 75 → 100 | `p75_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 177,664 | `p100_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 29,632 | 6.00× lower | — | previous | — |
| Predictors: 75 → 100 | `p75_cindex0.60_hazard1.00_censor0.50_target0.90_mcoxph` | 16,512 | `p100_cindex0.60_hazard1.00_censor0.50_target0.90_mcoxph` | 53,328 | 3.23× higher | — | next | — |
| Predictors: 75 → 100 | `p75_cindex0.60_hazard2.00_censor0.10_target0.95_mcoxph` | 177,664 | `p100_cindex0.60_hazard2.00_censor0.10_target0.95_mcoxph` | 29,632 | 6.00× lower | — | previous | — |
| Predictors: 75 → 100 | `p75_cindex0.75_hazard0.50_censor0.50_target0.95_mcoxph` | 6,592 | `p100_cindex0.75_hazard0.50_censor0.50_target0.95_mcoxph` | 21,336 | 3.24× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.60_hazard1.00_censor0.10_target0.90_mcoxph` | 7,417 | `p100_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 29,632 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.60_hazard1.00_censor0.30_target0.90_mcoxph` | 14,133 | `p100_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 48,487 | 3.43× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p100_cindex0.60_hazard1.00_censor0.50_target0.85_mcoxph` | 11,369 | `p100_cindex0.60_hazard1.00_censor0.50_target0.90_mcoxph` | 53,328 | 4.69× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.70_hazard1.00_censor0.10_target0.90_mcoxph` | 3,088 | `p100_cindex0.70_hazard1.00_censor0.10_target0.95_mcoxph` | 12,696 | 4.11× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.75_hazard0.50_censor0.50_target0.90_mcoxph` | 3,604 | `p100_cindex0.75_hazard0.50_censor0.50_target0.95_mcoxph` | 21,336 | 5.92× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.80_hazard2.00_censor0.30_target0.90_mcoxph` | 2,312 | `p100_cindex0.80_hazard2.00_censor0.30_target0.95_mcoxph` | 7,144 | 3.09× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard0.50_censor0.10_target0.90_mcoxph` | 1,926 | `p10_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 5,920 | 3.07× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard0.50_censor0.30_target0.90_mcoxph` | 2,170 | `p10_cindex0.60_hazard0.50_censor0.30_target0.95_mcoxph` | 7,616 | 3.51× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard0.50_censor0.50_target0.90_mcoxph` | 2,664 | `p10_cindex0.60_hazard0.50_censor0.50_target0.95_mcoxph` | 21,312 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard1.00_censor0.10_target0.90_mcoxph` | 1,657 | `p10_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 5,334 | 3.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard1.00_censor0.30_target0.90_mcoxph` | 1,904 | `p10_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 5,797 | 3.04× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard2.00_censor0.10_target0.90_mcoxph` | 1,445 | `p10_cindex0.60_hazard2.00_censor0.10_target0.95_mcoxph` | 5,620 | 3.89× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard2.00_censor0.50_target0.90_mcoxph` | 2,664 | `p10_cindex0.60_hazard2.00_censor0.50_target0.95_mcoxph` | 10,737 | 4.03× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.65_hazard0.50_censor0.30_target0.90_mcoxph` | 880 | `p10_cindex0.65_hazard0.50_censor0.30_target0.95_mcoxph` | 3,030 | 3.44× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.65_hazard1.00_censor0.10_target0.90_mcoxph` | 697 | `p10_cindex0.65_hazard1.00_censor0.10_target0.95_mcoxph` | 2,674 | 3.84× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.65_hazard1.00_censor0.30_target0.90_mcoxph` | 880 | `p10_cindex0.65_hazard1.00_censor0.30_target0.95_mcoxph` | 7,040 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.65_hazard2.00_censor0.10_target0.90_mcoxph` | 796 | `p10_cindex0.65_hazard2.00_censor0.10_target0.95_mcoxph` | 2,736 | 3.44× higher | — | — | — |
| Target slope: 0.85 → 0.95 | `p10_cindex0.65_hazard2.00_censor0.50_target0.85_mcoxph` | 604 | `p10_cindex0.65_hazard2.00_censor0.50_target0.95_mcoxph` | 4,928 | 8.16× higher | Yes | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.70_hazard0.50_censor0.30_target0.90_mcoxph` | 608 | `p10_cindex0.70_hazard0.50_censor0.30_target0.95_mcoxph` | 3,264 | 5.37× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.70_hazard0.50_censor0.50_target0.90_mcoxph` | 572 | `p10_cindex0.70_hazard0.50_censor0.50_target0.95_mcoxph` | 2,083 | 3.64× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.70_hazard1.00_censor0.10_target0.90_mcoxph` | 318 | `p10_cindex0.70_hazard1.00_censor0.10_target0.95_mcoxph` | 1,478 | 4.65× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.70_hazard1.00_censor0.50_target0.90_mcoxph` | 734 | `p10_cindex0.70_hazard1.00_censor0.50_target0.95_mcoxph` | 4,576 | 6.23× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p10_cindex0.70_hazard2.00_censor0.10_target0.85_mcoxph` | 163 | `p10_cindex0.70_hazard2.00_censor0.10_target0.90_mcoxph` | 516 | 3.17× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.75_hazard0.50_censor0.10_target0.90_mcoxph` | 278 | `p10_cindex0.75_hazard0.50_censor0.10_target0.95_mcoxph` | 893 | 3.21× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.75_hazard0.50_censor0.30_target0.90_mcoxph` | 420 | `p10_cindex0.75_hazard0.50_censor0.30_target0.95_mcoxph` | 1,528 | 3.64× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.75_hazard0.50_censor0.50_target0.90_mcoxph` | 495 | `p10_cindex0.75_hazard0.50_censor0.50_target0.95_mcoxph` | 1,513 | 3.06× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.75_hazard1.00_censor0.30_target0.90_mcoxph` | 382 | `p10_cindex0.75_hazard1.00_censor0.30_target0.95_mcoxph` | 1,528 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.75_hazard2.00_censor0.10_target0.90_mcoxph` | 374 | `p10_cindex0.75_hazard2.00_censor0.10_target0.95_mcoxph` | 1,184 | 3.17× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.75_hazard2.00_censor0.50_target0.90_mcoxph` | 539 | `p10_cindex0.75_hazard2.00_censor0.50_target0.95_mcoxph` | 4,272 | 7.93× higher | — | next | next |
| Target slope: 0.85 → 0.95 | `p10_cindex0.80_hazard2.00_censor0.30_target0.85_mcoxph` | 179 | `p10_cindex0.80_hazard2.00_censor0.30_target0.95_mcoxph` | 805 | 4.50× higher | Yes | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard2.00_censor0.30_target0.90_mcoxph` | 275 | `p10_cindex0.85_hazard2.00_censor0.30_target0.95_mcoxph` | 1,344 | 4.89× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.60_hazard0.50_censor0.10_target0.90_mcoxph` | 1,112 | `p15_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 4,388 | 3.95× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.60_hazard0.50_censor0.30_target0.90_mcoxph` | 3,140 | `p15_cindex0.60_hazard0.50_censor0.30_target0.95_mcoxph` | 10,969 | 3.49× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.60_hazard1.00_censor0.10_target0.90_mcoxph` | 2,356 | `p15_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 17,792 | 7.55× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p15_cindex0.60_hazard1.00_censor0.50_target0.90_mcoxph` | 3,906 | `p15_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 16,000 | 4.10× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.60_hazard2.00_censor0.10_target0.90_mcoxph` | 1,692 | `p15_cindex0.60_hazard2.00_censor0.10_target0.95_mcoxph` | 5,930 | 3.50× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p15_cindex0.60_hazard2.00_censor0.30_target0.85_mcoxph` | 1,640 | `p15_cindex0.60_hazard2.00_censor0.30_target0.90_mcoxph` | 5,712 | 3.48× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.65_hazard0.50_censor0.30_target0.90_mcoxph` | 1,336 | `p15_cindex0.65_hazard0.50_censor0.30_target0.95_mcoxph` | 5,264 | 3.94× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.65_hazard1.00_censor0.30_target0.90_mcoxph` | 1,277 | `p15_cindex0.65_hazard1.00_censor0.30_target0.95_mcoxph` | 5,264 | 4.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.65_hazard2.00_censor0.30_target0.90_mcoxph` | 1,424 | `p15_cindex0.65_hazard2.00_censor0.30_target0.95_mcoxph` | 5,264 | 3.70× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.70_hazard0.50_censor0.10_target0.90_mcoxph` | 522 | `p15_cindex0.70_hazard0.50_censor0.10_target0.95_mcoxph` | 1,740 | 3.33× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.70_hazard0.50_censor0.50_target0.90_mcoxph` | 955 | `p15_cindex0.70_hazard0.50_censor0.50_target0.95_mcoxph` | 6,864 | 7.19× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.70_hazard1.00_censor0.30_target0.90_mcoxph` | 722 | `p15_cindex0.70_hazard1.00_censor0.30_target0.95_mcoxph` | 2,295 | 3.18× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.70_hazard1.00_censor0.50_target0.90_mcoxph` | 1,003 | `p15_cindex0.70_hazard1.00_censor0.50_target0.95_mcoxph` | 3,432 | 3.42× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.70_hazard2.00_censor0.10_target0.90_mcoxph` | 664 | `p15_cindex0.70_hazard2.00_censor0.10_target0.95_mcoxph` | 3,824 | 5.76× higher | — | next | — |
| Target slope: 0.85 → 0.95 | `p15_cindex0.70_hazard2.00_censor0.50_target0.85_mcoxph` | 650 | `p15_cindex0.70_hazard2.00_censor0.50_target0.95_mcoxph` | 2,310 | 3.55× higher | Yes | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.75_hazard1.00_censor0.10_target0.90_mcoxph` | 425 | `p15_cindex0.75_hazard1.00_censor0.10_target0.95_mcoxph` | 1,306 | 3.07× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.75_hazard1.00_censor0.30_target0.90_mcoxph` | 574 | `p15_cindex0.75_hazard1.00_censor0.30_target0.95_mcoxph` | 2,280 | 3.97× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.75_hazard2.00_censor0.10_target0.90_mcoxph` | 446 | `p15_cindex0.75_hazard2.00_censor0.10_target0.95_mcoxph` | 3,568 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.75_hazard2.00_censor0.30_target0.90_mcoxph` | 600 | `p15_cindex0.75_hazard2.00_censor0.30_target0.95_mcoxph` | 2,280 | 3.80× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.80_hazard1.00_censor0.50_target0.90_mcoxph` | 553 | `p15_cindex0.80_hazard1.00_censor0.50_target0.95_mcoxph` | 3,000 | 5.42× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.80_hazard2.00_censor0.10_target0.90_mcoxph` | 411 | `p15_cindex0.80_hazard2.00_censor0.10_target0.95_mcoxph` | 1,672 | 4.07× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.60_hazard0.50_censor0.10_target0.90_mcoxph` | 2,960 | `p20_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 23,680 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.60_hazard1.00_censor0.30_target0.90_mcoxph` | 3,919 | `p20_cindex0.60_hazard1.00_censor0.30_target0.95_mcoxph` | 15,264 | 3.89× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.60_hazard1.00_censor0.50_target0.90_mcoxph` | 5,610 | `p20_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 17,020 | 3.03× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.60_hazard2.00_censor0.50_target0.90_mcoxph` | 5,436 | `p20_cindex0.60_hazard2.00_censor0.50_target0.95_mcoxph` | 21,344 | 3.93× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.65_hazard0.50_censor0.10_target0.90_mcoxph` | 1,117 | `p20_cindex0.65_hazard0.50_censor0.10_target0.95_mcoxph` | 3,624 | 3.24× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.65_hazard0.50_censor0.50_target0.90_mcoxph` | 2,239 | `p20_cindex0.65_hazard0.50_censor0.50_target0.95_mcoxph` | 9,840 | 4.39× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.70_hazard0.50_censor0.50_target0.90_mcoxph` | 1,142 | `p20_cindex0.70_hazard0.50_censor0.50_target0.95_mcoxph` | 3,484 | 3.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.70_hazard1.00_censor0.10_target0.90_mcoxph` | 638 | `p20_cindex0.70_hazard1.00_censor0.10_target0.95_mcoxph` | 2,340 | 3.67× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.70_hazard1.00_censor0.30_target0.90_mcoxph` | 974 | `p20_cindex0.70_hazard1.00_censor0.30_target0.95_mcoxph` | 3,272 | 3.36× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.70_hazard2.00_censor0.30_target0.90_mcoxph` | 1,038 | `p20_cindex0.70_hazard2.00_censor0.30_target0.95_mcoxph` | 3,272 | 3.15× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.80_hazard0.50_censor0.10_target0.90_mcoxph` | 470 | `p20_cindex0.80_hazard0.50_censor0.10_target0.95_mcoxph` | 2,224 | 4.73× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.85_hazard0.50_censor0.30_target0.90_mcoxph` | 318 | `p20_cindex0.85_hazard0.50_censor0.30_target0.95_mcoxph` | 1,174 | 3.69× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.85_hazard0.50_censor0.50_target0.90_mcoxph` | 621 | `p20_cindex0.85_hazard0.50_censor0.50_target0.95_mcoxph` | 1,884 | 3.03× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.65_hazard2.00_censor0.30_target0.90_mcoxph` | 2,056 | `p25_cindex0.65_hazard2.00_censor0.30_target0.95_mcoxph` | 8,784 | 4.27× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.70_hazard1.00_censor0.10_target0.90_mcoxph` | 794 | `p25_cindex0.70_hazard1.00_censor0.10_target0.95_mcoxph` | 2,831 | 3.57× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.75_hazard0.50_censor0.10_target0.90_mcoxph` | 766 | `p25_cindex0.75_hazard0.50_censor0.10_target0.95_mcoxph` | 2,968 | 3.87× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.75_hazard1.00_censor0.50_target0.90_mcoxph` | 1,334 | `p25_cindex0.75_hazard1.00_censor0.50_target0.95_mcoxph` | 5,336 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.85_hazard0.50_censor0.50_target0.90_mcoxph` | 747 | `p25_cindex0.85_hazard0.50_censor0.50_target0.95_mcoxph` | 2,352 | 3.15× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.60_hazard0.50_censor0.30_target0.90_mcoxph` | 5,180 | `p30_cindex0.60_hazard0.50_censor0.30_target0.95_mcoxph` | 16,451 | 3.18× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.60_hazard2.00_censor0.30_target0.90_mcoxph` | 5,720 | `p30_cindex0.60_hazard2.00_censor0.30_target0.95_mcoxph` | 22,880 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.60_hazard2.00_censor0.50_target0.90_mcoxph` | 7,212 | `p30_cindex0.60_hazard2.00_censor0.50_target0.95_mcoxph` | 32,000 | 4.44× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.65_hazard1.00_censor0.30_target0.90_mcoxph` | 2,316 | `p30_cindex0.65_hazard1.00_censor0.30_target0.95_mcoxph` | 10,560 | 4.56× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.70_hazard0.50_censor0.10_target0.90_mcoxph` | 926 | `p30_cindex0.70_hazard0.50_censor0.10_target0.95_mcoxph` | 3,123 | 3.37× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.70_hazard0.50_censor0.50_target0.90_mcoxph` | 1,714 | `p30_cindex0.70_hazard0.50_censor0.50_target0.95_mcoxph` | 6,856 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.70_hazard2.00_censor0.10_target0.90_mcoxph` | 1,160 | `p30_cindex0.70_hazard2.00_censor0.10_target0.95_mcoxph` | 7,616 | 6.57× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.80_hazard0.50_censor0.30_target0.90_mcoxph` | 775 | `p30_cindex0.80_hazard0.50_censor0.30_target0.95_mcoxph` | 4,288 | 5.53× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.80_hazard0.50_censor0.50_target0.90_mcoxph` | 964 | `p30_cindex0.80_hazard0.50_censor0.50_target0.95_mcoxph` | 3,000 | 3.11× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.80_hazard1.00_censor0.50_target0.90_mcoxph` | 669 | `p30_cindex0.80_hazard1.00_censor0.50_target0.95_mcoxph` | 2,516 | 3.76× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.85_hazard1.00_censor0.30_target0.90_mcoxph` | 704 | `p30_cindex0.85_hazard1.00_censor0.30_target0.95_mcoxph` | 4,040 | 5.74× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p40_cindex0.60_hazard2.00_censor0.10_target0.85_mcoxph` | 2,960 | `p40_cindex0.60_hazard2.00_censor0.10_target0.90_mcoxph` | 11,840 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.65_hazard0.50_censor0.10_target0.90_mcoxph` | 2,477 | `p40_cindex0.65_hazard0.50_censor0.10_target0.95_mcoxph` | 10,928 | 4.41× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.70_hazard0.50_censor0.10_target0.90_mcoxph` | 1,458 | `p40_cindex0.70_hazard0.50_censor0.10_target0.95_mcoxph` | 5,072 | 3.48× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.70_hazard0.50_censor0.30_target0.90_mcoxph` | 1,754 | `p40_cindex0.70_hazard0.50_censor0.30_target0.95_mcoxph` | 6,528 | 3.72× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.75_hazard0.50_censor0.10_target0.90_mcoxph` | 1,071 | `p40_cindex0.75_hazard0.50_censor0.10_target0.95_mcoxph` | 4,736 | 4.42× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.85_hazard1.00_censor0.30_target0.90_mcoxph` | 795 | `p40_cindex0.85_hazard1.00_censor0.30_target0.95_mcoxph` | 2,688 | 3.38× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.65_hazard0.50_censor0.30_target0.90_mcoxph` | 3,429 | `p50_cindex0.65_hazard0.50_censor0.30_target0.95_mcoxph` | 17,568 | 5.12× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.65_hazard2.00_censor0.10_target0.90_mcoxph` | 1,711 | `p50_cindex0.65_hazard2.00_censor0.10_target0.95_mcoxph` | 6,840 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.70_hazard2.00_censor0.50_target0.90_mcoxph` | 2,812 | `p50_cindex0.70_hazard2.00_censor0.50_target0.95_mcoxph` | 11,432 | 4.07× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.75_hazard0.50_censor0.10_target0.90_mcoxph` | 1,329 | `p50_cindex0.75_hazard0.50_censor0.10_target0.95_mcoxph` | 5,928 | 4.46× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.60_hazard0.50_censor0.10_target0.90_mcoxph` | 756 | `p5_cindex0.60_hazard0.50_censor0.10_target0.95_mcoxph` | 2,778 | 3.67× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.60_hazard1.00_censor0.10_target0.90_mcoxph` | 372 | `p5_cindex0.60_hazard1.00_censor0.10_target0.95_mcoxph` | 1,488 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.60_hazard1.00_censor0.50_target0.90_mcoxph` | 1,852 | `p5_cindex0.60_hazard1.00_censor0.50_target0.95_mcoxph` | 5,985 | 3.23× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.60_hazard2.00_censor0.10_target0.85_mcoxph` | 455 | `p5_cindex0.60_hazard2.00_censor0.10_target0.90_mcoxph` | 1,488 | 3.27× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.60_hazard2.00_censor0.30_target0.90_mcoxph` | 1,275 | `p5_cindex0.60_hazard2.00_censor0.30_target0.95_mcoxph` | 7,552 | 5.92× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.60_hazard2.00_censor0.50_target0.85_mcoxph` | 982 | `p5_cindex0.60_hazard2.00_censor0.50_target0.90_mcoxph` | 5,344 | 5.44× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard0.50_censor0.10_target0.90_mcoxph` | 429 | `p5_cindex0.65_hazard0.50_censor0.10_target0.95_mcoxph` | 1,376 | 3.21× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard0.50_censor0.30_target0.90_mcoxph` | 436 | `p5_cindex0.65_hazard0.50_censor0.30_target0.95_mcoxph` | 2,281 | 5.23× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard1.00_censor0.30_target0.90_mcoxph` | 564 | `p5_cindex0.65_hazard1.00_censor0.30_target0.95_mcoxph` | 1,948 | 3.45× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.65_hazard2.00_censor0.10_target0.85_mcoxph` | 172 | `p5_cindex0.65_hazard2.00_censor0.10_target0.90_mcoxph` | 688 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard2.00_censor0.50_target0.90_mcoxph` | 616 | `p5_cindex0.65_hazard2.00_censor0.50_target0.95_mcoxph` | 4,928 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.70_hazard0.50_censor0.30_target0.90_mcoxph` | 381 | `p5_cindex0.70_hazard0.50_censor0.30_target0.95_mcoxph` | 1,165 | 3.06× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.70_hazard0.50_censor0.50_target0.90_mcoxph` | 286 | `p5_cindex0.70_hazard0.50_censor0.50_target0.95_mcoxph` | 1,119 | 3.91× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.70_hazard1.00_censor0.30_target0.90_mcoxph` | 404 | `p5_cindex0.70_hazard1.00_censor0.30_target0.95_mcoxph` | 1,616 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.70_hazard2.00_censor0.10_target0.90_mcoxph` | 320 | `p5_cindex0.70_hazard2.00_censor0.10_target0.95_mcoxph` | 2,560 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.70_hazard2.00_censor0.30_target0.90_mcoxph` | 404 | `p5_cindex0.70_hazard2.00_censor0.30_target0.95_mcoxph` | 3,232 | 8.00× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p5_cindex0.75_hazard0.50_censor0.10_target0.90_mcoxph` | 258 | `p5_cindex0.75_hazard0.50_censor0.10_target0.95_mcoxph` | 1,200 | 4.65× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.75_hazard1.00_censor0.10_target0.90_mcoxph` | 150 | `p5_cindex0.75_hazard1.00_censor0.10_target0.95_mcoxph` | 1,200 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.75_hazard1.00_censor0.30_target0.90_mcoxph` | 190 | `p5_cindex0.75_hazard1.00_censor0.30_target0.95_mcoxph` | 760 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.75_hazard1.00_censor0.50_target0.90_mcoxph` | 333 | `p5_cindex0.75_hazard1.00_censor0.50_target0.95_mcoxph` | 1,064 | 3.20× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.80_hazard0.50_censor0.10_target0.90_mcoxph` | 150 | `p5_cindex0.80_hazard0.50_censor0.10_target0.95_mcoxph` | 561 | 3.74× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.80_hazard0.50_censor0.50_target0.85_mcoxph` | 94 | `p5_cindex0.80_hazard0.50_censor0.50_target0.90_mcoxph` | 500 | 5.32× higher | — | next | — |
| Target slope: 0.85 → 0.95 | `p5_cindex0.80_hazard1.00_censor0.30_target0.85_mcoxph` | 115 | `p5_cindex0.80_hazard1.00_censor0.30_target0.95_mcoxph` | 356 | 3.10× higher | Yes | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.80_hazard1.00_censor0.50_target0.90_mcoxph` | 257 | `p5_cindex0.80_hazard1.00_censor0.50_target0.95_mcoxph` | 772 | 3.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.80_hazard2.00_censor0.10_target0.90_mcoxph` | 179 | `p5_cindex0.80_hazard2.00_censor0.10_target0.95_mcoxph` | 560 | 3.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.80_hazard2.00_censor0.30_target0.90_mcoxph` | 198 | `p5_cindex0.80_hazard2.00_censor0.30_target0.95_mcoxph` | 630 | 3.18× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.85_hazard0.50_censor0.30_target0.90_mcoxph` | 183 | `p5_cindex0.85_hazard0.50_censor0.30_target0.95_mcoxph` | 672 | 3.67× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.85_hazard0.50_censor0.50_target0.90_mcoxph` | 118 | `p5_cindex0.85_hazard0.50_censor0.50_target0.95_mcoxph` | 944 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.85_hazard1.00_censor0.10_target0.90_mcoxph` | 132 | `p5_cindex0.85_hazard1.00_censor0.10_target0.95_mcoxph` | 409 | 3.10× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.85_hazard1.00_censor0.30_target0.90_mcoxph` | 168 | `p5_cindex0.85_hazard1.00_censor0.30_target0.95_mcoxph` | 504 | 3.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.85_hazard1.00_censor0.50_target0.90_mcoxph` | 230 | `p5_cindex0.85_hazard1.00_censor0.50_target0.95_mcoxph` | 710 | 3.09× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.85_hazard2.00_censor0.30_target0.90_mcoxph` | 176 | `p5_cindex0.85_hazard2.00_censor0.30_target0.95_mcoxph` | 672 | 3.82× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.60_hazard0.50_censor0.30_target0.90_mcoxph` | 7,140 | `p75_cindex0.60_hazard0.50_censor0.30_target0.95_mcoxph` | 28,560 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.70_hazard2.00_censor0.30_target0.90_mcoxph` | 3,084 | `p75_cindex0.70_hazard2.00_censor0.30_target0.95_mcoxph` | 12,240 | 3.97× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.85_hazard0.50_censor0.50_target0.90_mcoxph` | 1,765 | `p75_cindex0.85_hazard0.50_censor0.50_target0.95_mcoxph` | 7,060 | 4.00× higher | — | — | — |
| Baseline hazard: 1 → 2 | `p10_cindex0.65_hazard1.00_censor0.50_target0.95_mlasso` | 9,856 | `p10_cindex0.65_hazard2.00_censor0.50_target0.95_mlasso` | 2,464 | 4.00× lower | — | previous | previous |
| Baseline hazard: 0.5 → 1 | `p15_cindex0.65_hazard0.50_censor0.10_target0.80_mlasso` | 64 | `p15_cindex0.65_hazard1.00_censor0.10_target0.80_mlasso` | 229 | 3.58× higher | — | previous | previous |
| Baseline hazard: 0.5 → 1 | `p20_cindex0.60_hazard0.50_censor0.10_target0.95_mlasso` | 4,644 | `p20_cindex0.60_hazard1.00_censor0.10_target0.95_mlasso` | 23,680 | 5.10× higher | — | next | next |
| Baseline hazard: 1 → 2 | `p20_cindex0.60_hazard1.00_censor0.10_target0.95_mlasso` | 23,680 | `p20_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 6,110 | 3.88× lower | — | previous, next | previous |
| Baseline hazard: 0.5 → 1 | `p25_cindex0.60_hazard0.50_censor0.10_target0.95_mlasso` | 14,816 | `p25_cindex0.60_hazard1.00_censor0.10_target0.95_mlasso` | 4,815 | 3.08× lower | — | previous | — |
| Baseline hazard: 0.5 → 1 | `p25_cindex0.85_hazard0.50_censor0.10_target0.85_mlasso` | 40 | `p25_cindex0.85_hazard1.00_censor0.10_target0.85_mlasso` | 128 | 3.20× higher | — | previous | previous |
| Baseline hazard: 1 → 2 | `p50_cindex0.65_hazard1.00_censor0.10_target0.80_mlasso` | 213 | `p50_cindex0.65_hazard2.00_censor0.10_target0.80_mlasso` | 855 | 4.01× higher | — | — | — |
| Baseline hazard: 0.5 → 1 | `p5_cindex0.60_hazard0.50_censor0.50_target0.90_mlasso` | 2,672 | `p5_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | 10,688 | 4.00× higher | — | next | next |
| Baseline hazard: 1 → 2 | `p5_cindex0.60_hazard1.00_censor0.30_target0.95_mlasso` | 3,776 | `p5_cindex0.60_hazard2.00_censor0.30_target0.95_mlasso` | 16,983 | 4.50× higher | — | next | — |
| Baseline hazard: 1 → 2 | `p5_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | 10,688 | `p5_cindex0.60_hazard2.00_censor0.50_target0.90_mlasso` | 1,682 | 6.35× lower | — | previous | previous |
| Baseline hazard: 0.5 → 1 | `p5_cindex0.70_hazard0.50_censor0.50_target0.95_mlasso` | 2,156 | `p5_cindex0.70_hazard1.00_censor0.50_target0.95_mlasso` | 9,152 | 4.24× higher | — | next | next |
| Baseline hazard: 1 → 2 | `p5_cindex0.85_hazard1.00_censor0.30_target0.95_mlasso` | 431 | `p5_cindex0.85_hazard2.00_censor0.30_target0.95_mlasso` | 1,451 | 3.37× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p10_cindex0.65_hazard1.00_censor0.30_target0.95_mlasso` | 2,433 | `p10_cindex0.65_hazard1.00_censor0.50_target0.95_mlasso` | 9,856 | 4.05× higher | — | previous, next | next |
| Censoring: 0.1 → 0.3 | `p15_cindex0.65_hazard0.50_censor0.10_target0.80_mlasso` | 64 | `p15_cindex0.65_hazard0.50_censor0.30_target0.80_mlasso` | 329 | 5.14× higher | — | previous, next | previous |
| Censoring: 0.3 → 0.5 | `p20_cindex0.65_hazard0.50_censor0.30_target0.95_mlasso` | 1,610 | `p20_cindex0.65_hazard0.50_censor0.50_target0.95_mlasso` | 4,920 | 3.06× higher | — | — | — |
| Censoring: 0.1 → 0.3 | `p25_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 2,967 | `p25_cindex0.60_hazard2.00_censor0.30_target0.95_mlasso` | 9,520 | 3.21× higher | — | — | — |
| Censoring: 0.1 → 0.3 | `p25_cindex0.85_hazard0.50_censor0.10_target0.85_mlasso` | 40 | `p25_cindex0.85_hazard0.50_censor0.30_target0.85_mlasso` | 142 | 3.55× higher | — | previous | previous |
| Censoring: 0.3 → 0.5 | `p40_cindex0.70_hazard2.00_censor0.30_target0.90_mlasso` | 1,632 | `p40_cindex0.70_hazard2.00_censor0.50_target0.90_mlasso` | 507 | 3.22× lower | — | — | — |
| Censoring: 0.1 → 0.3 | `p5_cindex0.60_hazard0.50_censor0.10_target0.95_mlasso` | 2,373 | `p5_cindex0.60_hazard0.50_censor0.30_target0.95_mlasso` | 7,441 | 3.14× higher | — | — | — |
| Censoring: 0.3 → 0.5 | `p5_cindex0.60_hazard1.00_censor0.30_target0.90_mlasso` | 1,545 | `p5_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | 10,688 | 6.92× higher | — | next | next |
| Censoring: 0.1 → 0.3 | `p5_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 2,976 | `p5_cindex0.60_hazard2.00_censor0.30_target0.95_mlasso` | 16,983 | 5.71× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p5_cindex0.70_hazard1.00_censor0.30_target0.95_mlasso` | 1,616 | `p5_cindex0.70_hazard1.00_censor0.50_target0.95_mlasso` | 9,152 | 5.66× higher | — | next | next |
| Censoring: 0.3 → 0.5 | `p5_cindex0.75_hazard0.50_censor0.30_target0.95_mlasso` | 760 | `p5_cindex0.75_hazard0.50_censor0.50_target0.95_mlasso` | 2,535 | 3.34× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p5_cindex0.85_hazard1.00_censor0.30_target0.95_mlasso` | 431 | `p5_cindex0.85_hazard1.00_censor0.50_target0.95_mlasso` | 2,122 | 4.92× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p75_cindex0.70_hazard0.50_censor0.30_target0.95_mlasso` | 2,313 | `p75_cindex0.70_hazard0.50_censor0.50_target0.95_mlasso` | 8,572 | 3.71× higher | — | next | — |
| C-index: 0.6 → 0.65 | `p100_cindex0.60_hazard2.00_censor0.30_target0.80_mlasso` | 2,382 | `p100_cindex0.65_hazard2.00_censor0.30_target0.80_mlasso` | 763 | 3.12× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p100_cindex0.60_hazard2.00_censor0.30_target0.85_mlasso` | 4,764 | `p100_cindex0.65_hazard2.00_censor0.30_target0.85_mlasso` | 1,099 | 4.33× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p100_cindex0.60_hazard2.00_censor0.50_target0.80_mlasso` | 3,333 | `p100_cindex0.65_hazard2.00_censor0.50_target0.80_mlasso` | 990 | 3.37× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p100_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 26,664 | `p100_cindex0.65_hazard2.00_censor0.50_target0.95_mlasso` | 5,199 | 5.13× lower | — | previous | — |
| C-index: 0.65 → 0.7 | `p100_cindex0.65_hazard0.50_censor0.50_target0.95_mlasso` | 12,308 | `p100_cindex0.70_hazard0.50_censor0.50_target0.95_mlasso` | 3,367 | 3.66× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p100_cindex0.65_hazard1.00_censor0.50_target0.90_mlasso` | 2,808 | `p100_cindex0.70_hazard1.00_censor0.50_target0.90_mlasso` | 715 | 3.93× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p100_cindex0.65_hazard1.00_censor0.50_target0.95_mlasso` | 12,308 | `p100_cindex0.70_hazard1.00_censor0.50_target0.95_mlasso` | 2,857 | 4.31× lower | — | — | — |
| C-index: 0.7 → 0.75 | `p100_cindex0.70_hazard1.00_censor0.50_target0.80_mlasso` | 742 | `p100_cindex0.75_hazard1.00_censor0.50_target0.80_mlasso` | 166 | 4.47× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p10_cindex0.60_hazard0.50_censor0.10_target0.95_mlasso` | 7,642 | `p10_cindex0.65_hazard0.50_censor0.10_target0.95_mlasso` | 1,368 | 5.59× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p10_cindex0.60_hazard0.50_censor0.30_target0.95_mlasso` | 7,865 | `p10_cindex0.65_hazard0.50_censor0.30_target0.95_mlasso` | 2,494 | 3.15× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p10_cindex0.60_hazard0.50_censor0.50_target0.90_mlasso` | 2,729 | `p10_cindex0.65_hazard0.50_censor0.50_target0.90_mlasso` | 616 | 4.43× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p10_cindex0.60_hazard1.00_censor0.30_target0.90_mlasso` | 1,813 | `p10_cindex0.65_hazard1.00_censor0.30_target0.90_mlasso` | 440 | 4.12× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p10_cindex0.60_hazard1.00_censor0.30_target0.95_mlasso` | 15,232 | `p10_cindex0.65_hazard1.00_censor0.30_target0.95_mlasso` | 2,433 | 6.26× lower | — | previous, next | previous |
| C-index: 0.6 → 0.65 | `p10_cindex0.60_hazard2.00_censor0.30_target0.95_mlasso` | 7,616 | `p10_cindex0.65_hazard2.00_censor0.30_target0.95_mlasso` | 1,760 | 4.33× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p10_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 9,470 | `p10_cindex0.65_hazard2.00_censor0.50_target0.95_mlasso` | 2,464 | 3.84× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p10_cindex0.65_hazard1.00_censor0.50_target0.95_mlasso` | 9,856 | `p10_cindex0.70_hazard1.00_censor0.50_target0.95_mlasso` | 1,866 | 5.28× lower | — | previous | previous |
| C-index: 0.7 → 0.75 | `p10_cindex0.70_hazard2.00_censor0.30_target0.90_mlasso` | 816 | `p10_cindex0.75_hazard2.00_censor0.30_target0.90_mlasso` | 180 | 4.53× lower | — | — | — |
| C-index: 0.7 → 0.75 | `p10_cindex0.70_hazard2.00_censor0.50_target0.80_mlasso` | 228 | `p10_cindex0.75_hazard2.00_censor0.50_target0.80_mlasso` | 66 | 3.45× lower | — | — | — |
| C-index: 0.75 → 0.8 | `p10_cindex0.75_hazard0.50_censor0.10_target0.95_mlasso` | 2,368 | `p10_cindex0.80_hazard0.50_censor0.10_target0.95_mlasso` | 530 | 4.47× lower | — | previous | previous |
| C-index: 0.75 → 0.8 | `p10_cindex0.75_hazard1.00_censor0.50_target0.95_mlasso` | 1,492 | `p10_cindex0.80_hazard1.00_censor0.50_target0.95_mlasso` | 428 | 3.49× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p15_cindex0.60_hazard0.50_censor0.10_target0.80_mlasso` | 520 | `p15_cindex0.65_hazard0.50_censor0.10_target0.80_mlasso` | 64 | 8.12× lower | — | previous, next | next |
| C-index: 0.6 → 0.65 | `p15_cindex0.60_hazard1.00_censor0.10_target0.95_mlasso` | 8,896 | `p15_cindex0.65_hazard1.00_censor0.10_target0.95_mlasso` | 2,342 | 3.80× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p15_cindex0.60_hazard2.00_censor0.10_target0.85_mlasso` | 1,112 | `p15_cindex0.65_hazard2.00_censor0.10_target0.85_mlasso` | 325 | 3.42× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p15_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 8,896 | `p15_cindex0.65_hazard2.00_censor0.10_target0.95_mlasso` | 2,202 | 4.04× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p15_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 14,884 | `p15_cindex0.65_hazard2.00_censor0.50_target0.95_mlasso` | 3,666 | 4.06× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p15_cindex0.65_hazard0.50_censor0.10_target0.90_mlasso` | 789 | `p15_cindex0.70_hazard0.50_censor0.10_target0.90_mlasso` | 239 | 3.30× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard0.50_censor0.30_target0.95_mlasso` | 7,632 | `p20_cindex0.65_hazard0.50_censor0.30_target0.95_mlasso` | 1,610 | 4.74× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard1.00_censor0.10_target0.90_mlasso` | 2,960 | `p20_cindex0.65_hazard1.00_censor0.10_target0.90_mlasso` | 770 | 3.84× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard1.00_censor0.10_target0.95_mlasso` | 23,680 | `p20_cindex0.65_hazard1.00_censor0.10_target0.95_mlasso` | 2,436 | 9.72× lower | — | previous | previous |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | 2,819 | `p20_cindex0.65_hazard1.00_censor0.50_target0.90_mlasso` | 615 | 4.58× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard2.00_censor0.10_target0.80_mlasso` | 740 | `p20_cindex0.65_hazard2.00_censor0.10_target0.80_mlasso` | 245 | 3.02× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard2.00_censor0.30_target0.80_mlasso` | 693 | `p20_cindex0.65_hazard2.00_censor0.30_target0.80_mlasso` | 224 | 3.09× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard2.00_censor0.50_target0.90_mlasso` | 3,560 | `p20_cindex0.65_hazard2.00_censor0.50_target0.90_mlasso` | 1,179 | 3.02× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p20_cindex0.65_hazard0.50_censor0.50_target0.85_mlasso` | 1,230 | `p20_cindex0.70_hazard0.50_censor0.50_target0.85_mlasso` | 363 | 3.39× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p25_cindex0.60_hazard0.50_censor0.10_target0.95_mlasso` | 14,816 | `p25_cindex0.65_hazard0.50_censor0.10_target0.95_mlasso` | 2,544 | 5.82× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p25_cindex0.60_hazard0.50_censor0.50_target0.80_mlasso` | 1,073 | `p25_cindex0.65_hazard0.50_censor0.50_target0.80_mlasso` | 192 | 5.59× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p25_cindex0.60_hazard1.00_censor0.50_target0.80_mlasso` | 1,666 | `p25_cindex0.65_hazard1.00_censor0.50_target0.80_mlasso` | 429 | 3.88× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p25_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 13,328 | `p25_cindex0.65_hazard2.00_censor0.50_target0.95_mlasso` | 4,414 | 3.02× lower | — | — | — |
| C-index: 0.8 → 0.85 | `p25_cindex0.80_hazard0.50_censor0.10_target0.85_mlasso` | 174 | `p25_cindex0.85_hazard0.50_censor0.10_target0.85_mlasso` | 40 | 4.35× lower | — | next | next |
| C-index: 0.65 → 0.7 | `p30_cindex0.65_hazard2.00_censor0.10_target0.95_mlasso` | 4,096 | `p30_cindex0.70_hazard2.00_censor0.10_target0.95_mlasso` | 939 | 4.36× lower | — | — | — |
| C-index: 0.8 → 0.85 | `p30_cindex0.80_hazard2.00_censor0.50_target0.90_mlasso` | 750 | `p30_cindex0.85_hazard2.00_censor0.50_target0.90_mlasso` | 176 | 4.26× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p40_cindex0.60_hazard0.50_censor0.50_target0.95_mlasso` | 21,328 | `p40_cindex0.65_hazard0.50_censor0.50_target0.95_mlasso` | 5,413 | 3.94× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p40_cindex0.60_hazard1.00_censor0.30_target0.85_mlasso` | 1,530 | `p40_cindex0.65_hazard1.00_censor0.30_target0.85_mlasso` | 439 | 3.49× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p40_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 8,205 | `p40_cindex0.65_hazard2.00_censor0.10_target0.95_mlasso` | 2,687 | 3.05× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p40_cindex0.60_hazard2.00_censor0.50_target0.90_mlasso` | 5,332 | `p40_cindex0.65_hazard2.00_censor0.50_target0.90_mlasso` | 1,637 | 3.26× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p40_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 21,328 | `p40_cindex0.65_hazard2.00_censor0.50_target0.95_mlasso` | 6,712 | 3.18× lower | — | previous | — |
| C-index: 0.65 → 0.7 | `p40_cindex0.65_hazard1.00_censor0.10_target0.80_mlasso` | 445 | `p40_cindex0.70_hazard1.00_censor0.10_target0.80_mlasso` | 147 | 3.03× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p40_cindex0.65_hazard2.00_censor0.50_target0.90_mlasso` | 1,637 | `p40_cindex0.70_hazard2.00_censor0.50_target0.90_mlasso` | 507 | 3.23× lower | — | — | — |
| C-index: 0.7 → 0.75 | `p40_cindex0.70_hazard2.00_censor0.30_target0.90_mlasso` | 1,632 | `p40_cindex0.75_hazard2.00_censor0.30_target0.90_mlasso` | 423 | 3.86× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p50_cindex0.60_hazard0.50_censor0.50_target0.85_mlasso` | 2,707 | `p50_cindex0.65_hazard0.50_censor0.50_target0.85_mlasso` | 770 | 3.52× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p50_cindex0.60_hazard1.00_censor0.10_target0.80_mlasso` | 928 | `p50_cindex0.65_hazard1.00_censor0.10_target0.80_mlasso` | 213 | 4.36× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p50_cindex0.60_hazard1.00_censor0.30_target0.90_mlasso` | 4,760 | `p50_cindex0.65_hazard1.00_censor0.30_target0.90_mlasso` | 1,290 | 3.69× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p50_cindex0.65_hazard2.00_censor0.10_target0.80_mlasso` | 855 | `p50_cindex0.70_hazard2.00_censor0.10_target0.80_mlasso` | 274 | 3.12× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p5_cindex0.60_hazard0.50_censor0.50_target0.85_mlasso` | 1,036 | `p5_cindex0.65_hazard0.50_censor0.50_target0.85_mlasso` | 308 | 3.36× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p5_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | 10,688 | `p5_cindex0.65_hazard1.00_censor0.50_target0.90_mlasso` | 1,152 | 9.28× lower | — | previous | previous |
| C-index: 0.6 → 0.65 | `p5_cindex0.60_hazard2.00_censor0.30_target0.95_mlasso` | 16,983 | `p5_cindex0.65_hazard2.00_censor0.30_target0.95_mlasso` | 1,744 | 9.74× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p5_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 16,139 | `p5_cindex0.65_hazard2.00_censor0.50_target0.95_mlasso` | 3,773 | 4.28× lower | — | previous | — |
| C-index: 0.65 → 0.7 | `p5_cindex0.65_hazard0.50_censor0.10_target0.95_mlasso` | 5,504 | `p5_cindex0.70_hazard0.50_censor0.10_target0.95_mlasso` | 1,280 | 4.30× lower | — | previous | — |
| C-index: 0.7 → 0.75 | `p5_cindex0.70_hazard0.50_censor0.30_target0.95_mlasso` | 3,350 | `p5_cindex0.75_hazard0.50_censor0.30_target0.95_mlasso` | 760 | 4.41× lower | — | previous | — |
| C-index: 0.7 → 0.75 | `p5_cindex0.70_hazard1.00_censor0.10_target0.95_mlasso` | 1,993 | `p5_cindex0.75_hazard1.00_censor0.10_target0.95_mlasso` | 600 | 3.32× lower | — | previous | — |
| C-index: 0.7 → 0.75 | `p5_cindex0.70_hazard1.00_censor0.50_target0.95_mlasso` | 9,152 | `p5_cindex0.75_hazard1.00_censor0.50_target0.95_mlasso` | 2,128 | 4.30× lower | — | previous, next | previous |
| C-index: 0.8 → 0.85 | `p5_cindex0.80_hazard1.00_censor0.30_target0.95_mlasso` | 1,424 | `p5_cindex0.85_hazard1.00_censor0.30_target0.95_mlasso` | 431 | 3.30× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p75_cindex0.60_hazard0.50_censor0.30_target0.85_mlasso` | 2,209 | `p75_cindex0.65_hazard0.50_censor0.30_target0.85_mlasso` | 412 | 5.36× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p75_cindex0.60_hazard1.00_censor0.10_target0.95_mlasso` | 8,298 | `p75_cindex0.65_hazard1.00_censor0.10_target0.95_mlasso` | 2,564 | 3.24× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p75_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 22,208 | `p75_cindex0.65_hazard2.00_censor0.10_target0.95_mlasso` | 3,938 | 5.64× lower | — | previous | previous |
| C-index: 0.6 → 0.65 | `p75_cindex0.60_hazard2.00_censor0.50_target0.85_mlasso` | 5,000 | `p75_cindex0.65_hazard2.00_censor0.50_target0.85_mlasso` | 1,444 | 3.46× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p75_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 22,216 | `p75_cindex0.65_hazard2.00_censor0.50_target0.95_mlasso` | 6,349 | 3.50× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p75_cindex0.65_hazard1.00_censor0.30_target0.90_mlasso` | 1,648 | `p75_cindex0.70_hazard1.00_censor0.30_target0.90_mlasso` | 519 | 3.18× lower | — | — | — |
| C-index: 0.7 → 0.75 | `p75_cindex0.70_hazard0.50_censor0.50_target0.95_mlasso` | 8,572 | `p75_cindex0.75_hazard0.50_censor0.50_target0.95_mlasso` | 2,000 | 4.29× lower | — | previous | — |
| Predictors: 10 → 15 | `p10_cindex0.60_hazard1.00_censor0.30_target0.95_mlasso` | 15,232 | `p15_cindex0.60_hazard1.00_censor0.30_target0.95_mlasso` | 4,700 | 3.24× lower | — | previous | previous |
| Predictors: 10 → 15 | `p10_cindex0.65_hazard0.50_censor0.10_target0.80_mlasso` | 207 | `p15_cindex0.65_hazard0.50_censor0.10_target0.80_mlasso` | 64 | 3.23× lower | — | next | next |
| Predictors: 15 → 20 | `p15_cindex0.65_hazard0.50_censor0.10_target0.80_mlasso` | 64 | `p20_cindex0.65_hazard0.50_censor0.10_target0.80_mlasso` | 275 | 4.30× higher | — | previous | previous |
| Predictors: 15 → 20 | `p15_cindex0.85_hazard0.50_censor0.50_target0.95_mlasso` | 522 | `p20_cindex0.85_hazard0.50_censor0.50_target0.95_mlasso` | 1,884 | 3.61× higher | — | next | — |
| Predictors: 20 → 25 | `p20_cindex0.60_hazard0.50_censor0.10_target0.95_mlasso` | 4,644 | `p25_cindex0.60_hazard0.50_censor0.10_target0.95_mlasso` | 14,816 | 3.19× higher | — | next | — |
| Predictors: 20 → 25 | `p20_cindex0.60_hazard1.00_censor0.10_target0.95_mlasso` | 23,680 | `p25_cindex0.60_hazard1.00_censor0.10_target0.95_mlasso` | 4,815 | 4.92× lower | — | previous | previous |
| Predictors: 20 → 25 | `p20_cindex0.85_hazard0.50_censor0.10_target0.85_mlasso` | 124 | `p25_cindex0.85_hazard0.50_censor0.10_target0.85_mlasso` | 40 | 3.10× lower | — | next | next |
| Predictors: 25 → 30 | `p25_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 13,328 | `p30_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 4,000 | 3.33× lower | — | — | — |
| Predictors: 25 → 30 | `p25_cindex0.85_hazard0.50_censor0.10_target0.85_mlasso` | 40 | `p30_cindex0.85_hazard0.50_censor0.10_target0.85_mlasso` | 139 | 3.48× higher | — | previous | previous |
| Predictors: 30 → 40 | `p30_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 4,000 | `p40_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 21,328 | 5.33× higher | — | next | — |
| Predictors: 50 → 75 | `p50_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 6,987 | `p75_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 22,208 | 3.18× higher | — | next | next |
| Predictors: 50 → 75 | `p50_cindex0.65_hazard1.00_censor0.10_target0.80_mlasso` | 213 | `p75_cindex0.65_hazard1.00_censor0.10_target0.80_mlasso` | 642 | 3.01× higher | — | — | — |
| Predictors: 5 → 10 | `p5_cindex0.60_hazard0.50_censor0.10_target0.95_mlasso` | 2,373 | `p10_cindex0.60_hazard0.50_censor0.10_target0.95_mlasso` | 7,642 | 3.22× higher | — | next | — |
| Predictors: 5 → 10 | `p5_cindex0.60_hazard1.00_censor0.30_target0.95_mlasso` | 3,776 | `p10_cindex0.60_hazard1.00_censor0.30_target0.95_mlasso` | 15,232 | 4.03× higher | — | next | next |
| Predictors: 5 → 10 | `p5_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | 10,688 | `p10_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | 2,664 | 4.01× lower | — | previous | previous |
| Predictors: 5 → 10 | `p5_cindex0.65_hazard0.50_censor0.10_target0.95_mlasso` | 5,504 | `p10_cindex0.65_hazard0.50_censor0.10_target0.95_mlasso` | 1,368 | 4.02× lower | — | previous | — |
| Predictors: 5 → 10 | `p5_cindex0.70_hazard1.00_censor0.50_target0.95_mlasso` | 9,152 | `p10_cindex0.70_hazard1.00_censor0.50_target0.95_mlasso` | 1,866 | 4.90× lower | — | previous | previous |
| Predictors: 5 → 10 | `p5_cindex0.75_hazard0.50_censor0.10_target0.95_mlasso` | 600 | `p10_cindex0.75_hazard0.50_censor0.10_target0.95_mlasso` | 2,368 | 3.95× higher | — | next | next |
| Predictors: 5 → 10 | `p5_cindex0.80_hazard1.00_censor0.50_target0.95_mlasso` | 1,433 | `p10_cindex0.80_hazard1.00_censor0.50_target0.95_mlasso` | 428 | 3.35× lower | — | previous | — |
| Predictors: 75 → 100 | `p75_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 22,208 | `p100_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 5,425 | 4.09× lower | — | previous | previous |
| Predictors: 75 → 100 | `p75_cindex0.65_hazard0.50_censor0.30_target0.85_mlasso` | 412 | `p100_cindex0.65_hazard0.50_censor0.30_target0.85_mlasso` | 1,357 | 3.29× higher | — | — | — |
| Predictors: 75 → 100 | `p75_cindex0.75_hazard1.00_censor0.50_target0.80_mlasso` | 500 | `p100_cindex0.75_hazard1.00_censor0.50_target0.80_mlasso` | 166 | 3.01× lower | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.60_hazard0.50_censor0.50_target0.90_mlasso` | 6,710 | `p100_cindex0.60_hazard0.50_censor0.50_target0.95_mlasso` | 26,664 | 3.97× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | 6,723 | `p100_cindex0.60_hazard1.00_censor0.50_target0.95_mlasso` | 26,664 | 3.97× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.60_hazard2.00_censor0.50_target0.90_mlasso` | 6,666 | `p100_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 26,664 | 4.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.65_hazard0.50_censor0.50_target0.90_mlasso` | 2,490 | `p100_cindex0.65_hazard0.50_censor0.50_target0.95_mlasso` | 12,308 | 4.94× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.65_hazard1.00_censor0.50_target0.90_mlasso` | 2,808 | `p100_cindex0.65_hazard1.00_censor0.50_target0.95_mlasso` | 12,308 | 4.38× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.65_hazard2.00_censor0.10_target0.90_mlasso` | 1,709 | `p100_cindex0.65_hazard2.00_censor0.10_target0.95_mlasso` | 6,836 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.70_hazard0.50_censor0.10_target0.90_mlasso` | 913 | `p100_cindex0.70_hazard0.50_censor0.10_target0.95_mlasso` | 3,310 | 3.63× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.70_hazard1.00_censor0.50_target0.90_mlasso` | 715 | `p100_cindex0.70_hazard1.00_censor0.50_target0.95_mlasso` | 2,857 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.70_hazard2.00_censor0.30_target0.90_mlasso` | 1,085 | `p100_cindex0.70_hazard2.00_censor0.30_target0.95_mlasso` | 4,180 | 3.85× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.75_hazard0.50_censor0.10_target0.90_mlasso` | 740 | `p100_cindex0.75_hazard0.50_censor0.10_target0.95_mlasso` | 3,601 | 4.87× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.75_hazard0.50_censor0.50_target0.90_mlasso` | 871 | `p100_cindex0.75_hazard0.50_censor0.50_target0.95_mlasso` | 5,334 | 6.12× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.75_hazard1.00_censor0.10_target0.90_mlasso` | 740 | `p100_cindex0.75_hazard1.00_censor0.10_target0.95_mlasso` | 2,962 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.75_hazard1.00_censor0.30_target0.90_mlasso` | 695 | `p100_cindex0.75_hazard1.00_censor0.30_target0.95_mlasso` | 3,810 | 5.48× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p100_cindex0.75_hazard1.00_censor0.50_target0.80_mlasso` | 166 | `p100_cindex0.75_hazard1.00_censor0.50_target0.85_mlasso` | 529 | 3.19× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.75_hazard2.00_censor0.30_target0.90_mlasso` | 754 | `p100_cindex0.75_hazard2.00_censor0.30_target0.95_mlasso` | 3,810 | 5.05× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.75_hazard2.00_censor0.50_target0.90_mlasso` | 695 | `p100_cindex0.75_hazard2.00_censor0.50_target0.95_mlasso` | 2,667 | 3.84× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.80_hazard0.50_censor0.10_target0.90_mlasso` | 389 | `p100_cindex0.80_hazard0.50_censor0.10_target0.95_mlasso` | 2,907 | 7.47× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.80_hazard0.50_censor0.50_target0.90_mlasso` | 599 | `p100_cindex0.80_hazard0.50_censor0.50_target0.95_mlasso` | 4,478 | 7.48× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.80_hazard1.00_censor0.10_target0.90_mlasso` | 441 | `p100_cindex0.80_hazard1.00_censor0.10_target0.95_mlasso` | 2,973 | 6.74× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.80_hazard1.00_censor0.30_target0.90_mlasso` | 514 | `p100_cindex0.80_hazard1.00_censor0.30_target0.95_mlasso` | 3,572 | 6.95× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.80_hazard1.00_censor0.50_target0.90_mlasso` | 604 | `p100_cindex0.80_hazard1.00_censor0.50_target0.95_mlasso` | 4,285 | 7.09× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.80_hazard2.00_censor0.10_target0.90_mlasso` | 694 | `p100_cindex0.80_hazard2.00_censor0.10_target0.95_mlasso` | 2,778 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.80_hazard2.00_censor0.30_target0.90_mlasso` | 521 | `p100_cindex0.80_hazard2.00_censor0.30_target0.95_mlasso` | 1,786 | 3.43× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.80_hazard2.00_censor0.50_target0.90_mlasso` | 671 | `p100_cindex0.80_hazard2.00_censor0.50_target0.95_mlasso` | 5,000 | 7.45× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p100_cindex0.85_hazard0.50_censor0.10_target0.85_mlasso` | 237 | `p100_cindex0.85_hazard0.50_censor0.10_target0.90_mlasso` | 793 | 3.35× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.85_hazard0.50_censor0.10_target0.90_mlasso` | 793 | `p100_cindex0.85_hazard0.50_censor0.10_target0.95_mlasso` | 2,614 | 3.30× higher | — | — | — |
| Target slope: 0.8 → 0.9 | `p100_cindex0.85_hazard0.50_censor0.30_target0.80_mlasso` | 210 | `p100_cindex0.85_hazard0.50_censor0.30_target0.90_mlasso` | 840 | 4.00× higher | Yes | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.85_hazard0.50_censor0.30_target0.90_mlasso` | 840 | `p100_cindex0.85_hazard0.50_censor0.30_target0.95_mlasso` | 2,883 | 3.43× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.85_hazard0.50_censor0.50_target0.90_mlasso` | 602 | `p100_cindex0.85_hazard0.50_censor0.50_target0.95_mlasso` | 3,872 | 6.43× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p100_cindex0.85_hazard1.00_censor0.10_target0.85_mlasso` | 230 | `p100_cindex0.85_hazard1.00_censor0.10_target0.90_mlasso` | 742 | 3.23× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.85_hazard1.00_censor0.10_target0.90_mlasso` | 742 | `p100_cindex0.85_hazard1.00_censor0.10_target0.95_mlasso` | 2,614 | 3.52× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.85_hazard1.00_censor0.30_target0.90_mlasso` | 420 | `p100_cindex0.85_hazard1.00_censor0.30_target0.95_mlasso` | 2,781 | 6.62× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.85_hazard1.00_censor0.50_target0.90_mlasso` | 588 | `p100_cindex0.85_hazard1.00_censor0.50_target0.95_mlasso` | 3,627 | 6.17× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.85_hazard2.00_censor0.10_target0.90_mlasso` | 326 | `p100_cindex0.85_hazard2.00_censor0.10_target0.95_mlasso` | 2,194 | 6.73× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p100_cindex0.85_hazard2.00_censor0.30_target0.85_mlasso` | 276 | `p100_cindex0.85_hazard2.00_censor0.30_target0.90_mlasso` | 840 | 3.04× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.85_hazard2.00_censor0.30_target0.90_mlasso` | 840 | `p100_cindex0.85_hazard2.00_censor0.30_target0.95_mlasso` | 2,887 | 3.44× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.85_hazard2.00_censor0.50_target0.90_mlasso` | 555 | `p100_cindex0.85_hazard2.00_censor0.50_target0.95_mlasso` | 3,745 | 6.75× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard0.50_censor0.10_target0.90_mlasso` | 1,480 | `p10_cindex0.60_hazard0.50_censor0.10_target0.95_mlasso` | 7,642 | 5.16× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard0.50_censor0.30_target0.90_mlasso` | 1,904 | `p10_cindex0.60_hazard0.50_censor0.30_target0.95_mlasso` | 7,865 | 4.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard1.00_censor0.10_target0.90_mlasso` | 1,524 | `p10_cindex0.60_hazard1.00_censor0.10_target0.95_mlasso` | 7,661 | 5.03× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard1.00_censor0.30_target0.90_mlasso` | 1,813 | `p10_cindex0.60_hazard1.00_censor0.30_target0.95_mlasso` | 15,232 | 8.40× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | 2,664 | `p10_cindex0.60_hazard1.00_censor0.50_target0.95_mlasso` | 10,656 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard2.00_censor0.10_target0.90_mlasso` | 1,480 | `p10_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 6,045 | 4.08× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard2.00_censor0.30_target0.90_mlasso` | 1,680 | `p10_cindex0.60_hazard2.00_censor0.30_target0.95_mlasso` | 7,616 | 4.53× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard2.00_censor0.50_target0.90_mlasso` | 2,675 | `p10_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 9,470 | 3.54× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.65_hazard0.50_censor0.50_target0.90_mlasso` | 616 | `p10_cindex0.65_hazard0.50_censor0.50_target0.95_mlasso` | 3,701 | 6.01× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.65_hazard1.00_censor0.10_target0.90_mlasso` | 684 | `p10_cindex0.65_hazard1.00_censor0.10_target0.95_mlasso` | 2,736 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.65_hazard1.00_censor0.30_target0.90_mlasso` | 440 | `p10_cindex0.65_hazard1.00_censor0.30_target0.95_mlasso` | 2,433 | 5.53× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.65_hazard1.00_censor0.50_target0.90_mlasso` | 1,232 | `p10_cindex0.65_hazard1.00_censor0.50_target0.95_mlasso` | 9,856 | 8.00× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p10_cindex0.65_hazard2.00_censor0.10_target0.90_mlasso` | 646 | `p10_cindex0.65_hazard2.00_censor0.10_target0.95_mlasso` | 2,736 | 4.24× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.70_hazard0.50_censor0.10_target0.90_mlasso` | 318 | `p10_cindex0.70_hazard0.50_censor0.10_target0.95_mlasso` | 2,544 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.70_hazard0.50_censor0.50_target0.90_mlasso` | 648 | `p10_cindex0.70_hazard0.50_censor0.50_target0.95_mlasso` | 2,288 | 3.53× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.70_hazard1.00_censor0.10_target0.90_mlasso` | 318 | `p10_cindex0.70_hazard1.00_censor0.10_target0.95_mlasso` | 2,544 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.70_hazard1.00_censor0.30_target0.90_mlasso` | 479 | `p10_cindex0.70_hazard1.00_censor0.30_target0.95_mlasso` | 1,632 | 3.41× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.70_hazard1.00_censor0.50_target0.90_mlasso` | 572 | `p10_cindex0.70_hazard1.00_censor0.50_target0.95_mlasso` | 1,866 | 3.26× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.70_hazard2.00_censor0.10_target0.90_mlasso` | 361 | `p10_cindex0.70_hazard2.00_censor0.10_target0.95_mlasso` | 1,387 | 3.84× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p10_cindex0.70_hazard2.00_censor0.30_target0.85_mlasso` | 204 | `p10_cindex0.70_hazard2.00_censor0.30_target0.90_mlasso` | 816 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.70_hazard2.00_censor0.50_target0.90_mlasso` | 658 | `p10_cindex0.70_hazard2.00_censor0.50_target0.95_mlasso` | 2,288 | 3.48× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.75_hazard0.50_censor0.10_target0.90_mlasso` | 296 | `p10_cindex0.75_hazard0.50_censor0.10_target0.95_mlasso` | 2,368 | 8.00× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p10_cindex0.75_hazard0.50_censor0.30_target0.90_mlasso` | 349 | `p10_cindex0.75_hazard0.50_censor0.30_target0.95_mlasso` | 1,061 | 3.04× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.75_hazard0.50_censor0.50_target0.90_mlasso` | 411 | `p10_cindex0.75_hazard0.50_censor0.50_target0.95_mlasso` | 1,327 | 3.23× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.75_hazard1.00_censor0.10_target0.90_mlasso` | 274 | `p10_cindex0.75_hazard1.00_censor0.10_target0.95_mlasso` | 1,205 | 4.40× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.75_hazard1.00_censor0.50_target0.90_mlasso` | 422 | `p10_cindex0.75_hazard1.00_censor0.50_target0.95_mlasso` | 1,492 | 3.54× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.75_hazard2.00_censor0.30_target0.90_mlasso` | 180 | `p10_cindex0.75_hazard2.00_censor0.30_target0.95_mlasso` | 1,193 | 6.63× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p10_cindex0.75_hazard2.00_censor0.50_target0.80_mlasso` | 66 | `p10_cindex0.75_hazard2.00_censor0.50_target0.85_mlasso` | 267 | 4.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.75_hazard2.00_censor0.50_target0.90_mlasso` | 424 | `p10_cindex0.75_hazard2.00_censor0.50_target0.95_mlasso` | 2,136 | 5.04× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.80_hazard0.50_censor0.30_target0.90_mlasso` | 221 | `p10_cindex0.80_hazard0.50_censor0.30_target0.95_mlasso` | 716 | 3.24× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.80_hazard2.00_censor0.30_target0.90_mlasso` | 266 | `p10_cindex0.80_hazard2.00_censor0.30_target0.95_mlasso` | 1,432 | 5.38× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.80_hazard2.00_censor0.50_target0.90_mlasso` | 301 | `p10_cindex0.80_hazard2.00_censor0.50_target0.95_mlasso` | 1,007 | 3.35× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard0.50_censor0.10_target0.90_mlasso` | 127 | `p10_cindex0.85_hazard0.50_censor0.10_target0.95_mlasso` | 553 | 4.35× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard0.50_censor0.30_target0.90_mlasso` | 172 | `p10_cindex0.85_hazard0.50_censor0.30_target0.95_mlasso` | 676 | 3.93× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard0.50_censor0.50_target0.90_mlasso` | 235 | `p10_cindex0.85_hazard0.50_censor0.50_target0.95_mlasso` | 940 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard1.00_censor0.10_target0.90_mlasso` | 131 | `p10_cindex0.85_hazard1.00_censor0.10_target0.95_mlasso` | 535 | 4.08× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard1.00_censor0.30_target0.90_mlasso` | 199 | `p10_cindex0.85_hazard1.00_censor0.30_target0.95_mlasso` | 626 | 3.15× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard1.00_censor0.50_target0.90_mlasso` | 235 | `p10_cindex0.85_hazard1.00_censor0.50_target0.95_mlasso` | 843 | 3.59× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p10_cindex0.85_hazard2.00_censor0.10_target0.85_mlasso` | 65 | `p10_cindex0.85_hazard2.00_censor0.10_target0.90_mlasso` | 205 | 3.15× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard2.00_censor0.10_target0.90_mlasso` | 205 | `p10_cindex0.85_hazard2.00_censor0.10_target0.95_mlasso` | 1,048 | 5.11× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard2.00_censor0.30_target0.90_mlasso` | 181 | `p10_cindex0.85_hazard2.00_censor0.30_target0.95_mlasso` | 572 | 3.16× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard2.00_censor0.50_target0.90_mlasso` | 235 | `p10_cindex0.85_hazard2.00_censor0.50_target0.95_mlasso` | 941 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.60_hazard0.50_censor0.50_target0.90_mlasso` | 1,946 | `p15_cindex0.60_hazard0.50_censor0.50_target0.95_mlasso` | 7,494 | 3.85× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.60_hazard1.00_censor0.10_target0.90_mlasso` | 1,078 | `p15_cindex0.60_hazard1.00_censor0.10_target0.95_mlasso` | 8,896 | 8.25× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.60_hazard1.00_censor0.30_target0.90_mlasso` | 1,470 | `p15_cindex0.60_hazard1.00_censor0.30_target0.95_mlasso` | 4,700 | 3.20× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p15_cindex0.60_hazard1.00_censor0.50_target0.85_mlasso` | 955 | `p15_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | 2,969 | 3.11× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | 2,969 | `p15_cindex0.60_hazard1.00_censor0.50_target0.95_mlasso` | 10,662 | 3.59× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.60_hazard2.00_censor0.10_target0.90_mlasso` | 1,704 | `p15_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 8,896 | 5.22× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.60_hazard2.00_censor0.30_target0.90_mlasso` | 1,939 | `p15_cindex0.60_hazard2.00_censor0.30_target0.95_mlasso` | 7,441 | 3.84× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.60_hazard2.00_censor0.50_target0.90_mlasso` | 4,000 | `p15_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 14,884 | 3.72× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p15_cindex0.65_hazard0.50_censor0.10_target0.80_mlasso` | 64 | `p15_cindex0.65_hazard0.50_censor0.10_target0.85_mlasso` | 383 | 5.98× higher | — | previous, next | previous |
| Target slope: 0.9 → 0.95 | `p15_cindex0.65_hazard0.50_censor0.50_target0.90_mlasso` | 859 | `p15_cindex0.65_hazard0.50_censor0.50_target0.95_mlasso` | 3,743 | 4.36× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.65_hazard1.00_censor0.10_target0.90_mlasso` | 732 | `p15_cindex0.65_hazard1.00_censor0.10_target0.95_mlasso` | 2,342 | 3.20× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.65_hazard1.00_censor0.30_target0.90_mlasso` | 734 | `p15_cindex0.65_hazard1.00_censor0.30_target0.95_mlasso` | 2,408 | 3.28× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.65_hazard1.00_censor0.50_target0.90_mlasso` | 1,333 | `p15_cindex0.65_hazard1.00_censor0.50_target0.95_mlasso` | 4,189 | 3.14× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.65_hazard2.00_censor0.10_target0.90_mlasso` | 702 | `p15_cindex0.65_hazard2.00_censor0.10_target0.95_mlasso` | 2,202 | 3.14× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.65_hazard2.00_censor0.30_target0.90_mlasso` | 923 | `p15_cindex0.65_hazard2.00_censor0.30_target0.95_mlasso` | 5,264 | 5.70× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.70_hazard0.50_censor0.10_target0.90_mlasso` | 239 | `p15_cindex0.70_hazard0.50_censor0.10_target0.95_mlasso` | 1,912 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.70_hazard0.50_censor0.50_target0.90_mlasso` | 743 | `p15_cindex0.70_hazard0.50_censor0.50_target0.95_mlasso` | 3,432 | 4.62× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.70_hazard1.00_censor0.10_target0.90_mlasso` | 432 | `p15_cindex0.70_hazard1.00_censor0.10_target0.95_mlasso` | 1,415 | 3.28× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.70_hazard1.00_censor0.30_target0.90_mlasso` | 612 | `p15_cindex0.70_hazard1.00_censor0.30_target0.95_mlasso` | 2,448 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.70_hazard1.00_censor0.50_target0.90_mlasso` | 668 | `p15_cindex0.70_hazard1.00_censor0.50_target0.95_mlasso` | 2,091 | 3.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.70_hazard2.00_censor0.10_target0.90_mlasso` | 472 | `p15_cindex0.70_hazard2.00_censor0.10_target0.95_mlasso` | 1,912 | 4.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.70_hazard2.00_censor0.30_target0.90_mlasso` | 612 | `p15_cindex0.70_hazard2.00_censor0.30_target0.95_mlasso` | 2,448 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.75_hazard0.50_censor0.30_target0.90_mlasso` | 285 | `p15_cindex0.75_hazard0.50_censor0.30_target0.95_mlasso` | 1,131 | 3.97× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.75_hazard0.50_censor0.50_target0.90_mlasso` | 376 | `p15_cindex0.75_hazard0.50_censor0.50_target0.95_mlasso` | 1,436 | 3.82× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.75_hazard1.00_censor0.30_target0.90_mlasso` | 285 | `p15_cindex0.75_hazard1.00_censor0.30_target0.95_mlasso` | 1,140 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.75_hazard1.00_censor0.50_target0.90_mlasso` | 420 | `p15_cindex0.75_hazard1.00_censor0.50_target0.95_mlasso` | 1,521 | 3.62× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.75_hazard2.00_censor0.50_target0.90_mlasso` | 402 | `p15_cindex0.75_hazard2.00_censor0.50_target0.95_mlasso` | 1,600 | 3.98× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.80_hazard0.50_censor0.50_target0.90_mlasso` | 375 | `p15_cindex0.80_hazard0.50_censor0.50_target0.95_mlasso` | 1,500 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.80_hazard1.00_censor0.10_target0.90_mlasso` | 209 | `p15_cindex0.80_hazard1.00_censor0.10_target0.95_mlasso` | 638 | 3.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.80_hazard1.00_censor0.30_target0.90_mlasso` | 270 | `p15_cindex0.80_hazard1.00_censor0.30_target0.95_mlasso` | 1,072 | 3.97× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.80_hazard2.00_censor0.10_target0.90_mlasso` | 209 | `p15_cindex0.80_hazard2.00_censor0.10_target0.95_mlasso` | 737 | 3.53× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.85_hazard2.00_censor0.10_target0.90_mlasso` | 179 | `p15_cindex0.85_hazard2.00_censor0.10_target0.95_mlasso` | 784 | 4.38× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.85_hazard2.00_censor0.30_target0.90_mlasso` | 220 | `p15_cindex0.85_hazard2.00_censor0.30_target0.95_mlasso` | 1,008 | 4.58× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.85_hazard2.00_censor0.50_target0.90_mlasso` | 182 | `p15_cindex0.85_hazard2.00_censor0.50_target0.95_mlasso` | 773 | 4.25× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.60_hazard0.50_censor0.10_target0.90_mlasso` | 1,361 | `p20_cindex0.60_hazard0.50_censor0.10_target0.95_mlasso` | 4,644 | 3.41× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.60_hazard0.50_censor0.30_target0.90_mlasso` | 2,113 | `p20_cindex0.60_hazard0.50_censor0.30_target0.95_mlasso` | 7,632 | 3.61× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.60_hazard0.50_censor0.50_target0.90_mlasso` | 2,605 | `p20_cindex0.60_hazard0.50_censor0.50_target0.95_mlasso` | 9,341 | 3.59× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p20_cindex0.60_hazard1.00_censor0.10_target0.85_mlasso` | 873 | `p20_cindex0.60_hazard1.00_censor0.10_target0.90_mlasso` | 2,960 | 3.39× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.60_hazard1.00_censor0.10_target0.90_mlasso` | 2,960 | `p20_cindex0.60_hazard1.00_censor0.10_target0.95_mlasso` | 23,680 | 8.00× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p20_cindex0.60_hazard1.00_censor0.30_target0.90_mlasso` | 2,007 | `p20_cindex0.60_hazard1.00_censor0.30_target0.95_mlasso` | 7,931 | 3.95× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | 2,819 | `p20_cindex0.60_hazard1.00_censor0.50_target0.95_mlasso` | 10,525 | 3.73× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.60_hazard2.00_censor0.10_target0.90_mlasso` | 1,141 | `p20_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 6,110 | 5.35× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.60_hazard2.00_censor0.30_target0.90_mlasso` | 2,234 | `p20_cindex0.60_hazard2.00_censor0.30_target0.95_mlasso` | 7,577 | 3.39× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.60_hazard2.00_censor0.50_target0.90_mlasso` | 3,560 | `p20_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 12,910 | 3.63× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.65_hazard0.50_censor0.10_target0.90_mlasso` | 753 | `p20_cindex0.65_hazard0.50_censor0.10_target0.95_mlasso` | 2,779 | 3.69× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.65_hazard0.50_censor0.50_target0.90_mlasso` | 1,393 | `p20_cindex0.65_hazard0.50_censor0.50_target0.95_mlasso` | 4,920 | 3.53× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.65_hazard1.00_censor0.10_target0.90_mlasso` | 770 | `p20_cindex0.65_hazard1.00_censor0.10_target0.95_mlasso` | 2,436 | 3.16× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.65_hazard1.00_censor0.30_target0.90_mlasso` | 967 | `p20_cindex0.65_hazard1.00_censor0.30_target0.95_mlasso` | 3,520 | 3.64× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.65_hazard1.00_censor0.50_target0.90_mlasso` | 615 | `p20_cindex0.65_hazard1.00_censor0.50_target0.95_mlasso` | 4,339 | 7.06× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.65_hazard2.00_censor0.10_target0.90_mlasso` | 655 | `p20_cindex0.65_hazard2.00_censor0.10_target0.95_mlasso` | 2,737 | 4.18× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.65_hazard2.00_censor0.30_target0.90_mlasso` | 880 | `p20_cindex0.65_hazard2.00_censor0.30_target0.95_mlasso` | 3,520 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.65_hazard2.00_censor0.50_target0.90_mlasso` | 1,179 | `p20_cindex0.65_hazard2.00_censor0.50_target0.95_mlasso` | 4,920 | 4.17× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.70_hazard0.50_censor0.10_target0.90_mlasso` | 461 | `p20_cindex0.70_hazard0.50_censor0.10_target0.95_mlasso` | 2,536 | 5.50× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.70_hazard0.50_censor0.30_target0.90_mlasso` | 515 | `p20_cindex0.70_hazard0.50_censor0.30_target0.95_mlasso` | 1,576 | 3.06× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.70_hazard1.00_censor0.30_target0.90_mlasso` | 583 | `p20_cindex0.70_hazard1.00_censor0.30_target0.95_mlasso` | 1,845 | 3.16× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.70_hazard2.00_censor0.10_target0.90_mlasso` | 517 | `p20_cindex0.70_hazard2.00_censor0.10_target0.95_mlasso` | 1,555 | 3.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.70_hazard2.00_censor0.50_target0.90_mlasso` | 571 | `p20_cindex0.70_hazard2.00_censor0.50_target0.95_mlasso` | 2,284 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.75_hazard0.50_censor0.10_target0.90_mlasso` | 292 | `p20_cindex0.75_hazard0.50_censor0.10_target0.95_mlasso` | 931 | 3.19× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.75_hazard1.00_censor0.10_target0.90_mlasso` | 339 | `p20_cindex0.75_hazard1.00_censor0.10_target0.95_mlasso` | 1,093 | 3.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.75_hazard1.00_censor0.30_target0.90_mlasso` | 364 | `p20_cindex0.75_hazard1.00_censor0.30_target0.95_mlasso` | 1,158 | 3.18× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.75_hazard1.00_censor0.50_target0.90_mlasso` | 400 | `p20_cindex0.75_hazard1.00_censor0.50_target0.95_mlasso` | 1,571 | 3.93× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.75_hazard2.00_censor0.10_target0.90_mlasso` | 280 | `p20_cindex0.75_hazard2.00_censor0.10_target0.95_mlasso` | 1,184 | 4.23× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.75_hazard2.00_censor0.50_target0.90_mlasso` | 533 | `p20_cindex0.75_hazard2.00_censor0.50_target0.95_mlasso` | 1,642 | 3.08× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.80_hazard0.50_censor0.30_target0.90_mlasso` | 254 | `p20_cindex0.80_hazard0.50_censor0.30_target0.95_mlasso` | 857 | 3.37× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.80_hazard0.50_censor0.50_target0.90_mlasso` | 368 | `p20_cindex0.80_hazard0.50_censor0.50_target0.95_mlasso` | 1,225 | 3.33× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.80_hazard1.00_censor0.10_target0.90_mlasso` | 246 | `p20_cindex0.80_hazard1.00_censor0.10_target0.95_mlasso` | 767 | 3.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.80_hazard1.00_censor0.30_target0.90_mlasso` | 281 | `p20_cindex0.80_hazard1.00_censor0.30_target0.95_mlasso` | 925 | 3.29× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.85_hazard0.50_censor0.10_target0.90_mlasso` | 196 | `p20_cindex0.85_hazard0.50_censor0.10_target0.95_mlasso` | 590 | 3.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.85_hazard0.50_censor0.50_target0.90_mlasso` | 314 | `p20_cindex0.85_hazard0.50_censor0.50_target0.95_mlasso` | 1,884 | 6.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.60_hazard0.50_censor0.10_target0.90_mlasso` | 2,010 | `p25_cindex0.60_hazard0.50_censor0.10_target0.95_mlasso` | 14,816 | 7.37× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.60_hazard0.50_censor0.30_target0.90_mlasso` | 2,101 | `p25_cindex0.60_hazard0.50_censor0.30_target0.95_mlasso` | 9,520 | 4.53× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.60_hazard1.00_censor0.30_target0.90_mlasso` | 2,385 | `p25_cindex0.60_hazard1.00_censor0.30_target0.95_mlasso` | 8,588 | 3.60× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | 3,411 | `p25_cindex0.60_hazard1.00_censor0.50_target0.95_mlasso` | 13,727 | 4.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.60_hazard2.00_censor0.30_target0.90_mlasso` | 2,380 | `p25_cindex0.60_hazard2.00_censor0.30_target0.95_mlasso` | 9,520 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.60_hazard2.00_censor0.50_target0.90_mlasso` | 3,251 | `p25_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 13,328 | 4.10× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.65_hazard0.50_censor0.10_target0.90_mlasso` | 671 | `p25_cindex0.65_hazard0.50_censor0.10_target0.95_mlasso` | 2,544 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.65_hazard0.50_censor0.30_target0.90_mlasso` | 1,012 | `p25_cindex0.65_hazard0.50_censor0.30_target0.95_mlasso` | 4,392 | 4.34× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p25_cindex0.65_hazard0.50_censor0.50_target0.80_mlasso` | 192 | `p25_cindex0.65_hazard0.50_censor0.50_target0.85_mlasso` | 673 | 3.51× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.65_hazard1.00_censor0.10_target0.90_mlasso` | 758 | `p25_cindex0.65_hazard1.00_censor0.10_target0.95_mlasso` | 2,716 | 3.58× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.65_hazard1.00_censor0.50_target0.90_mlasso` | 1,383 | `p25_cindex0.65_hazard1.00_censor0.50_target0.95_mlasso` | 5,870 | 4.24× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.65_hazard2.00_censor0.10_target0.90_mlasso` | 856 | `p25_cindex0.65_hazard2.00_censor0.10_target0.95_mlasso` | 2,954 | 3.45× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.65_hazard2.00_censor0.30_target0.90_mlasso` | 1,098 | `p25_cindex0.65_hazard2.00_censor0.30_target0.95_mlasso` | 3,553 | 3.24× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.65_hazard2.00_censor0.50_target0.90_mlasso` | 1,349 | `p25_cindex0.65_hazard2.00_censor0.50_target0.95_mlasso` | 4,414 | 3.27× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.70_hazard0.50_censor0.10_target0.90_mlasso` | 406 | `p25_cindex0.70_hazard0.50_censor0.10_target0.95_mlasso` | 1,500 | 3.69× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.70_hazard0.50_censor0.30_target0.90_mlasso` | 613 | `p25_cindex0.70_hazard0.50_censor0.30_target0.95_mlasso` | 1,995 | 3.25× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.70_hazard0.50_censor0.50_target0.90_mlasso` | 714 | `p25_cindex0.70_hazard0.50_censor0.50_target0.95_mlasso` | 2,856 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.70_hazard1.00_censor0.30_target0.90_mlasso` | 518 | `p25_cindex0.70_hazard1.00_censor0.30_target0.95_mlasso` | 1,828 | 3.53× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.70_hazard1.00_censor0.50_target0.90_mlasso` | 779 | `p25_cindex0.70_hazard1.00_censor0.50_target0.95_mlasso` | 2,396 | 3.08× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.70_hazard2.00_censor0.10_target0.90_mlasso` | 445 | `p25_cindex0.70_hazard2.00_censor0.10_target0.95_mlasso` | 1,614 | 3.63× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.70_hazard2.00_censor0.30_target0.90_mlasso` | 510 | `p25_cindex0.70_hazard2.00_censor0.30_target0.95_mlasso` | 1,626 | 3.19× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.70_hazard2.00_censor0.50_target0.90_mlasso` | 742 | `p25_cindex0.70_hazard2.00_censor0.50_target0.95_mlasso` | 2,968 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.75_hazard1.00_censor0.10_target0.90_mlasso` | 398 | `p25_cindex0.75_hazard1.00_censor0.10_target0.95_mlasso` | 1,239 | 3.11× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.75_hazard2.00_censor0.10_target0.90_mlasso` | 358 | `p25_cindex0.75_hazard2.00_censor0.10_target0.95_mlasso` | 1,145 | 3.20× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.75_hazard2.00_censor0.50_target0.90_mlasso` | 551 | `p25_cindex0.75_hazard2.00_censor0.50_target0.95_mlasso` | 1,794 | 3.26× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.80_hazard0.50_censor0.10_target0.90_mlasso` | 183 | `p25_cindex0.80_hazard0.50_censor0.10_target0.95_mlasso` | 696 | 3.80× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.80_hazard0.50_censor0.50_target0.90_mlasso` | 372 | `p25_cindex0.80_hazard0.50_censor0.50_target0.95_mlasso` | 1,250 | 3.36× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.80_hazard1.00_censor0.50_target0.90_mlasso` | 312 | `p25_cindex0.80_hazard1.00_censor0.50_target0.95_mlasso` | 1,355 | 4.34× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.80_hazard2.00_censor0.30_target0.90_mlasso` | 446 | `p25_cindex0.80_hazard2.00_censor0.30_target0.95_mlasso` | 1,784 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.80_hazard2.00_censor0.50_target0.90_mlasso` | 400 | `p25_cindex0.80_hazard2.00_censor0.50_target0.95_mlasso` | 1,250 | 3.12× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_cindex0.85_hazard0.50_censor0.10_target0.85_mlasso` | 40 | `p25_cindex0.85_hazard0.50_censor0.10_target0.90_mlasso` | 215 | 5.38× higher | — | previous, next | previous |
| Target slope: 0.9 → 0.95 | `p25_cindex0.85_hazard0.50_censor0.50_target0.90_mlasso` | 284 | `p25_cindex0.85_hazard0.50_censor0.50_target0.95_mlasso` | 907 | 3.19× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.85_hazard1.00_censor0.30_target0.90_mlasso` | 249 | `p25_cindex0.85_hazard1.00_censor0.30_target0.95_mlasso` | 798 | 3.20× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.85_hazard1.00_censor0.50_target0.90_mlasso` | 310 | `p25_cindex0.85_hazard1.00_censor0.50_target0.95_mlasso` | 1,176 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.60_hazard0.50_censor0.10_target0.90_mlasso` | 2,220 | `p30_cindex0.60_hazard0.50_censor0.10_target0.95_mlasso` | 8,880 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.60_hazard0.50_censor0.30_target0.90_mlasso` | 2,169 | `p30_cindex0.60_hazard0.50_censor0.30_target0.95_mlasso` | 10,517 | 4.85× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.60_hazard1.00_censor0.10_target0.90_mlasso` | 2,222 | `p30_cindex0.60_hazard1.00_censor0.10_target0.95_mlasso` | 7,012 | 3.16× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.60_hazard1.00_censor0.30_target0.90_mlasso` | 2,518 | `p30_cindex0.60_hazard1.00_censor0.30_target0.95_mlasso` | 8,634 | 3.43× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | 3,184 | `p30_cindex0.60_hazard1.00_censor0.50_target0.95_mlasso` | 9,819 | 3.08× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.60_hazard2.00_censor0.10_target0.90_mlasso` | 1,704 | `p30_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 6,976 | 4.09× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.60_hazard2.00_censor0.30_target0.90_mlasso` | 2,290 | `p30_cindex0.60_hazard2.00_censor0.30_target0.95_mlasso` | 8,558 | 3.74× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.65_hazard0.50_censor0.10_target0.90_mlasso` | 843 | `p30_cindex0.65_hazard0.50_censor0.10_target0.95_mlasso` | 3,328 | 3.95× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.65_hazard0.50_censor0.50_target0.90_mlasso` | 1,406 | `p30_cindex0.65_hazard0.50_censor0.50_target0.95_mlasso` | 5,548 | 3.95× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.65_hazard1.00_censor0.10_target0.90_mlasso` | 1,024 | `p30_cindex0.65_hazard1.00_censor0.10_target0.95_mlasso` | 4,096 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.65_hazard1.00_censor0.30_target0.90_mlasso` | 1,320 | `p30_cindex0.65_hazard1.00_censor0.30_target0.95_mlasso` | 4,130 | 3.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.65_hazard1.00_censor0.50_target0.90_mlasso` | 1,624 | `p30_cindex0.65_hazard1.00_censor0.50_target0.95_mlasso` | 7,384 | 4.55× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.65_hazard2.00_censor0.10_target0.90_mlasso` | 1,024 | `p30_cindex0.65_hazard2.00_censor0.10_target0.95_mlasso` | 4,096 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.65_hazard2.00_censor0.50_target0.90_mlasso` | 1,361 | `p30_cindex0.65_hazard2.00_censor0.50_target0.95_mlasso` | 5,792 | 4.26× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.70_hazard0.50_censor0.10_target0.90_mlasso` | 598 | `p30_cindex0.70_hazard0.50_censor0.10_target0.95_mlasso` | 1,979 | 3.31× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.70_hazard0.50_censor0.30_target0.90_mlasso` | 613 | `p30_cindex0.70_hazard0.50_censor0.30_target0.95_mlasso` | 2,452 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.70_hazard0.50_censor0.50_target0.90_mlasso` | 853 | `p30_cindex0.70_hazard0.50_censor0.50_target0.95_mlasso` | 3,006 | 3.52× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.70_hazard1.00_censor0.10_target0.90_mlasso` | 598 | `p30_cindex0.70_hazard1.00_censor0.10_target0.95_mlasso` | 1,904 | 3.18× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.70_hazard1.00_censor0.30_target0.90_mlasso` | 693 | `p30_cindex0.70_hazard1.00_censor0.30_target0.95_mlasso` | 2,452 | 3.54× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.70_hazard1.00_censor0.50_target0.90_mlasso` | 889 | `p30_cindex0.70_hazard1.00_censor0.50_target0.95_mlasso` | 3,078 | 3.46× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.70_hazard2.00_censor0.30_target0.90_mlasso` | 620 | `p30_cindex0.70_hazard2.00_censor0.30_target0.95_mlasso` | 2,543 | 4.10× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.70_hazard2.00_censor0.50_target0.90_mlasso` | 857 | `p30_cindex0.70_hazard2.00_censor0.50_target0.95_mlasso` | 2,801 | 3.27× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.75_hazard0.50_censor0.30_target0.90_mlasso` | 425 | `p30_cindex0.75_hazard0.50_censor0.30_target0.95_mlasso` | 1,584 | 3.73× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.75_hazard0.50_censor0.50_target0.90_mlasso` | 550 | `p30_cindex0.75_hazard0.50_censor0.50_target0.95_mlasso` | 2,137 | 3.89× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.75_hazard1.00_censor0.50_target0.90_mlasso` | 589 | `p30_cindex0.75_hazard1.00_censor0.50_target0.95_mlasso` | 1,951 | 3.31× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.75_hazard2.00_censor0.10_target0.90_mlasso` | 444 | `p30_cindex0.75_hazard2.00_censor0.10_target0.95_mlasso` | 1,776 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.75_hazard2.00_censor0.30_target0.90_mlasso` | 286 | `p30_cindex0.75_hazard2.00_censor0.30_target0.95_mlasso` | 1,563 | 5.47× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.80_hazard0.50_censor0.30_target0.90_mlasso` | 348 | `p30_cindex0.80_hazard0.50_censor0.30_target0.95_mlasso` | 1,101 | 3.16× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.80_hazard1.00_censor0.50_target0.90_mlasso` | 493 | `p30_cindex0.80_hazard1.00_censor0.50_target0.95_mlasso` | 1,499 | 3.04× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.80_hazard2.00_censor0.30_target0.90_mlasso` | 303 | `p30_cindex0.80_hazard2.00_censor0.30_target0.95_mlasso` | 1,072 | 3.54× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p30_cindex0.80_hazard2.00_censor0.50_target0.85_mlasso` | 244 | `p30_cindex0.80_hazard2.00_censor0.50_target0.90_mlasso` | 750 | 3.07× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.85_hazard0.50_censor0.30_target0.90_mlasso` | 279 | `p30_cindex0.85_hazard0.50_censor0.30_target0.95_mlasso` | 878 | 3.15× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.85_hazard1.00_censor0.10_target0.90_mlasso` | 256 | `p30_cindex0.85_hazard1.00_censor0.10_target0.95_mlasso` | 784 | 3.06× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.85_hazard1.00_censor0.50_target0.90_mlasso` | 313 | `p30_cindex0.85_hazard1.00_censor0.50_target0.95_mlasso` | 1,045 | 3.34× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.85_hazard2.00_censor0.50_target0.90_mlasso` | 176 | `p30_cindex0.85_hazard2.00_censor0.50_target0.95_mlasso` | 1,412 | 8.02× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.60_hazard0.50_censor0.50_target0.90_mlasso` | 2,424 | `p40_cindex0.60_hazard0.50_censor0.50_target0.95_mlasso` | 21,328 | 8.80× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.60_hazard1.00_censor0.10_target0.90_mlasso` | 1,965 | `p40_cindex0.60_hazard1.00_censor0.10_target0.95_mlasso` | 7,727 | 3.93× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.60_hazard1.00_censor0.30_target0.90_mlasso` | 2,623 | `p40_cindex0.60_hazard1.00_censor0.30_target0.95_mlasso` | 8,298 | 3.16× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | 3,401 | `p40_cindex0.60_hazard1.00_censor0.50_target0.95_mlasso` | 19,944 | 5.86× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.60_hazard2.00_censor0.10_target0.90_mlasso` | 1,991 | `p40_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 8,205 | 4.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.60_hazard2.00_censor0.30_target0.90_mlasso` | 2,061 | `p40_cindex0.60_hazard2.00_censor0.30_target0.95_mlasso` | 7,373 | 3.58× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.60_hazard2.00_censor0.50_target0.90_mlasso` | 5,332 | `p40_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 21,328 | 4.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.65_hazard0.50_censor0.10_target0.90_mlasso` | 914 | `p40_cindex0.65_hazard0.50_censor0.10_target0.95_mlasso` | 5,464 | 5.98× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.65_hazard0.50_censor0.50_target0.90_mlasso` | 1,400 | `p40_cindex0.65_hazard0.50_censor0.50_target0.95_mlasso` | 5,413 | 3.87× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.65_hazard1.00_censor0.10_target0.90_mlasso` | 916 | `p40_cindex0.65_hazard1.00_censor0.10_target0.95_mlasso` | 3,917 | 4.28× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.65_hazard1.00_censor0.30_target0.90_mlasso` | 1,188 | `p40_cindex0.65_hazard1.00_censor0.30_target0.95_mlasso` | 4,107 | 3.46× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.65_hazard1.00_censor0.50_target0.90_mlasso` | 1,231 | `p40_cindex0.65_hazard1.00_censor0.50_target0.95_mlasso` | 9,848 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.65_hazard2.00_censor0.30_target0.90_mlasso` | 862 | `p40_cindex0.65_hazard2.00_censor0.30_target0.95_mlasso` | 7,024 | 8.15× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.65_hazard2.00_censor0.50_target0.90_mlasso` | 1,637 | `p40_cindex0.65_hazard2.00_censor0.50_target0.95_mlasso` | 6,712 | 4.10× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.70_hazard0.50_censor0.10_target0.90_mlasso` | 635 | `p40_cindex0.70_hazard0.50_censor0.10_target0.95_mlasso` | 2,222 | 3.50× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.70_hazard0.50_censor0.30_target0.90_mlasso` | 816 | `p40_cindex0.70_hazard0.50_censor0.30_target0.95_mlasso` | 3,264 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.70_hazard0.50_censor0.50_target0.90_mlasso` | 1,143 | `p40_cindex0.70_hazard0.50_censor0.50_target0.95_mlasso` | 3,467 | 3.03× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.70_hazard1.00_censor0.10_target0.90_mlasso` | 639 | `p40_cindex0.70_hazard1.00_censor0.10_target0.95_mlasso` | 2,167 | 3.39× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.70_hazard1.00_censor0.30_target0.90_mlasso` | 816 | `p40_cindex0.70_hazard1.00_censor0.30_target0.95_mlasso` | 2,669 | 3.27× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.70_hazard1.00_censor0.50_target0.90_mlasso` | 988 | `p40_cindex0.70_hazard1.00_censor0.50_target0.95_mlasso` | 3,950 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.70_hazard2.00_censor0.10_target0.90_mlasso` | 603 | `p40_cindex0.70_hazard2.00_censor0.10_target0.95_mlasso` | 2,127 | 3.53× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p40_cindex0.70_hazard2.00_censor0.30_target0.85_mlasso` | 409 | `p40_cindex0.70_hazard2.00_censor0.30_target0.90_mlasso` | 1,632 | 3.99× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.70_hazard2.00_censor0.50_target0.90_mlasso` | 507 | `p40_cindex0.70_hazard2.00_censor0.50_target0.95_mlasso` | 4,572 | 9.02× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.75_hazard0.50_censor0.10_target0.90_mlasso` | 433 | `p40_cindex0.75_hazard0.50_censor0.10_target0.95_mlasso` | 2,368 | 5.47× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.75_hazard0.50_censor0.30_target0.90_mlasso` | 472 | `p40_cindex0.75_hazard0.50_censor0.30_target0.95_mlasso` | 1,781 | 3.77× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.75_hazard0.50_censor0.50_target0.90_mlasso` | 567 | `p40_cindex0.75_hazard0.50_censor0.50_target0.95_mlasso` | 2,511 | 4.43× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.75_hazard1.00_censor0.10_target0.90_mlasso` | 466 | `p40_cindex0.75_hazard1.00_censor0.10_target0.95_mlasso` | 1,781 | 3.82× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.75_hazard1.00_censor0.30_target0.90_mlasso` | 590 | `p40_cindex0.75_hazard1.00_censor0.30_target0.95_mlasso` | 1,934 | 3.28× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.75_hazard1.00_censor0.50_target0.90_mlasso` | 561 | `p40_cindex0.75_hazard1.00_censor0.50_target0.95_mlasso` | 2,520 | 4.49× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.75_hazard2.00_censor0.10_target0.90_mlasso` | 538 | `p40_cindex0.75_hazard2.00_censor0.10_target0.95_mlasso` | 1,625 | 3.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.75_hazard2.00_censor0.30_target0.90_mlasso` | 423 | `p40_cindex0.75_hazard2.00_censor0.30_target0.95_mlasso` | 1,856 | 4.39× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.75_hazard2.00_censor0.50_target0.90_mlasso` | 533 | `p40_cindex0.75_hazard2.00_censor0.50_target0.95_mlasso` | 1,722 | 3.23× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.80_hazard0.50_censor0.30_target0.90_mlasso` | 436 | `p40_cindex0.80_hazard0.50_censor0.30_target0.95_mlasso` | 1,369 | 3.14× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.80_hazard0.50_censor0.50_target0.90_mlasso` | 438 | `p40_cindex0.80_hazard0.50_censor0.50_target0.95_mlasso` | 1,865 | 4.26× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.80_hazard1.00_censor0.10_target0.90_mlasso` | 289 | `p40_cindex0.80_hazard1.00_censor0.10_target0.95_mlasso` | 1,215 | 4.20× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.80_hazard1.00_censor0.30_target0.90_mlasso` | 361 | `p40_cindex0.80_hazard1.00_censor0.30_target0.95_mlasso` | 1,488 | 4.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.80_hazard1.00_censor0.50_target0.90_mlasso` | 537 | `p40_cindex0.80_hazard1.00_censor0.50_target0.95_mlasso` | 1,985 | 3.70× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.80_hazard2.00_censor0.10_target0.90_mlasso` | 277 | `p40_cindex0.80_hazard2.00_censor0.10_target0.95_mlasso` | 1,151 | 4.16× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.80_hazard2.00_censor0.30_target0.90_mlasso` | 375 | `p40_cindex0.80_hazard2.00_censor0.30_target0.95_mlasso` | 1,400 | 3.73× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p40_cindex0.80_hazard2.00_censor0.50_target0.85_mlasso` | 280 | `p40_cindex0.80_hazard2.00_censor0.50_target0.90_mlasso` | 1,000 | 3.57× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.85_hazard0.50_censor0.30_target0.90_mlasso` | 336 | `p40_cindex0.85_hazard0.50_censor0.30_target0.95_mlasso` | 1,045 | 3.11× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.85_hazard0.50_censor0.50_target0.90_mlasso` | 235 | `p40_cindex0.85_hazard0.50_censor0.50_target0.95_mlasso` | 1,407 | 5.99× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p40_cindex0.85_hazard1.00_censor0.10_target0.85_mlasso` | 65 | `p40_cindex0.85_hazard1.00_censor0.10_target0.90_mlasso` | 294 | 4.52× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.85_hazard1.00_censor0.30_target0.90_mlasso` | 326 | `p40_cindex0.85_hazard1.00_censor0.30_target0.95_mlasso` | 1,344 | 4.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.85_hazard1.00_censor0.50_target0.90_mlasso` | 419 | `p40_cindex0.85_hazard1.00_censor0.50_target0.95_mlasso` | 1,381 | 3.30× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.85_hazard2.00_censor0.10_target0.90_mlasso` | 243 | `p40_cindex0.85_hazard2.00_censor0.10_target0.95_mlasso` | 880 | 3.62× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.60_hazard0.50_censor0.10_target0.90_mlasso` | 1,855 | `p50_cindex0.60_hazard0.50_censor0.10_target0.95_mlasso` | 8,449 | 4.55× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.60_hazard0.50_censor0.30_target0.90_mlasso` | 2,658 | `p50_cindex0.60_hazard0.50_censor0.30_target0.95_mlasso` | 8,332 | 3.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.60_hazard0.50_censor0.50_target0.90_mlasso` | 4,326 | `p50_cindex0.60_hazard0.50_censor0.50_target0.95_mlasso` | 13,262 | 3.07× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.60_hazard1.00_censor0.10_target0.90_mlasso` | 2,384 | `p50_cindex0.60_hazard1.00_censor0.10_target0.95_mlasso` | 7,416 | 3.11× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.60_hazard2.00_censor0.10_target0.90_mlasso` | 2,322 | `p50_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 6,987 | 3.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.60_hazard2.00_censor0.30_target0.90_mlasso` | 2,397 | `p50_cindex0.60_hazard2.00_censor0.30_target0.95_mlasso` | 9,542 | 3.98× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.60_hazard2.00_censor0.50_target0.90_mlasso` | 4,137 | `p50_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 15,979 | 3.86× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.65_hazard0.50_censor0.30_target0.90_mlasso` | 1,357 | `p50_cindex0.65_hazard0.50_censor0.30_target0.95_mlasso` | 4,392 | 3.24× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.65_hazard0.50_censor0.50_target0.90_mlasso` | 1,990 | `p50_cindex0.65_hazard0.50_censor0.50_target0.95_mlasso` | 6,449 | 3.24× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.65_hazard1.00_censor0.10_target0.90_mlasso` | 1,080 | `p50_cindex0.65_hazard1.00_censor0.10_target0.95_mlasso` | 3,482 | 3.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.65_hazard1.00_censor0.50_target0.90_mlasso` | 1,662 | `p50_cindex0.65_hazard1.00_censor0.50_target0.95_mlasso` | 5,202 | 3.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.65_hazard2.00_censor0.10_target0.90_mlasso` | 947 | `p50_cindex0.65_hazard2.00_censor0.10_target0.95_mlasso` | 6,840 | 7.22× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.65_hazard2.00_censor0.30_target0.90_mlasso` | 1,230 | `p50_cindex0.65_hazard2.00_censor0.30_target0.95_mlasso` | 4,428 | 3.60× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.65_hazard2.00_censor0.50_target0.90_mlasso` | 1,660 | `p50_cindex0.65_hazard2.00_censor0.50_target0.95_mlasso` | 6,152 | 3.71× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.70_hazard0.50_censor0.10_target0.90_mlasso` | 794 | `p50_cindex0.70_hazard0.50_censor0.10_target0.95_mlasso` | 2,953 | 3.72× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.70_hazard0.50_censor0.30_target0.90_mlasso` | 844 | `p50_cindex0.70_hazard0.50_censor0.30_target0.95_mlasso` | 4,080 | 4.83× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.70_hazard0.50_censor0.50_target0.90_mlasso` | 968 | `p50_cindex0.70_hazard0.50_censor0.50_target0.95_mlasso` | 3,287 | 3.40× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.70_hazard1.00_censor0.10_target0.90_mlasso` | 605 | `p50_cindex0.70_hazard1.00_censor0.10_target0.95_mlasso` | 2,382 | 3.94× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.70_hazard1.00_censor0.30_target0.90_mlasso` | 753 | `p50_cindex0.70_hazard1.00_censor0.30_target0.95_mlasso` | 3,077 | 4.09× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.70_hazard2.00_censor0.10_target0.90_mlasso` | 641 | `p50_cindex0.70_hazard2.00_censor0.10_target0.95_mlasso` | 2,885 | 4.50× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.70_hazard2.00_censor0.50_target0.90_mlasso` | 969 | `p50_cindex0.70_hazard2.00_censor0.50_target0.95_mlasso` | 5,716 | 5.90× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.75_hazard0.50_censor0.10_target0.90_mlasso` | 510 | `p50_cindex0.75_hazard0.50_censor0.10_target0.95_mlasso` | 1,821 | 3.57× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p50_cindex0.75_hazard0.50_censor0.50_target0.85_mlasso` | 378 | `p50_cindex0.75_hazard0.50_censor0.50_target0.90_mlasso` | 1,333 | 3.53× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p50_cindex0.75_hazard1.00_censor0.10_target0.85_mlasso` | 185 | `p50_cindex0.75_hazard1.00_censor0.10_target0.90_mlasso` | 741 | 4.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.75_hazard1.00_censor0.30_target0.90_mlasso` | 525 | `p50_cindex0.75_hazard1.00_censor0.30_target0.95_mlasso` | 2,315 | 4.41× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.75_hazard1.00_censor0.50_target0.90_mlasso` | 679 | `p50_cindex0.75_hazard1.00_censor0.50_target0.95_mlasso` | 3,244 | 4.78× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.75_hazard2.00_censor0.30_target0.90_mlasso` | 561 | `p50_cindex0.75_hazard2.00_censor0.30_target0.95_mlasso` | 2,416 | 4.31× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.75_hazard2.00_censor0.50_target0.90_mlasso` | 631 | `p50_cindex0.75_hazard2.00_censor0.50_target0.95_mlasso` | 2,666 | 4.23× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.80_hazard0.50_censor0.10_target0.90_mlasso` | 359 | `p50_cindex0.80_hazard0.50_censor0.10_target0.95_mlasso` | 1,492 | 4.16× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.80_hazard0.50_censor0.30_target0.90_mlasso` | 446 | `p50_cindex0.80_hazard0.50_censor0.30_target0.95_mlasso` | 1,437 | 3.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.80_hazard0.50_censor0.50_target0.90_mlasso` | 625 | `p50_cindex0.80_hazard0.50_censor0.50_target0.95_mlasso` | 2,500 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.80_hazard1.00_censor0.10_target0.90_mlasso` | 347 | `p50_cindex0.80_hazard1.00_censor0.10_target0.95_mlasso` | 1,390 | 4.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.80_hazard1.00_censor0.30_target0.90_mlasso` | 467 | `p50_cindex0.80_hazard1.00_censor0.30_target0.95_mlasso` | 1,771 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.80_hazard1.00_censor0.50_target0.90_mlasso` | 625 | `p50_cindex0.80_hazard1.00_censor0.50_target0.95_mlasso` | 2,306 | 3.69× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.80_hazard2.00_censor0.10_target0.90_mlasso` | 374 | `p50_cindex0.80_hazard2.00_censor0.10_target0.95_mlasso` | 1,487 | 3.98× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.80_hazard2.00_censor0.30_target0.90_mlasso` | 435 | `p50_cindex0.80_hazard2.00_censor0.30_target0.95_mlasso` | 1,837 | 4.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.80_hazard2.00_censor0.50_target0.90_mlasso` | 512 | `p50_cindex0.80_hazard2.00_censor0.50_target0.95_mlasso` | 2,516 | 4.91× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.85_hazard0.50_censor0.10_target0.90_mlasso` | 419 | `p50_cindex0.85_hazard0.50_censor0.10_target0.95_mlasso` | 1,308 | 3.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.85_hazard1.00_censor0.30_target0.90_mlasso` | 420 | `p50_cindex0.85_hazard1.00_censor0.30_target0.95_mlasso` | 1,279 | 3.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.85_hazard1.00_censor0.50_target0.90_mlasso` | 559 | `p50_cindex0.85_hazard1.00_censor0.50_target0.95_mlasso` | 1,840 | 3.29× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.85_hazard2.00_censor0.30_target0.90_mlasso` | 426 | `p50_cindex0.85_hazard2.00_censor0.30_target0.95_mlasso` | 1,299 | 3.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.85_hazard2.00_censor0.50_target0.90_mlasso` | 294 | `p50_cindex0.85_hazard2.00_censor0.50_target0.95_mlasso` | 1,795 | 6.11× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.60_hazard0.50_censor0.30_target0.90_mlasso` | 1,657 | `p5_cindex0.60_hazard0.50_censor0.30_target0.95_mlasso` | 7,441 | 4.49× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.60_hazard0.50_censor0.50_target0.90_mlasso` | 2,672 | `p5_cindex0.60_hazard0.50_censor0.50_target0.95_mlasso` | 10,688 | 4.00× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.60_hazard1.00_censor0.50_target0.85_mlasso` | 1,336 | `p5_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | 10,688 | 8.00× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p5_cindex0.60_hazard2.00_censor0.10_target0.90_mlasso` | 734 | `p5_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 2,976 | 4.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.60_hazard2.00_censor0.50_target0.90_mlasso` | 1,682 | `p5_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 16,139 | 9.60× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard0.50_censor0.10_target0.90_mlasso` | 639 | `p5_cindex0.65_hazard0.50_censor0.10_target0.95_mlasso` | 5,504 | 8.61× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard0.50_censor0.30_target0.90_mlasso` | 872 | `p5_cindex0.65_hazard0.50_censor0.30_target0.95_mlasso` | 5,135 | 5.89× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard0.50_censor0.50_target0.90_mlasso` | 903 | `p5_cindex0.65_hazard0.50_censor0.50_target0.95_mlasso` | 4,865 | 5.39× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard1.00_censor0.10_target0.90_mlasso` | 601 | `p5_cindex0.65_hazard1.00_censor0.10_target0.95_mlasso` | 3,230 | 5.37× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.65_hazard1.00_censor0.30_target0.85_mlasso` | 208 | `p5_cindex0.65_hazard1.00_censor0.30_target0.90_mlasso` | 728 | 3.50× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard1.00_censor0.30_target0.90_mlasso` | 728 | `p5_cindex0.65_hazard1.00_censor0.30_target0.95_mlasso` | 3,488 | 4.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard1.00_censor0.50_target0.90_mlasso` | 1,152 | `p5_cindex0.65_hazard1.00_censor0.50_target0.95_mlasso` | 4,928 | 4.28× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard2.00_censor0.50_target0.90_mlasso` | 784 | `p5_cindex0.65_hazard2.00_censor0.50_target0.95_mlasso` | 3,773 | 4.81× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.70_hazard0.50_censor0.10_target0.90_mlasso` | 320 | `p5_cindex0.70_hazard0.50_censor0.10_target0.95_mlasso` | 1,280 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.70_hazard0.50_censor0.30_target0.90_mlasso` | 414 | `p5_cindex0.70_hazard0.50_censor0.30_target0.95_mlasso` | 3,350 | 8.09× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.70_hazard0.50_censor0.50_target0.90_mlasso` | 604 | `p5_cindex0.70_hazard0.50_censor0.50_target0.95_mlasso` | 2,156 | 3.57× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.70_hazard1.00_censor0.10_target0.90_mlasso` | 336 | `p5_cindex0.70_hazard1.00_censor0.10_target0.95_mlasso` | 1,993 | 5.93× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.70_hazard1.00_censor0.30_target0.90_mlasso` | 409 | `p5_cindex0.70_hazard1.00_censor0.30_target0.95_mlasso` | 1,616 | 3.95× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p5_cindex0.70_hazard2.00_censor0.10_target0.80_mlasso` | 80 | `p5_cindex0.70_hazard2.00_censor0.10_target0.85_mlasso` | 320 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.70_hazard2.00_censor0.10_target0.90_mlasso` | 320 | `p5_cindex0.70_hazard2.00_censor0.10_target0.95_mlasso` | 1,203 | 3.76× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.70_hazard2.00_censor0.30_target0.90_mlasso` | 394 | `p5_cindex0.70_hazard2.00_censor0.30_target0.95_mlasso` | 1,899 | 4.82× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.70_hazard2.00_censor0.50_target0.90_mlasso` | 572 | `p5_cindex0.70_hazard2.00_censor0.50_target0.95_mlasso` | 3,422 | 5.98× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.75_hazard0.50_censor0.50_target0.90_mlasso` | 346 | `p5_cindex0.75_hazard0.50_censor0.50_target0.95_mlasso` | 2,535 | 7.33× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.75_hazard1.00_censor0.50_target0.90_mlasso` | 352 | `p5_cindex0.75_hazard1.00_censor0.50_target0.95_mlasso` | 2,128 | 6.05× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.75_hazard2.00_censor0.10_target0.90_mlasso` | 150 | `p5_cindex0.75_hazard2.00_censor0.10_target0.95_mlasso` | 1,226 | 8.17× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.75_hazard2.00_censor0.50_target0.85_mlasso` | 176 | `p5_cindex0.75_hazard2.00_censor0.50_target0.90_mlasso` | 532 | 3.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.75_hazard2.00_censor0.50_target0.90_mlasso` | 532 | `p5_cindex0.75_hazard2.00_censor0.50_target0.95_mlasso` | 2,128 | 4.00× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.80_hazard0.50_censor0.10_target0.85_mlasso` | 87 | `p5_cindex0.80_hazard0.50_censor0.10_target0.90_mlasso` | 280 | 3.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.80_hazard0.50_censor0.30_target0.90_mlasso` | 171 | `p5_cindex0.80_hazard0.50_censor0.30_target0.95_mlasso` | 712 | 4.16× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.80_hazard0.50_censor0.50_target0.90_mlasso` | 223 | `p5_cindex0.80_hazard0.50_censor0.50_target0.95_mlasso` | 2,000 | 8.97× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.80_hazard1.00_censor0.10_target0.90_mlasso` | 174 | `p5_cindex0.80_hazard1.00_censor0.10_target0.95_mlasso` | 844 | 4.85× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.80_hazard1.00_censor0.30_target0.90_mlasso` | 178 | `p5_cindex0.80_hazard1.00_censor0.30_target0.95_mlasso` | 1,424 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.80_hazard1.00_censor0.50_target0.90_mlasso` | 250 | `p5_cindex0.80_hazard1.00_censor0.50_target0.95_mlasso` | 1,433 | 5.73× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.80_hazard2.00_censor0.10_target0.90_mlasso` | 207 | `p5_cindex0.80_hazard2.00_censor0.10_target0.95_mlasso` | 1,265 | 6.11× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.80_hazard2.00_censor0.30_target0.85_mlasso` | 68 | `p5_cindex0.80_hazard2.00_censor0.30_target0.90_mlasso` | 356 | 5.24× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.80_hazard2.00_censor0.50_target0.90_mlasso` | 250 | `p5_cindex0.80_hazard2.00_censor0.50_target0.95_mlasso` | 2,000 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.85_hazard0.50_censor0.10_target0.90_mlasso` | 145 | `p5_cindex0.85_hazard0.50_censor0.10_target0.95_mlasso` | 1,056 | 7.28× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.85_hazard0.50_censor0.30_target0.90_mlasso` | 163 | `p5_cindex0.85_hazard0.50_censor0.30_target0.95_mlasso` | 1,037 | 6.36× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.85_hazard0.50_censor0.50_target0.90_mlasso` | 222 | `p5_cindex0.85_hazard0.50_censor0.50_target0.95_mlasso` | 1,888 | 8.50× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.85_hazard1.00_censor0.10_target0.90_mlasso` | 163 | `p5_cindex0.85_hazard1.00_censor0.10_target0.95_mlasso` | 528 | 3.24× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.85_hazard1.00_censor0.50_target0.90_mlasso` | 222 | `p5_cindex0.85_hazard1.00_censor0.50_target0.95_mlasso` | 2,122 | 9.56× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.85_hazard2.00_censor0.10_target0.90_mlasso` | 157 | `p5_cindex0.85_hazard2.00_censor0.10_target0.95_mlasso` | 528 | 3.36× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.85_hazard2.00_censor0.50_target0.90_mlasso` | 118 | `p5_cindex0.85_hazard2.00_censor0.50_target0.95_mlasso` | 716 | 6.07× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.60_hazard0.50_censor0.10_target0.90_mlasso` | 2,630 | `p75_cindex0.60_hazard0.50_censor0.10_target0.95_mlasso` | 11,104 | 4.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.60_hazard0.50_censor0.50_target0.90_mlasso` | 5,541 | `p75_cindex0.60_hazard0.50_censor0.50_target0.95_mlasso` | 20,312 | 3.67× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.60_hazard1.00_censor0.50_target0.90_mlasso` | 4,580 | `p75_cindex0.60_hazard1.00_censor0.50_target0.95_mlasso` | 20,252 | 4.42× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.60_hazard2.00_censor0.10_target0.90_mlasso` | 2,816 | `p75_cindex0.60_hazard2.00_censor0.10_target0.95_mlasso` | 22,208 | 7.89× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p75_cindex0.60_hazard2.00_censor0.30_target0.90_mlasso` | 3,500 | `p75_cindex0.60_hazard2.00_censor0.30_target0.95_mlasso` | 14,425 | 4.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.60_hazard2.00_censor0.50_target0.90_mlasso` | 5,000 | `p75_cindex0.60_hazard2.00_censor0.50_target0.95_mlasso` | 22,216 | 4.44× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.65_hazard0.50_censor0.10_target0.90_mlasso` | 1,268 | `p75_cindex0.65_hazard0.50_censor0.10_target0.95_mlasso` | 5,494 | 4.33× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p75_cindex0.65_hazard0.50_censor0.30_target0.85_mlasso` | 412 | `p75_cindex0.65_hazard0.50_censor0.30_target0.90_mlasso` | 1,656 | 4.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.65_hazard0.50_censor0.30_target0.90_mlasso` | 1,656 | `p75_cindex0.65_hazard0.50_censor0.30_target0.95_mlasso` | 6,600 | 3.99× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.65_hazard0.50_censor0.50_target0.90_mlasso` | 2,332 | `p75_cindex0.65_hazard0.50_censor0.50_target0.95_mlasso` | 10,074 | 4.32× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.65_hazard1.00_censor0.30_target0.90_mlasso` | 1,648 | `p75_cindex0.65_hazard1.00_censor0.30_target0.95_mlasso` | 4,975 | 3.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.65_hazard1.00_censor0.50_target0.90_mlasso` | 2,117 | `p75_cindex0.65_hazard1.00_censor0.50_target0.95_mlasso` | 7,947 | 3.75× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.65_hazard2.00_censor0.10_target0.90_mlasso` | 1,282 | `p75_cindex0.65_hazard2.00_censor0.10_target0.95_mlasso` | 3,938 | 3.07× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.65_hazard2.00_censor0.30_target0.90_mlasso` | 1,662 | `p75_cindex0.65_hazard2.00_censor0.30_target0.95_mlasso` | 6,594 | 3.97× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.70_hazard0.50_censor0.10_target0.90_mlasso` | 738 | `p75_cindex0.70_hazard0.50_censor0.10_target0.95_mlasso` | 3,750 | 5.08× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.70_hazard0.50_censor0.30_target0.90_mlasso` | 765 | `p75_cindex0.70_hazard0.50_censor0.30_target0.95_mlasso` | 2,313 | 3.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.70_hazard0.50_censor0.50_target0.90_mlasso` | 1,213 | `p75_cindex0.70_hazard0.50_censor0.50_target0.95_mlasso` | 8,572 | 7.07× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.70_hazard1.00_censor0.30_target0.90_mlasso` | 519 | `p75_cindex0.70_hazard1.00_censor0.30_target0.95_mlasso` | 4,166 | 8.03× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.70_hazard1.00_censor0.50_target0.90_mlasso` | 1,374 | `p75_cindex0.70_hazard1.00_censor0.50_target0.95_mlasso` | 4,190 | 3.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.70_hazard2.00_censor0.10_target0.90_mlasso` | 813 | `p75_cindex0.70_hazard2.00_censor0.10_target0.95_mlasso` | 3,286 | 4.04× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.70_hazard2.00_censor0.30_target0.90_mlasso` | 910 | `p75_cindex0.70_hazard2.00_censor0.30_target0.95_mlasso` | 3,637 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.70_hazard2.00_censor0.50_target0.90_mlasso` | 1,078 | `p75_cindex0.70_hazard2.00_censor0.50_target0.95_mlasso` | 4,286 | 3.98× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.75_hazard0.50_censor0.10_target0.90_mlasso` | 555 | `p75_cindex0.75_hazard0.50_censor0.10_target0.95_mlasso` | 2,783 | 5.01× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.75_hazard0.50_censor0.30_target0.90_mlasso` | 583 | `p75_cindex0.75_hazard0.50_censor0.30_target0.95_mlasso` | 3,378 | 5.79× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.75_hazard1.00_censor0.10_target0.90_mlasso` | 558 | `p75_cindex0.75_hazard1.00_censor0.10_target0.95_mlasso` | 2,782 | 4.99× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.75_hazard1.00_censor0.30_target0.90_mlasso` | 570 | `p75_cindex0.75_hazard1.00_censor0.30_target0.95_mlasso` | 2,680 | 4.70× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.75_hazard1.00_censor0.50_target0.90_mlasso` | 766 | `p75_cindex0.75_hazard1.00_censor0.50_target0.95_mlasso` | 3,949 | 5.16× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.75_hazard2.00_censor0.10_target0.90_mlasso` | 479 | `p75_cindex0.75_hazard2.00_censor0.10_target0.95_mlasso` | 3,147 | 6.57× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.75_hazard2.00_censor0.30_target0.90_mlasso` | 621 | `p75_cindex0.75_hazard2.00_censor0.30_target0.95_mlasso` | 2,821 | 4.54× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.75_hazard2.00_censor0.50_target0.90_mlasso` | 761 | `p75_cindex0.75_hazard2.00_censor0.50_target0.95_mlasso` | 3,860 | 5.07× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.80_hazard0.50_censor0.10_target0.90_mlasso` | 431 | `p75_cindex0.80_hazard0.50_censor0.10_target0.95_mlasso` | 2,209 | 5.13× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.80_hazard0.50_censor0.30_target0.90_mlasso` | 669 | `p75_cindex0.80_hazard0.50_censor0.30_target0.95_mlasso` | 2,734 | 4.09× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.80_hazard0.50_censor0.50_target0.90_mlasso` | 562 | `p75_cindex0.80_hazard0.50_censor0.50_target0.95_mlasso` | 3,304 | 5.88× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.80_hazard1.00_censor0.10_target0.90_mlasso` | 520 | `p75_cindex0.80_hazard1.00_censor0.10_target0.95_mlasso` | 2,352 | 4.52× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.80_hazard1.00_censor0.30_target0.90_mlasso` | 449 | `p75_cindex0.80_hazard1.00_censor0.30_target0.95_mlasso` | 2,678 | 5.96× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.80_hazard1.00_censor0.50_target0.90_mlasso` | 487 | `p75_cindex0.80_hazard1.00_censor0.50_target0.95_mlasso` | 3,101 | 6.37× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.80_hazard2.00_censor0.10_target0.90_mlasso` | 520 | `p75_cindex0.80_hazard2.00_censor0.10_target0.95_mlasso` | 1,941 | 3.73× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.80_hazard2.00_censor0.30_target0.90_mlasso` | 520 | `p75_cindex0.80_hazard2.00_censor0.30_target0.95_mlasso` | 2,713 | 5.22× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.80_hazard2.00_censor0.50_target0.90_mlasso` | 589 | `p75_cindex0.80_hazard2.00_censor0.50_target0.95_mlasso` | 3,554 | 6.03× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.85_hazard0.50_censor0.10_target0.90_mlasso` | 473 | `p75_cindex0.85_hazard0.50_censor0.10_target0.95_mlasso` | 1,628 | 3.44× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.85_hazard0.50_censor0.50_target0.90_mlasso` | 441 | `p75_cindex0.85_hazard0.50_censor0.50_target0.95_mlasso` | 2,697 | 6.12× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.85_hazard1.00_censor0.10_target0.90_mlasso` | 481 | `p75_cindex0.85_hazard1.00_censor0.10_target0.95_mlasso` | 1,584 | 3.29× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.85_hazard1.00_censor0.30_target0.90_mlasso` | 630 | `p75_cindex0.85_hazard1.00_censor0.30_target0.95_mlasso` | 2,520 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.85_hazard1.00_censor0.50_target0.90_mlasso` | 441 | `p75_cindex0.85_hazard1.00_censor0.50_target0.95_mlasso` | 2,851 | 6.46× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.85_hazard2.00_censor0.10_target0.90_mlasso` | 368 | `p75_cindex0.85_hazard2.00_censor0.10_target0.95_mlasso` | 1,652 | 4.49× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.85_hazard2.00_censor0.30_target0.90_mlasso` | 630 | `p75_cindex0.85_hazard2.00_censor0.30_target0.95_mlasso` | 2,060 | 3.27× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.85_hazard2.00_censor0.50_target0.90_mlasso` | 441 | `p75_cindex0.85_hazard2.00_censor0.50_target0.95_mlasso` | 2,750 | 6.24× higher | — | next | — |
| Baseline hazard: 0.5 → 1 | `p10_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 42,624 | `p10_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | 10,359 | 4.11× lower | — | previous | — |
| Baseline hazard: 1 → 2 | `p10_cindex0.60_hazard1.00_censor0.10_target0.95_mridge` | 11,840 | `p10_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | 2,960 | 4.00× lower | — | — | — |
| Baseline hazard: 1 → 2 | `p10_cindex0.80_hazard1.00_censor0.50_target0.95_mridge` | 1,000 | `p10_cindex0.80_hazard2.00_censor0.50_target0.95_mridge` | 6,045 | 6.04× higher | — | next | — |
| Baseline hazard: 1 → 2 | `p15_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 8,000 | `p15_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 2,000 | 4.00× lower | — | — | — |
| Baseline hazard: 1 → 2 | `p15_cindex0.80_hazard1.00_censor0.10_target0.95_mridge` | 836 | `p15_cindex0.80_hazard2.00_censor0.10_target0.95_mridge` | 3,465 | 4.14× higher | — | next | — |
| Baseline hazard: 0.5 → 1 | `p20_cindex0.60_hazard0.50_censor0.30_target0.95_mridge` | 61,056 | `p20_cindex0.60_hazard1.00_censor0.30_target0.95_mridge` | 6,458 | 9.45× lower | — | previous, next | next |
| Baseline hazard: 1 → 2 | `p20_cindex0.60_hazard1.00_censor0.30_target0.95_mridge` | 6,458 | `p20_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | 61,056 | 9.45× higher | — | previous, next | previous, next |
| Baseline hazard: 0.5 → 1 | `p20_cindex0.80_hazard0.50_censor0.30_target0.95_mridge` | 1,551 | `p20_cindex0.80_hazard1.00_censor0.30_target0.95_mridge` | 5,728 | 3.69× higher | — | next | next |
| Baseline hazard: 1 → 2 | `p20_cindex0.80_hazard1.00_censor0.30_target0.95_mridge` | 5,728 | `p20_cindex0.80_hazard2.00_censor0.30_target0.95_mridge` | 1,439 | 3.98× lower | — | previous | previous |
| Baseline hazard: 1 → 2 | `p25_cindex0.60_hazard1.00_censor0.10_target0.95_mridge` | 15,956 | `p25_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | 59,264 | 3.71× higher | — | next | next |
| Baseline hazard: 1 → 2 | `p25_cindex0.85_hazard1.00_censor0.30_target0.95_mridge` | 1,680 | `p25_cindex0.85_hazard2.00_censor0.30_target0.95_mridge` | 6,720 | 4.00× higher | — | next | next |
| Baseline hazard: 0.5 → 1 | `p30_cindex0.60_hazard0.50_censor0.50_target0.90_mridge` | 8,000 | `p30_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 32,000 | 4.00× higher | — | next | — |
| Baseline hazard: 0.5 → 1 | `p30_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 15,083 | `p30_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | 128,000 | 8.49× higher | — | next | next |
| Baseline hazard: 1 → 2 | `p30_cindex0.60_hazard1.00_censor0.30_target0.90_mridge` | 11,440 | `p30_cindex0.60_hazard2.00_censor0.30_target0.90_mridge` | 2,860 | 4.00× lower | — | — | — |
| Baseline hazard: 1 → 2 | `p30_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | 128,000 | `p30_cindex0.60_hazard2.00_censor0.50_target0.95_mridge` | 33,928 | 3.77× lower | — | previous | previous |
| Baseline hazard: 0.5 → 1 | `p30_cindex0.65_hazard0.50_censor0.10_target0.90_mridge` | 4,096 | `p30_cindex0.65_hazard1.00_censor0.10_target0.90_mridge` | 1,024 | 4.00× lower | — | previous | — |
| Baseline hazard: 1 → 2 | `p30_cindex0.70_hazard1.00_censor0.50_target0.95_mridge` | 6,166 | `p30_cindex0.70_hazard2.00_censor0.50_target0.95_mridge` | 27,424 | 4.45× higher | — | next | next |
| Baseline hazard: 0.5 → 1 | `p30_cindex0.75_hazard0.50_censor0.50_target0.80_mridge` | 200 | `p30_cindex0.75_hazard1.00_censor0.50_target0.80_mridge` | 800 | 4.00× higher | — | — | — |
| Baseline hazard: 1 → 2 | `p30_cindex0.85_hazard1.00_censor0.30_target0.95_mridge` | 8,080 | `p30_cindex0.85_hazard2.00_censor0.30_target0.95_mridge` | 2,020 | 4.00× lower | — | previous | previous |
| Baseline hazard: 1 → 2 | `p30_cindex0.85_hazard1.00_censor0.50_target0.95_mridge` | 5,648 | `p30_cindex0.85_hazard2.00_censor0.50_target0.95_mridge` | 1,578 | 3.58× lower | — | previous | previous |
| Baseline hazard: 1 → 2 | `p40_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 5,250 | `p40_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 21,328 | 4.06× higher | — | — | — |
| Baseline hazard: 1 → 2 | `p5_cindex0.60_hazard1.00_censor0.30_target0.95_mridge` | 7,552 | `p5_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | 30,458 | 4.03× higher | — | next | — |
| Baseline hazard: 0.5 → 1 | `p5_cindex0.70_hazard0.50_censor0.50_target0.95_mridge` | 1,144 | `p5_cindex0.70_hazard1.00_censor0.50_target0.95_mridge` | 3,643 | 3.18× higher | — | — | — |
| Baseline hazard: 0.5 → 1 | `p5_cindex0.75_hazard0.50_censor0.50_target0.95_mridge` | 1,064 | `p5_cindex0.75_hazard1.00_censor0.50_target0.95_mridge` | 4,078 | 3.83× higher | — | next | — |
| Baseline hazard: 1 → 2 | `p5_cindex0.75_hazard1.00_censor0.10_target0.95_mridge` | 1,200 | `p5_cindex0.75_hazard2.00_censor0.10_target0.95_mridge` | 4,800 | 4.00× higher | — | next | next |
| Baseline hazard: 0.5 → 1 | `p5_cindex0.80_hazard0.50_censor0.10_target0.95_mridge` | 4,480 | `p5_cindex0.80_hazard1.00_censor0.10_target0.95_mridge` | 1,120 | 4.00× lower | — | previous | — |
| Baseline hazard: 0.5 → 1 | `p5_cindex0.80_hazard0.50_censor0.30_target0.95_mridge` | 5,590 | `p5_cindex0.80_hazard1.00_censor0.30_target0.95_mridge` | 1,424 | 3.93× lower | — | previous | — |
| Baseline hazard: 0.5 → 1 | `p5_cindex0.85_hazard0.50_censor0.10_target0.95_mridge` | 528 | `p5_cindex0.85_hazard1.00_censor0.10_target0.95_mridge` | 2,112 | 4.00× higher | — | — | — |
| Baseline hazard: 0.5 → 1 | `p5_cindex0.85_hazard0.50_censor0.50_target0.90_mridge` | 118 | `p5_cindex0.85_hazard1.00_censor0.50_target0.90_mridge` | 944 | 8.00× higher | — | next | — |
| Baseline hazard: 0.5 → 1 | `p5_cindex0.85_hazard0.50_censor0.50_target0.95_mridge` | 1,424 | `p5_cindex0.85_hazard1.00_censor0.50_target0.95_mridge` | 4,855 | 3.41× higher | — | previous, next | — |
| Baseline hazard: 1 → 2 | `p5_cindex0.85_hazard1.00_censor0.50_target0.90_mridge` | 944 | `p5_cindex0.85_hazard2.00_censor0.50_target0.90_mridge` | 236 | 4.00× lower | — | previous | — |
| Baseline hazard: 0.5 → 1 | `p75_cindex0.60_hazard0.50_censor0.50_target0.85_mridge` | 2,409 | `p75_cindex0.60_hazard1.00_censor0.50_target0.85_mridge` | 7,515 | 3.12× higher | — | — | — |
| Censoring: 0.3 → 0.5 | `p100_cindex0.65_hazard1.00_censor0.30_target0.90_mridge` | 4,088 | `p100_cindex0.65_hazard1.00_censor0.50_target0.90_mridge` | 12,308 | 3.01× higher | — | — | — |
| Censoring: 0.3 → 0.5 | `p10_cindex0.60_hazard0.50_censor0.30_target0.95_mridge` | 7,616 | `p10_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 42,624 | 5.60× higher | — | next | — |
| Censoring: 0.1 → 0.3 | `p10_cindex0.65_hazard2.00_censor0.10_target0.85_mridge` | 1,368 | `p10_cindex0.65_hazard2.00_censor0.30_target0.85_mridge` | 441 | 3.10× lower | — | — | — |
| Censoring: 0.3 → 0.5 | `p10_cindex0.75_hazard2.00_censor0.30_target0.95_mridge` | 1,246 | `p10_cindex0.75_hazard2.00_censor0.50_target0.95_mridge` | 4,272 | 3.43× higher | — | — | — |
| Censoring: 0.1 → 0.3 | `p10_cindex0.80_hazard0.50_censor0.10_target0.95_mridge` | 6,692 | `p10_cindex0.80_hazard0.50_censor0.30_target0.95_mridge` | 1,998 | 3.35× lower | — | previous, next | — |
| Censoring: 0.1 → 0.3 | `p10_cindex0.80_hazard2.00_censor0.10_target0.95_mridge` | 526 | `p10_cindex0.80_hazard2.00_censor0.30_target0.95_mridge` | 2,864 | 5.44× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p15_cindex0.75_hazard2.00_censor0.30_target0.95_mridge` | 1,744 | `p15_cindex0.75_hazard2.00_censor0.50_target0.95_mridge` | 6,400 | 3.67× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p15_cindex0.80_hazard0.50_censor0.30_target0.90_mridge` | 235 | `p15_cindex0.80_hazard0.50_censor0.50_target0.90_mridge` | 709 | 3.02× higher | — | — | — |
| Censoring: 0.1 → 0.3 | `p20_cindex0.60_hazard0.50_censor0.10_target0.95_mridge` | 9,445 | `p20_cindex0.60_hazard0.50_censor0.30_target0.95_mridge` | 61,056 | 6.46× higher | — | next | — |
| Censoring: 0.1 → 0.3 | `p20_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | 11,840 | `p20_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | 61,056 | 5.16× higher | — | next | next |
| Censoring: 0.3 → 0.5 | `p20_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | 61,056 | `p20_cindex0.60_hazard2.00_censor0.50_target0.95_mridge` | 10,672 | 5.72× lower | — | previous | previous |
| Censoring: 0.3 → 0.5 | `p20_cindex0.75_hazard0.50_censor0.30_target0.95_mridge` | 1,524 | `p20_cindex0.75_hazard0.50_censor0.50_target0.95_mridge` | 8,528 | 5.60× higher | — | next | next |
| Censoring: 0.1 → 0.3 | `p20_cindex0.80_hazard1.00_censor0.10_target0.95_mridge` | 1,112 | `p20_cindex0.80_hazard1.00_censor0.30_target0.95_mridge` | 5,728 | 5.15× higher | — | next | next |
| Censoring: 0.1 → 0.3 | `p25_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | 59,264 | `p25_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | 19,040 | 3.11× lower | — | previous | previous |
| Censoring: 0.1 → 0.3 | `p25_cindex0.65_hazard2.00_censor0.10_target0.95_mridge` | 5,008 | `p25_cindex0.65_hazard2.00_censor0.30_target0.95_mridge` | 17,568 | 3.51× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p25_cindex0.70_hazard0.50_censor0.30_target0.80_mridge` | 194 | `p25_cindex0.70_hazard0.50_censor0.50_target0.80_mridge` | 712 | 3.67× higher | — | — | — |
| Censoring: 0.1 → 0.3 | `p25_cindex0.85_hazard2.00_censor0.10_target0.95_mridge` | 1,308 | `p25_cindex0.85_hazard2.00_censor0.30_target0.95_mridge` | 6,720 | 5.14× higher | — | next | next |
| Censoring: 0.3 → 0.5 | `p25_cindex0.85_hazard2.00_censor0.30_target0.95_mridge` | 6,720 | `p25_cindex0.85_hazard2.00_censor0.50_target0.95_mridge` | 1,521 | 4.42× lower | — | previous | previous |
| Censoring: 0.3 → 0.5 | `p30_cindex0.60_hazard0.50_censor0.30_target0.85_mridge` | 1,430 | `p30_cindex0.60_hazard0.50_censor0.50_target0.85_mridge` | 4,593 | 3.21× higher | — | — | — |
| Censoring: 0.3 → 0.5 | `p30_cindex0.60_hazard2.00_censor0.30_target0.90_mridge` | 2,860 | `p30_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 16,000 | 5.59× higher | — | next | — |
| Censoring: 0.3 → 0.5 | `p30_cindex0.70_hazard2.00_censor0.30_target0.95_mridge` | 4,941 | `p30_cindex0.70_hazard2.00_censor0.50_target0.95_mridge` | 27,424 | 5.55× higher | — | next | next |
| Censoring: 0.1 → 0.3 | `p30_cindex0.85_hazard1.00_censor0.10_target0.95_mridge` | 1,174 | `p30_cindex0.85_hazard1.00_censor0.30_target0.95_mridge` | 8,080 | 6.88× higher | — | next | next |
| Censoring: 0.3 → 0.5 | `p40_cindex0.60_hazard0.50_censor0.30_target0.95_mridge` | 22,131 | `p40_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 85,312 | 3.85× higher | — | next | — |
| Censoring: 0.1 → 0.3 | `p40_cindex0.65_hazard0.50_censor0.10_target0.95_mridge` | 8,450 | `p40_cindex0.65_hazard0.50_censor0.30_target0.95_mridge` | 28,096 | 3.32× higher | — | next | — |
| Censoring: 0.1 → 0.3 | `p50_cindex0.60_hazard0.50_censor0.10_target0.85_mridge` | 1,273 | `p50_cindex0.60_hazard0.50_censor0.30_target0.85_mridge` | 4,760 | 3.74× higher | — | — | — |
| Censoring: 0.3 → 0.5 | `p50_cindex0.60_hazard1.00_censor0.30_target0.95_mridge` | 27,492 | `p50_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | 106,688 | 3.88× higher | — | next | — |
| Censoring: 0.1 → 0.3 | `p5_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | 5,885 | `p5_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | 30,458 | 5.18× higher | — | next | — |
| Censoring: 0.1 → 0.3 | `p5_cindex0.65_hazard2.00_censor0.10_target0.95_mridge` | 2,752 | `p5_cindex0.65_hazard2.00_censor0.30_target0.95_mridge` | 13,952 | 5.07× higher | — | next | next |
| Censoring: 0.3 → 0.5 | `p5_cindex0.65_hazard2.00_censor0.30_target0.95_mridge` | 13,952 | `p5_cindex0.65_hazard2.00_censor0.50_target0.95_mridge` | 2,464 | 5.66× lower | — | previous | previous |
| Censoring: 0.1 → 0.3 | `p5_cindex0.85_hazard0.50_censor0.10_target0.90_mridge` | 528 | `p5_cindex0.85_hazard0.50_censor0.30_target0.90_mridge` | 4,953 | 9.38× higher | — | previous, next | — |
| Censoring: 0.1 → 0.3 | `p5_cindex0.85_hazard0.50_censor0.10_target0.95_mridge` | 528 | `p5_cindex0.85_hazard0.50_censor0.30_target0.95_mridge` | 2,688 | 5.09× higher | — | next | — |
| Censoring: 0.1 → 0.3 | `p5_cindex0.85_hazard2.00_censor0.10_target0.90_mridge` | 1,347 | `p5_cindex0.85_hazard2.00_censor0.30_target0.90_mridge` | 6,603 | 4.90× higher | — | previous, next | — |
| C-index: 0.6 → 0.65 | `p100_cindex0.60_hazard0.50_censor0.30_target0.90_mridge` | 19,056 | `p100_cindex0.65_hazard0.50_censor0.30_target0.90_mridge` | 5,527 | 3.45× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p100_cindex0.60_hazard0.50_censor0.30_target0.95_mridge` | 76,224 | `p100_cindex0.65_hazard0.50_censor0.30_target0.95_mridge` | 8,361 | 9.12× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p100_cindex0.60_hazard1.00_censor0.30_target0.80_mridge` | 3,152 | `p100_cindex0.65_hazard1.00_censor0.30_target0.80_mridge` | 812 | 3.88× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p100_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | 106,656 | `p100_cindex0.65_hazard1.00_censor0.50_target0.95_mridge` | 23,968 | 4.45× lower | — | previous | — |
| C-index: 0.65 → 0.7 | `p100_cindex0.65_hazard1.00_censor0.30_target0.85_mridge` | 3,297 | `p100_cindex0.70_hazard1.00_censor0.30_target0.85_mridge` | 995 | 3.31× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p100_cindex0.65_hazard2.00_censor0.10_target0.80_mridge` | 1,181 | `p100_cindex0.70_hazard2.00_censor0.10_target0.80_mridge` | 381 | 3.10× lower | — | — | — |
| C-index: 0.7 → 0.75 | `p100_cindex0.70_hazard1.00_censor0.30_target0.95_mridge` | 16,328 | `p100_cindex0.75_hazard1.00_censor0.30_target0.95_mridge` | 5,324 | 3.07× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p10_cindex0.60_hazard0.50_censor0.10_target0.95_mridge` | 11,840 | `p10_cindex0.65_hazard0.50_censor0.10_target0.95_mridge` | 2,736 | 4.33× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p10_cindex0.60_hazard1.00_censor0.10_target0.95_mridge` | 11,840 | `p10_cindex0.65_hazard1.00_censor0.10_target0.95_mridge` | 3,406 | 3.48× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p10_cindex0.60_hazard1.00_censor0.30_target0.95_mridge` | 15,336 | `p10_cindex0.65_hazard1.00_censor0.30_target0.95_mridge` | 4,472 | 3.43× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p10_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 5,660 | `p10_cindex0.65_hazard1.00_censor0.50_target0.90_mridge` | 1,723 | 3.28× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p10_cindex0.60_hazard2.00_censor0.30_target0.85_mridge` | 1,494 | `p10_cindex0.65_hazard2.00_censor0.30_target0.85_mridge` | 441 | 3.39× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p10_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | 30,464 | `p10_cindex0.65_hazard2.00_censor0.30_target0.95_mridge` | 3,551 | 8.58× lower | — | previous | — |
| C-index: 0.65 → 0.7 | `p10_cindex0.65_hazard2.00_censor0.10_target0.85_mridge` | 1,368 | `p10_cindex0.70_hazard2.00_censor0.10_target0.85_mridge` | 318 | 4.30× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p10_cindex0.65_hazard2.00_censor0.10_target0.95_mridge` | 5,472 | `p10_cindex0.70_hazard2.00_censor0.10_target0.95_mridge` | 1,186 | 4.61× lower | — | previous | — |
| C-index: 0.7 → 0.75 | `p10_cindex0.70_hazard0.50_censor0.30_target0.80_mridge` | 298 | `p10_cindex0.75_hazard0.50_censor0.30_target0.80_mridge` | 95 | 3.14× lower | — | — | — |
| C-index: 0.75 → 0.8 | `p10_cindex0.75_hazard0.50_censor0.10_target0.95_mridge` | 1,184 | `p10_cindex0.80_hazard0.50_censor0.10_target0.95_mridge` | 6,692 | 5.65× higher | — | next | — |
| C-index: 0.8 → 0.85 | `p10_cindex0.80_hazard0.50_censor0.10_target0.95_mridge` | 6,692 | `p10_cindex0.85_hazard0.50_censor0.10_target0.95_mridge` | 2,096 | 3.19× lower | — | previous, next | — |
| C-index: 0.8 → 0.85 | `p10_cindex0.80_hazard1.00_censor0.10_target0.95_mridge` | 556 | `p10_cindex0.85_hazard1.00_censor0.10_target0.95_mridge` | 2,096 | 3.77× higher | — | next | — |
| C-index: 0.8 → 0.85 | `p10_cindex0.80_hazard2.00_censor0.50_target0.95_mridge` | 6,045 | `p10_cindex0.85_hazard2.00_censor0.50_target0.95_mridge` | 1,880 | 3.22× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p15_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 23,804 | `p15_cindex0.65_hazard0.50_censor0.50_target0.95_mridge` | 4,735 | 5.03× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p15_cindex0.60_hazard1.00_censor0.30_target0.85_mridge` | 2,539 | `p15_cindex0.65_hazard1.00_censor0.30_target0.85_mridge` | 630 | 4.03× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p15_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 8,000 | `p15_cindex0.65_hazard1.00_censor0.50_target0.90_mridge` | 2,511 | 3.19× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p15_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | 13,731 | `p15_cindex0.65_hazard2.00_censor0.10_target0.95_mridge` | 4,372 | 3.14× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p15_cindex0.65_hazard0.50_censor0.30_target0.95_mridge` | 10,528 | `p15_cindex0.70_hazard0.50_censor0.30_target0.95_mridge` | 2,389 | 4.41× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p15_cindex0.65_hazard1.00_censor0.10_target0.95_mridge` | 8,224 | `p15_cindex0.70_hazard1.00_censor0.10_target0.95_mridge` | 2,469 | 3.33× lower | — | previous | — |
| C-index: 0.65 → 0.7 | `p15_cindex0.65_hazard2.00_censor0.10_target0.85_mridge` | 1,028 | `p15_cindex0.70_hazard2.00_censor0.10_target0.85_mridge` | 241 | 4.27× lower | — | — | — |
| C-index: 0.75 → 0.8 | `p15_cindex0.75_hazard2.00_censor0.50_target0.95_mridge` | 6,400 | `p15_cindex0.80_hazard2.00_censor0.50_target0.95_mridge` | 1,658 | 3.86× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard0.50_censor0.30_target0.90_mridge` | 6,621 | `p20_cindex0.65_hazard0.50_censor0.30_target0.90_mridge` | 1,760 | 3.76× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard0.50_censor0.30_target0.95_mridge` | 61,056 | `p20_cindex0.65_hazard0.50_censor0.30_target0.95_mridge` | 7,040 | 8.67× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard0.50_censor0.50_target0.80_mridge` | 2,682 | `p20_cindex0.65_hazard0.50_censor0.50_target0.80_mridge` | 683 | 3.93× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard0.50_censor0.50_target0.90_mridge` | 10,672 | `p20_cindex0.65_hazard0.50_censor0.50_target0.90_mridge` | 3,143 | 3.40× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 30,431 | `p20_cindex0.65_hazard0.50_censor0.50_target0.95_mridge` | 9,840 | 3.09× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard1.00_censor0.30_target0.80_mridge` | 3,816 | `p20_cindex0.65_hazard1.00_censor0.30_target0.80_mridge` | 713 | 5.35× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard1.00_censor0.30_target0.90_mridge` | 5,709 | `p20_cindex0.65_hazard1.00_censor0.30_target0.90_mridge` | 1,706 | 3.35× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | 19,050 | `p20_cindex0.65_hazard1.00_censor0.50_target0.95_mridge` | 4,920 | 3.87× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard2.00_censor0.30_target0.80_mridge` | 1,401 | `p20_cindex0.65_hazard2.00_censor0.30_target0.80_mridge` | 430 | 3.26× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard2.00_censor0.30_target0.85_mridge` | 3,816 | `p20_cindex0.65_hazard2.00_censor0.30_target0.85_mridge` | 883 | 4.32× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | 61,056 | `p20_cindex0.65_hazard2.00_censor0.30_target0.95_mridge` | 6,937 | 8.80× lower | — | previous | previous |
| C-index: 0.6 → 0.65 | `p20_cindex0.60_hazard2.00_censor0.50_target0.85_mridge` | 3,938 | `p20_cindex0.65_hazard2.00_censor0.50_target0.85_mridge` | 986 | 3.99× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p20_cindex0.65_hazard0.50_censor0.10_target0.95_mridge` | 10,944 | `p20_cindex0.70_hazard0.50_censor0.10_target0.95_mridge` | 2,817 | 3.88× lower | — | previous | — |
| C-index: 0.65 → 0.7 | `p20_cindex0.65_hazard0.50_censor0.50_target0.95_mridge` | 9,840 | `p20_cindex0.70_hazard0.50_censor0.50_target0.95_mridge` | 2,265 | 4.34× lower | — | next | next |
| C-index: 0.7 → 0.75 | `p20_cindex0.70_hazard0.50_censor0.50_target0.95_mridge` | 2,265 | `p20_cindex0.75_hazard0.50_censor0.50_target0.95_mridge` | 8,528 | 3.77× higher | — | previous, next | previous, next |
| C-index: 0.75 → 0.8 | `p20_cindex0.75_hazard0.50_censor0.50_target0.95_mridge` | 8,528 | `p20_cindex0.80_hazard0.50_censor0.50_target0.95_mridge` | 2,215 | 3.85× lower | — | previous | previous |
| C-index: 0.6 → 0.65 | `p25_cindex0.60_hazard0.50_censor0.30_target0.80_mridge` | 2,380 | `p25_cindex0.65_hazard0.50_censor0.30_target0.80_mridge` | 787 | 3.02× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p25_cindex0.60_hazard0.50_censor0.30_target0.95_mridge` | 38,080 | `p25_cindex0.65_hazard0.50_censor0.30_target0.95_mridge` | 8,544 | 4.46× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p25_cindex0.60_hazard0.50_censor0.50_target0.90_mridge` | 13,328 | `p25_cindex0.65_hazard0.50_censor0.50_target0.90_mridge` | 4,082 | 3.27× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p25_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 42,949 | `p25_cindex0.65_hazard0.50_censor0.50_target0.95_mridge` | 12,055 | 3.56× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p25_cindex0.60_hazard1.00_censor0.30_target0.90_mridge` | 9,520 | `p25_cindex0.65_hazard1.00_censor0.30_target0.90_mridge` | 2,163 | 4.40× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p25_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | 34,851 | `p25_cindex0.65_hazard1.00_censor0.50_target0.95_mridge` | 11,536 | 3.02× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p25_cindex0.60_hazard2.00_censor0.10_target0.85_mridge` | 3,704 | `p25_cindex0.65_hazard2.00_censor0.10_target0.85_mridge` | 856 | 4.33× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p25_cindex0.60_hazard2.00_censor0.50_target0.85_mridge` | 6,664 | `p25_cindex0.65_hazard2.00_censor0.50_target0.85_mridge` | 1,855 | 3.59× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p25_cindex0.65_hazard0.50_censor0.30_target0.80_mridge` | 787 | `p25_cindex0.70_hazard0.50_censor0.30_target0.80_mridge` | 194 | 4.06× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p25_cindex0.65_hazard0.50_censor0.50_target0.90_mridge` | 4,082 | `p25_cindex0.70_hazard0.50_censor0.50_target0.90_mridge` | 1,239 | 3.29× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p25_cindex0.65_hazard1.00_censor0.50_target0.85_mridge` | 3,076 | `p25_cindex0.70_hazard1.00_censor0.50_target0.85_mridge` | 1,017 | 3.02× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p25_cindex0.65_hazard2.00_censor0.30_target0.95_mridge` | 17,568 | `p25_cindex0.70_hazard2.00_censor0.30_target0.95_mridge` | 4,567 | 3.85× lower | — | previous | — |
| C-index: 0.7 → 0.75 | `p25_cindex0.70_hazard0.50_censor0.50_target0.80_mridge` | 712 | `p25_cindex0.75_hazard0.50_censor0.50_target0.80_mridge` | 232 | 3.07× lower | — | — | — |
| C-index: 0.7 → 0.75 | `p25_cindex0.70_hazard2.00_censor0.10_target0.90_mridge` | 1,588 | `p25_cindex0.75_hazard2.00_censor0.10_target0.90_mridge` | 371 | 4.28× lower | — | — | — |
| C-index: 0.75 → 0.8 | `p25_cindex0.75_hazard0.50_censor0.30_target0.95_mridge` | 2,772 | `p25_cindex0.80_hazard0.50_censor0.30_target0.95_mridge` | 892 | 3.11× lower | — | — | — |
| C-index: 0.75 → 0.8 | `p25_cindex0.75_hazard0.50_censor0.50_target0.95_mridge` | 5,336 | `p25_cindex0.80_hazard0.50_censor0.50_target0.95_mridge` | 963 | 5.54× lower | — | previous | — |
| C-index: 0.8 → 0.85 | `p25_cindex0.80_hazard2.00_censor0.30_target0.95_mridge` | 1,784 | `p25_cindex0.85_hazard2.00_censor0.30_target0.95_mridge` | 6,720 | 3.77× higher | — | next | next |
| C-index: 0.6 → 0.65 | `p30_cindex0.60_hazard0.50_censor0.10_target0.85_mridge` | 2,808 | `p30_cindex0.65_hazard0.50_censor0.10_target0.85_mridge` | 512 | 5.48× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p30_cindex0.60_hazard1.00_censor0.10_target0.90_mridge` | 8,880 | `p30_cindex0.65_hazard1.00_censor0.10_target0.90_mridge` | 1,024 | 8.67× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p30_cindex0.60_hazard1.00_censor0.30_target0.90_mridge` | 11,440 | `p30_cindex0.65_hazard1.00_censor0.30_target0.90_mridge` | 2,796 | 4.09× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p30_cindex0.60_hazard1.00_censor0.30_target0.95_mridge` | 45,760 | `p30_cindex0.65_hazard1.00_censor0.30_target0.95_mridge` | 10,465 | 4.37× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p30_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 32,000 | `p30_cindex0.65_hazard1.00_censor0.50_target0.90_mridge` | 3,359 | 9.53× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p30_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | 128,000 | `p30_cindex0.65_hazard1.00_censor0.50_target0.95_mridge` | 13,494 | 9.49× lower | — | previous | previous |
| C-index: 0.6 → 0.65 | `p30_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 16,000 | `p30_cindex0.65_hazard2.00_censor0.50_target0.90_mridge` | 4,172 | 3.84× lower | — | previous | — |
| C-index: 0.65 → 0.7 | `p30_cindex0.65_hazard0.50_censor0.10_target0.90_mridge` | 4,096 | `p30_cindex0.70_hazard0.50_censor0.10_target0.90_mridge` | 983 | 4.17× lower | — | previous | — |
| C-index: 0.65 → 0.7 | `p30_cindex0.65_hazard0.50_censor0.30_target0.85_mridge` | 1,429 | `p30_cindex0.70_hazard0.50_censor0.30_target0.85_mridge` | 458 | 3.12× lower | — | — | — |
| C-index: 0.7 → 0.75 | `p30_cindex0.70_hazard1.00_censor0.30_target0.90_mridge` | 1,618 | `p30_cindex0.75_hazard1.00_censor0.30_target0.90_mridge` | 484 | 3.34× lower | — | — | — |
| C-index: 0.7 → 0.75 | `p30_cindex0.70_hazard2.00_censor0.50_target0.95_mridge` | 27,424 | `p30_cindex0.75_hazard2.00_censor0.50_target0.95_mridge` | 6,400 | 4.29× lower | — | previous | previous |
| C-index: 0.8 → 0.85 | `p30_cindex0.80_hazard1.00_censor0.30_target0.95_mridge` | 1,945 | `p30_cindex0.85_hazard1.00_censor0.30_target0.95_mridge` | 8,080 | 4.15× higher | — | next | next |
| C-index: 0.6 → 0.65 | `p40_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 85,312 | `p40_cindex0.65_hazard0.50_censor0.50_target0.95_mridge` | 14,512 | 5.88× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p40_cindex0.60_hazard1.00_censor0.50_target0.80_mridge` | 3,264 | `p40_cindex0.65_hazard1.00_censor0.50_target0.80_mridge` | 1,068 | 3.06× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p40_cindex0.60_hazard2.00_censor0.50_target0.80_mridge` | 5,332 | `p40_cindex0.65_hazard2.00_censor0.50_target0.80_mridge` | 1,248 | 4.27× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p40_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 21,328 | `p40_cindex0.65_hazard2.00_censor0.50_target0.90_mridge` | 4,622 | 4.61× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p40_cindex0.60_hazard2.00_censor0.50_target0.95_mridge` | 36,025 | `p40_cindex0.65_hazard2.00_censor0.50_target0.95_mridge` | 9,848 | 3.66× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p40_cindex0.65_hazard0.50_censor0.30_target0.95_mridge` | 28,096 | `p40_cindex0.70_hazard0.50_censor0.30_target0.95_mridge` | 6,550 | 4.29× lower | — | previous | — |
| C-index: 0.7 → 0.75 | `p40_cindex0.70_hazard2.00_censor0.50_target0.80_mridge` | 818 | `p40_cindex0.75_hazard2.00_censor0.50_target0.80_mridge` | 266 | 3.08× lower | — | — | — |
| C-index: 0.75 → 0.8 | `p40_cindex0.75_hazard0.50_censor0.50_target0.95_mridge` | 8,536 | `p40_cindex0.80_hazard0.50_censor0.50_target0.95_mridge` | 1,793 | 4.76× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p50_cindex0.60_hazard0.50_censor0.10_target0.95_mridge` | 23,134 | `p50_cindex0.65_hazard0.50_censor0.10_target0.95_mridge` | 6,840 | 3.38× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p50_cindex0.60_hazard0.50_censor0.30_target0.85_mridge` | 4,760 | `p50_cindex0.65_hazard0.50_censor0.30_target0.85_mridge` | 1,098 | 4.34× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p50_cindex0.60_hazard1.00_censor0.10_target0.90_mridge` | 14,832 | `p50_cindex0.65_hazard1.00_censor0.10_target0.90_mridge` | 3,041 | 4.88× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p50_cindex0.60_hazard1.00_censor0.30_target0.95_mridge` | 27,492 | `p50_cindex0.65_hazard1.00_censor0.30_target0.95_mridge` | 8,437 | 3.26× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p50_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | 106,688 | `p50_cindex0.65_hazard1.00_censor0.50_target0.95_mridge` | 17,921 | 5.95× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p50_cindex0.60_hazard2.00_censor0.10_target0.80_mridge` | 3,708 | `p50_cindex0.65_hazard2.00_censor0.10_target0.80_mridge` | 782 | 4.74× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p50_cindex0.65_hazard2.00_censor0.50_target0.90_mridge` | 6,152 | `p50_cindex0.70_hazard2.00_censor0.50_target0.90_mridge` | 1,429 | 4.31× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p5_cindex0.60_hazard0.50_censor0.10_target0.90_mridge` | 2,976 | `p5_cindex0.65_hazard0.50_censor0.10_target0.90_mridge` | 721 | 4.13× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p5_cindex0.60_hazard0.50_censor0.10_target0.95_mridge` | 21,361 | `p5_cindex0.65_hazard0.50_censor0.10_target0.95_mridge` | 4,076 | 5.24× lower | — | previous, next | — |
| C-index: 0.6 → 0.65 | `p5_cindex0.60_hazard0.50_censor0.30_target0.90_mridge` | 3,776 | `p5_cindex0.65_hazard0.50_censor0.30_target0.90_mridge` | 924 | 4.09× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p5_cindex0.60_hazard1.00_censor0.10_target0.95_mridge` | 11,904 | `p5_cindex0.65_hazard1.00_censor0.10_target0.95_mridge` | 2,752 | 4.33× lower | — | previous | — |
| C-index: 0.6 → 0.65 | `p5_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 5,344 | `p5_cindex0.65_hazard2.00_censor0.50_target0.90_mridge` | 1,357 | 3.94× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p5_cindex0.65_hazard0.50_censor0.10_target0.95_mridge` | 4,076 | `p5_cindex0.70_hazard0.50_censor0.10_target0.95_mridge` | 1,280 | 3.18× lower | — | previous | — |
| C-index: 0.65 → 0.7 | `p5_cindex0.65_hazard0.50_censor0.30_target0.95_mridge` | 6,976 | `p5_cindex0.70_hazard0.50_censor0.30_target0.95_mridge` | 1,616 | 4.32× lower | — | previous | — |
| C-index: 0.65 → 0.7 | `p5_cindex0.65_hazard0.50_censor0.50_target0.95_mridge` | 4,928 | `p5_cindex0.70_hazard0.50_censor0.50_target0.95_mridge` | 1,144 | 4.31× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p5_cindex0.65_hazard1.00_censor0.30_target0.95_mridge` | 7,174 | `p5_cindex0.70_hazard1.00_censor0.30_target0.95_mridge` | 1,616 | 4.44× lower | — | previous | — |
| C-index: 0.65 → 0.7 | `p5_cindex0.65_hazard1.00_censor0.50_target0.85_mridge` | 819 | `p5_cindex0.70_hazard1.00_censor0.50_target0.85_mridge` | 268 | 3.06× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p5_cindex0.65_hazard2.00_censor0.10_target0.80_mridge` | 243 | `p5_cindex0.70_hazard2.00_censor0.10_target0.80_mridge` | 80 | 3.04× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p5_cindex0.65_hazard2.00_censor0.30_target0.95_mridge` | 13,952 | `p5_cindex0.70_hazard2.00_censor0.30_target0.95_mridge` | 3,232 | 4.32× lower | — | previous | previous |
| C-index: 0.7 → 0.75 | `p5_cindex0.70_hazard1.00_censor0.10_target0.85_mridge` | 320 | `p5_cindex0.75_hazard1.00_censor0.10_target0.85_mridge` | 70 | 4.57× lower | — | — | — |
| C-index: 0.7 → 0.75 | `p5_cindex0.70_hazard2.00_censor0.10_target0.95_mridge` | 1,280 | `p5_cindex0.75_hazard2.00_censor0.10_target0.95_mridge` | 4,800 | 3.75× higher | — | next | next |
| C-index: 0.75 → 0.8 | `p5_cindex0.75_hazard0.50_censor0.10_target0.95_mridge` | 1,200 | `p5_cindex0.80_hazard0.50_censor0.10_target0.95_mridge` | 4,480 | 3.73× higher | — | next | — |
| C-index: 0.75 → 0.8 | `p5_cindex0.75_hazard1.00_censor0.50_target0.95_mridge` | 4,078 | `p5_cindex0.80_hazard1.00_censor0.50_target0.95_mridge` | 1,000 | 4.08× lower | — | previous, next | next |
| C-index: 0.75 → 0.8 | `p5_cindex0.75_hazard2.00_censor0.10_target0.95_mridge` | 4,800 | `p5_cindex0.80_hazard2.00_censor0.10_target0.95_mridge` | 1,120 | 4.29× lower | — | previous, next | previous, next |
| C-index: 0.8 → 0.85 | `p5_cindex0.80_hazard0.50_censor0.10_target0.95_mridge` | 4,480 | `p5_cindex0.85_hazard0.50_censor0.10_target0.95_mridge` | 528 | 8.48× lower | — | previous | — |
| C-index: 0.8 → 0.85 | `p5_cindex0.80_hazard0.50_censor0.50_target0.95_mridge` | 12,288 | `p5_cindex0.85_hazard0.50_censor0.50_target0.95_mridge` | 1,424 | 8.63× lower | — | previous, next | previous |
| C-index: 0.8 → 0.85 | `p5_cindex0.80_hazard1.00_censor0.30_target0.90_mridge` | 712 | `p5_cindex0.85_hazard1.00_censor0.30_target0.90_mridge` | 2,669 | 3.75× higher | — | previous, next | — |
| C-index: 0.8 → 0.85 | `p5_cindex0.80_hazard1.00_censor0.50_target0.95_mridge` | 1,000 | `p5_cindex0.85_hazard1.00_censor0.50_target0.95_mridge` | 4,855 | 4.86× higher | — | previous, next | previous |
| C-index: 0.8 → 0.85 | `p5_cindex0.80_hazard2.00_censor0.10_target0.90_mridge` | 232 | `p5_cindex0.85_hazard2.00_censor0.10_target0.90_mridge` | 1,347 | 5.81× higher | — | next | — |
| C-index: 0.8 → 0.85 | `p5_cindex0.80_hazard2.00_censor0.10_target0.95_mridge` | 1,120 | `p5_cindex0.85_hazard2.00_censor0.10_target0.95_mridge` | 4,111 | 3.67× higher | — | previous | previous |
| C-index: 0.6 → 0.65 | `p75_cindex0.60_hazard0.50_censor0.30_target0.90_mridge` | 10,970 | `p75_cindex0.65_hazard0.50_censor0.30_target0.90_mridge` | 3,296 | 3.33× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p75_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 80,000 | `p75_cindex0.65_hazard0.50_censor0.50_target0.95_mridge` | 22,634 | 3.53× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p75_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 20,000 | `p75_cindex0.65_hazard1.00_censor0.50_target0.90_mridge` | 6,539 | 3.06× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p75_cindex0.60_hazard2.00_censor0.30_target0.85_mridge` | 7,140 | `p75_cindex0.65_hazard2.00_censor0.30_target0.85_mridge` | 2,126 | 3.36× lower | — | — | — |
| C-index: 0.6 → 0.65 | `p75_cindex0.60_hazard2.00_censor0.50_target0.95_mridge` | 61,348 | `p75_cindex0.65_hazard2.00_censor0.50_target0.95_mridge` | 16,078 | 3.82× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p75_cindex0.65_hazard0.50_censor0.50_target0.90_mridge` | 9,232 | `p75_cindex0.70_hazard0.50_censor0.50_target0.90_mridge` | 2,939 | 3.14× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p75_cindex0.65_hazard1.00_censor0.30_target0.95_mridge` | 26,368 | `p75_cindex0.70_hazard1.00_censor0.30_target0.95_mridge` | 8,127 | 3.24× lower | — | — | — |
| C-index: 0.65 → 0.7 | `p75_cindex0.65_hazard1.00_censor0.50_target0.85_mridge` | 3,443 | `p75_cindex0.70_hazard1.00_censor0.50_target0.85_mridge` | 1,058 | 3.25× lower | — | — | — |
| Predictors: 10 → 15 | `p10_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | 2,960 | `p15_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | 13,731 | 4.64× higher | — | — | — |
| Predictors: 10 → 15 | `p10_cindex0.70_hazard1.00_censor0.30_target0.80_mridge` | 204 | `p15_cindex0.70_hazard1.00_censor0.30_target0.80_mridge` | 612 | 3.00× higher | — | — | — |
| Predictors: 10 → 15 | `p10_cindex0.80_hazard0.50_censor0.10_target0.95_mridge` | 6,692 | `p15_cindex0.80_hazard0.50_censor0.10_target0.95_mridge` | 787 | 8.50× lower | — | previous | — |
| Predictors: 10 → 15 | `p10_cindex0.80_hazard0.50_censor0.50_target0.95_mridge` | 978 | `p15_cindex0.80_hazard0.50_censor0.50_target0.95_mridge` | 3,000 | 3.07× higher | — | — | — |
| Predictors: 10 → 15 | `p10_cindex0.80_hazard2.00_censor0.10_target0.95_mridge` | 526 | `p15_cindex0.80_hazard2.00_censor0.10_target0.95_mridge` | 3,465 | 6.59× higher | — | next | — |
| Predictors: 10 → 15 | `p10_cindex0.80_hazard2.00_censor0.50_target0.95_mridge` | 6,045 | `p15_cindex0.80_hazard2.00_censor0.50_target0.95_mridge` | 1,658 | 3.65× lower | — | previous | — |
| Predictors: 15 → 20 | `p15_cindex0.60_hazard0.50_censor0.30_target0.95_mridge` | 11,424 | `p20_cindex0.60_hazard0.50_censor0.30_target0.95_mridge` | 61,056 | 5.34× higher | — | next | — |
| Predictors: 15 → 20 | `p15_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | 11,137 | `p20_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | 61,056 | 5.48× higher | — | next | next |
| Predictors: 15 → 20 | `p15_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 2,000 | `p20_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 8,265 | 4.13× higher | — | — | — |
| Predictors: 15 → 20 | `p15_cindex0.75_hazard2.00_censor0.50_target0.95_mridge` | 6,400 | `p20_cindex0.75_hazard2.00_censor0.50_target0.95_mridge` | 2,132 | 3.00× lower | — | previous | — |
| Predictors: 15 → 20 | `p15_cindex0.80_hazard0.50_censor0.30_target0.90_mridge` | 235 | `p20_cindex0.80_hazard0.50_censor0.30_target0.90_mridge` | 716 | 3.05× higher | — | — | — |
| Predictors: 15 → 20 | `p15_cindex0.80_hazard1.00_censor0.30_target0.95_mridge` | 1,052 | `p20_cindex0.80_hazard1.00_censor0.30_target0.95_mridge` | 5,728 | 5.44× higher | — | next | next |
| Predictors: 20 → 25 | `p20_cindex0.60_hazard2.00_censor0.10_target0.80_mridge` | 598 | `p25_cindex0.60_hazard2.00_censor0.10_target0.80_mridge` | 1,852 | 3.10× higher | — | — | — |
| Predictors: 20 → 25 | `p20_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | 11,840 | `p25_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | 59,264 | 5.01× higher | — | next | next |
| Predictors: 20 → 25 | `p20_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | 61,056 | `p25_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | 19,040 | 3.21× lower | — | previous | previous |
| Predictors: 20 → 25 | `p20_cindex0.80_hazard1.00_censor0.30_target0.95_mridge` | 5,728 | `p25_cindex0.80_hazard1.00_censor0.30_target0.95_mridge` | 1,784 | 3.21× lower | — | previous | previous |
| Predictors: 20 → 25 | `p20_cindex0.85_hazard0.50_censor0.10_target0.95_mridge` | 775 | `p25_cindex0.85_hazard0.50_censor0.10_target0.95_mridge` | 2,616 | 3.38× higher | — | next | — |
| Predictors: 25 → 30 | `p25_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 8,478 | `p30_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 32,000 | 3.77× higher | — | next | — |
| Predictors: 25 → 30 | `p25_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | 34,851 | `p30_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | 128,000 | 3.67× higher | — | next | next |
| Predictors: 25 → 30 | `p25_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | 59,264 | `p30_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | 16,874 | 3.51× lower | — | previous | previous |
| Predictors: 25 → 30 | `p25_cindex0.70_hazard2.00_censor0.50_target0.95_mridge` | 5,722 | `p30_cindex0.70_hazard2.00_censor0.50_target0.95_mridge` | 27,424 | 4.79× higher | — | next | next |
| Predictors: 25 → 30 | `p25_cindex0.85_hazard0.50_censor0.50_target0.95_mridge` | 1,656 | `p30_cindex0.85_hazard0.50_censor0.50_target0.95_mridge` | 5,648 | 3.41× higher | — | next | next |
| Predictors: 25 → 30 | `p25_cindex0.85_hazard1.00_censor0.30_target0.95_mridge` | 1,680 | `p30_cindex0.85_hazard1.00_censor0.30_target0.95_mridge` | 8,080 | 4.81× higher | — | next | next |
| Predictors: 25 → 30 | `p25_cindex0.85_hazard1.00_censor0.50_target0.95_mridge` | 1,584 | `p30_cindex0.85_hazard1.00_censor0.50_target0.95_mridge` | 5,648 | 3.57× higher | — | next | next |
| Predictors: 25 → 30 | `p25_cindex0.85_hazard2.00_censor0.30_target0.95_mridge` | 6,720 | `p30_cindex0.85_hazard2.00_censor0.30_target0.95_mridge` | 2,020 | 3.33× lower | — | previous | previous |
| Predictors: 30 → 40 | `p30_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 15,083 | `p40_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 85,312 | 5.66× higher | — | next | — |
| Predictors: 30 → 40 | `p30_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 32,000 | `p40_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 5,250 | 6.10× lower | — | previous | — |
| Predictors: 30 → 40 | `p30_cindex0.75_hazard1.00_censor0.30_target0.90_mridge` | 484 | `p40_cindex0.75_hazard1.00_censor0.30_target0.90_mridge` | 1,522 | 3.14× higher | — | — | — |
| Predictors: 30 → 40 | `p30_cindex0.85_hazard1.00_censor0.30_target0.95_mridge` | 8,080 | `p40_cindex0.85_hazard1.00_censor0.30_target0.95_mridge` | 1,344 | 6.01× lower | — | previous | previous |
| Predictors: 30 → 40 | `p30_cindex0.85_hazard1.00_censor0.50_target0.95_mridge` | 5,648 | `p40_cindex0.85_hazard1.00_censor0.50_target0.95_mridge` | 1,882 | 3.00× lower | — | previous | previous |
| Predictors: 50 → 75 | `p50_cindex0.65_hazard1.00_censor0.30_target0.95_mridge` | 8,437 | `p75_cindex0.65_hazard1.00_censor0.30_target0.95_mridge` | 26,368 | 3.13× higher | — | — | — |
| Predictors: 5 → 10 | `p5_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 12,905 | `p10_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 42,624 | 3.30× higher | — | next | — |
| Predictors: 5 → 10 | `p5_cindex0.65_hazard2.00_censor0.30_target0.95_mridge` | 13,952 | `p10_cindex0.65_hazard2.00_censor0.30_target0.95_mridge` | 3,551 | 3.93× lower | — | previous | previous |
| Predictors: 5 → 10 | `p5_cindex0.65_hazard2.00_censor0.50_target0.95_mridge` | 2,464 | `p10_cindex0.65_hazard2.00_censor0.50_target0.95_mridge` | 7,464 | 3.03× higher | — | — | — |
| Predictors: 5 → 10 | `p5_cindex0.70_hazard2.00_censor0.10_target0.80_mridge` | 80 | `p10_cindex0.70_hazard2.00_censor0.10_target0.80_mridge` | 318 | 3.98× higher | — | — | — |
| Predictors: 5 → 10 | `p5_cindex0.75_hazard1.00_censor0.10_target0.85_mridge` | 70 | `p10_cindex0.75_hazard1.00_censor0.10_target0.85_mridge` | 271 | 3.87× higher | — | — | — |
| Predictors: 5 → 10 | `p5_cindex0.75_hazard2.00_censor0.10_target0.95_mridge` | 4,800 | `p10_cindex0.75_hazard2.00_censor0.10_target0.95_mridge` | 829 | 5.79× lower | — | previous | previous |
| Predictors: 5 → 10 | `p5_cindex0.75_hazard2.00_censor0.30_target0.95_mridge` | 6,080 | `p10_cindex0.75_hazard2.00_censor0.30_target0.95_mridge` | 1,246 | 4.88× lower | — | previous | — |
| Predictors: 5 → 10 | `p5_cindex0.80_hazard1.00_censor0.30_target0.90_mridge` | 712 | `p10_cindex0.80_hazard1.00_censor0.30_target0.90_mridge` | 223 | 3.19× lower | — | previous | — |
| Predictors: 5 → 10 | `p5_cindex0.85_hazard0.50_censor0.10_target0.95_mridge` | 528 | `p10_cindex0.85_hazard0.50_censor0.10_target0.95_mridge` | 2,096 | 3.97× higher | — | next | — |
| Predictors: 5 → 10 | `p5_cindex0.85_hazard0.50_censor0.30_target0.90_mridge` | 4,953 | `p10_cindex0.85_hazard0.50_censor0.30_target0.90_mridge` | 672 | 7.37× lower | — | previous | — |
| Predictors: 5 → 10 | `p5_cindex0.85_hazard0.50_censor0.50_target0.85_mridge` | 59 | `p10_cindex0.85_hazard0.50_censor0.50_target0.85_mridge` | 235 | 3.98× higher | — | — | — |
| Predictors: 5 → 10 | `p5_cindex0.85_hazard1.00_censor0.10_target0.90_mridge` | 1,056 | `p10_cindex0.85_hazard1.00_censor0.10_target0.90_mridge` | 245 | 4.31× lower | — | previous | — |
| Predictors: 5 → 10 | `p5_cindex0.85_hazard2.00_censor0.10_target0.90_mridge` | 1,347 | `p10_cindex0.85_hazard2.00_censor0.10_target0.90_mridge` | 222 | 6.07× lower | — | previous | — |
| Predictors: 5 → 10 | `p5_cindex0.85_hazard2.00_censor0.10_target0.95_mridge` | 4,111 | `p10_cindex0.85_hazard2.00_censor0.10_target0.95_mridge` | 1,048 | 3.92× lower | — | — | — |
| Predictors: 5 → 10 | `p5_cindex0.85_hazard2.00_censor0.50_target0.95_mridge` | 7,552 | `p10_cindex0.85_hazard2.00_censor0.50_target0.95_mridge` | 1,880 | 4.02× lower | — | previous | — |
| Predictors: 75 → 100 | `p75_cindex0.60_hazard0.50_censor0.50_target0.85_mridge` | 2,409 | `p100_cindex0.60_hazard0.50_censor0.50_target0.85_mridge` | 7,631 | 3.17× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p100_cindex0.60_hazard0.50_censor0.30_target0.85_mridge` | 5,840 | `p100_cindex0.60_hazard0.50_censor0.30_target0.90_mridge` | 19,056 | 3.26× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.60_hazard0.50_censor0.30_target0.90_mridge` | 19,056 | `p100_cindex0.60_hazard0.50_censor0.30_target0.95_mridge` | 76,224 | 4.00× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p100_cindex0.60_hazard0.50_censor0.50_target0.80_mridge` | 1,666 | `p100_cindex0.60_hazard0.50_censor0.50_target0.85_mridge` | 7,631 | 4.58× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.60_hazard0.50_censor0.50_target0.90_mridge` | 13,332 | `p100_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 47,987 | 3.60× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.60_hazard1.00_censor0.10_target0.90_mridge` | 11,051 | `p100_cindex0.60_hazard1.00_censor0.10_target0.95_mridge` | 36,407 | 3.29× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p100_cindex0.60_hazard1.00_censor0.30_target0.80_mridge` | 3,152 | `p100_cindex0.60_hazard1.00_censor0.30_target0.85_mridge` | 9,528 | 3.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.60_hazard1.00_censor0.30_target0.90_mridge` | 10,564 | `p100_cindex0.60_hazard1.00_censor0.30_target0.95_mridge` | 46,850 | 4.43× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 17,625 | `p100_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | 106,656 | 6.05× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.60_hazard2.00_censor0.10_target0.90_mridge` | 7,408 | `p100_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | 33,700 | 4.55× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.60_hazard2.00_censor0.30_target0.90_mridge` | 9,528 | `p100_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | 37,125 | 3.90× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 19,500 | `p100_cindex0.60_hazard2.00_censor0.50_target0.95_mridge` | 70,812 | 3.63× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.65_hazard0.50_censor0.50_target0.90_mridge` | 7,687 | `p100_cindex0.65_hazard0.50_censor0.50_target0.95_mridge` | 24,616 | 3.20× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p100_cindex0.65_hazard1.00_censor0.10_target0.85_mridge` | 1,718 | `p100_cindex0.65_hazard1.00_censor0.10_target0.90_mridge` | 6,836 | 3.98× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p100_cindex0.65_hazard1.00_censor0.30_target0.80_mridge` | 812 | `p100_cindex0.65_hazard1.00_censor0.30_target0.85_mridge` | 3,297 | 4.06× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.65_hazard1.00_censor0.30_target0.90_mridge` | 4,088 | `p100_cindex0.65_hazard1.00_censor0.30_target0.95_mridge` | 16,796 | 4.11× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p100_cindex0.65_hazard1.00_censor0.50_target0.85_mridge` | 3,555 | `p100_cindex0.65_hazard1.00_censor0.50_target0.90_mridge` | 12,308 | 3.46× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p100_cindex0.65_hazard2.00_censor0.10_target0.85_mridge` | 1,653 | `p100_cindex0.65_hazard2.00_censor0.10_target0.90_mridge` | 6,836 | 4.14× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.65_hazard2.00_censor0.30_target0.90_mridge` | 4,210 | `p100_cindex0.65_hazard2.00_censor0.30_target0.95_mridge` | 17,584 | 4.18× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.65_hazard2.00_censor0.50_target0.90_mridge` | 7,242 | `p100_cindex0.65_hazard2.00_censor0.50_target0.95_mridge` | 24,616 | 3.40× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.70_hazard0.50_censor0.30_target0.90_mridge` | 2,041 | `p100_cindex0.70_hazard0.50_censor0.30_target0.95_mridge` | 9,278 | 4.55× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p100_cindex0.70_hazard0.50_censor0.50_target0.80_mridge` | 912 | `p100_cindex0.70_hazard0.50_censor0.50_target0.85_mridge` | 2,857 | 3.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.70_hazard1.00_censor0.30_target0.90_mridge` | 2,921 | `p100_cindex0.70_hazard1.00_censor0.30_target0.95_mridge` | 16,328 | 5.59× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p100_cindex0.70_hazard2.00_censor0.10_target0.85_mridge` | 800 | `p100_cindex0.70_hazard2.00_censor0.10_target0.90_mridge` | 2,642 | 3.30× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.70_hazard2.00_censor0.50_target0.90_mridge` | 2,811 | `p100_cindex0.70_hazard2.00_censor0.50_target0.95_mridge` | 12,360 | 4.40× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.75_hazard0.50_censor0.10_target0.90_mridge` | 1,481 | `p100_cindex0.75_hazard0.50_censor0.10_target0.95_mridge` | 5,924 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.75_hazard0.50_censor0.50_target0.90_mridge` | 2,436 | `p100_cindex0.75_hazard0.50_censor0.50_target0.95_mridge` | 7,659 | 3.14× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.75_hazard1.00_censor0.30_target0.90_mridge` | 1,731 | `p100_cindex0.75_hazard1.00_censor0.30_target0.95_mridge` | 5,324 | 3.08× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.75_hazard1.00_censor0.50_target0.90_mridge` | 2,667 | `p100_cindex0.75_hazard1.00_censor0.50_target0.95_mridge` | 8,001 | 3.00× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p100_cindex0.80_hazard0.50_censor0.50_target0.80_mridge` | 227 | `p100_cindex0.80_hazard0.50_censor0.50_target0.85_mridge` | 901 | 3.97× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p100_cindex0.80_hazard1.00_censor0.50_target0.90_mridge` | 1,064 | `p100_cindex0.80_hazard1.00_censor0.50_target0.95_mridge` | 5,000 | 4.70× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p100_cindex0.80_hazard2.00_censor0.50_target0.80_mridge` | 312 | `p100_cindex0.80_hazard2.00_censor0.50_target0.85_mridge` | 1,250 | 4.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard0.50_censor0.10_target0.90_mridge` | 3,014 | `p10_cindex0.60_hazard0.50_censor0.10_target0.95_mridge` | 11,840 | 3.93× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard0.50_censor0.50_target0.90_mridge` | 5,328 | `p10_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 42,624 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard1.00_censor0.10_target0.90_mridge` | 3,031 | `p10_cindex0.60_hazard1.00_censor0.10_target0.95_mridge` | 11,840 | 3.91× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard1.00_censor0.30_target0.90_mridge` | 3,607 | `p10_cindex0.60_hazard1.00_censor0.30_target0.95_mridge` | 15,336 | 4.25× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 2,664 | `p10_cindex0.60_hazard2.00_censor0.50_target0.95_mridge` | 10,656 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.65_hazard1.00_censor0.50_target0.90_mridge` | 1,723 | `p10_cindex0.65_hazard1.00_censor0.50_target0.95_mridge` | 8,201 | 4.76× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p10_cindex0.65_hazard2.00_censor0.10_target0.80_mridge` | 342 | `p10_cindex0.65_hazard2.00_censor0.10_target0.85_mridge` | 1,368 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.65_hazard2.00_censor0.10_target0.90_mridge` | 1,043 | `p10_cindex0.65_hazard2.00_censor0.10_target0.95_mridge` | 5,472 | 5.25× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p10_cindex0.65_hazard2.00_censor0.30_target0.85_mridge` | 441 | `p10_cindex0.65_hazard2.00_censor0.30_target0.90_mridge` | 1,582 | 3.59× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.65_hazard2.00_censor0.50_target0.90_mridge` | 2,268 | `p10_cindex0.65_hazard2.00_censor0.50_target0.95_mridge` | 7,464 | 3.29× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.70_hazard0.50_censor0.10_target0.90_mridge` | 490 | `p10_cindex0.70_hazard0.50_censor0.10_target0.95_mridge` | 2,544 | 5.19× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.70_hazard0.50_censor0.30_target0.90_mridge` | 816 | `p10_cindex0.70_hazard0.50_censor0.30_target0.95_mridge` | 2,532 | 3.10× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.70_hazard1.00_censor0.10_target0.90_mridge` | 586 | `p10_cindex0.70_hazard1.00_censor0.10_target0.95_mridge` | 2,238 | 3.82× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.70_hazard1.00_censor0.50_target0.90_mridge` | 1,154 | `p10_cindex0.70_hazard1.00_censor0.50_target0.95_mridge` | 4,300 | 3.73× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.70_hazard2.00_censor0.30_target0.90_mridge` | 769 | `p10_cindex0.70_hazard2.00_censor0.30_target0.95_mridge` | 3,264 | 4.24× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.70_hazard2.00_censor0.50_target0.90_mridge` | 848 | `p10_cindex0.70_hazard2.00_censor0.50_target0.95_mridge` | 3,260 | 3.84× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p10_cindex0.75_hazard0.50_censor0.30_target0.80_mridge` | 95 | `p10_cindex0.75_hazard0.50_censor0.30_target0.85_mridge` | 382 | 4.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.75_hazard1.00_censor0.10_target0.90_mridge` | 327 | `p10_cindex0.75_hazard1.00_censor0.10_target0.95_mridge` | 1,100 | 3.36× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p10_cindex0.75_hazard1.00_censor0.50_target0.85_mridge` | 267 | `p10_cindex0.75_hazard1.00_censor0.50_target0.90_mridge` | 804 | 3.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.75_hazard2.00_censor0.50_target0.90_mridge` | 1,068 | `p10_cindex0.75_hazard2.00_censor0.50_target0.95_mridge` | 4,272 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.80_hazard0.50_censor0.30_target0.90_mridge` | 358 | `p10_cindex0.80_hazard0.50_censor0.30_target0.95_mridge` | 1,998 | 5.58× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.80_hazard1.00_censor0.30_target0.90_mridge` | 223 | `p10_cindex0.80_hazard1.00_censor0.30_target0.95_mridge` | 1,432 | 6.42× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.80_hazard2.00_censor0.30_target0.90_mridge` | 315 | `p10_cindex0.80_hazard2.00_censor0.30_target0.95_mridge` | 2,864 | 9.09× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard0.50_censor0.10_target0.90_mridge` | 262 | `p10_cindex0.85_hazard0.50_censor0.10_target0.95_mridge` | 2,096 | 8.00× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p10_cindex0.85_hazard0.50_censor0.30_target0.85_mridge` | 172 | `p10_cindex0.85_hazard0.50_censor0.30_target0.90_mridge` | 672 | 3.91× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard0.50_censor0.50_target0.90_mridge` | 352 | `p10_cindex0.85_hazard0.50_censor0.50_target0.95_mridge` | 1,880 | 5.34× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard1.00_censor0.10_target0.90_mridge` | 245 | `p10_cindex0.85_hazard1.00_censor0.10_target0.95_mridge` | 2,096 | 8.56× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard1.00_censor0.30_target0.90_mridge` | 265 | `p10_cindex0.85_hazard1.00_censor0.30_target0.95_mridge` | 1,344 | 5.07× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard1.00_censor0.50_target0.90_mridge` | 393 | `p10_cindex0.85_hazard1.00_censor0.50_target0.95_mridge` | 1,880 | 4.78× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard2.00_censor0.10_target0.90_mridge` | 222 | `p10_cindex0.85_hazard2.00_censor0.10_target0.95_mridge` | 1,048 | 4.72× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard2.00_censor0.30_target0.90_mridge` | 292 | `p10_cindex0.85_hazard2.00_censor0.30_target0.95_mridge` | 2,688 | 9.21× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p10_cindex0.85_hazard2.00_censor0.50_target0.90_mridge` | 409 | `p10_cindex0.85_hazard2.00_censor0.50_target0.95_mridge` | 1,880 | 4.60× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.60_hazard0.50_censor0.10_target0.90_mridge` | 3,689 | `p15_cindex0.60_hazard0.50_censor0.10_target0.95_mridge` | 11,984 | 3.25× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p15_cindex0.60_hazard0.50_censor0.50_target0.80_mridge` | 992 | `p15_cindex0.60_hazard0.50_censor0.50_target0.85_mridge` | 4,006 | 4.04× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.60_hazard1.00_censor0.30_target0.90_mridge` | 2,856 | `p15_cindex0.60_hazard1.00_censor0.30_target0.95_mridge` | 15,087 | 5.28× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.60_hazard2.00_censor0.10_target0.90_mridge` | 4,448 | `p15_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | 13,731 | 3.09× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p15_cindex0.60_hazard2.00_censor0.30_target0.85_mridge` | 1,438 | `p15_cindex0.60_hazard2.00_censor0.30_target0.90_mridge` | 5,158 | 3.59× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 2,000 | `p15_cindex0.60_hazard2.00_censor0.50_target0.95_mridge` | 15,732 | 7.87× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.65_hazard0.50_censor0.30_target0.90_mridge` | 2,632 | `p15_cindex0.65_hazard0.50_censor0.30_target0.95_mridge` | 10,528 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.65_hazard1.00_censor0.10_target0.90_mridge` | 1,550 | `p15_cindex0.65_hazard1.00_censor0.10_target0.95_mridge` | 8,224 | 5.31× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p15_cindex0.65_hazard1.00_censor0.30_target0.85_mridge` | 630 | `p15_cindex0.65_hazard1.00_censor0.30_target0.90_mridge` | 2,632 | 4.18× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.65_hazard2.00_censor0.30_target0.90_mridge` | 1,917 | `p15_cindex0.65_hazard2.00_censor0.30_target0.95_mridge` | 6,763 | 3.53× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.70_hazard0.50_censor0.10_target0.90_mridge` | 936 | `p15_cindex0.70_hazard0.50_censor0.10_target0.95_mridge` | 3,824 | 4.09× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.70_hazard0.50_censor0.50_target0.90_mridge` | 1,527 | `p15_cindex0.70_hazard0.50_censor0.50_target0.95_mridge` | 4,823 | 3.16× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.70_hazard1.00_censor0.10_target0.90_mridge` | 682 | `p15_cindex0.70_hazard1.00_censor0.10_target0.95_mridge` | 2,469 | 3.62× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p15_cindex0.70_hazard2.00_censor0.10_target0.85_mridge` | 241 | `p15_cindex0.70_hazard2.00_censor0.10_target0.90_mridge` | 961 | 3.99× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.70_hazard2.00_censor0.50_target0.90_mridge` | 1,369 | `p15_cindex0.70_hazard2.00_censor0.50_target0.95_mridge` | 4,270 | 3.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.75_hazard0.50_censor0.50_target0.90_mridge` | 800 | `p15_cindex0.75_hazard0.50_censor0.50_target0.95_mridge` | 2,856 | 3.57× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.75_hazard1.00_censor0.10_target0.90_mridge` | 591 | `p15_cindex0.75_hazard1.00_censor0.10_target0.95_mridge` | 1,784 | 3.02× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p15_cindex0.75_hazard1.00_censor0.30_target0.85_mridge` | 374 | `p15_cindex0.75_hazard1.00_censor0.30_target0.90_mridge` | 1,140 | 3.05× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p15_cindex0.75_hazard1.00_censor0.50_target0.85_mridge` | 521 | `p15_cindex0.75_hazard1.00_censor0.50_target0.90_mridge` | 1,600 | 3.07× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.75_hazard2.00_censor0.30_target0.90_mridge` | 570 | `p15_cindex0.75_hazard2.00_censor0.30_target0.95_mridge` | 1,744 | 3.06× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.75_hazard2.00_censor0.50_target0.90_mridge` | 800 | `p15_cindex0.75_hazard2.00_censor0.50_target0.95_mridge` | 6,400 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.80_hazard0.50_censor0.30_target0.90_mridge` | 235 | `p15_cindex0.80_hazard0.50_censor0.30_target0.95_mridge` | 1,016 | 4.32× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p15_cindex0.80_hazard0.50_censor0.50_target0.85_mridge` | 187 | `p15_cindex0.80_hazard0.50_censor0.50_target0.90_mridge` | 709 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.80_hazard0.50_censor0.50_target0.90_mridge` | 709 | `p15_cindex0.80_hazard0.50_censor0.50_target0.95_mridge` | 3,000 | 4.23× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.80_hazard2.00_censor0.10_target0.90_mridge` | 421 | `p15_cindex0.80_hazard2.00_censor0.10_target0.95_mridge` | 3,465 | 8.23× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.80_hazard2.00_censor0.30_target0.90_mridge` | 536 | `p15_cindex0.80_hazard2.00_censor0.30_target0.95_mridge` | 2,144 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.85_hazard0.50_censor0.10_target0.90_mridge` | 392 | `p15_cindex0.85_hazard0.50_censor0.10_target0.95_mridge` | 1,568 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.85_hazard0.50_censor0.30_target0.90_mridge` | 252 | `p15_cindex0.85_hazard0.50_censor0.30_target0.95_mridge` | 2,016 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.85_hazard0.50_censor0.50_target0.90_mridge` | 449 | `p15_cindex0.85_hazard0.50_censor0.50_target0.95_mridge` | 1,412 | 3.14× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.85_hazard1.00_censor0.10_target0.90_mridge` | 163 | `p15_cindex0.85_hazard1.00_censor0.10_target0.95_mridge` | 1,568 | 9.62× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.85_hazard1.00_censor0.50_target0.90_mridge` | 447 | `p15_cindex0.85_hazard1.00_censor0.50_target0.95_mridge` | 2,824 | 6.32× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.85_hazard2.00_censor0.10_target0.90_mridge` | 310 | `p15_cindex0.85_hazard2.00_censor0.10_target0.95_mridge` | 1,568 | 5.06× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p15_cindex0.85_hazard2.00_censor0.50_target0.90_mridge` | 358 | `p15_cindex0.85_hazard2.00_censor0.50_target0.95_mridge` | 1,412 | 3.94× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.60_hazard0.50_censor0.30_target0.90_mridge` | 6,621 | `p20_cindex0.60_hazard0.50_censor0.30_target0.95_mridge` | 61,056 | 9.22× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p20_cindex0.60_hazard0.50_censor0.50_target0.85_mridge` | 2,882 | `p20_cindex0.60_hazard0.50_censor0.50_target0.90_mridge` | 10,672 | 3.70× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.60_hazard1.00_censor0.10_target0.90_mridge` | 2,806 | `p20_cindex0.60_hazard1.00_censor0.10_target0.95_mridge` | 11,221 | 4.00× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p20_cindex0.60_hazard2.00_censor0.10_target0.80_mridge` | 598 | `p20_cindex0.60_hazard2.00_censor0.10_target0.85_mridge` | 1,944 | 3.25× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.65_hazard0.50_censor0.10_target0.90_mridge` | 1,697 | `p20_cindex0.65_hazard0.50_censor0.10_target0.95_mridge` | 10,944 | 6.45× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.65_hazard0.50_censor0.30_target0.90_mridge` | 1,760 | `p20_cindex0.65_hazard0.50_censor0.30_target0.95_mridge` | 7,040 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.65_hazard0.50_censor0.50_target0.90_mridge` | 3,143 | `p20_cindex0.65_hazard0.50_censor0.50_target0.95_mridge` | 9,840 | 3.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.65_hazard1.00_censor0.30_target0.90_mridge` | 1,706 | `p20_cindex0.65_hazard1.00_censor0.30_target0.95_mridge` | 7,040 | 4.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.65_hazard2.00_censor0.30_target0.90_mridge` | 2,022 | `p20_cindex0.65_hazard2.00_censor0.30_target0.95_mridge` | 6,937 | 3.43× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p20_cindex0.65_hazard2.00_censor0.50_target0.85_mridge` | 986 | `p20_cindex0.65_hazard2.00_censor0.50_target0.90_mridge` | 4,920 | 4.99× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.70_hazard1.00_censor0.30_target0.90_mridge` | 1,017 | `p20_cindex0.70_hazard1.00_censor0.30_target0.95_mridge` | 3,730 | 3.67× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.70_hazard1.00_censor0.50_target0.90_mridge` | 1,731 | `p20_cindex0.70_hazard1.00_censor0.50_target0.95_mridge` | 5,261 | 3.04× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.75_hazard0.50_censor0.10_target0.90_mridge` | 604 | `p20_cindex0.75_hazard0.50_censor0.10_target0.95_mridge` | 2,368 | 3.92× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.75_hazard0.50_censor0.50_target0.90_mridge` | 1,122 | `p20_cindex0.75_hazard0.50_censor0.50_target0.95_mridge` | 8,528 | 7.60× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p20_cindex0.75_hazard1.00_censor0.10_target0.90_mridge` | 632 | `p20_cindex0.75_hazard1.00_censor0.10_target0.95_mridge` | 2,368 | 3.75× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.75_hazard1.00_censor0.30_target0.90_mridge` | 789 | `p20_cindex0.75_hazard1.00_censor0.30_target0.95_mridge` | 3,048 | 3.86× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.75_hazard1.00_censor0.50_target0.90_mridge` | 1,066 | `p20_cindex0.75_hazard1.00_censor0.50_target0.95_mridge` | 3,519 | 3.30× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.80_hazard0.50_censor0.50_target0.90_mridge` | 723 | `p20_cindex0.80_hazard0.50_censor0.50_target0.95_mridge` | 2,215 | 3.06× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p20_cindex0.80_hazard1.00_censor0.30_target0.85_mridge` | 161 | `p20_cindex0.80_hazard1.00_censor0.30_target0.90_mridge` | 527 | 3.27× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.80_hazard1.00_censor0.50_target0.90_mridge` | 500 | `p20_cindex0.80_hazard1.00_censor0.50_target0.95_mridge` | 2,192 | 4.38× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.80_hazard2.00_censor0.50_target0.90_mridge` | 650 | `p20_cindex0.80_hazard2.00_censor0.50_target0.95_mridge` | 1,967 | 3.03× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.85_hazard0.50_censor0.30_target0.90_mridge` | 391 | `p20_cindex0.85_hazard0.50_censor0.30_target0.95_mridge` | 1,344 | 3.44× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.85_hazard0.50_censor0.50_target0.90_mridge` | 535 | `p20_cindex0.85_hazard0.50_censor0.50_target0.95_mridge` | 1,884 | 3.52× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.85_hazard1.00_censor0.10_target0.90_mridge` | 338 | `p20_cindex0.85_hazard1.00_censor0.10_target0.95_mridge` | 2,088 | 6.18× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.85_hazard1.00_censor0.30_target0.90_mridge` | 434 | `p20_cindex0.85_hazard1.00_censor0.30_target0.95_mridge` | 2,688 | 6.19× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p20_cindex0.85_hazard1.00_censor0.50_target0.85_mridge` | 185 | `p20_cindex0.85_hazard1.00_censor0.50_target0.90_mridge` | 560 | 3.03× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.85_hazard1.00_censor0.50_target0.90_mridge` | 560 | `p20_cindex0.85_hazard1.00_censor0.50_target0.95_mridge` | 1,884 | 3.36× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.85_hazard2.00_censor0.10_target0.90_mridge` | 341 | `p20_cindex0.85_hazard2.00_censor0.10_target0.95_mridge` | 2,088 | 6.12× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.85_hazard2.00_censor0.30_target0.90_mridge` | 336 | `p20_cindex0.85_hazard2.00_censor0.30_target0.95_mridge` | 2,688 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p20_cindex0.85_hazard2.00_censor0.50_target0.90_mridge` | 471 | `p20_cindex0.85_hazard2.00_censor0.50_target0.95_mridge` | 3,768 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.60_hazard0.50_censor0.10_target0.90_mridge` | 3,855 | `p25_cindex0.60_hazard0.50_censor0.10_target0.95_mridge` | 13,438 | 3.49× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.60_hazard0.50_censor0.30_target0.90_mridge` | 5,236 | `p25_cindex0.60_hazard0.50_censor0.30_target0.95_mridge` | 38,080 | 7.27× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p25_cindex0.60_hazard0.50_censor0.50_target0.85_mridge` | 3,657 | `p25_cindex0.60_hazard0.50_censor0.50_target0.90_mridge` | 13,328 | 3.64× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.60_hazard0.50_censor0.50_target0.90_mridge` | 13,328 | `p25_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 42,949 | 3.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.60_hazard1.00_censor0.10_target0.90_mridge` | 3,460 | `p25_cindex0.60_hazard1.00_censor0.10_target0.95_mridge` | 15,956 | 4.61× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_cindex0.60_hazard1.00_censor0.30_target0.85_mridge` | 2,380 | `p25_cindex0.60_hazard1.00_censor0.30_target0.90_mridge` | 9,520 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 8,478 | `p25_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | 34,851 | 4.11× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.60_hazard2.00_censor0.30_target0.90_mridge` | 4,760 | `p25_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | 19,040 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 8,940 | `p25_cindex0.60_hazard2.00_censor0.50_target0.95_mridge` | 31,029 | 3.47× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.65_hazard0.50_censor0.10_target0.90_mridge` | 2,037 | `p25_cindex0.65_hazard0.50_censor0.10_target0.95_mridge` | 6,873 | 3.37× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.65_hazard0.50_censor0.30_target0.90_mridge` | 2,108 | `p25_cindex0.65_hazard0.50_censor0.30_target0.95_mridge` | 8,544 | 4.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.65_hazard1.00_censor0.10_target0.90_mridge` | 2,043 | `p25_cindex0.65_hazard1.00_censor0.10_target0.95_mridge` | 7,271 | 3.56× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.65_hazard1.00_censor0.30_target0.90_mridge` | 2,163 | `p25_cindex0.65_hazard1.00_censor0.30_target0.95_mridge` | 8,784 | 4.06× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p25_cindex0.65_hazard1.00_censor0.50_target0.80_mridge` | 997 | `p25_cindex0.65_hazard1.00_censor0.50_target0.85_mridge` | 3,076 | 3.09× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.65_hazard1.00_censor0.50_target0.90_mridge` | 3,791 | `p25_cindex0.65_hazard1.00_censor0.50_target0.95_mridge` | 11,536 | 3.04× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.65_hazard2.00_censor0.30_target0.90_mridge` | 2,263 | `p25_cindex0.65_hazard2.00_censor0.30_target0.95_mridge` | 17,568 | 7.76× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p25_cindex0.70_hazard0.50_censor0.10_target0.85_mridge` | 375 | `p25_cindex0.70_hazard0.50_censor0.10_target0.90_mridge` | 1,267 | 3.38× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p25_cindex0.70_hazard0.50_censor0.30_target0.80_mridge` | 194 | `p25_cindex0.70_hazard0.50_censor0.30_target0.85_mridge` | 1,020 | 5.26× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.70_hazard0.50_censor0.50_target0.90_mridge` | 1,239 | `p25_cindex0.70_hazard0.50_censor0.50_target0.95_mridge` | 5,445 | 4.39× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.70_hazard1.00_censor0.30_target0.90_mridge` | 1,021 | `p25_cindex0.70_hazard1.00_censor0.30_target0.95_mridge` | 4,030 | 3.95× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.70_hazard1.00_censor0.50_target0.90_mridge` | 1,420 | `p25_cindex0.70_hazard1.00_censor0.50_target0.95_mridge` | 5,712 | 4.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.70_hazard2.00_censor0.30_target0.90_mridge` | 1,508 | `p25_cindex0.70_hazard2.00_censor0.30_target0.95_mridge` | 4,567 | 3.03× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.75_hazard0.50_censor0.30_target0.90_mridge` | 746 | `p25_cindex0.75_hazard0.50_censor0.30_target0.95_mridge` | 2,772 | 3.72× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p25_cindex0.75_hazard0.50_censor0.50_target0.80_mridge` | 232 | `p25_cindex0.75_hazard0.50_censor0.50_target0.85_mridge` | 747 | 3.22× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.75_hazard0.50_censor0.50_target0.90_mridge` | 1,341 | `p25_cindex0.75_hazard0.50_censor0.50_target0.95_mridge` | 5,336 | 3.98× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.75_hazard2.00_censor0.10_target0.90_mridge` | 371 | `p25_cindex0.75_hazard2.00_censor0.10_target0.95_mridge` | 2,030 | 5.47× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.75_hazard2.00_censor0.30_target0.90_mridge` | 996 | `p25_cindex0.75_hazard2.00_censor0.30_target0.95_mridge` | 3,808 | 3.82× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.80_hazard0.50_censor0.10_target0.90_mridge` | 348 | `p25_cindex0.80_hazard0.50_censor0.10_target0.95_mridge` | 1,392 | 4.00× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_cindex0.80_hazard0.50_censor0.50_target0.85_mridge` | 265 | `p25_cindex0.80_hazard0.50_censor0.50_target0.90_mridge` | 944 | 3.56× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.80_hazard1.00_censor0.30_target0.90_mridge` | 551 | `p25_cindex0.80_hazard1.00_censor0.30_target0.95_mridge` | 1,784 | 3.24× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.80_hazard2.00_censor0.30_target0.90_mridge` | 437 | `p25_cindex0.80_hazard2.00_censor0.30_target0.95_mridge` | 1,784 | 4.08× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.85_hazard0.50_censor0.10_target0.90_mridge` | 370 | `p25_cindex0.85_hazard0.50_censor0.10_target0.95_mridge` | 2,616 | 7.07× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.85_hazard0.50_censor0.30_target0.90_mridge` | 449 | `p25_cindex0.85_hazard0.50_censor0.30_target0.95_mridge` | 1,362 | 3.03× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.85_hazard1.00_censor0.30_target0.90_mridge` | 481 | `p25_cindex0.85_hazard1.00_censor0.30_target0.95_mridge` | 1,680 | 3.49× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p25_cindex0.85_hazard2.00_censor0.10_target0.90_mridge` | 396 | `p25_cindex0.85_hazard2.00_censor0.10_target0.95_mridge` | 1,308 | 3.30× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p25_cindex0.85_hazard2.00_censor0.30_target0.85_mridge` | 158 | `p25_cindex0.85_hazard2.00_censor0.30_target0.90_mridge` | 493 | 3.12× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.60_hazard0.50_censor0.10_target0.90_mridge` | 4,112 | `p30_cindex0.60_hazard0.50_censor0.10_target0.95_mridge` | 18,705 | 4.55× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p30_cindex0.60_hazard0.50_censor0.30_target0.85_mridge` | 1,430 | `p30_cindex0.60_hazard0.50_censor0.30_target0.90_mridge` | 6,862 | 4.80× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.60_hazard0.50_censor0.30_target0.90_mridge` | 6,862 | `p30_cindex0.60_hazard0.50_censor0.30_target0.95_mridge` | 22,873 | 3.33× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p30_cindex0.60_hazard1.00_censor0.10_target0.85_mridge` | 2,220 | `p30_cindex0.60_hazard1.00_censor0.10_target0.90_mridge` | 8,880 | 4.00× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p30_cindex0.60_hazard1.00_censor0.30_target0.85_mridge` | 3,585 | `p30_cindex0.60_hazard1.00_censor0.30_target0.90_mridge` | 11,440 | 3.19× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.60_hazard1.00_censor0.30_target0.90_mridge` | 11,440 | `p30_cindex0.60_hazard1.00_censor0.30_target0.95_mridge` | 45,760 | 4.00× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p30_cindex0.60_hazard1.00_censor0.50_target0.85_mridge` | 4,000 | `p30_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 32,000 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 32,000 | `p30_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | 128,000 | 4.00× higher | — | previous, next | next |
| Target slope: 0.9 → 0.95 | `p30_cindex0.60_hazard2.00_censor0.10_target0.90_mridge` | 5,006 | `p30_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | 16,874 | 3.37× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.60_hazard2.00_censor0.30_target0.90_mridge` | 2,860 | `p30_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | 24,203 | 8.46× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p30_cindex0.60_hazard2.00_censor0.50_target0.85_mridge` | 4,830 | `p30_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 16,000 | 3.31× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p30_cindex0.65_hazard0.50_censor0.10_target0.85_mridge` | 512 | `p30_cindex0.65_hazard0.50_censor0.10_target0.90_mridge` | 4,096 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.65_hazard0.50_censor0.30_target0.90_mridge` | 3,068 | `p30_cindex0.65_hazard0.50_censor0.30_target0.95_mridge` | 9,820 | 3.20× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.65_hazard0.50_censor0.50_target0.90_mridge` | 4,412 | `p30_cindex0.65_hazard0.50_censor0.50_target0.95_mridge` | 14,768 | 3.35× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.65_hazard1.00_censor0.10_target0.90_mridge` | 1,024 | `p30_cindex0.65_hazard1.00_censor0.10_target0.95_mridge` | 7,182 | 7.01× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.65_hazard1.00_censor0.30_target0.90_mridge` | 2,796 | `p30_cindex0.65_hazard1.00_censor0.30_target0.95_mridge` | 10,465 | 3.74× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.65_hazard1.00_censor0.50_target0.90_mridge` | 3,359 | `p30_cindex0.65_hazard1.00_censor0.50_target0.95_mridge` | 13,494 | 4.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.65_hazard2.00_censor0.10_target0.90_mridge` | 2,155 | `p30_cindex0.65_hazard2.00_censor0.10_target0.95_mridge` | 7,671 | 3.56× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.65_hazard2.00_censor0.30_target0.90_mridge` | 2,743 | `p30_cindex0.65_hazard2.00_censor0.30_target0.95_mridge` | 10,616 | 3.87× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.65_hazard2.00_censor0.50_target0.90_mridge` | 4,172 | `p30_cindex0.65_hazard2.00_censor0.50_target0.95_mridge` | 14,768 | 3.54× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.70_hazard0.50_censor0.10_target0.90_mridge` | 983 | `p30_cindex0.70_hazard0.50_censor0.10_target0.95_mridge` | 3,567 | 3.63× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p30_cindex0.70_hazard0.50_censor0.30_target0.85_mridge` | 458 | `p30_cindex0.70_hazard0.50_censor0.30_target0.90_mridge` | 1,680 | 3.67× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.70_hazard0.50_censor0.30_target0.90_mridge` | 1,680 | `p30_cindex0.70_hazard0.50_censor0.30_target0.95_mridge` | 5,073 | 3.02× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.70_hazard0.50_censor0.50_target0.90_mridge` | 1,640 | `p30_cindex0.70_hazard0.50_censor0.50_target0.95_mridge` | 5,780 | 3.52× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.70_hazard1.00_censor0.30_target0.90_mridge` | 1,618 | `p30_cindex0.70_hazard1.00_censor0.30_target0.95_mridge` | 5,129 | 3.17× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.70_hazard1.00_censor0.50_target0.90_mridge` | 1,679 | `p30_cindex0.70_hazard1.00_censor0.50_target0.95_mridge` | 6,166 | 3.67× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p30_cindex0.70_hazard2.00_censor0.50_target0.85_mridge` | 1,094 | `p30_cindex0.70_hazard2.00_censor0.50_target0.90_mridge` | 3,428 | 3.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.70_hazard2.00_censor0.50_target0.90_mridge` | 3,428 | `p30_cindex0.70_hazard2.00_censor0.50_target0.95_mridge` | 27,424 | 8.00× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p30_cindex0.75_hazard0.50_censor0.30_target0.90_mridge` | 934 | `p30_cindex0.75_hazard0.50_censor0.30_target0.95_mridge` | 3,140 | 3.36× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p30_cindex0.75_hazard0.50_censor0.50_target0.80_mridge` | 200 | `p30_cindex0.75_hazard0.50_censor0.50_target0.85_mridge` | 800 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.75_hazard1.00_censor0.30_target0.90_mridge` | 484 | `p30_cindex0.75_hazard1.00_censor0.30_target0.95_mridge` | 2,887 | 5.96× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.75_hazard2.00_censor0.30_target0.90_mridge` | 1,144 | `p30_cindex0.75_hazard2.00_censor0.30_target0.95_mridge` | 4,576 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.75_hazard2.00_censor0.50_target0.90_mridge` | 1,600 | `p30_cindex0.75_hazard2.00_censor0.50_target0.95_mridge` | 6,400 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.80_hazard0.50_censor0.30_target0.90_mridge` | 690 | `p30_cindex0.80_hazard0.50_censor0.30_target0.95_mridge` | 2,144 | 3.11× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.80_hazard2.00_censor0.10_target0.90_mridge` | 465 | `p30_cindex0.80_hazard2.00_censor0.10_target0.95_mridge` | 1,522 | 3.27× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.80_hazard2.00_censor0.50_target0.90_mridge` | 750 | `p30_cindex0.80_hazard2.00_censor0.50_target0.95_mridge` | 3,000 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.85_hazard0.50_censor0.10_target0.90_mridge` | 448 | `p30_cindex0.85_hazard0.50_censor0.10_target0.95_mridge` | 1,568 | 3.50× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.85_hazard0.50_censor0.30_target0.90_mridge` | 495 | `p30_cindex0.85_hazard0.50_censor0.30_target0.95_mridge` | 4,040 | 8.16× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.85_hazard0.50_censor0.50_target0.90_mridge` | 706 | `p30_cindex0.85_hazard0.50_censor0.50_target0.95_mridge` | 5,648 | 8.00× higher | — | next | next |
| Target slope: 0.8 → 0.85 | `p30_cindex0.85_hazard1.00_censor0.50_target0.80_mridge` | 171 | `p30_cindex0.85_hazard1.00_censor0.50_target0.85_mridge` | 706 | 4.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.85_hazard1.00_censor0.50_target0.90_mridge` | 706 | `p30_cindex0.85_hazard1.00_censor0.50_target0.95_mridge` | 5,648 | 8.00× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p30_cindex0.85_hazard2.00_censor0.10_target0.90_mridge` | 421 | `p30_cindex0.85_hazard2.00_censor0.10_target0.95_mridge` | 3,136 | 7.45× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p30_cindex0.85_hazard2.00_censor0.30_target0.90_mridge` | 505 | `p30_cindex0.85_hazard2.00_censor0.30_target0.95_mridge` | 2,020 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.60_hazard0.50_censor0.10_target0.90_mridge` | 5,859 | `p40_cindex0.60_hazard0.50_censor0.10_target0.95_mridge` | 22,109 | 3.77× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.60_hazard0.50_censor0.50_target0.90_mridge` | 10,664 | `p40_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 85,312 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.60_hazard1.00_censor0.30_target0.90_mridge` | 8,422 | `p40_cindex0.60_hazard1.00_censor0.30_target0.95_mridge` | 30,464 | 3.62× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 5,250 | `p40_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | 43,324 | 8.25× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p40_cindex0.60_hazard2.00_censor0.50_target0.85_mridge` | 5,549 | `p40_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 21,328 | 3.84× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.65_hazard0.50_censor0.30_target0.90_mridge` | 3,512 | `p40_cindex0.65_hazard0.50_censor0.30_target0.95_mridge` | 28,096 | 8.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.65_hazard0.50_censor0.50_target0.90_mridge` | 4,831 | `p40_cindex0.65_hazard0.50_censor0.50_target0.95_mridge` | 14,512 | 3.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.65_hazard1.00_censor0.30_target0.90_mridge` | 3,623 | `p40_cindex0.65_hazard1.00_censor0.30_target0.95_mridge` | 14,048 | 3.88× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.65_hazard1.00_censor0.50_target0.90_mridge` | 4,598 | `p40_cindex0.65_hazard1.00_censor0.50_target0.95_mridge` | 16,073 | 3.50× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.65_hazard2.00_censor0.10_target0.90_mridge` | 2,377 | `p40_cindex0.65_hazard2.00_censor0.10_target0.95_mridge` | 10,928 | 4.60× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.70_hazard0.50_censor0.10_target0.90_mridge` | 1,268 | `p40_cindex0.70_hazard0.50_censor0.10_target0.95_mridge` | 5,083 | 4.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.70_hazard0.50_censor0.30_target0.90_mridge` | 2,128 | `p40_cindex0.70_hazard0.50_censor0.30_target0.95_mridge` | 6,550 | 3.08× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.70_hazard1.00_censor0.10_target0.90_mridge` | 1,271 | `p40_cindex0.70_hazard1.00_censor0.10_target0.95_mridge` | 4,550 | 3.58× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p40_cindex0.70_hazard1.00_censor0.30_target0.85_mridge` | 925 | `p40_cindex0.70_hazard1.00_censor0.30_target0.90_mridge` | 3,264 | 3.53× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.70_hazard1.00_censor0.50_target0.90_mridge` | 2,667 | `p40_cindex0.70_hazard1.00_censor0.50_target0.95_mridge` | 8,173 | 3.06× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.70_hazard2.00_censor0.10_target0.90_mridge` | 1,379 | `p40_cindex0.70_hazard2.00_censor0.10_target0.95_mridge` | 4,709 | 3.41× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.70_hazard2.00_censor0.30_target0.90_mridge` | 1,818 | `p40_cindex0.70_hazard2.00_censor0.30_target0.95_mridge` | 5,713 | 3.14× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.70_hazard2.00_censor0.50_target0.90_mridge` | 2,602 | `p40_cindex0.70_hazard2.00_censor0.50_target0.95_mridge` | 9,144 | 3.51× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p40_cindex0.75_hazard0.50_censor0.10_target0.85_mridge` | 297 | `p40_cindex0.75_hazard0.50_censor0.10_target0.90_mridge` | 973 | 3.28× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.75_hazard0.50_censor0.50_target0.90_mridge` | 1,583 | `p40_cindex0.75_hazard0.50_censor0.50_target0.95_mridge` | 8,536 | 5.39× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.75_hazard2.00_censor0.30_target0.90_mridge` | 1,165 | `p40_cindex0.75_hazard2.00_censor0.30_target0.95_mridge` | 3,598 | 3.09× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p40_cindex0.75_hazard2.00_censor0.50_target0.80_mridge` | 266 | `p40_cindex0.75_hazard2.00_censor0.50_target0.85_mridge` | 1,067 | 4.01× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p40_cindex0.80_hazard0.50_censor0.10_target0.85_mridge` | 364 | `p40_cindex0.80_hazard0.50_censor0.10_target0.90_mridge` | 1,110 | 3.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p40_cindex0.80_hazard0.50_censor0.30_target0.90_mridge` | 782 | `p40_cindex0.80_hazard0.50_censor0.30_target0.95_mridge` | 2,567 | 3.28× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p40_cindex0.80_hazard1.00_censor0.10_target0.85_mridge` | 277 | `p40_cindex0.80_hazard1.00_censor0.10_target0.90_mridge` | 1,110 | 4.01× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p50_cindex0.60_hazard0.50_censor0.10_target0.85_mridge` | 1,273 | `p50_cindex0.60_hazard0.50_censor0.10_target0.90_mridge` | 7,416 | 5.83× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.60_hazard0.50_censor0.10_target0.90_mridge` | 7,416 | `p50_cindex0.60_hazard0.50_censor0.10_target0.95_mridge` | 23,134 | 3.12× higher | — | previous | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.60_hazard0.50_censor0.30_target0.90_mridge` | 6,519 | `p50_cindex0.60_hazard0.50_censor0.30_target0.95_mridge` | 38,080 | 5.84× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.60_hazard0.50_censor0.50_target0.90_mridge` | 6,668 | `p50_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 46,927 | 7.04× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p50_cindex0.60_hazard1.00_censor0.10_target0.85_mridge` | 3,708 | `p50_cindex0.60_hazard1.00_censor0.10_target0.90_mridge` | 14,832 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 12,449 | `p50_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | 106,688 | 8.57× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.60_hazard2.00_censor0.10_target0.90_mridge` | 7,756 | `p50_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | 23,651 | 3.05× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.60_hazard2.00_censor0.30_target0.90_mridge` | 9,520 | `p50_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | 33,670 | 3.54× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 10,104 | `p50_cindex0.60_hazard2.00_censor0.50_target0.95_mridge` | 47,239 | 4.68× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p50_cindex0.65_hazard0.50_censor0.10_target0.85_mridge` | 855 | `p50_cindex0.65_hazard0.50_censor0.10_target0.90_mridge` | 3,420 | 4.00× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p50_cindex0.65_hazard0.50_censor0.30_target0.85_mridge` | 1,098 | `p50_cindex0.65_hazard0.50_censor0.30_target0.90_mridge` | 4,156 | 3.79× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.65_hazard0.50_censor0.30_target0.90_mridge` | 4,156 | `p50_cindex0.65_hazard0.50_censor0.30_target0.95_mridge` | 17,568 | 4.23× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.65_hazard0.50_censor0.50_target0.90_mridge` | 5,660 | `p50_cindex0.65_hazard0.50_censor0.50_target0.95_mridge` | 17,514 | 3.09× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.65_hazard1.00_censor0.10_target0.90_mridge` | 3,041 | `p50_cindex0.65_hazard1.00_censor0.10_target0.95_mridge` | 10,768 | 3.54× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.70_hazard0.50_censor0.30_target0.90_mridge` | 2,142 | `p50_cindex0.70_hazard0.50_censor0.30_target0.95_mridge` | 6,747 | 3.15× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.70_hazard0.50_censor0.50_target0.90_mridge` | 3,402 | `p50_cindex0.70_hazard0.50_censor0.50_target0.95_mridge` | 11,432 | 3.36× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.70_hazard1.00_censor0.10_target0.90_mridge` | 1,607 | `p50_cindex0.70_hazard1.00_censor0.10_target0.95_mridge` | 4,833 | 3.01× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.70_hazard1.00_censor0.30_target0.90_mridge` | 1,921 | `p50_cindex0.70_hazard1.00_censor0.30_target0.95_mridge` | 6,536 | 3.40× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.70_hazard1.00_censor0.50_target0.90_mridge` | 2,858 | `p50_cindex0.70_hazard1.00_censor0.50_target0.95_mridge` | 8,574 | 3.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.70_hazard2.00_censor0.10_target0.90_mridge` | 1,588 | `p50_cindex0.70_hazard2.00_censor0.10_target0.95_mridge` | 6,352 | 4.00× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.70_hazard2.00_censor0.50_target0.90_mridge` | 1,429 | `p50_cindex0.70_hazard2.00_censor0.50_target0.95_mridge` | 9,716 | 6.80× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.75_hazard0.50_censor0.10_target0.90_mridge` | 1,144 | `p50_cindex0.75_hazard0.50_censor0.10_target0.95_mridge` | 5,928 | 5.18× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.75_hazard0.50_censor0.30_target0.90_mridge` | 913 | `p50_cindex0.75_hazard0.50_censor0.30_target0.95_mridge` | 3,818 | 4.18× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p50_cindex0.75_hazard0.50_censor0.50_target0.85_mridge` | 770 | `p50_cindex0.75_hazard0.50_censor0.50_target0.90_mridge` | 2,666 | 3.46× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.75_hazard1.00_censor0.50_target0.90_mridge` | 1,333 | `p50_cindex0.75_hazard1.00_censor0.50_target0.95_mridge` | 5,712 | 4.29× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.75_hazard2.00_censor0.50_target0.90_mridge` | 1,669 | `p50_cindex0.75_hazard2.00_censor0.50_target0.95_mridge` | 5,135 | 3.08× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.80_hazard0.50_censor0.10_target0.90_mridge` | 663 | `p50_cindex0.80_hazard0.50_censor0.10_target0.95_mridge` | 2,207 | 3.33× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p50_cindex0.80_hazard0.50_censor0.50_target0.85_mridge` | 637 | `p50_cindex0.80_hazard0.50_censor0.50_target0.90_mridge` | 2,500 | 3.92× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p50_cindex0.85_hazard0.50_censor0.10_target0.90_mridge` | 327 | `p50_cindex0.85_hazard0.50_censor0.10_target0.95_mridge` | 1,412 | 4.32× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.60_hazard0.50_censor0.10_target0.85_mridge` | 749 | `p5_cindex0.60_hazard0.50_censor0.10_target0.90_mridge` | 2,976 | 3.97× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.60_hazard0.50_censor0.10_target0.90_mridge` | 2,976 | `p5_cindex0.60_hazard0.50_censor0.10_target0.95_mridge` | 21,361 | 7.18× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.60_hazard0.50_censor0.30_target0.85_mridge` | 878 | `p5_cindex0.60_hazard0.50_censor0.30_target0.90_mridge` | 3,776 | 4.30× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.60_hazard0.50_censor0.50_target0.90_mridge` | 3,027 | `p5_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 12,905 | 4.26× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.60_hazard1.00_censor0.10_target0.90_mridge` | 1,817 | `p5_cindex0.60_hazard1.00_censor0.10_target0.95_mridge` | 11,904 | 6.55× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.60_hazard1.00_censor0.50_target0.90_mridge` | 3,278 | `p5_cindex0.60_hazard1.00_censor0.50_target0.95_mridge` | 10,688 | 3.26× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.60_hazard2.00_censor0.10_target0.90_mridge` | 1,689 | `p5_cindex0.60_hazard2.00_censor0.10_target0.95_mridge` | 5,885 | 3.48× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.60_hazard2.00_censor0.50_target0.85_mridge` | 1,379 | `p5_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 5,344 | 3.88× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 5,344 | `p5_cindex0.60_hazard2.00_censor0.50_target0.95_mridge` | 26,720 | 5.00× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard0.50_censor0.10_target0.90_mridge` | 721 | `p5_cindex0.65_hazard0.50_censor0.10_target0.95_mridge` | 4,076 | 5.65× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard0.50_censor0.30_target0.90_mridge` | 924 | `p5_cindex0.65_hazard0.50_censor0.30_target0.95_mridge` | 6,976 | 7.55× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard0.50_censor0.50_target0.90_mridge` | 1,223 | `p5_cindex0.65_hazard0.50_censor0.50_target0.95_mridge` | 4,928 | 4.03× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard1.00_censor0.10_target0.90_mridge` | 690 | `p5_cindex0.65_hazard1.00_censor0.10_target0.95_mridge` | 2,752 | 3.99× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard1.00_censor0.30_target0.90_mridge` | 968 | `p5_cindex0.65_hazard1.00_censor0.30_target0.95_mridge` | 7,174 | 7.41× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard1.00_censor0.50_target0.90_mridge` | 1,232 | `p5_cindex0.65_hazard1.00_censor0.50_target0.95_mridge` | 6,648 | 5.40× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.65_hazard2.00_censor0.10_target0.90_mridge` | 850 | `p5_cindex0.65_hazard2.00_censor0.10_target0.95_mridge` | 2,752 | 3.24× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.70_hazard1.00_censor0.10_target0.90_mridge` | 445 | `p5_cindex0.70_hazard1.00_censor0.10_target0.95_mridge` | 2,123 | 4.77× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.70_hazard1.00_censor0.50_target0.90_mridge` | 798 | `p5_cindex0.70_hazard1.00_censor0.50_target0.95_mridge` | 3,643 | 4.57× higher | — | — | — |
| Target slope: 0.8 → 0.85 | `p5_cindex0.70_hazard2.00_censor0.10_target0.80_mridge` | 80 | `p5_cindex0.70_hazard2.00_censor0.10_target0.85_mridge` | 268 | 3.35× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.70_hazard2.00_censor0.30_target0.90_mridge` | 717 | `p5_cindex0.70_hazard2.00_censor0.30_target0.95_mridge` | 3,232 | 4.51× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.70_hazard2.00_censor0.50_target0.90_mridge` | 1,144 | `p5_cindex0.70_hazard2.00_censor0.50_target0.95_mridge` | 4,933 | 4.31× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.75_hazard0.50_censor0.10_target0.90_mridge` | 326 | `p5_cindex0.75_hazard0.50_censor0.10_target0.95_mridge` | 1,200 | 3.68× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.75_hazard0.50_censor0.30_target0.90_mridge` | 421 | `p5_cindex0.75_hazard0.50_censor0.30_target0.95_mridge` | 3,040 | 7.22× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.75_hazard1.00_censor0.10_target0.85_mridge` | 70 | `p5_cindex0.75_hazard1.00_censor0.10_target0.90_mridge` | 306 | 4.37× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.75_hazard1.00_censor0.10_target0.90_mridge` | 306 | `p5_cindex0.75_hazard1.00_censor0.10_target0.95_mridge` | 1,200 | 3.92× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.75_hazard1.00_censor0.30_target0.90_mridge` | 384 | `p5_cindex0.75_hazard1.00_censor0.30_target0.95_mridge` | 3,040 | 7.92× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.75_hazard1.00_censor0.50_target0.90_mridge` | 547 | `p5_cindex0.75_hazard1.00_censor0.50_target0.95_mridge` | 4,078 | 7.46× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.80_hazard1.00_censor0.10_target0.85_mridge` | 137 | `p5_cindex0.80_hazard1.00_censor0.10_target0.90_mridge` | 560 | 4.09× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.80_hazard1.00_censor0.30_target0.85_mridge` | 85 | `p5_cindex0.80_hazard1.00_censor0.30_target0.90_mridge` | 712 | 8.38× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.80_hazard2.00_censor0.10_target0.90_mridge` | 232 | `p5_cindex0.80_hazard2.00_censor0.10_target0.95_mridge` | 1,120 | 4.83× higher | — | next | next |
| Target slope: 0.9 → 0.95 | `p5_cindex0.80_hazard2.00_censor0.30_target0.90_mridge` | 356 | `p5_cindex0.80_hazard2.00_censor0.30_target0.95_mridge` | 2,848 | 8.00× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.85_hazard0.50_censor0.10_target0.85_mridge` | 66 | `p5_cindex0.85_hazard0.50_censor0.10_target0.90_mridge` | 528 | 8.00× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.85_hazard1.00_censor0.10_target0.85_mridge` | 196 | `p5_cindex0.85_hazard1.00_censor0.10_target0.90_mridge` | 1,056 | 5.39× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p5_cindex0.85_hazard1.00_censor0.50_target0.85_mridge` | 128 | `p5_cindex0.85_hazard1.00_censor0.50_target0.90_mridge` | 944 | 7.38× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.85_hazard1.00_censor0.50_target0.90_mridge` | 944 | `p5_cindex0.85_hazard1.00_censor0.50_target0.95_mridge` | 4,855 | 5.14× higher | — | previous, next | — |
| Target slope: 0.9 → 0.95 | `p5_cindex0.85_hazard2.00_censor0.10_target0.90_mridge` | 1,347 | `p5_cindex0.85_hazard2.00_censor0.10_target0.95_mridge` | 4,111 | 3.05× higher | — | previous | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.60_hazard0.50_censor0.10_target0.90_mridge` | 8,297 | `p75_cindex0.60_hazard0.50_censor0.10_target0.95_mridge` | 33,391 | 4.02× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p75_cindex0.60_hazard0.50_censor0.50_target0.85_mridge` | 2,409 | `p75_cindex0.60_hazard0.50_censor0.50_target0.90_mridge` | 17,908 | 7.43× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.60_hazard0.50_censor0.50_target0.90_mridge` | 17,908 | `p75_cindex0.60_hazard0.50_censor0.50_target0.95_mridge` | 80,000 | 4.47× higher | — | previous | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.60_hazard1.00_censor0.30_target0.90_mridge` | 10,466 | `p75_cindex0.60_hazard1.00_censor0.30_target0.95_mridge` | 57,120 | 5.46× higher | — | next | — |
| Target slope: 0.8 → 0.85 | `p75_cindex0.60_hazard1.00_censor0.50_target0.80_mridge` | 2,500 | `p75_cindex0.60_hazard1.00_censor0.50_target0.85_mridge` | 7,515 | 3.01× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p75_cindex0.60_hazard2.00_censor0.10_target0.85_mridge` | 2,776 | `p75_cindex0.60_hazard2.00_censor0.10_target0.90_mridge` | 8,863 | 3.19× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.60_hazard2.00_censor0.30_target0.90_mridge` | 12,103 | `p75_cindex0.60_hazard2.00_censor0.30_target0.95_mridge` | 40,282 | 3.33× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p75_cindex0.60_hazard2.00_censor0.50_target0.85_mridge` | 4,908 | `p75_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 16,845 | 3.43× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.60_hazard2.00_censor0.50_target0.90_mridge` | 16,845 | `p75_cindex0.60_hazard2.00_censor0.50_target0.95_mridge` | 61,348 | 3.64× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.65_hazard0.50_censor0.30_target0.90_mridge` | 3,296 | `p75_cindex0.65_hazard0.50_censor0.30_target0.95_mridge` | 16,557 | 5.02× higher | — | next | — |
| Target slope: 0.85 → 0.9 | `p75_cindex0.65_hazard0.50_censor0.50_target0.85_mridge` | 3,018 | `p75_cindex0.65_hazard0.50_censor0.50_target0.90_mridge` | 9,232 | 3.06× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.65_hazard1.00_censor0.10_target0.90_mridge` | 3,224 | `p75_cindex0.65_hazard1.00_censor0.10_target0.95_mridge` | 12,418 | 3.85× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.65_hazard1.00_censor0.30_target0.90_mridge` | 5,287 | `p75_cindex0.65_hazard1.00_censor0.30_target0.95_mridge` | 26,368 | 4.99× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p75_cindex0.65_hazard2.00_censor0.10_target0.85_mridge` | 1,284 | `p75_cindex0.65_hazard2.00_censor0.10_target0.90_mridge` | 4,017 | 3.13× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.65_hazard2.00_censor0.30_target0.90_mridge` | 4,785 | `p75_cindex0.65_hazard2.00_censor0.30_target0.95_mridge` | 15,094 | 3.15× higher | — | — | — |
| Target slope: 0.8 → 0.9 | `p75_cindex0.65_hazard2.00_censor0.50_target0.80_mridge` | 1,678 | `p75_cindex0.65_hazard2.00_censor0.50_target0.90_mridge` | 9,232 | 5.50× higher | Yes | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.70_hazard1.00_censor0.10_target0.90_mridge` | 1,142 | `p75_cindex0.70_hazard1.00_censor0.10_target0.95_mridge` | 6,082 | 5.33× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.70_hazard1.00_censor0.30_target0.90_mridge` | 2,524 | `p75_cindex0.70_hazard1.00_censor0.30_target0.95_mridge` | 8,127 | 3.22× higher | — | — | — |
| Target slope: 0.85 → 0.9 | `p75_cindex0.70_hazard1.00_censor0.50_target0.85_mridge` | 1,058 | `p75_cindex0.70_hazard1.00_censor0.50_target0.90_mridge` | 3,660 | 3.46× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.70_hazard1.00_censor0.50_target0.90_mridge` | 3,660 | `p75_cindex0.70_hazard1.00_censor0.50_target0.95_mridge` | 11,580 | 3.16× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.70_hazard2.00_censor0.10_target0.90_mridge` | 1,977 | `p75_cindex0.70_hazard2.00_censor0.10_target0.95_mridge` | 6,508 | 3.29× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.70_hazard2.00_censor0.50_target0.90_mridge` | 3,464 | `p75_cindex0.70_hazard2.00_censor0.50_target0.95_mridge` | 11,382 | 3.29× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.75_hazard0.50_censor0.10_target0.90_mridge` | 1,226 | `p75_cindex0.75_hazard0.50_censor0.10_target0.95_mridge` | 4,444 | 3.62× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.75_hazard0.50_censor0.50_target0.90_mridge` | 1,516 | `p75_cindex0.75_hazard0.50_censor0.50_target0.95_mridge` | 8,000 | 5.28× higher | — | next | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.75_hazard1.00_censor0.10_target0.90_mridge` | 1,301 | `p75_cindex0.75_hazard1.00_censor0.10_target0.95_mridge` | 4,139 | 3.18× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.75_hazard1.00_censor0.30_target0.90_mridge` | 1,723 | `p75_cindex0.75_hazard1.00_censor0.30_target0.95_mridge` | 5,712 | 3.32× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.75_hazard1.00_censor0.50_target0.90_mridge` | 2,113 | `p75_cindex0.75_hazard1.00_censor0.50_target0.95_mridge` | 7,474 | 3.54× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.75_hazard2.00_censor0.10_target0.90_mridge` | 1,111 | `p75_cindex0.75_hazard2.00_censor0.10_target0.95_mridge` | 3,700 | 3.33× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.75_hazard2.00_censor0.30_target0.90_mridge` | 1,400 | `p75_cindex0.75_hazard2.00_censor0.30_target0.95_mridge` | 4,795 | 3.42× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.75_hazard2.00_censor0.50_target0.90_mridge` | 1,968 | `p75_cindex0.75_hazard2.00_censor0.50_target0.95_mridge` | 8,000 | 4.07× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.85_hazard0.50_censor0.50_target0.90_mridge` | 1,069 | `p75_cindex0.85_hazard0.50_censor0.50_target0.95_mridge` | 3,530 | 3.30× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.85_hazard1.00_censor0.50_target0.90_mridge` | 1,094 | `p75_cindex0.85_hazard1.00_censor0.50_target0.95_mridge` | 3,530 | 3.23× higher | — | — | — |
| Target slope: 0.9 → 0.95 | `p75_cindex0.85_hazard2.00_censor0.50_target0.90_mridge` | 1,164 | `p75_cindex0.85_hazard2.00_censor0.50_target0.95_mridge` | 3,530 | 3.03× higher | — | — | — |
