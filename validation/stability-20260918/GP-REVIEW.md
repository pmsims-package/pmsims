# Unchanged GP final-selection rule

The installed dependency is **mlpwr 1.1.1**, also used by the earlier predictor-count study. This version's `get.pred` uses the same point-selection rule for exploration and for the final returned design. With the default `use_noise=TRUE`, its cost objective penalises a shortfall only when:

`GP mean + 0.3 * GP standard deviation < target`.

This is an optimistic allowance for uncertainty. It does not require the mean or a lower confidence bound to meet the target. The constraint is a soft penalty. The separate “bad prediction” check uses an absolute mean-to-target difference of 0.4, which is large compared with CSSE targets of -0.01 and -0.0025.

The current 15-predictor binary ridge cache-seed run returns N=1,871. At that N, GP mean is -0.01021732, SD is 0.0007346312, and the optimistic score is -0.009996928. That score reaches the -0.01 target although the mean does not. The independent 1,000-replicate check places performance below target too.

This code rule explains how a below-target predicted mean can be returned. It does not establish the cause or magnitude of every discrepancy between GP predictions and independent performance. Small per-point assurance batches, quantile estimation, fit quality and simulation noise remain relevant; this study does not isolate those effects or validate a different GP architecture.

`gp-diagnostics.R` records each saved answer's internal mean, SD, optimistic score, boundary status and per-point replication counts in gp-diagnostics.csv. It captures the current dependency source in mlpwr-get-pred-source.txt. It runs no new fits and consumes no random draws. The production engine remains unchanged from Ridwan's commit.
