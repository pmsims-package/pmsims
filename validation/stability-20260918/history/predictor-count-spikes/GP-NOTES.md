# What stage two returns

This note describes the **installed mlpwr 1.1.1** code used in this study, inspected with `mlpwr:::get.pred` and `mlpwr::find.design`. It is an additional diagnostic observation, not an intervention.

For the desired-performance task, with the default `use_noise=TRUE`, the point-selection objective combines cost with a large penalty when:

`GP mean + 0.3 × GP standard deviation < target`.

This is an optimistic allowance for uncertainty. It does not require the predicted mean itself, or a lower confidence bound, to reach the target. The final returned design uses this point-selection rule too. The package's separate “bad prediction” check uses an absolute discrepancy of 0.4; it does not certify attainment of the ridge CSSE target −0.01.

For the original 15-predictor generator with broad boundaries, the returned N=1,711 has GP mean −0.01033872 and GP standard deviation 0.001185672. Its optimistic score is −0.009983023, above the −0.01 target, although its predicted mean is below target. Independent 1,000-rep validation also places this N below target.

The acquisition rule explains how a below-target predicted mean can be returned. It does **not** by itself establish the cause or magnitude of every discrepancy with independent performance. GP fit quality and finite simulation noise remain relevant. `results/gp-decisions.csv` records these quantities for all completed searches.

Across the 12 searches, 11 final predicted means are below target. Three returned points also have optimistic scores below target. The objective is a soft penalty, not a target-attainment guarantee. The two baseline boundary cases are particularly clear: the 10-predictor shared-seed search returns its lower boundary and the 20-predictor cache-seed search its upper boundary despite predicted shortfalls.

Production GP code and its defaults were not changed. A future fix needs to distinguish points useful for exploration from a final estimate supported by evidence of target attainment.
