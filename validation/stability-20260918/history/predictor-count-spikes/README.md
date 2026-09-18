# Investigation of an isolated predictor-count peak

This study starts from **main at a88e701**, without either proposed fix commit. It tests the original reported binary ridge slice: prevalence 0.25, maximum achievable C-statistic 0.85, target calibration slope 0.90, 10/15/20 signal predictors, no noise predictors, complexity 1. The cache reports N=1,288 / 10,672 / 1,764.

The hypothesis under test is that the preliminary adaptive search excludes the real crossing at the peak. It is not assumed to be the cause of every cache anomaly.

## Protocol

- Six original-code searches: original cache seeds 1077/1725/2373 at 10/15/20 predictors, plus shared public-call seed 48 at each count. Each requests **1,000 GP replications**. The original adaptive stage retains its 500-rep budget and 20-rep batches.
- Public-call arguments and RNG at engine entry are captured once per case. Running the original engine from that state preserves the original search trajectory while omitting the wrapper's later auxiliary AUC fit.
- Observers save stage-1 traces, the original fixed test dataset, and RNG at GP entry. They consume no RNG and change no search decisions. Stage-1 verbose messages are enabled solely for observation.
- Independent reference curves use **1,000 fresh training/test/model draws per grid point**. Initial grid N=1,000 / 2,000 / 4,000 for each original cache-seed generator. Shared-seed generators receive independent endpoint checks; additional points may be added if the common domain is unsupported.
- Use a common broad domain containing all original pilot Ns and independently checked Ns. Reference below/above pairs establish supported crossings inside it; unsupported cases remain flagged. The exact domain and support are written before interventions run. See PROTOCOL-AMENDMENTS.md for the clarification that the domain must permit the original large answer.
- Six intervention searches bypass stage 1 and retain the original GP, tuned generator, metric, and settings. Each starts from its baseline RNG at GP entry and requests **1,000 reps**. Only the boundaries differ.
- Independently check each baseline and intervention answer with **1,000 fresh draws**. No returned N is corrected.

Ridge calibration-slope search uses **q20(CSSE)** internally, where CSSE=-(1-slope)^2 and the target is -0.01. Displayed equivalent slope is 1-sqrt(-q20(CSSE)); it is **not the raw slope's q20**. The study does not change this criterion or test_n=30,000.

Quantile intervals are binomial order-statistic 95% intervals. They describe individual validation points; they are not simultaneous guarantees or intervals for the minimum N. A grid crossing is an interval, not an exact minimum. Local smoothness across three counts is tested rather than enforced. This small study covers one slice and two seeds per count, not the entire cache.

## Execution

Run from the repository root. `prepare.R` needs the sibling pmsims-chatbot cache and scenario grid. Once prepared, raw RDS arguments and RNG states permit reruns without retuning. `run-search.R ID baseline`, `run-grid.R ID N SEED`, `run-search.R ID common-bounds`, and `run-check.R ID MODE SEED` run individual jobs. `run-batch.py JOBS_JSON` runs an explicit job list with at most three independent R processes. The reference and final pipeline drivers share at most four fitting slots and resume completed cells. Early peak and recovery jobs were also scheduled separately; see PROTOCOL-AMENDMENTS.md. Exact job plans are preserved with the study.

The preparation writes `cases.csv` and prepared RDS files. `plan-jobs.py` writes the baseline/reference/intervention/check manifests. Run baseline searches, then reference checks and `select-bounds.R`, then common-bound searches and answer checks. `run-reference-pipeline.py` and `run-final-pipeline.py` coordinate these dependencies. After all jobs finish, run `Rscript validation/predictor-count-spikes/analyse.R`, then `Rscript validation/predictor-count-spikes/make-report.R`. Read RESULTS.md for findings and GP-NOTES.md for the installed package's final-point selection rule.

Scripts, summaries, plots and the final findings belong on this investigation branch. Raw RDS draws and progress logs are excluded by the study-local .gitignore. Production R code is unchanged.

After the simulations complete, `Rscript validation/predictor-count-spikes/verify.R` checks saved draws, quantiles, matched GP-entry RNG states, absence of GP error fallback values, and unchanged production code. It requires the local raw RDS files and performs no new simulations.
