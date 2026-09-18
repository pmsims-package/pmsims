# Protocol clarification before the bounds intervention

The common GP domain will be the **union of all original pilot Ns and independently checked Ns**, shared by all cases. It includes the original large peak and upper bounds. This isolates the effect of stage 1's restrictions while leaving the GP free to return its original large N. A common upper limit of 4,000 would mechanically prevent the original N=10,672 and would therefore be a poor causal test.

Independent reference points establish whether a below/above pair lies inside this broad domain for each generator. They do not certify its endpoints or establish global monotonicity. Cases without independently observed above-target performance remain explicitly unsupported; a GP answer for such a case is not a validated minimum.

The initial 4,000-rep-N result for the shared-seed 10-predictor generator was below target. If necessary, additional N=8,000 checks will be run for the 10-predictor generators, with 1,000 fresh replications each, to distinguish a higher crossing from failure to establish one within the study. No assurance definition or production code is changed.

Held-out checks additionally record raw calibration slopes and whether CV selected the weakest penalty in the tested ridge path. They reuse each original fitted model, return the original CSSE unchanged, and draw no extra RNG. Raw-slope re-evaluation is checked against the original CSSE numerically. These observations are diagnostic, not a new optimisation criterion or proof that the ridge path is defective.

The two 15-predictor ablations and their planned held-out checks were scheduled early after their 4,000 reference points supported attainment. The broad numeric domain was computed from all six completed original pilots. It remains unchanged for later ablations; support summaries are updated as the remaining reference checks finish. The early schedule temporarily uses two additional fitting processes alongside the four-slot reference/final pipelines.

The first two early ablations failed in the native GP optimiser because read.csv inferred integer boundary columns. Original automatic bounds are doubles. The adapter now explicitly converts the common boundaries to numeric; the failed attempts are archived under raw/failed-integer-domain and excluded from scientific results. Both cases were restarted from their original saved baseline GP-entry RNG states.

One held-out process (p15-seed1725, baseline) encountered a script-stream error after writing all completed results: an active source file had been rewritten while adding scheduling locks. Its 1,000 draws, performance CSV/RDS and ridge metadata CSV/RDS were already saved and checked. These completed results were retained. Subsequent script changes use atomic replacement or wait until workers finish. The final scheduler was resumed after its active children completed; this did not change any study seed or refit completed cells.
