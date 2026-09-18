# Stability work: fixes and provenance

The branch `validation/ridwan-dev-stability-20260918` starts at Ridwan Olaniran's latest fetched dev commit, **d00a137640d47a9f1a75482ead8109b1ae1e463d** (18 September 2026, “updated adaptive start values”). This study does not combine the different proposed repairs. Production R files match that commit; only study material and a package-build exclusion are added.

## Ridwan's repair being tested

Changes relative to dev at 38e55c4 affect R/start_values.R, R/engines.R and R/simulate_custom.R. The mlpwr adaptive stage defaults to seed 20240101, restoring an existing caller RNG stream. Adaptive summaries use a two-standard-error band; uncertain ladder points buy more replicates, up to four initial batches. Values are winsorised at median +/- five MADs. Assurance uses the Type-7 20th percentile and a 200-bootstrap standard error. Bounds come from all classified trace points. Plateau detection now compares gains with estimated noise. The second-stage GP and its answer are unchanged.

These changes can stabilise a pilot conditional on its data-generating function. The public wrapper's tuning still depends on its caller seed, and stage 2 still uses that caller stream. A fixed pilot seed alone does not establish correctness or full-call reproducibility across different seeds.

## Confirmed limitations of this commit

- The pilot still draws one test dataset and reuses it for all training fits. More training replications and bootstrap SEs do not estimate that test dataset's run-level shift.
- If no point is classified above target, the returned upper bound is twice the largest tested N. Plateau/budget termination can therefore hand an untested upper bound to the GP. [A deterministic diagnostic](review-diagnostics.csv) produces a below-target plateau, bounds 80–160 and no observed above-target point.
- Winsorisation changes the pilot's summary relative to the unchanged GP, which uses unmodified replicate metrics. Finite error fallback values are not counted by `n_fail`, because that counter only counts nonfinite values.
- Existing bisection and hybrid tests fail because simulate_custom passes adaptive_seed to engines that do not accept it. [The test log](existing-tests.txt) records 25 start-value assertions passed (three small-test-set warnings), 23 engine assertions passed, and 25 custom assertions passed with two errors.
- An existing RNG stream is restored, but a previously absent .Random.seed is left created by calculate_adaptive_bounds; the separate with_preserved_seed helper handles this case but is not used there. [The RNG diagnostic](rng-diagnostics.csv) records both cases.

These are code-review and controlled-diagnostic findings. The [new validation report](REPORT.md) separately reports observed neighbour ratios and independent target checks.

## Earlier work gathered here

| Source | Preserved material | Meaning |
|---|---|---|
| pmsims stash 3d77313, untracked parent 8b58444 | [Original investigation](history/notes/CALIBRATION-SLOPE-SEARCH-INSTABILITY.md), [refined problem/fix note](history/notes/CALIBRATION-SLOPE-PROBLEM-AND-FIX.md), [predictor-count study](history/predictor-count-spikes/README.md) | Historical diagnosis and partial experiment artifacts, saved before cleaning main on 17 September. Status statements reflect their original dates. |
| fix/calibration-slope-search-instability at 7a39ab7 | [Calibration search study and seed reproduction](history/calibration-slope-search/README.md) | Broader proposed repair, independent confirmation, and original-code seed-instability reproduction. Its production changes are not applied here. |
| validation/adaptive-stage1-calibration at b338c93 | [Adaptive validation](history/adaptive-stage1/RESULTS.md) | Local 1,000-replicate checks of a different repair using fresh test draws and supported bounds. It still showed second-stage shortfalls. Its production changes are not applied here. |
| fix/adaptive-stage1-calibration at 4b4bdf7 | Git branch retained | Later focused implementation and optional progress/time budget. No production changes imported into the Ridwan study. |
| pmsims-chatbot working copy | [3–10x cell report](history/cache-audit/PROBLEMATIC-GRID-CELLS-3-10X.md), [comparison appendix](history/cache-audit/PROBLEMATIC-GRID-CELLS-3-10X-COMPARISONS.md), known_bad_cells.json | Historical cache audit and 900 exclusions; new results do not change the serving cache. |

The [manifest](history/manifest.json) records every recovered artifact's exact source and SHA-256. Historical folders preserve their files without cherry-picking code into the package. Their executable scripts retain old path assumptions and are reference material. Use the scripts at the new study root for reruns. The source stash, branches and sibling originals are retained as backups; cleanup here means one discoverable, committed home for the evidence.
