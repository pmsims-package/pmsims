# Adaptive stage 1: a limited repair

**The original error:** stage 1 evaluates every fitted model on one fixed test
dataset. An unlucky test draw can shift its entire curve below target;
increasing training N cannot average away that fixed test-set error. When
doubling then plateaus, it returns its last two Ns as bounds anyway. The GP is
confined to that unsupported range and cannot recover the smaller crossing.

The reproduced GLM example gave bounds **5,120,000–10,240,000** for seed 298,
although both preliminary slopes were about **0.904**, below the **0.95** target.
Seed 1 returned **80,000** for the same inputs. These observations establish the
failure, not the true minimum N.

**This branch implements options 1 and 2:**

1. Generate a fresh test dataset and training dataset for every adaptive
   replicate, matching the GP's sampling scheme. Preserve the original mean
   and Type-7 20th-percentile summaries and existing finite error fallbacks.
2. Require a confidence interval wholly below target at the lower bound and
   wholly above target at the upper bound. Pool extra batches at ambiguous Ns,
   up to five batches before continuing exploration, within the
   existing 500-replicate stage-1 budget. Missing, reversed, exhausted or
   plateaued bounds produce an inconclusive-search error before stage 2 starts.

Mean intervals use Student's t; assurance intervals use binomial order
statistics. The plateau tolerance is relative to the remaining target gap, so
a change of metric units does not change stopping. A plateau never establishes
an unreachable target or a maximum achievable performance.

**Stage 2 is unchanged:** the same GP algorithm, hard bounds, quantile estimator,
budget and returned candidate. There is no production confirmation or correction
of its answer. The bisection and hybrid callers also reject unsuccessful initial
adaptive bounds; their subsequent search algorithms are unchanged.

This repairs the identified sampling mismatch and invalid-bound handoff across
metrics and models; it does **not** guarantee the global minimum or target
attainment in every scenario. Pointwise intervals inspected repeatedly do not
provide simultaneous 95% coverage. Nonmonotone curves remain a limitation of the
existing design. A slowly rising curve can still trigger an inconclusive plateau
stop; a regression test explicitly checks that this is not called unreachable.
The unchanged GP can also return a candidate whose independent interval overlaps
or falls below target. Those outcomes are reported in the local study.
The preliminary budget limits replicate counts, not maximum N or wall time;
automatic doubling can still be costly.

## Reproduce the small local study

Run from the repository root with the package's dependencies and `pkgload` and
`testthat` installed:

```sh
Rscript validation/adaptive-stage1/run-study.R glm_stress 1
Rscript validation/adaptive-stage1/run-study.R glm_stress 298
Rscript validation/adaptive-stage1/run-study.R ridge_reported 48
Rscript validation/adaptive-stage1/run-study.R continuous_control 47
Rscript validation/adaptive-stage1/make-report.R
```

Each public search requests **1,000 GP reps**. The original preliminary budget
remains 500, in batches of 20. Each independent check uses **1,000 reps** at the
lower bound, reported N and upper bound, with fresh training/test datasets and
separate recorded seeds. The observers do not reset search RNG or override bounds.
The library can slightly exceed the GP budget because of batching; actual counts
are recorded. Validation uses the internal metric, including CSSE, before
converting its display to an equivalent slope.

Per-case CSVs and session information are tracked; raw draws and saved objects
are retained locally as ignored RDS files. Results and their limits are described
in [RESULTS.md](RESULTS.md). This is a small regression study, not evidence of
reliability across all inputs or all seeds.

The implementation used for these runs is committed as `d60e988`; its R sources
for sampling and search were unchanged between launching the study and
committing the repair. Runtime accounting was subsequently corrected to count
pooled stage-1 replicates; that only changes the displayed cost estimate and
does not consume RNG or change the candidate.
