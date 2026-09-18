# Calibration-slope sample-size search: instability investigation

**Status:** root cause located and empirically confirmed; fix not yet started.
**Where this came from:** investigation in the `pmsims-chatbot` repo (sibling
directory, `../pmsims-chatbot` from here), triggered by a user-reported bad
number in the chatbot's "Explorer" tool. Written up here as a starting point
for fixing it in `pmsims` itself, on a fresh branch.

---

## 1. The symptom

A user asked the Explorer tool (in `pmsims-chatbot`) for: binary outcome,
15 signal predictors, 25% prevalence, max achievable C-statistic 0.85, target
calibration slope 0.90, model = ridge regression. The app reported **10,672
participants required**. The user ran the equivalent `simulate_binary()` call
locally and got **1,691** — a number in line with every neighbouring scenario
in the grid. The app's number was **6.3x too high**, and — critically — it
wasn't a case of "calibration slope 0.95 legitimately needs a huge N somewhere
near a feasibility boundary." This was an ordinary, unremarkable scenario
(cstat 0.85 is the *easiest* level in the grid, target 0.90 is not extreme).

## 2. Scope: a systematic cache audit

`pmsims-chatbot` serves a pre-computed cache of `simulate_binary` /
`simulate_continuous` / `simulate_survival` results
(`cache/versions/cache-1f-create-20260827-161720.json`, a full factorial grid:
10 predictor counts × grid-specific performance/target/etc levels × 3 model
classes per outcome type, ~13,680 rows total). Believing the cache was built
on `pmsims` 1.0.0 (see §5), I swept every 1-D slice of the grid (holding every
other dimension fixed, varying one at a time) looking for cells that broke an
otherwise smooth local trend.

**Method:** for every `(outcome_type, model, swept_dimension, other-dims-fixed)`
combination, compare each interior point's `minimum_sample_size` to its two
immediate grid-neighbours (in the swept dimension). Two tiers:

- **Tier 1 — isolated single-cell spikes.** Both neighbours present, both
  neighbours agree with each other (within 1.6x), but the candidate point
  deviates ≥3x from their average. This is the same signature as the original
  ridge report: a smooth, low-magnitude neighbourhood with one point way off.
- **Tier 2 — raw consecutive-grid-step jumps ≥5x**, no smoothness requirement
  on the far side. Broader net, weaker per-cell confidence.

**Tier 1 results — 206 rows, split by outcome type / model:**

| outcome/model | count |
|---|---|
| binary/ridge | 96 |
| binary/lasso | 39 |
| binary/glm | 35 |
| survival/ridge | 17 |
| survival/lasso | 9 |
| survival/coxph | 9 |
| continuous/lasso | 1 |
| continuous/lm, continuous/ridge | 0 |

Regularised models (`ridge`, `lasso`) dominate — roughly 2.5–2.7x the rate of
`glm`/`coxph` within the same outcome type. `continuous` is almost entirely
clean (1 row out of 720).

**Tier 2 — 1,497 adjacent-pair jumps ≥5x.** Heavily loaded onto the
`target_performance` axis: 148 of the 206 Tier-1 spikes sit at
`target_performance = 0.95` (72%), and the `('binary','ridge','target_performance')`
/ `('binary','lasso','target_performance')` buckets alone account for 202 and
182 of the Tier-2 pairs respectively (out of 1,497 total).

**A side observation, not fully chased down:** ~15% of Tier-2 jumps (225/1497)
have a ratio within 2% of an exact power of two (e.g. one binary/glm cell:
neighbour = 5,000 at target 0.90, flagged cell = 5,120,000 at target 0.95 —
exactly 1024x = 2^10). This *looks* like a doubling-based bracket-expansion
procedure hitting some ceiling and reporting it as an answer, but this wasn't
independently verified beyond the pattern-matching — worth checking whether
`calculate_adaptive_bounds()`'s `*0.8` / `*1.2` fallback (§3) is somehow being
applied repeatedly, or whether this is coincidental.

## 3. Root cause: the two-stage search, and where it can fail

Sample-size search happens in two stages (both surfaced in the CLI output as
"Estimating first stage... (Adaptive starting value search algorithm)" then
"Estimating second stage... (Gaussian process algorithm)"):

**Stage 1 — `calculate_adaptive_bounds()` / `adaptive_startvalues()`**
(`R/start_values.R`). Runs a small bisection search, simulating
`n_reps_per` replicates at each candidate N (single-threaded by default —
`parallel = FALSE`, and nothing in the cache pipeline overrides this). At each
candidate N it builds a normal-approximation CI around the aggregated
performance estimate (`est ± se * qnorm(0.975)`), then (`adaptive_startvalues()`,
lines 84–106):

- **lower bound** = the largest N whose CI upper bound is still confidently
  below target; if no such N was tried, **falls back to `min(n) * 0.8`**.
- **upper bound** = the smallest N whose CI lower bound is confidently above
  target; if no such N was tried, **falls back to `max(n) * 1.2`**.

**Stage 2 — `engines.R:262-290`.** Calls `mlpwr::find.design(..., boundaries =
c(start_min_sample_size, start_max_sample_size), ...)` — a Gaussian-process
surrogate search that is **hard-bounded to stage 1's bracket**. It cannot
report anything outside that range, however wrong the range is.

**The failure mode:** stage 1's CIs are built from a small number of noisy
Monte Carlo replicates. If a particular random draw makes the bisection never
actually straddle the true crossing point — plausible whenever performance
increases slowly with N (weak achievable discrimination, a demanding target,
or apparently something about the regularised-model path making this worse,
see below) — the `*0.8`/`*1.2` fallback multipliers produce a bracket that
doesn't contain the true answer at all. Stage 2 then faithfully searches
within that wrong bracket and reports a boundary value, which is not a
genuine converged estimate.

There's a related, disabled safety net already in the code
(`R/start_values.R`, `large_perf_check` param, default `FALSE`): a
preliminary large-sample probe that would catch an unreachable target early.
Its docstring says it's off *"because the preliminary large-sample fit did
not work reliably for some machine-learning models"* — i.e. this exact class
of fragility was already known, and the chosen default (plateau/CI detection
alone) is a known compromise, not an oversight.

**Why regularised models look worse:** `cache/cache_lib.R` (in
`pmsims-chatbot`) has this comment, describing behaviour that "landed in
pmsims 1.0.0":

> The calibration slope is unstable to optimise under shrinkage, so the
> search for lasso/ridge (and rf/xgboost) still runs on the squared-error
> scale ("csse") internally. As of pmsims 1.0.0 the package does that
> conversion itself: `plan_internal_csse()` converts the target with
> `-(1 - S)^2` [...] and `restore_calibration_slope_scale()` converts
> `perf_n` back to a slope before returning.

So for `lasso`/`ridge`, stage 1/2 actually search on a *transformed* scale and
convert back at the end. That transform-and-restore path is a plausible extra
source of fragility specific to regularised models, on top of the general
bracket-fallback issue above — worth checking `plan_internal_csse()` /
`restore_calibration_slope_scale()` (search the package for these names) as
part of any fix.

## 4. Empirical verification

Three cells were re-simulated locally (`pkgload::load_all("../pmsims")`,
i.e. this checkout, `DESCRIPTION` version 1.0.0) with the pipeline's real
fixed params (`noise_parameters=0, complexity=1, mean_or_assurance=
"assurance", n_reps_total=1000, metric="calibration_slope"` — see
`pmsims-chatbot/cache/scenarios_grid.R`'s `FIXED_PARAMS` and
`cache/cache_lib.R`'s `run_binary()`/`run_survival()`):

| scenario | cache value | fresh re-run | ratio | slope achieved at re-run's N |
|---|---|---|---|---|
| binary/glm, sp=5, prev=0.2, cstat=0.6, target=0.95 | 5,120,000 | 4,446 | 1,152x too high | 0.871 (target not met — likely at/past the edge of feasibility for cstat=0.6) |
| binary/lasso, sp=5, prev=0.05, cstat=0.75, target=0.95 | 341,248 | 16,405 | 20.8x too high | 0.941 (close, clean convergence — cstat 0.75 is **not** a documented hard corner) |
| binary/ridge, sp=5, prev=0.4, cstat=0.6, target=0.95 | 135,168 | 33,792 | 4.0x too high | 0.910 (not quite met, but far more plausible) |

The lasso case is the cleanest evidence of a real bug rather than "genuinely
hard parameter region": cstat=0.75 is an ordinary, non-extreme discrimination
level, so there's no legitimate reason for a 20.8x gap.

**Seed reproducibility, confirmed directly.** Each cache row's seed is
deterministic — `seed = offset + row_index` within the phase's grid-building
order (`cache/cache_lib.R:build_manifest()`, `pmsims-chatbot`; offset 0 for
binary, 100000 continuous, 200000 survival; row order from
`tidyr::expand_grid()`, which varies the *last* listed dimension fastest).
For the lasso cell above, this resolves to **seed 47**. Re-running with
`set.seed(47L)` before the identical `simulate_binary()` call reproduced
stage 1's bracket as `min sample size = 341,248, max sample size = 682,496`
— **341,248 is exactly the cache's stored value.** This is strong,
direct confirmation that:

- the search is fully deterministic given a fixed seed (this isn't run-to-run
  flakiness in the sense of "retry and you might get a different number");
- the bad number is a reproducible *consequence* of one specific seed's noisy
  trajectory tripping stage 1's fallback logic, not corruption or a
  data-pipeline bug;
- each cache cell's seed (`offset + row_index`) is arbitrary and uncorrelated
  with the scenario's actual difficulty — every cell got one dice roll, and
  harder scenarios (weak achievable performance, high targets, the
  CSSE-transform path) are more likely to roll badly.

(Full second-stage confirmation for this seed was abandoned — the tool
itself estimated ~2.8 hours given the bad bracket — but the bracket match
alone is decisive.)

## 5. What's confirmed vs. still open

**Confirmed:**
- The mechanism (§3) and that it's real, not "large-but-correct" (§4).
- Determinism given a seed (§4).
- Regularised models are disproportionately affected (§2 table).
- The currently-served `pmsims-chatbot` cache was almost certainly built on
  `pmsims` 1.0.0 — commit `2e7bf7a` in `pmsims-chatbot` ("Require pmsims 1.0.0
  for cache builds on both paths") pinned the build to the `v1.0.0` tag
  ~6.5 hours before the cache file's build timestamp, same day.

**Open / not yet chased down:**
- The power-of-two pattern in ~15% of Tier-2 jumps (§2) — plausible
  bracket-doubling artifact, not confirmed.
- Whether the CSSE transform path (`plan_internal_csse()` /
  `restore_calibration_slope_scale()`) has its own bug independent of the
  general stage-1 fragility, or just inherits/amplifies it.
- Tier 2 as a whole (1,497 pairs) is inferred-affected from 3 spot checks +
  the mechanism, not individually re-verified. Could be mostly real (my
  working assumption) or could include some genuinely-large-but-correct
  cells, especially outside the `target=0.95` cluster.
- No fix has been designed yet. Candidate directions, roughly in order of
  invasiveness:
  1. Make the CI-based bracket-fallback in `adaptive_startvalues()` more
     conservative — e.g. widen the `*0.8`/`*1.2` fallback multipliers, or
     require more evidence before trusting a bracket that never actually
     straddled the target.
  2. Re-enable (or make default) something like `large_perf_check`, having
     first found out *why* it "did not work reliably for some
     machine-learning models" and fixing that instead of leaving it off.
  3. Increase `n_reps_per` (replicates per candidate N during stage 1) to
     shrink the CIs driving the bracket decision, at a runtime cost.
  4. Add a post-hoc sanity check: after stage 2 returns, verify the reported
     N actually achieves something close to the target (as the printed
     summary already computes and displays — "Performance at N = ... target
     ≥ ..." — this is already visible per-run, just not acted on
     programmatically), and flag/retry when it doesn't.

## 6. Reproducing this

From this repo (`pmsims`), with a `pmsims-chatbot` checkout at `../pmsims-chatbot`:

```r
pkgload::load_all(".")  # or wherever this checkout lives relative to pmsims-chatbot
result <- simulate_binary(
  signal_parameters = 5L, noise_parameters = 0L, complexity = 1L,
  outcome_prevalence = 0.05, maximum_achievable_cstatistic = 0.75,
  model = "lasso", metric = "calibration_slope", target_performance = 0.95,
  n_reps_total = 1000L, mean_or_assurance = "assurance"
)
# Unseeded: expect something in the ~16,000 range (fast, ~6 min).
# set.seed(47L) first to reproduce the cache's bad bracket (341,248) — this
# one is slow: stage 1 alone estimates ~2.8h once seeded this way.
```

The full Tier-1/Tier-2 sweep script (Python, reads the cache JSON directly,
no R needed, runs in seconds) is not currently checked into either repo —
it lived in a scratch location during the investigation session. Worth
recreating properly (e.g. as a `pmsims-chatbot` maintenance script) rather
than re-deriving from scratch; the method is fully described in §2.
