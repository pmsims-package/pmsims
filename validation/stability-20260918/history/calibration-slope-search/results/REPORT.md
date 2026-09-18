# Local validation of the calibration-slope search fix

**Status: development branch. Several underlying defects are corrected, but
this is not yet a generally validated minimum-sample-size solver.** The local
study was used while developing the changes; it is not a held-out study of
unseen scenarios. The implementation plan and reproduction commands are in
[the study README](../README.md).

## What was the problem?

The package is supposed to find the smallest training sample size N that meets
a performance target. It repeatedly generates data, fits the requested model,
and measures its performance. In assurance mode it searches for the 20th
percentile to meet the target: roughly 80% of replicate performances should
meet or exceed it. Finding a sufficiently large N and finding the minimum N
are different tasks.

The search first chooses a range of N values, then fits a Gaussian-process
learning curve inside that range. The main reproduced bug was that **failure
to find a target crossing was turned into an apparently valid search range**.
The default preliminary search doubled N until it judged performance flat,
then returned its last two doubled Ns as bounds, even when neither met the
target. The GP was forbidden to leave those bounds.

For seed 47's rare-outcome lasso case, this produced [341,248, 682,496]. Once
that range was supplied, the second stage could not return a smaller answer.
The large number reflected where preliminary doubling stopped, rather than
established evidence that so many participants were required. This exact
failure was reproduced. It does not establish the cause of every cache spike,
or reproduce the original ridge cache value of 10,672 under its cache seed.

Two underlying problems made this failure easier to trigger:

1. **The stopping threshold used the wrong scale.** For regularised models the
   slope target 0.95 becomes squared-error target -0.0025, with perfect
   calibration at zero. The old absolute plateau tolerance was 0.005: larger
   than that whole target-to-perfect gap. Small improvements could therefore
   be treated as a plateau while the target remained unmet. The squared-error
   conversion itself is algebraically correct.
2. **The two stages estimated different distributions.** Preliminary
   replicates reused one test dataset, whereas GP replicates generated fresh
   test data. Random test-set error could shift the preliminary curve even
   with perfect population calibration. Increasing training N cannot remove
   that conditional test-set error.

There is also a separate second-stage problem: a fitted GP curve can suggest
an N whose actual performance misses the target. Independent simulation
exposed this in a continuous control, not just the reported binary case.
Checking performance at the proposed N detects some such failures, but does
not by itself establish that N is minimal.

## How general are the changes?

No production rule checks a particular seed, predictor count, prevalence or
reported cache value. However, absence of scenario-specific code does not prove
a generally correct algorithm. The changes have different strengths:

| Change | What it addresses | What remains unproved |
|---|---|---|
| Fresh test data in every stage | A structural mismatch in the simulated distribution | General convergence and consistent treatment of failed replicates |
| Refuse automatic bounds without below/above-target evidence | Fabricated ranges from an unsuccessful search | That finite-sample, repeatedly inspected intervals contain the true crossing |
| Empirical assurance percentile throughout | A statistical estimator choice motivated by small-batch interpolation in the demonstrated example | That this choice is unbiased or preferable for every metric/distribution |
| Relative tolerance and overlapping-interval plateau stop | Excessive doubling and the old scale-dependent threshold | That the curve really cannot reach target later; these remain heuristics |
| Independent GP confirmation with upward retries | Clearly underperforming proposed Ns | Minimum N, overestimated Ns, or simultaneous error control across retries |

The fresh-test and no-fabricated-bounds changes correct general design defects.
The percentile change defines a consistent empirical estimator, but is not a
universal cure for finite-batch quantile bias. With 20 expected uniform order
statistics, default interpolation estimates 0.2286 for a population percentile
of 0.2; the empirical fourth order statistic instead estimates 0.1905. That
example shows a tradeoff, not proof of unbiasedness on arbitrary distributions.
Using type 1 is a statistical design choice, rather than proof that the previous
type 7 definition was inherently invalid. Its effect should be benchmarked
separately from changes to bounds and GP confirmation.

The new plateau and upward-retry rules need further work. Their constants
(four recent Ns, tolerance 0.005, growth factor 1.5, five confirmation looks)
were not validated on a predeclared set of unseen scenarios. Independent RNG
streams hold out datasets; they do not hold out the scenarios that influenced
algorithm development.

## Counterexamples found on review

[audit-generality.R](../audit-generality.R) exercises the current code with
1,000 synthetic replicate values per candidate. These are deterministic
logical checks, not Monte Carlo coverage or clinical-model validation studies.
Results are in [generality-audit.csv](generality-audit.csv).

* In mean mode, a smooth, increasing, bounded performance curve meets target 0.8 at
  N=6,693. The default new plateau rule stops at N=80 because the recent
  below-target intervals overlap. With the plateau stop disabled, exactly the
  same curve, replicate values and exploration budget establish a crossing
  range [10, 10,240]. Thus interval overlap does not justify concluding that
  further exploration is pointless. Returning no estimate avoids fabricating
  a number, but can prematurely reject a feasible search.
* On a noiseless monotone curve with exact minimum N=300, confirmation accepts
  a hypothetical GP proposal N=10,000 immediately. Its performance exceeds
  target, so no smaller N is investigated. Thus confirmation can accept a
  requirement over 33 times the true minimum. It is a diagnostic safeguard,
  not a replacement for a correct search for the minimum. This check supplies
  the GP proposal; it tests confirmation behavior, not the frequency of such
  GP errors in normal model runs.

These counterexamples prevent a claim that the complete branch is a general
solution to instability or overestimated sample-size requirements. The real
model results below remain useful development evidence, with that narrower
interpretation.

## Additional development evidence

The original investigation mixes two paths: the default `mlpwr` preliminary
search doubles N, whereas `mlpwr-bs` also uses bisection summaries. The hybrid's
multiplier fallbacks were a related issue, rather than the mechanism on the
exact default reproduction path.

The old preliminary stage reused one test dataset; the GP generated fresh test
data every replicate. For the reproduction's original test draw, the known
true predictor has raw slope 1.06927 and equivalent CSSE slope 0.93073. More
training participants cannot repair that conditional test bias. Independent
oracle results are recorded in [oracle-results.csv](oracle-results.csv).

A first attempted fix still failed independent validation: the public ridge
search selected N=1,609, but 1,000 holdout replicates gave equivalent slope
0.88366 (95% interval 0.87936–0.88945), below target 0.90. Default percentile
interpolation can overestimate lower percentiles in small assurance batches;
the uniform example above demonstrates this, rather than a universal bias law.
The current branch uses the empirical 20th percentile in preliminary, bisection and GP stages.
The failed check is retained in
[production-before-percentile-fix.csv](production-before-percentile-fix.csv).

## Development results and public-API validation

The baseline reproduction returned bounds [341,248, 682,496] after 576.7 seconds;
no GP was launched within that unsupported range. The current safeguard stops
with `plateau_without_bracket`, after 11 batches (220 preliminary replicates),
with largest evaluated N=21,328 and no estimate, taking 64.7 seconds. Failure to
establish a crossing does not prove a target is unreachable.

An unmodified public `simulate_binary()` call for the original 15-predictor
ridge scenario, seed 47, used 1,000 GP evaluations and returned **N=3,002**.
Its original GP prediction was 0.90270; independent production confirmation
(1,000 replicates) gave 0.91185. The independent 1,000-replicate
check gave **0.91202, 95% interval [0.90882, 0.91640]**, above target 0.90.
Final search runtime, including confirmation, was 633.9 seconds. See
[production-results.csv](production-results.csv). Matching the historical
N=1,691 alone would not verify target attainment. Adequate performance at
N=3,002 also does not establish that 3,002 is the minimum.

The empirical assurance estimator alone did not remove all GP failures. In the
continuous seed-47 control, GP N=150 independently gave 0.86097 (95% interval
[0.85091, 0.86933]) from 1,000 replicates. Current production confirmation rejected
N=150 and N=225, then accepted N=338. The before-confirmation checks are in
[continuous-before-confirmation.csv](continuous-before-confirmation.csv).

Confirmation uses 1,000 independent draws **per candidate**, with at most five
candidates within the established bounds. A candidate is rejected when its
upper confidence bound is below target; the final look is reserved for the
upper search bound. The returned performance is empirical confirmation
performance, while `mlpwr_ds$final` retains the original GP prediction and
`mlpwr_ds$validation` records the confirmation metrics and intervals. A
compatible interval can contain the target even when the point estimate is
slightly below it. Requiring the entire interval above target would change the
criterion toward a conservative upper requirement and inflate estimates.

## Ordinary scenarios

Four scenarios, seeds 47 and 48, each used a 1,000-replicate GP budget, default
tuning, 20-replicate batches, test N=30,000 and assurance aggregation. The ordinary
harness separates the GP RNG stream from stage 1 to allow exact resumption;
the public check above preserves the normal API RNG continuation. The final
holdout checks use 1,000 independent replicates at 0.5N, N and 2N per case.
Binary ridge uses 15 signal predictors, prevalence 0.25 and achievable
C-statistic 0.85. Binary GLM and lasso use five signal predictors, prevalence
0.20 and achievable C-statistic 0.75. Continuous ridge uses five signal
predictors and achievable R-squared 0.50. All use complexity 1 and no noise
predictors.

This is 24,000 ordinary validation replicates plus the public check's 1,000.
Measured GP evaluations were 1,000 in seven ordinary runs and 1,012 in
continuous seed 47: mlpwr rounds its final batch allocation. Actual counts
are recorded in `search-results.csv`; all confirmation and holdout batches
contained exactly 1,000 replicates.

| Scenario | Seed | Selected N | Holdout equivalent slope | 95% interval |
|---|---:|---:|---:|---:|
| Binary ridge (original report) | 47 | 2,016 | 0.89725 | [0.88800, 0.90365] |
| Binary ridge (original report) | 48 | 2,017 | 0.90212 | [0.89761, 0.90772] |
| Binary GLM | 47 | 1,272 | 0.90450 | [0.89828, 0.91291] |
| Binary GLM | 48 | 1,808 | 0.92004 | [0.91428, 0.92398] |
| Binary lasso | 47 | 1,597 | 0.89886 | [0.89352, 0.90499] |
| Binary lasso | 48 | 1,986 | 0.90711 | [0.90304, 0.91149] |
| Continuous ridge | 47 | 338 | 0.91986 | [0.91336, 0.92309] |
| Continuous ridge | 48 | 254 | 0.90055 | [0.89377, 0.90762] |

All eight selected-N intervals are compatible with target 0.90. The point
estimates range around or above target; this is not a claim that every
population percentile is proven above target. All eight half-N intervals are
below target, and all eight twice-N intervals are above target. See
[validation-results.csv](validation-results.csv) and
[the plot](validation.png). Search estimates and budgets are in
[search-results.csv](search-results.csv); the original GP candidates are in
[search-before-confirmation.csv](search-before-confirmation.csv).

The GP fits used the final empirical-percentile estimator. Confirmation was
appended after that unchanged GP work and run with its own seeded stream.
Preliminary traces were reused only after replay verified identical evaluation
order, batch counts, bounds and stop reasons for all nine fixed traces. Replay
results are in [replay-results.csv](replay-results.csv). The public run used the
complete final API and normal RNG continuation.

## Checks and limitations

The full test suite passes (one optional tuneRanger dependency test skipped).
R CMD check, including rebuilt vignettes, passes with **0 errors, 0 warnings,
0 notes**, using `--no-manual` and `_R_CHECK_FORCE_SUGGESTS_=false` because some
optional learners are unavailable. Formatting and whitespace checks pass.
[session-info.txt](session-info.txt) records versions, source hashes, seeds
and budgets. Raw draws, fitted objects and traces remain local as ignored RDS.

This is a small four-scenario, two-seed study plus one public-API scenario and
one rare-outcome failure reproduction. It does not establish behavior across
all survival/tree models or audit every chatbot-cache spike. Quantile intervals
are pointwise; adaptive repeated looks do not give simultaneous 95% coverage.
GP interpolation and Monte Carlo uncertainty remain. Confirmation can increase
N conservatively and is not proof of an exact global minimum. It adds up to
five further batches of `n_reps_total` draws (1,000 per candidate in this study)
and can stop without an estimate. Recorded runtimes reflect local concurrent
runs, not a controlled speed benchmark.

The rare-outcome lasso result validates rejection of the old artifact, not a
new minimum or proof of infeasibility. Independent oracle checks give equivalent assurance slopes near 0.9604 at
test N=30,000 and 0.9808 at test N=120,000, so target 0.95 remains sensitive to
test-set noise and training error. Targets without established brackets need
additional investigation or independently validated supplied bounds. The
oracle results describe the known true predictor under finite-test evaluation;
they are not rigorously proved upper bounds for every possible learner.

## Revised implementation and validation plan

1. Define the intended result before choosing search constants: a minimum-N
   interval, a conservative sufficient N, and a target-compatible point
   estimate have different guarantees. Specify performance/N tolerances,
   budget, failure policy and assumptions such as monotonicity. For assurance,
   explicitly define whether the criterion is a quantile or at least 80%
   probability of meeting target, including ties. The probability formulation
   can use success indicators and binomial intervals directly, reducing
   dependence on small-batch quantile interpolation.
2. Retain and separately review the structural corrections: consistent test
   sampling and explicit failure when no bracket exists. Review the assurance
   estimator choice separately. Use a common policy for failed replicates;
   preliminary intervals currently discard non-finite values while
   confirmation rejects them.
3. Replace the greedy confirmation retries with refinement of an evidenced
   crossing interval. Use GP proposals to choose evaluations, but let
   simulation evidence update both bounds: above-target evidence can lower the
   upper bound, and below-target evidence can raise the lower bound. Uncertain
   evaluations need more information or an explicit inconclusive result.
   Overlapping intervals alone should not terminate a feasible search.
   Account for error across candidate Ns and repeated looks, rather than
   interpreting ordinary pointwise intervals as a global coverage guarantee.
4. Freeze the algorithm, then evaluate a predeclared matrix of unseen scenarios
   and more than two seeds. Cover binary, continuous and survival outcomes;
   mean and assurance modes; unregularised and regularised learners; noise and
   nonlinear predictors; additional metrics; and difficult targets. Use
   1,000 replicates per candidate/independent evaluation as requested. Measure
   overestimation, underperformance, false abstention, crossing-interval width
   and runtime, not only whether the reported N performs adequately.
5. Include the two logical counterexamples, nonmonotone curves and failed-fit
   cases in that validation. Review the revised algorithm before release or
   cache rebuilding. The current branch should remain a development candidate
   rather than be rolled out as a generally validated minimum-sample-size fix.
   Arbitrary custom models or nonmonotone curves cannot be promised universal
   convergence under a finite budget.

Relevant methodological foundations include batched noisy root finding in
[Rodriguez and Ludkovski, Generalized Probabilistic Bisection](https://arxiv.org/abs/1711.00843),
and intervals valid over repeated sampling in
[Howard et al., Time-uniform confidence sequences](https://arxiv.org/abs/1810.08240).
These inform the proposed redesign; they do not validate this implementation
or remove the need to state and test its assumptions.
