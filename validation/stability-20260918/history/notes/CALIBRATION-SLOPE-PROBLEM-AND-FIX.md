# Calibration-slope search: problem and suggested fix

## The problem

**A failed preliminary search is turned into an apparently valid sample-size
range. The final search is trapped inside that range.**

The package first chooses an interval, then runs a Gaussian process (GP) search
inside it to estimate the minimum N meeting the target. Two defects combine:

- The preliminary stage reuses **one random test dataset**. An unlucky draw can
  shift its entire performance curve downward. Increasing training N cannot
  average away that test-set error. The GP uses fresh test draws instead.
- When preliminary performance plateaus **below the target**, the original code
  nevertheless returns its last two Ns as bounds. The GP cannot look below that
  range. Its answer can therefore reflect where doubling stopped, rather than
  the minimum N required.

**Confirmed example:** seed 298 produced bounds **5,120,000–10,240,000**, although
both preliminary slope estimates were approximately **0.904**, below the **0.95**
target. Seed 1 returned **80,000** for the same inputs. Neither establishes the true
minimum.

## The suggested fix

1. **Use fresh training/test draws throughout**, with consistent aggregation and
   treatment of failed replicates.
2. **Accept bounds only with evidence below and above the target.** Otherwise
   report an inconclusive search, not a required N or an unreachable target.
3. **Use the GP to propose candidates; use simulations to refine both sides of
   the crossing.** Check smaller alternatives, specify stopping accuracy and
   report uncertainty about the minimum.

## What remains uncertain

**The reproduced failure is understood; a generally reliable fix is not yet
established.** The development branch's plateau heuristics can stop too early;
upward-only confirmation can accept oversized Ns. A complete replacement needs
uncertainty rules, handling of nonmonotone curves, and validation on unseen
scenarios/seeds with **1,000 reps per check**. This cause is confirmed for the
reproduced cases, not every cache anomaly.
