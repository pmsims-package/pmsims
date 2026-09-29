# Scaling sweep (perf-investigation)

This is a follow-up to `../perf-review-20260929/REPORT.md`. That review profiled 8 typical scenarios and found no single pathology. This sweep looks for costs that grow faster than they should as the number of predictors (p) and the training size (n) increase.

The package code under test is this branch, which is `faster-correlation` @ `bcf0dbe` plus these scripts. The scripts load it with `pkgload::load_all()` from the repo root, so the installed pmsims version is irrelevant.

## Parts

**Part A: per-replicate component costs.** This is 360 cells: 18 configurations × p ∈ {5, 20, 50, 100, 200} × n ∈ {200, 2k, 20k, 100k}.

- The 18 configurations are every outcome × model (glm/lm/coxph, lasso, rf, xgboost), plus complexity 3 and t-distributed predictors for the regression models.
- Each cell builds the data, model and metric functions exactly as `simulate_*()` does. It then times one replicate's components (median of up to 3 runs): training-data generation, test-set generation (30,000 rows), model fit, primary metric (calibration slope, or CSSE for ML models), and secondary metric (AUC / R² / C-index).
- Tuning cost is timed once per configuration and cached.
- On Linux, each component also records peak RSS, which is reset per component.
- Cells run cheapest first. If a configuration times out or runs out of memory at some n, its larger-n cells are skipped.

**Part B: end-to-end runs.** These are 14 `simulate_*()` calls at package defaults that push towards large p or large n: p = 50–100, prevalence 0.05, 70–90% censoring, complexity 3 at p = 50, t predictors, lasso, rf and xgboost. Each runs under `Rprof`, and the run records wall time, peak RSS, min_n and search bounds.

## Running on the server

```bash
git fetch origin && git switch perf-investigation   # or: git worktree add ../pmsims-perf origin/perf-investigation
cd validation/perf-scaling-20260929
Rscript -e 'for (p in c("pkgload","glmnet","ranger","xgboost","mlpwr","survival","pROC","DiceKriging")) cat(p, requireNamespace(p, quietly = TRUE), "\n")'
tmux new -s perf
./run_all.sh 2>&1 | tee sweep.log       # detach with Ctrl-b d
```

- It is resumable. Rerun `./run_all.sh` to pick up where it stopped; finished cells are skipped. Use `./run_all.sh A` or `./run_all.sh B` to run one part only.
- The limits are per-cell and configurable: `PERF_TIMEOUT_A` (default 1200 s), `PERF_TIMEOUT_B` (default 3600 s) and `PERF_MEM_GB` (a virtual-memory cap; the default is 75% of RAM, Linux only). The cap stops one runaway cell (for example, a survival forest predicting at n = 100k) from taking down the server.
- Run nothing else heavy alongside it. Cells run one at a time so that timings are clean. ranger uses `detectCores() - 2` threads.
- Expected duration: Part A takes a few hours, most of it in the n = 100k rf/lasso/xgboost cells. Part B takes up to 14 h in the worst case (14 × 60-min cap), but most runs should be well under the cap. If it doesn't finish overnight, rerun the next night.
- xgboost cells are recorded as `skipped` if xgboost isn't installed.

To check it at tiny sizes first (a few minutes; writes to a separate directory):

```bash
PERF_SMOKE=1 PERF_RESULTS=/tmp/perf-smoke ./run_all.sh
```

## Results

Everything is written to `results/` (or `$PERF_RESULTS`):

- `system.txt`: host, cores, RAM, BLAS, package versions, git SHA
- `A/<cell>.csv`, `B/<run>.info.rds`, `B/<run>.Rprof`, `logs/<cell>.log`

Then run:

```bash
Rscript summarise.R > results/summary.txt
```

This writes `summary_A.csv` (all cells), `scaling_A.csv` (log-log scaling exponents of each component vs n and vs p), `summary_B.csv` and `profile_B.csv` (stage/component and hot-line breakdown per run). The digest flags any component with a scaling exponent above 1.3.

Bring back `results/` (e.g. `rsync -av server:.../results/ ./results/`). The `.Rprof` files can be large.
