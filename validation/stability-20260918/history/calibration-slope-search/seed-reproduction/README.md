# Original seed-instability reproduction

Open **seed-instability.html** to read or share the demonstration. It is a
self-contained HTML document. **seed-instability.qmd** is its Quarto source;
**reproduce.R** is the standalone R reproduction.

From the pmsims repository root:

```sh
quarto render validation/calibration-slope-search/seed-reproduction/seed-instability.qmd
Rscript validation/calibration-slope-search/seed-reproduction/reproduce.R --seeds=1
Rscript validation/calibration-slope-search/seed-reproduction/reproduce.R --seeds=298 --stage-only
```

Normal rendering uses the supplied small CSV summaries and traces in `runs/`.
It does not run simulations. Explicit Quarto parameters `rerun_small:true`,
`rerun_large_stage:true`, and `rerun_large_full:true` enable fresh runs. The small
rerun also performs 1,000 independent validation draws per tuned generator.

The R script loads the whole pre-fix package from pinned commit
`a88e7011c9998bf02f7518d5cc4ead9d35b86fbb`, using read-only `git archive` and a
temporary directory. Git, R, the package dependencies and `pkgload` are required.
The fixed-test diagnostic additionally requires `testthat`. R and dependency
versions are recorded in the supplied session files. Windows
runs independent validation serially; Unix defaults to four workers.

Both seeds have identical scenario inputs and a 1,000-rep GP budget. The original
preliminary search's separate 500-rep budget and batches of 20 are preserved.
Seed 1 has a fresh full-search result. Seed 298 has fresh preliminary bounds and
a historical full-search cache result of 5,120,000. The document labels these
sources separately. Rerunning the full search for seed 298 can take hours and
substantial memory:

```sh
Rscript validation/calibration-slope-search/seed-reproduction/reproduce.R --seeds=1,298
```

The compact historical extract contains the cache key, seed derivation, inputs,
learning-curve summaries and source-file SHA-256. Raw results and validation
draws (`*.rds`) are saved locally and ignored by Git. This is an original-bug
counterexample, not evidence that the current fix is generally validated.
