This is an update of pmsims (1.0.0 -> 1.1.0). It adds a new default search
engine and fixes several bugs in the simulation; NEWS.md lists the changes.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

- local macOS Tahoe 26.7 (aarch64), R 4.6.0: `R CMD check --as-cran`
- win-builder, Windows Server 2022 (x86_64), R-devel (2026-11-07 r90850 ucrt)

## Dependencies

`timeROC` has moved from Imports to Suggests and is now used only in the
tests. `doParallel` and `foreach` have been dropped from Suggests; parallel
replicates use the base `parallel` package.

## Reverse dependencies

There are no reverse dependencies on CRAN.
