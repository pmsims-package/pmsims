This is a patch release (1.1.0 -> 1.1.1) that fixes the test error on
r-devel-linux-x86_64-fedora-gcc.

xgboost's multi-threaded row subsampling is not reproducible across thread
counts with gcc 16's libstdc++: it assumes one random draw per row, which
`std::bernoulli_distribution` there does not always use. The test of
thread-invariance now fits xgboost with `subsample = 1`, where its results do
not depend on the number of threads. Package behaviour is unchanged.

## R CMD check results

0 errors | 0 warnings | 1 note

The note is "Days since last update: 3": this release fixes the check error
above.

## Test environments

- local macOS Tahoe 26.7 (aarch64), R 4.6.0: `R CMD check --as-cran`
- Fedora 44 (x86_64), gcc 16.2.1, R 4.6.2: `R CMD check --as-cran`; the
  test that failed on r-devel-linux-x86_64-fedora-gcc fails there with
  1.1.0 and passes with 1.1.1

## Reverse dependencies

There are no reverse dependencies on CRAN.
