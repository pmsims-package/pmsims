This is a new submission.

## Comments for the reviewer

The examples for the four exported functions (`simulate_binary()`,
`simulate_continuous()`, `simulate_survival()` and `simulate_custom()`) are
wrapped in `\dontrun{}` rather than `\donttest{}`.

These functions estimate a minimum sample size by repeatedly simulating
datasets, fitting a prediction model to each, and evaluating its performance
across a search over candidate sample sizes. A realistic run therefore takes
minutes to hours, well beyond the limits for examples.

We could reduce the number of simulations and narrow the search space enough to
fit within the time limit, but the resulting estimates would be too unstable to
be meaningful. Publishing such settings as the documented examples would
demonstrate a use of the package that we would advise users against, so we have
kept the examples at realistic settings and marked them `\dontrun{}`.

The vignette illustrates the full workflow using results computed in advance
and stored with the package, for the same reason.
