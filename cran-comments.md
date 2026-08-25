This is a new submission.

## Comments for the reviewer

### NOTEs from the previous pre-test

The pre-test for the initial submission reported two NOTEs.

The first flagged possibly misspelled words in DESCRIPTION: "Olaniran",
"Shamsutdinova", "et", "al" and "generalisable". These are the surnames of
authors of the two cited papers, the standard abbreviation "et al.", and
the British spelling of "generalisable", which is used consistently throughout
the package. We believe no change is needed.

The second flagged the non-standard top-level directory 'scripts'. This
contained a helper script used when building the pkgdown site and should not
have been included in the tarball. It is now listed in .Rbuildignore.

### Use of \dontrun{} in examples

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
