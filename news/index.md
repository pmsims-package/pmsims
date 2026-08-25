# Changelog

## pmsims 1.0.0

### Data-generating mechanisms

- Added four signal-complexity levels: linear, quadratic, quadratic with
  interactions, and the Friedman benchmark function.
- Added control over nonlinear signal strength, predictor correlation,
  predictor distribution, and binary predictor prevalence through
  `complexity` and `data_control`.
- Updated binary, continuous, and survival tuning so that calibration
  uses the same data-generating configuration as the sample-size
  simulation.

### Models and performance metrics

- Added ridge regression across binary, continuous, and survival
  outcomes and refined the random-forest defaults and XGBoost tuning.
- Added calibration-slope squared error (`"csse"`) for all supported
  outcome types, defined as `-(1 - slope)^2` so that larger is better
  and `0` is perfect calibration. Unlike the calibration slope itself,
  it penalises slopes above one as well as below it.
- Routed calibration-slope searches through CSSE for the
  machine-learning models. The search looks for the sample size at which
  performance *equals* the target, which requires the metric to move
  monotonically with `n`. The calibration slope does so for classical
  regression models, which overfit and approach one from below, but not
  for penalised and tree-based learners, whose slope can approach one
  from either side depending on how much the fit is shrunk, and so can
  meet the target at more than one sample size. CSSE is a squared
  distance from perfect calibration, so it improves monotonically
  regardless of the direction of miscalibration. Requesting
  `metric = "calibration_slope"` with `lasso`, `ridge`, `rf`, or
  `xgboost` now searches on the CSSE scale internally and translates the
  answer back, so targets and results stay on the calibration slope
  scale.

### Sample-size search and results

- Improved adaptive bounds and detection of performance targets that
  appear unreachable.
- Improved fallback behaviour when model fitting or metric calculation
  fails.
- Forwarded additional engine arguments consistently through
  [`simulate_custom()`](https://pmsims-package.github.io/pmsims/reference/simulate_custom.md)
  and the wrapper functions.
- Expanded and standardised the metadata returned by wrapper
  simulations, which now records the full data-generating configuration
  alongside the search settings.
- Added an up-front runtime estimate: the adaptive first stage is timed
  and extrapolated to the full search. Runs expected to take more than
  30 minutes report the estimate; runs expected to take more than two
  hours also warn and ask for confirmation before continuing. The prompt
  is interactive-only and can be disabled with
  `options(pmsims.confirm_long_runs = FALSE)`.

### Printed output

- Reorganised the console summary into sections: `Inputs` is now divided
  into `Data-generating scenario`, `Model and performance`, and
  `Simulation`, and `Results` leads with the minimum sample size,
  followed by the performance expected at that sample size.
- Reported the data-generating configuration under
  `Data-generating scenario`: signal complexity, nonlinear strength,
  predictor distribution, predictor correlation, and binary predictor
  prevalence.
- Reported performance estimates beneath a `Performance at N = ...`
  heading, with the achieved value for the chosen metric shown alongside
  its target.
- Expressed the sample-size target as a criterion including its
  direction, for example `Calibration slope >= 0.900`, using the
  direction in which the metric improves.
- Replaced internal identifiers with human-readable model names
  (`Random forest` rather than `rf`) and standardised names for
  statistical quantities (AUC and the C-statistic are both reported as
  `C-statistic` for a binary outcome).
- Combined the signal and noise predictor counts onto one line, and
  stopped repeating the model and mode in `Results`.
- Moved the mean/assurance explanation and the CSSE footnote below a
  closing rule, and de-emphasised them.
- Added `print(x, verbose = TRUE)` for implementation-level detail:
  internal metric identifiers, the engine settings used for the search,
  and quantities recorded on an internal search scale.
  [`summary()`](https://rdrr.io/r/base/summary.html) now prints this
  detailed display.

### Interface changes

- Updated the wrapper interface to express predictor configuration
  through `complexity` and `data_control`. The top-level
  `predictor_type` and `binary_predictor_prevalence` arguments have been
  removed; both are now `data_control` fields. See the current examples
  and function documentation for details.
- Candidate predictors are now correlated by default
  (`data_control$correlation = 0.3`), whereas `0.5.0` generated
  independent predictors. Set `correlation = 0` to reproduce the earlier
  behaviour.

## pmsims 0.5.0

### Initial release

#### New features

- `pmsims` introduces a simulation-based framework for minimum sample
  size estimation in prediction model development.
- The package provides wrapper workflows for binary, continuous, and
  survival outcomes via
  [`simulate_binary()`](https://pmsims-package.github.io/pmsims/reference/simulate_binary.md),
  [`simulate_continuous()`](https://pmsims-package.github.io/pmsims/reference/simulate_continuous.md),
  and
  [`simulate_survival()`](https://pmsims-package.github.io/pmsims/reference/simulate_survival.md).
- These workflows support both mean-based and assurance-based criteria
  for identifying the smallest sample size that meets a target level of
  predictive performance.
- A lower-level
  [`simulate_custom()`](https://pmsims-package.github.io/pmsims/reference/simulate_custom.md)
  interface is available for user-defined data generators, model-fitting
  functions, and performance metrics.

#### Experimental machine-learning support

- The wrapper workflows include experimental machine-learning options
  via regularised regression, random forest, and XGBoost.
- These machine-learning methods have not yet undergone the package’s
  main validation study and should be treated as experimental in
  `0.5.0`.
