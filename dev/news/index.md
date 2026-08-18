# Changelog

## pmsims (development version)

- `metric = "calibration_slope"` is now optimised internally as the
  calibration slope squared error when a machine-learning model
  (`"lasso"`, `"ridge"`, `"rf"`, `"xgboost"`) is used, and translated
  back before results are returned. Targets are still supplied on the
  calibration slope scale and results are still reported on it;
  converted results are marked with a dagger in the printed output.
  Regression models (`"glm"`, `"lm"`, `"coxph"`) are unaffected.
- `metric = "csse"` remains available as an explicit choice for advanced
  use. Targets supplied that way must already be on the CSSE scale; no
  adjustment is applied.
- Long runs are now flagged up front: the adaptive first stage is timed
  and extrapolated, and runs estimated to exceed an hour warn before the
  main search begins.

## pmsims 0.5.0

### Initial release

#### New features

- `pmsims` introduces a simulation-based framework for minimum sample
  size estimation in prediction model development.
- The package provides wrapper workflows for binary, continuous, and
  survival outcomes via
  [`simulate_binary()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_binary.md),
  [`simulate_continuous()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_continuous.md),
  and
  [`simulate_survival()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_survival.md).
- These workflows support both mean-based and assurance-based criteria
  for identifying the smallest sample size that meets a target level of
  predictive performance.
- A lower-level
  [`simulate_custom()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_custom.md)
  interface is available for user-defined data generators, model-fitting
  functions, and performance metrics.

#### Experimental machine-learning support

- The wrapper workflows include experimental machine-learning options
  via regularised regression, random forest, and XGBoost.
- These machine-learning methods have not yet undergone the package’s
  main validation study and should be treated as experimental in
  `0.5.0`.
