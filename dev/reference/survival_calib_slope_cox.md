# Cox calibration slope at a landmark horizon

Predicted survival at `eval_time` is obtained exactly as in the
reference script – via `survfit()` for every proportional-hazards model,
and read off the ensemble matrix for the two forests – then mapped to
the complementary log-log scale and used as the sole covariate in a Cox
model fitted to the validation outcome. The coefficient is the
calibration slope.

## Usage

``` r
survival_calib_slope_cox(
  data,
  fit,
  model,
  eval_time = NULL,
  train_data = NULL,
  eps = 1e-06
)
```

## Arguments

- data:

  Validation data with columns `time`, `event` and the predictors.

- fit:

  Fitted model object.

- model:

  Model string, as passed by the engines. Used only for messages and for
  the glmnet lambda; dispatch is on class.

- eval_time:

  Landmark horizon. Defaults to `median(data$time)`, as in the script.

- train_data:

  Training data, same columns as `data`. REQUIRED for ridge, lasso and
  xgboost, which cannot reconstruct their baseline hazard from the fit
  object alone. Ignored for coxph, ranger and rfsrc.

- eps:

  Clamp applied to predicted survival before the cloglog transform.

## Value

A single numeric calibration slope (1 = calibrated), or `NaN`.

## Details

Baselines come from the TRAINING data (the model's own `survfit`
baseline for `coxph`; `x`/`y` at `lambda.min` for `glmnet`; a
training-fitted `coxph` on the xgboost linear predictor).
