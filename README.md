# `pmsims`

 [![R-CMD-check](https://github.com/ewancarr/pmsims/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ewancarr/pmsims/actions/workflows/R-CMD-check.yaml)

![Styler](https://github.com/ewancarr/pmsims/actions/workflows/style.yml/badge.svg)

The functions in the package are currently organised in three scripts:

- R/tune_generate_data.R: function that tunes the data generating model
- R/calculate_sample_size.R: function that calculates sample size for
  prediction model by running a surrogate model
- R/surrogate_models.R: functions for implementing surrogate models

At the moment data generating mechanisms, performance metrics, and models are
arguments for the calculate sample_size.R function, to be provided by users.
The example.R script shows how this can be done for a binary outcome analysed
with logistic or logistic lasso regression.
