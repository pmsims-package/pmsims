# TODO

- [ ] Respect the `eval_time` argument in `survival_calib_slope_free()` instead of overwriting it with `NULL`. Current behavior ignores any caller-supplied evaluation time. File: `R/metric_generators.R:409`.

- [ ] Stop hard-coding `test_n = 30000` after `...` are merged in the wrapper helpers, because that makes follow-up `metric_2_at_n` calculations ignore a user override even when `simulate_custom()` used the overridden value. Files: `R/simulate_wrappers.R:156`, `R/simulate_wrappers.R:295`, `R/simulate_wrappers.R:439`.
