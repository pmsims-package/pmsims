# Title:        Investigating instability
# Author:       Ewan Carr
# Started:      2023-07-24

library(pmsims)
library(tidyverse)
library(furrr)
library(progressr)
plan(multisession, workers = 20)

opt <- expand_grid(
  type = c("binary", "continuous"),
  n_reps_total = 1000,
  n_reps_per = 10,
  signal_parameters = c(10, 50, 100),
  perf = c("low", "middle", "high"),
  baseline_prob = c(0.1, 0.5),
  minimum_threshold = 0.05,
  min_sample_size = 100,
  max_sample_size = 3000,
  run = 1:50
)

perf <- data.frame(
  type = rep(c("binary", "continuous"), each = 3),
  perf = rep(c("low", "middle", "high"), 2),
  large_sample_performance = c(0.7, 0.8, 0.9, 0.2, 0.4, 0.6)
)

opt <- left_join(opt, perf) |>
  select(-perf)

opt


with_progress({
    p <- progressor(steps = nrow(opt))
    result <- future_pmap(opt, \(type, run, ...) {
        if (type == "binary") {
            simulate_binary(...)
        } else if (type == "continuous") {
            simulate_continuous(...)
        }
    })
})
