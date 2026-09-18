#!/usr/bin/env Rscript
# Logical counterexamples to universal search correctness, not coverage studies.
# Each candidate uses 1,000 synthetic replicate values. No clinical model fits.
pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args)) {
  args[1L]
} else {
  "validation/calibration-slope-search/results"
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# A smooth, increasing, bounded learning curve with a later target crossing.
# Alternating perturbations make the input and its intervals reproducible.
learning_curve <- function(n) .4 + .5 * plogis(log2(n / 10) - 8)
run_bounds <- function(plateau_k) {
  draws <- 0L
  calculate_adaptive_bounds(
    data_function = function(n) list(n = n),
    model_function = function(data) data,
    metric_function = function(test, fit, model) {
      draws <<- draws + 1L
      learning_curve(fit$n) + if (draws %% 2L) -.1 else .1
    },
    value_on_error = -Inf,
    start_n = 10,
    test_n = 1,
    n_reps_per = 1000L,
    n_reps_total = 15000L,
    target_performance = .8,
    mean_or_assurance = "mean",
    plateau_k = plateau_k
  )
}
early <- run_bounds(3L)
continued <- run_bounds(20L)

# A perfect, noiseless learning curve: the exact minimum is N=300.
# Confirmation at a much larger GP candidate does not investigate smaller N.
minimum_curve <- function(n) .5 + .4 * n / (n + 100)
confirmed <- confirm_gp_design(
  design = 10000,
  simfun = minimum_curve,
  target = .8,
  mean_or_assurance = "mean",
  lower = 10,
  upper = 20000,
  n_reps_total = 1000L,
  n_reps_per = 20L
)
rows <- data.frame(
  check = c(
    "default_plateau",
    "same_curve_plateau_disabled",
    "excessive_gp_candidate"
  ),
  stop_reason = c(
    early$stop_reason,
    continued$stop_reason,
    confirmed$validation$status
  ),
  largest_evaluated_n = c(
    max(vapply(early$track, `[[`, numeric(1), "n")),
    max(vapply(continued$track, `[[`, numeric(1), "n")),
    confirmed$min_n
  ),
  returned_lower = c(early$min_sample_size, continued$min_sample_size, NA),
  returned_upper = c(early$max_sample_size, continued$max_sample_size, NA),
  returned_n = c(NA, NA, confirmed$min_n),
  known_crossing_n = c(6693, 6693, 300),
  replicates_per_candidate = 1000L
)
stopifnot(
  early$stop_reason == "plateau_without_bracket",
  is.na(early$min_sample_size),
  max(vapply(early$track, `[[`, numeric(1), "n")) == 80,
  continued$stop_reason == "target_bracketed",
  confirmed$min_n == 10000,
  learning_curve(6692) < .8,
  learning_curve(6693) >= .8,
  abs(minimum_curve(300) - .8) < 1e-12
)
write.csv(rows, file.path(out_dir, "generality-audit.csv"), row.names = FALSE)
print(rows)
