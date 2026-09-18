#!/usr/bin/env Rscript
# Summarise existing GP measurements; this does not run more simulations.
root <- "validation/adaptive-stage1/results"
cases <- c(
  "glm_stress-seed1",
  "glm_stress-seed298",
  "ridge_reported-seed48",
  "continuous_control-seed47"
)
rows <- lapply(cases, function(case) {
  result <- readRDS(file.path(root, case, "search.rds"))
  search <- read.csv(file.path(root, case, "search.csv"))
  summary <- do.call(
    rbind,
    lapply(result$data, function(z) {
      data.frame(
        case = case,
        n = unname(z$x),
        reps = length(z$y),
        internal_q20 = unname(quantile(z$y, .2, type = 7)),
        gp_answer = result$min_n
      )
    })
  )
  summary <- summary[head(order(abs(summary$n - result$min_n)), 5L), ]
  summary$equivalent_slope <- if (search$scenario == "glm_stress") {
    summary$internal_q20
  } else {
    1 - sqrt(pmax(0, -summary$internal_q20))
  }
  summary
})
write.csv(
  do.call(rbind, rows),
  file.path(root, "gp-near-candidate.csv"),
  row.names = FALSE
)
