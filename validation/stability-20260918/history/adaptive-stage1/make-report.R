#!/usr/bin/env Rscript
# Run after run-study.R has finished the four cases listed in README.md.
root <- "validation/adaptive-stage1"
cases <- c(
  "glm_stress-seed1",
  "glm_stress-seed298",
  "ridge_reported-seed48",
  "continuous_control-seed47"
)
read_case <- function(case, name) {
  read.csv(file.path(root, "results", case, name), stringsAsFactors = FALSE)
}
search <- do.call(rbind, lapply(cases, read_case, "search.csv"))
validation <- do.call(
  rbind,
  lapply(cases, function(case) {
    filename <- file.path(root, "results", case, "validation.csv")
    if (file.exists(filename)) {
      read.csv(filename, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  })
)
write.csv(search, file.path(root, "results", "search.csv"), row.names = FALSE)
write.csv(
  validation,
  file.path(root, "results", "validation.csv"),
  row.names = FALSE
)
validation$case <- paste(validation$scenario, "seed", validation$seed)
search$case <- paste(search$scenario, "seed", search$seed)
search$target <- ifelse(search$scenario == "glm_stress", 0.95, 0.90)
figure <- ggplot2::ggplot(validation, ggplot2::aes(n, equivalent_slope)) +
  ggplot2::geom_hline(
    data = search,
    ggplot2::aes(yintercept = target),
    linetype = "dashed",
    colour = "grey45"
  ) +
  ggplot2::geom_line(colour = "#337da4") +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = slope_ci_low, ymax = slope_ci_high),
    width = 0,
    colour = "#337da4",
    linewidth = 0.7
  ) +
  ggplot2::geom_point(
    ggplot2::aes(shape = role),
    size = 2.8,
    colour = "#337da4"
  ) +
  ggplot2::geom_point(
    data = search,
    ggplot2::aes(n, predicted_slope, colour = "GP prediction"),
    shape = 4,
    size = 3.5,
    stroke = 1.2
  ) +
  ggplot2::scale_colour_manual(values = c("GP prediction" = "#b3443f")) +
  ggplot2::scale_x_log10(labels = function(x) {
    format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  }) +
  ggplot2::facet_wrap(~case, scales = "free", ncol = 2) +
  ggplot2::labs(
    x = "Training sample size N (log scale)",
    y = "Equivalent calibration slope",
    shape = "Independent check",
    colour = NULL,
    title = "Valid bounds do not guarantee a valid GP answer",
    subtitle = "Independent 1,000-replicate estimates with 95% intervals; dashed line is the target",
    caption = paste(
      "Ridge uses the equivalent slope from the 20th percentile of CSSE.",
      "Lines connect evaluated points only; they are not fitted learning curves.",
      sep = "\n"
    )
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank()
  )
ggplot2::ggsave(
  file.path(root, "validation.png"),
  figure,
  width = 10,
  height = 7,
  dpi = 160
)
fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
rows <- vapply(
  seq_len(nrow(search)),
  function(i) {
    s <- search[i, ]
    v <- validation[
      validation$scenario == s$scenario &
        validation$seed == s$seed &
        validation$role == "GP answer",
    ]
    interval <- if (nrow(v)) {
      sprintf(
        "%.3f [%.3f, %.3f]",
        v$equivalent_slope,
        v$slope_ci_low,
        v$slope_ci_high
      )
    } else {
      "—"
    }
    evidence <- if (nrow(v)) v$evidence else "inconclusive search"
    sprintf(
      "| %s, seed %d | %s–%s | %s | %s | %s |",
      s$scenario,
      s$seed,
      fmt(s$lower),
      fmt(s$upper),
      fmt(s$n),
      interval,
      evidence
    )
  },
  character(1)
)
bound_rows <- vapply(
  seq_len(nrow(search)),
  function(i) {
    s <- search[i, ]
    v <- validation[
      validation$scenario == s$scenario & validation$seed == s$seed,
    ]
    lo <- v$evidence[v$n == s$lower]
    hi <- v$evidence[v$n == s$upper]
    sprintf(
      "- %s, seed %d: lower %s; upper %s.",
      s$scenario,
      s$seed,
      if (length(lo)) lo else "not checked",
      if (length(hi)) hi else "not checked"
    )
  },
  character(1)
)
writeLines(
  c(
    "# Local validation of the stage-1 repair",
    "",
    "The repair removes the reproduced unsupported-range handoff. It does not make the unchanged GP a verified minimum finder.",
    "",
    "Each search requested **1,000 GP reps**; each independent check used **1,000 fresh training/test draws**. The existing adaptive budget remained 500, with 20-replicate batches. No bounds were supplied manually and no GP answer was corrected.",
    "",
    "| Case | Adaptive bounds | GP N | Independent equivalent slope [95% CI] | Evidence at GP N |",
    "|---|---:|---:|---:|---|",
    rows,
    "",
    "The GLM target is **0.95**; the ridge targets are **0.90**. Ridge searches use CSSE internally: the displayed equivalent slope is `1 - sqrt(-q20(CSSE))`, not the raw slope's 20th percentile. Confidence intervals use binomial order statistics.",
    "",
    "![Independent checks compared with GP predictions](validation.png)",
    "",
    "Independent checks of the automatic bounds:",
    "",
    bound_rows,
    "",
    "For the original GLM inputs, the prior original-code seed-298 run returned **5,120,000–10,240,000** after a below-target plateau; seed 1 returned **80,000** in its full GP search. The copied [baseline records](baseline/README.md) identify their provenance. The new seed-298 interval allows the GP to search smaller Ns. The original huge GP was not rerun.",
    "",
    "A below-target independent interval at a GP answer is a residual stage-2 shortfall, not a successful target check. An overlapping interval is inconclusive; it does not prove attainment or failure. The branch intentionally leaves these answers unchanged, implementing only options 1 and 2.",
    "",
    "The existing GP measurements near the seed-298 answer are noisy: at N=24,215, 20 reps gave a slope q20 of 1.002; at N=24,808, 20 reps gave 0.940. The independent 1,000-rep estimate at its reported N=24,494 was 0.942. These nearby Ns are different, so this comparison does not isolate the whole cause of the GP shortfall. The [near-candidate summaries](results/gp-near-candidate.csv) record these observations; `diagnose-gp.R` reproduces them from the locally saved search objects. This study has not established that changing the GP architecture is necessary, or validated alternative settings.",
    "",
    "This four-case study supports the identified stage-1 repair and exposes its practical limits. It does not establish general target attainment, the true minimum, simultaneous coverage of the adaptively inspected intervals, or reliability on all models, outcomes and seeds. Plateau stopping can still be premature, but it now reports an inconclusive search instead of inventing bounds or claiming unreachability.",
    "",
    paste0(
      "Actual GP counts: ",
      paste(
        paste0(
          search$scenario,
          " seed ",
          search$seed,
          " = ",
          search$actual_gp_reps
        ),
        collapse = "; "
      ),
      ". Small budget overshoots come from existing library batching."
    ),
    "",
    "Scripts, CSV traces, summaries and per-case session information accompany this note; raw RDS draws remain locally available. Reproduce the runs using [README.md](README.md), then run `Rscript validation/adaptive-stage1/make-report.R`.",
    "",
    "Package verification: the full test suite passed (one optional-dependency skip); `R CMD check --no-manual --no-build-vignettes --ignore-vignettes` returned **Status: OK**. Manuals and vignettes were excluded from that check."
  ),
  file.path(root, "RESULTS.md")
)
