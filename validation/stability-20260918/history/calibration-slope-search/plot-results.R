#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args)) {
  args[1L]
} else {
  "validation/calibration-slope-search/results"
}
validation <- read.csv(file.path(out_dir, "validation-results.csv"))
validation <- validation[validation$gp_reps == 1000L, ]
labels <- c(
  ridge_reported = "Binary ridge: original report",
  glm_control = "Binary GLM control",
  lasso_control = "Binary lasso control",
  continuous_control = "Continuous ridge control"
)
png(
  file.path(out_dir, "validation.png"),
  width = 2000,
  height = 1800,
  res = 180,
  type = if (capabilities("aqua")) "quartz" else "cairo"
)
par(
  mfrow = c(4, 2),
  mar = c(2.8, 3.3, 3.2, 1),
  oma = c(2, 3, 4.5, 0.5),
  family = "sans",
  las = 1
)
for (scenario in names(labels)) {
  for (seed in c(47L, 48L)) {
    data <- validation[
      validation$scenario == scenario & validation$seed == seed,
    ]
    data <- data[order(data$factor_n), ]
    stopifnot(nrow(data) == 3L)
    plot(
      data$factor_n,
      data$equivalent_slope,
      type = "n",
      log = "x",
      xlim = c(0.4, 2.5),
      ylim = c(0.7, 1.03),
      xaxt = "n",
      xlab = "",
      ylab = "",
      main = paste0(labels[[scenario]], "\nSeed ", seed)
    )
    axis(1, at = c(0.5, 1, 2), labels = c("0.5N", "N", "2N"))
    abline(h = 0.9, col = "#bd4036", lty = 2)
    arrows(
      data$factor_n,
      data$slope_ci_low,
      data$factor_n,
      data$slope_ci_high,
      angle = 90,
      code = 3,
      length = 0.04,
      col = "#284e6c"
    )
    lines(data$factor_n, data$equivalent_slope, col = "#284e6c")
    points(data$factor_n, data$equivalent_slope, pch = 19, col = "#284e6c")
    text(
      0.42,
      1.015,
      paste0("N = ", data$n[data$factor_n == 1]),
      pos = 4,
      cex = 0.8,
      col = "#284e6c"
    )
  }
}
mtext(
  "Independent calibration checks after the search fix",
  side = 3,
  outer = TRUE,
  line = 2,
  font = 2,
  cex = 1.3
)
mtext(
  "Search budget: 1,000 reps; 1,000 fresh training/test draws per point; bars: pointwise 95% intervals",
  side = 3,
  outer = TRUE,
  line = 0.6,
  cex = 0.85
)
mtext(
  "Dashed line: target 0.90. Shrinkage models are shown on the equivalent CSSE distance scale.",
  side = 1,
  outer = TRUE,
  line = 0.3,
  cex = 0.8
)
mtext(
  "Equivalent calibration slope",
  side = 2,
  outer = TRUE,
  line = 1.5,
  las = 0,
  cex = 0.95
)
dev.off()
