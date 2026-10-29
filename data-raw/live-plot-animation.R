# Generates vignettes/images/live_plot.gif, the animation of the learning-curve
# search shown in vignettes/pmsims.Rmd.
#
# The search is the binary_example call in data-raw/precomputed-examples.R, run
# with live_plot = TRUE. Each redraw is written to a PNG, and the frames are
# joined into a GIF with ffmpeg, which must be installed. The last frame is
# plot() of the finished result, held for four seconds. live_plot does not
# change the results, so the animation ends at binary_example$min_n.
#
# Re-run with:  Rscript data-raw/live-plot-animation.R

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install pkgload first, e.g. with install.packages('pkgload').")
}
if (!nzchar(Sys.which("ffmpeg"))) {
  stop("ffmpeg is needed to assemble the GIF.")
}

pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)

frames <- tempfile("live-plot-")
dir.create(frames)

# Draw the live plot in a non-interactive session, one PNG per redraw.
options(pmsims.live_plot_force = TRUE)
grDevices::png(
  file.path(frames, "frame%03d.png"),
  width = 1200,
  height = 800,
  res = 150
)
set.seed(123)
result <- simulate_binary(
  signal_parameters = 20,
  noise_parameters = 0,
  complexity = 1,
  data_control = list(correlation = 0.3),
  outcome_prevalence = 0.30,
  maximum_achievable_cstatistic = 0.80,
  model = "glm",
  metric = "calibration_slope",
  target_performance = 0.85,
  n_reps_total = 1000,
  mean_or_assurance = "assurance",
  live_plot = TRUE
)
grDevices::dev.off()

final <- file.path(frames, "final.png")
grDevices::png(final, width = 1200, height = 800, res = 150)
print(plot(result))
grDevices::dev.off()

# ffmpeg's concat demuxer ignores the duration of the last entry, so the final
# frame is listed twice.
pngs <- sort(list.files(frames, "^frame[0-9]+\\.png$", full.names = TRUE))
concat <- file.path(frames, "concat.txt")
writeLines(
  c(
    sprintf("file '%s'\nduration 0.5", pngs),
    sprintf("file '%s'\nduration 4", final),
    sprintf("file '%s'", final)
  ),
  concat
)
status <- system2(
  "ffmpeg",
  c(
    "-loglevel error -y -f concat -safe 0",
    "-i",
    shQuote(concat),
    "-vf",
    shQuote(paste0(
      "scale=800:-1:flags=lanczos,split[a][b];",
      "[a]palettegen=max_colors=64:stats_mode=full[p];",
      "[b][p]paletteuse=dither=none"
    )),
    "-loop 0",
    shQuote("vignettes/images/live_plot.gif")
  )
)
if (status != 0) {
  stop("ffmpeg failed.")
}

cat(sprintf(
  "%d frames; min_n = %s (binary_example$min_n should match).\n",
  length(pngs),
  format(result$min_n)
))
