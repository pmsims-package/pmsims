# Generates vignettes/images/live_plot.mp4, the animation of the learning-curve
# search shown in vignettes/pmsims.Rmd.
#
# The search is the binary_example call in data-raw/precomputed-examples.R, run
# with live_plot = TRUE. Each redraw is written to a PNG, and the frames are
# joined into an H.264 video with ffmpeg, which must be installed. A video is
# used rather than a GIF because a GIF has at most 256 colours, which leaves
# text and lines ragged; the frames are drawn at twice the width of the
# vignette's text column so they stay sharp on high-resolution screens. The last frame is
# plot() of the finished result, held for four seconds. live_plot does not
# change the results, so the animation ends at binary_example$min_n.
#
# Re-run with:  Rscript data-raw/live-plot-animation.R

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install pkgload first, e.g. with install.packages('pkgload').")
}
if (!nzchar(Sys.which("ffmpeg"))) {
  stop("ffmpeg is needed to assemble the video.")
}

pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)

frames <- tempfile("live-plot-")
dir.create(frames)

# Draw the live plot in a non-interactive session, one PNG per redraw.
options(pmsims.live_plot_force = TRUE)
grDevices::png(
  file.path(frames, "frame%03d.png"),
  width = 1600,
  height = 1066,
  res = 200
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
grDevices::png(final, width = 1600, height = 1066, res = 200)
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
    # A constant 10 frames per second holds each frame for its duration.
    # yuv420p and faststart let every browser play the video, and start
    # playing before it has fully downloaded.
    "-r 10 -c:v libx264 -preset slow -crf 18 -tune animation",
    "-pix_fmt yuv420p -movflags +faststart",
    shQuote("vignettes/images/live_plot.mp4")
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
