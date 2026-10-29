make_track <- function(ns) {
  lapply(ns, function(n) list(n = n, performance = 0.8, raw = c(0.8, 0.8)))
}

test_that("total runtime is extrapolated from the timed first stage", {
  # Stage 1 did 3 x 10 reps at n = 100, 200, 400 in 70 seconds, i.e.
  # 7000 units of (n x rep) work.
  track <- make_track(c(100, 200, 400))

  estimate <- estimate_total_runtime(
    stage_1_secs = 70,
    track = track,
    n_reps_per = 10,
    n_reps_total = 1000,
    min_sample_size = 400,
    max_sample_size = 600
  )

  # Stage 2: 500 (midpoint) x 1000 reps = 500,000 units at 70 / 7,000 s/unit.
  expect_equal(estimate, 70 + 5000)
})

test_that("runtime estimation falls back to the tracked sizes without bounds", {
  estimate <- estimate_total_runtime(
    stage_1_secs = 10,
    track = make_track(c(100, 300)),
    n_reps_per = 10,
    n_reps_total = 100,
    min_sample_size = NA,
    max_sample_size = NA
  )

  expect_true(is.finite(estimate))
  expect_gt(estimate, 10)
})

test_that("runtime estimation returns NA when it cannot extrapolate", {
  track <- make_track(c(100, 200))

  expect_true(is.na(estimate_total_runtime(0, track, 10, 1000, 100, 200)))
  expect_true(is.na(estimate_total_runtime(
    NA_real_,
    track,
    10,
    1000,
    100,
    200
  )))
  expect_true(is.na(estimate_total_runtime(10, list(), 10, 1000, 100, 200)))
  expect_true(is.na(estimate_total_runtime(
    10,
    make_track(numeric(0)),
    10,
    1000,
    100,
    200
  )))
  expect_true(is.na(estimate_total_runtime(10, track, 10, 0, 100, 200)))
})

test_that("a long estimated run warns the user", {
  # 1 second of stage-1 work per unit makes the extrapolated run enormous.
  expect_message(
    warn_if_long_run(
      stage_1_secs = 600,
      track = make_track(c(1000, 2000)),
      n_reps_per = 20,
      n_reps_total = 1000,
      min_sample_size = 2000,
      max_sample_size = 4000,
      model = "rf"
    ),
    "estimated to take about"
  )
})

test_that("the long-run warning names the model and reports hours", {
  msg <- capture_messages(
    warn_if_long_run(
      stage_1_secs = 600,
      track = make_track(c(1000, 2000)),
      n_reps_per = 20,
      n_reps_total = 1000,
      min_sample_size = 2000,
      max_sample_size = 4000,
      model = "rf"
    )
  )
  msg <- paste(msg, collapse = "\n")

  expect_match(msg, "'rf'", fixed = TRUE)
  expect_match(msg, "hours", fixed = TRUE)
})

test_that("a moderately long run is noted but not confirmed", {
  # 100s over 3,000 units of stage-1 work, extrapolated to 300 x 300 units of
  # stage-2 work, gives ~52 minutes: past the notice threshold, short of the
  # confirmation one.
  called <- FALSE
  testthat::local_mocked_bindings(
    confirm_long_run = function() {
      called <<- TRUE
      TRUE
    }
  )

  msg <- capture_messages(
    warn_if_long_run(
      stage_1_secs = 100,
      track = make_track(c(100, 200)),
      n_reps_per = 10,
      n_reps_total = 300,
      min_sample_size = 200,
      max_sample_size = 400,
      model = "glm"
    )
  )
  msg <- paste(msg, collapse = "\n")

  expect_match(msg, "estimated to take about")
  # A note, not the warning's follow-up advice, and no prompt.
  expect_no_match(msg, "n_reps_total", fixed = TRUE)
  expect_false(called)
})

test_that("declining the confirmation stops the run", {
  testthat::local_mocked_bindings(confirm_long_run = function() FALSE)

  expect_error(
    suppressMessages(warn_if_long_run(
      stage_1_secs = 600,
      track = make_track(c(1000, 2000)),
      n_reps_per = 20,
      n_reps_total = 1000,
      min_sample_size = 2000,
      max_sample_size = 4000,
      model = "rf"
    )),
    "Run cancelled"
  )
})

test_that("accepting the confirmation returns the estimate", {
  testthat::local_mocked_bindings(confirm_long_run = function() TRUE)

  estimated <- suppressMessages(warn_if_long_run(
    stage_1_secs = 600,
    track = make_track(c(1000, 2000)),
    n_reps_per = 20,
    n_reps_total = 1000,
    min_sample_size = 2000,
    max_sample_size = 4000,
    model = "rf"
  ))

  expect_gt(estimated, long_run_confirm_secs)
})

test_that("non-interactive sessions are never prompted", {
  # Tests always run non-interactively, so this is the path scripts, CI and
  # vignette builds take.
  expect_true(confirm_long_run())

  withr_option <- getOption("pmsims.confirm_long_runs")
  on.exit(options(pmsims.confirm_long_runs = withr_option), add = TRUE)
  options(pmsims.confirm_long_runs = FALSE)
  expect_true(confirm_long_run())
})

test_that("durations are described in readable units", {
  expect_equal(format_runtime(45 * 60), "45 minutes")
  expect_equal(format_runtime(3.5 * 3600), "3.5 hours")
  expect_equal(format_runtime(72 * 3600), "3.0 days")
})

test_that("a short estimated run stays quiet", {
  expect_silent(
    warn_if_long_run(
      stage_1_secs = 0.5,
      track = make_track(c(50, 100)),
      n_reps_per = 10,
      n_reps_total = 100,
      min_sample_size = 100,
      max_sample_size = 200,
      model = "glm"
    )
  )
})

test_that("an unusable estimate stays quiet", {
  expect_silent(
    warn_if_long_run(
      stage_1_secs = NA_real_,
      track = list(),
      n_reps_per = 10,
      n_reps_total = 100,
      min_sample_size = 100,
      max_sample_size = 200
    )
  )
})
