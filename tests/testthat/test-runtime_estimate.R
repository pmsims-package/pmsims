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
    "estimated to take approximately"
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
