# =============================================================================
# Learning-curve engine (method = "curve")
#
# Replaces the adaptive-bracket + mlpwr search. Every replicate at every sample
# size is kept, and one monotone learning curve
#
#     C(n) = a - b * n^(-c),   b >= 0,  0.25 <= c <= 2.5,  a <= metric maximum
#
# is fitted to the per-n criterion (mean or 20th percentile of the replicate
# values). The next batch of replicates goes where the curve predicts the
# target is crossed, so the search range is never fixed: a first guess that is
# too low or too high is corrected as evidence accumulates, instead of leaving
# the answer on an edge. The answer is where the fitted curve crosses the
# target (no optimism margin, unlike mlpwr's GP mean + 0.3 SD).
#
# Design choices, each from a measured problem (see the review notes):
# - Weights come from a smoothed model of the replicate spread against n, not
#   from each point's own bootstrap SE: for a 20-replicate 20th percentile the
#   estimate and its SE are correlated (about -0.5), which pulls a per-point
#   weighted fit upwards.
# - The ceiling `a` is capped at the metric's maximum (0 for CSSE, 1 for AUC,
#   C-index, R2) and `c` is kept in a plausible range; with few points and no
#   such limits, the fit chose c near 0 and ceilings above what is possible.
# - Points where 20% or more of replicates failed are left out of the fit: the
#   20th percentile there is the fallback value, not a point on the curve.
# - Batches land on an existing sample size within 7%, so replicates
#   accumulate there and each point's summary gets more precise.
# - Points are weighted smoothly by their distance (on the log scale) from the
#   predicted crossing, rather than switching abruptly to a local window.
# - The search stops when the target is out of reach: clearly below target at
#   the largest n tried, and either a ceiling clearly below the target or 97.5%
#   of bootstrap refits putting the crossing beyond max_n.
# =============================================================================

# Largest value a metric can take, used to cap the curve's ceiling.
metric_maximum <- function(metric) {
  switch(
    metric %||% "",
    auc = 1,
    cindex = 1,
    r2 = 1,
    brier_score_scaled = 1,
    csse = 0,
    Inf
  )
}

# Points where this share of replicates or more failed are left out of the fit.
curve_max_failed <- 0.2

# Grid of exponents c searched by fit_learning_curve().
curve_c_grid <- exp(seq(log(0.25), log(2.5), length.out = 40))

#' Fit the learning curve C(n) = a - b n^(-c)
#'
#' For each c on a grid, a and b follow from a weighted linear least-squares
#' fit with b clamped at 0 (non-decreasing curve) and a capped at `a_max`; the
#' c with the smallest weighted residual sum of squares is chosen.
#' @return list(a, b, c, rss), or NULL with fewer than two distinct n.
#' @keywords internal
#' @noRd
fit_learning_curve <- function(n, est, w, a_max = Inf) {
  ok <- is.finite(n) & is.finite(est) & is.finite(w) & w > 0
  n <- n[ok]
  est <- est[ok]
  w <- w[ok]
  if (length(unique(n)) < 2L) {
    return(NULL)
  }
  sw <- sum(w)
  ym <- sum(w * est) / sw
  best <- NULL
  for (cc in curve_c_grid) {
    x <- n^(-cc)
    xm <- sum(w * x) / sw
    sxx <- sum(w * (x - xm)^2)
    b <- if (sxx > 0) max(0, -sum(w * (x - xm) * (est - ym)) / sxx) else 0
    a <- ym + b * xm
    if (a > a_max) {
      # Ceiling fixed at the maximum: est = a_max - b x, least squares for b.
      a <- a_max
      b <- max(0, sum(w * x * (a_max - est)) / sum(w * x^2))
    }
    rss <- sum(w * (est - (a - b * x))^2)
    if (is.null(best) || rss < best$rss - 1e-12) {
      best <- list(a = a, b = b, c = cc, rss = rss)
    }
  }
  best
}

curve_value <- function(fit, n) fit$a - fit$b * n^(-fit$c)

# Smallest n with C(n) >= target: Inf when the ceiling is at or below the
# target, and 0 when the curve is flat above it.
curve_crossing <- function(fit, target) {
  if (is.null(fit)) {
    return(NA_real_)
  }
  if (fit$a <= target) {
    return(Inf)
  }
  if (fit$b <= 0) {
    return(0)
  }
  (fit$b / (fit$a - target))^(1 / fit$c)
}

#' Learning-curve engine
#'
#' @inheritParams simulate_custom
#' @param se_final Not used: this engine stops when the replicate budget is
#'   spent, and reports a bootstrap interval for its answer in
#'   `diagnostics$curve$n_ci`.
#' @param progress Logical; show a progress bar over the replicate budget.
#' @param value_on_error Numeric value recorded for a replicate whose fit or
#'   metric fails.
#' @param evaluator Optional evaluator from `new_evaluator()`, shared by every
#'   stage of the search. Created from the data, model and metric functions
#'   when `NULL`.
#' @param max_n Largest sample size the search may try when no
#'   `min_sample_size` and `max_sample_size` are given.
#' @param parallel,cores Run the replicates of each batch in parallel with
#'   [parallel::mclapply()]; used only when `evaluator` is `NULL`.
#' @param min_n_floor Smallest sample size the search may evaluate. Defaults to
#'   `max(10, p + 5)` for `p` predictors.
#' @param boot_reps Number of bootstrap refits for the interval of the answer.
#' @param start_n Optional first sample size of the pilot; defaults to the
#'   heuristic start value.
#' @param live_plot Redraw the learning curve after each batch of replicates
#'   (interactive sessions with a graphics device only).
#' @param ... Unused.
#' @keywords internal
calculate_curve <- function(
  test_n,
  n_reps_total,
  n_reps_per,
  se_final = NULL,
  min_sample_size,
  max_sample_size,
  target_performance,
  c_statistic,
  mean_or_assurance,
  progress = TRUE,
  data_function,
  model_function,
  metric_function,
  value_on_error,
  evaluator = NULL,
  max_n = 1e6,
  parallel = FALSE,
  cores = 1L,
  min_n_floor = NULL,
  boot_reps = 200L,
  start_n = NULL,
  live_plot = FALSE,
  ...
) {
  if (is.null(evaluator)) {
    evaluator <- new_evaluator(
      data_function,
      model_function,
      metric_function,
      test_n,
      value_on_error,
      parallel = parallel,
      cores = cores
    )
  }
  # For the curve fit, the 20th percentile of each point is estimated with the
  # approximately median-unbiased estimator (type 8). The default (type 7) is
  # optimistic for the few dozen replicates at a point (about +0.06 SD at 20
  # replicates), which made the fitted crossing slightly too small; at the 100
  # replicates of the verification the difference is small.
  crit_fit <- criterion_function(mean_or_assurance, type = 8L)
  target <- target_performance
  a_max <- metric_maximum(attr(metric_function, "metric", exact = TRUE))
  user_bounds <- !is.null(min_sample_size) && !is.null(max_sample_size)

  start <- compute_start_sample_sizes(
    data_function = data_function,
    metric_function = metric_function,
    target_performance = target_performance,
    c_statistic = c_statistic,
    mean_or_assurance = mean_or_assurance
  )
  npar <- start$npar %||% 1
  if (is.null(min_n_floor)) {
    min_n_floor <- max(10, npar + 5)
  }
  lo_limit <- if (user_bounds) min_sample_size else min_n_floor
  hi_limit <- if (user_bounds) max_sample_size else max_n
  hi_name <- if (user_bounds) "max_sample_size" else "max_n"
  clamp <- function(n) round(min(hi_limit, max(lo_limit, n)))
  # Replicates for a precise point: what the largest n is topped up to before
  # a stopping rule fires, and the batch the answer's flags are judged for.
  reps_precise <- 4L * as.integer(n_reps_per)

  # ---- data -----------------------------------------------------------------
  store <- new.env(parent = emptyenv())
  store$y <- list()
  store$failed <- list()
  used <- 0L
  pb <- if (isTRUE(progress)) {
    cli::cli_progress_bar("Learning-curve search", total = n_reps_total)
  }
  on.exit(if (!is.null(pb)) cli::cli_progress_done(id = pb), add = TRUE)

  # Evaluate `reps` replicates at n, or at an existing n within 7% of it. A
  # request on a search limit is never moved, so that the limit itself is
  # simulated.
  add <- function(n, reps) {
    n <- clamp(n)
    have <- as.numeric(names(store$y))
    if (length(have) && n > lo_limit && n < hi_limit) {
      near <- have[abs(log(have / n)) < log(1.07)]
      if (length(near)) n <- near[which.min(abs(log(near / n)))]
    }
    reps <- as.integer(min(reps, n_reps_total - used))
    if (reps < 1L) {
      return(invisible(NULL))
    }
    key <- format(n, scientific = FALSE)
    vals <- evaluator$batch(n, reps, "search")
    store$y[[key]] <- c(store$y[[key]], as.numeric(vals))
    store$failed[[key]] <- c(
      store$failed[[key]],
      attr(vals, "failed") %||% rep(FALSE, length(vals))
    )
    used <<- used + reps
    if (!is.null(pb)) {
      cli::cli_progress_update(id = pb, set = used)
    }
    if (show_live) {
      draw_live()
    }
    invisible(n)
  }

  # Live plot: the state of the search after each batch, drawn with the same
  # function as plot(). Interactive sessions with a graphics device only (or
  # when forced, for tests); it never affects the results.
  show_live <- isTRUE(live_plot) &&
    (isTRUE(getOption("pmsims.live_plot_force")) ||
      (interactive() && grDevices::dev.interactive(orNone = TRUE)))
  # As in plot(), a calibration slope target searched on the CSSE scale
  # (see R/csse_internal.R) is shown on the slope scale; a CSSE target is not.
  csse_direction <- attr(metric_function, "csse_direction", exact = TRUE)
  internal_csse <- !is.null(csse_direction)
  draw_live <- function() {
    pts <- summarise_points()
    f <- if (sum(pts$usable) >= 3L) fit_points(pts)
    n_now <- if (!is.null(f)) curve_crossing(f, target) else NA_real_
    sy <- sorted_y()
    state <- list(
      data = lapply(seq_along(sy$n), function(i) {
        list(x = c(n = sy$n[i]), y = sy$y[[i]])
      }),
      diagnostics = list(
        curve = if (!is.null(f)) {
          list(a = f$a, b = f$b, c = f$c, se_factor = attr(pts, "se_factor"))
        } else {
          list(se_factor = attr(pts, "se_factor"))
        }
      ),
      mean_or_assurance = mean_or_assurance,
      min_n = if (is.finite(n_now) && n_now > 0) n_now else NA_real_,
      metric = if (internal_csse) {
        "calibration_slope"
      } else {
        attr(metric_function, "metric", exact = TRUE)
      },
      outcome = attr(data_function, "outcome", exact = TRUE),
      internal_csse = internal_csse,
      csse_direction = csse_direction,
      csse_target_performance = target,
      target_performance = if (internal_csse) {
        csse_to_calibration_slope(target, direction = csse_direction)
      } else {
        target
      }
    )
    subtitle <- sprintf(
      "Searching: %s of %s replicates; current estimate %s",
      format(used, big.mark = ","),
      format(n_reps_total, big.mark = ","),
      if (is.finite(state$min_n)) {
        format(round(state$min_n), big.mark = ",")
      } else {
        "not yet"
      }
    )
    tryCatch(
      suppressWarnings(plot_learning_curve(state, subtitle = subtitle)),
      error = function(e) invisible(NULL)
    )
  }

  sorted_y <- function() {
    ns <- as.numeric(names(store$y))
    o <- order(ns)
    list(n = ns[o], y = store$y[o], failed = store$failed[names(store$y)[o]])
  }

  # Per-point summaries, with standard errors from a smoothed spread model.
  summarise_points <- function() {
    sy <- sorted_y()
    reps <- lengths(sy$y)
    est <- vapply(sy$y, crit_fit, numeric(1))
    fail <- vapply(sy$failed, mean, numeric(1))
    # Spread of the replicates that did not fail: one fallback value (e.g. -1
    # among CSSE values near -0.01) would otherwise inflate it many-fold.
    sd_rep <- mapply(
      function(v, f) {
        v <- v[!f]
        if (length(v) > 1L) stats::sd(v) else NA_real_
      },
      sy$y,
      sy$failed
    )
    # Spread of single replicates against n, smoothed on the log-log scale so
    # that weights do not depend on each point's own noisy estimate.
    use <- is.finite(sd_rep) & sd_rep > 0 & fail < curve_max_failed
    sd_hat <- if (sum(use) >= 3L) {
      co <- stats::coef(stats::lm(
        log(sd_rep[use]) ~ log(sy$n[use]),
        weights = reps[use]
      ))
      exp(co[1] + co[2] * log(sy$n))
    } else {
      rep(stats::median(sd_rep[use], na.rm = TRUE), length(sy$n))
    }
    # No point with a usable spread (e.g. one replicate each): equal weights.
    sd_hat[!is.finite(sd_hat)] <- 1
    # SE of the criterion: inflate x sd / sqrt(reps). For the mean the factor
    # is 1. For the 20th percentile it is about 1.4 for normal values but
    # about 1.5 times that for skewed metrics such as CSSE, so it is
    # estimated from the data: the median, over points with enough
    # replicates, of the bootstrap SE of the point's 20th percentile divided
    # by sd / sqrt(reps).
    inflate <- if (identical(mean_or_assurance, "mean")) {
      1
    } else {
      quantile_se_factor(sy, use, sd_rep, reps)
    }
    se_hat <- inflate * pmax(sd_hat, 1e-8) / sqrt(reps)
    out <- data.frame(
      n = sy$n,
      reps = reps,
      est = unname(est),
      se = unname(se_hat),
      fail = unname(fail),
      usable = unname(fail < curve_max_failed)
    )
    attr(out, "se_factor") <- inflate
    out
  }

  ratio_cache <- new.env(parent = emptyenv())
  quantile_se_factor <- function(sy, use, sd_rep, reps) {
    ok <- which(use & reps >= 20L)
    if (length(ok) < 3L) {
      return(1.4)
    }
    ratios <- vapply(
      ok,
      function(i) {
        key <- paste(format(sy$n[i], scientific = FALSE), reps[i])
        hit <- ratio_cache[[key]]
        if (is.null(hit)) {
          v <- sy$y[[i]][!sy$failed[[i]]]
          boot <- with_stream(
            evaluator$streams,
            "bootstrap",
            sy$n[i],
            reps[i],
            {
              stats::sd(replicate(
                200L,
                crit_fit(sample(v, length(v), replace = TRUE))
              ))
            }
          )
          hit <- boot / (sd_rep[i] / sqrt(length(v)))
          ratio_cache[[key]] <- hit
        }
        hit
      },
      numeric(1)
    )
    ratios <- ratios[is.finite(ratios) & ratios > 0]
    if (length(ratios) < 3L) 1.4 else stats::median(ratios)
  }

  # Weights by distance from the predicted crossing: Gaussian on log n with an
  # SD of log(3), so a point 3x away keeps about 60% of its weight.
  kernel <- function(n, near) {
    if (is.null(near) || !is.finite(near) || near <= 0) {
      return(rep(1, length(n)))
    }
    exp(-0.5 * (log(n / near) / log(3))^2)
  }
  fit_points <- function(pts, near = NULL) {
    p <- pts[pts$usable, ]
    fit_learning_curve(p$n, p$est, kernel(p$n, near) / p$se^2, a_max = a_max)
  }

  # Bootstrap: resample replicates within each n and refit with the same
  # weights. The stream is keyed by the replicates used so far, offset by 1e6
  # to keep it apart from the bootstrap streams keyed by a sample size
  # (quantile_se_factor() and the verification).
  bootstrap_fit <- function(pts, near, B = boot_reps) {
    sy <- sorted_y()
    with_stream(evaluator$streams, "bootstrap", used + 1e6, 1L, {
      t(vapply(
        seq_len(B),
        function(b) {
          bp <- pts
          bp$est <- vapply(
            sy$y,
            function(v) crit_fit(sample(v, length(v), replace = TRUE)),
            numeric(1)
          )
          f <- fit_points(bp, near)
          if (is.null(f)) {
            c(a = NA_real_, n = NA_real_)
          } else {
            c(a = f$a, n = curve_crossing(f, target))
          }
        },
        numeric(2)
      ))
    })
  }

  # Performance values in messages: on the metric's own scale, except CSSE
  # (used internally for calibration-slope targets of penalised and ML
  # models), which is shown as the distance of the calibration slope from 1.
  is_csse <- identical(attr(metric_function, "metric", exact = TRUE), "csse")
  fmt_perf <- function(v) {
    if (is_csse) {
      sprintf(
        "a calibration slope within %s of 1",
        format(signif(sqrt(max(0, -v)), 3))
      )
    } else {
      format(signif(v, 4))
    }
  }

  stop_status <- function(pts, kind, f, extra = "") {
    top <- which.max(pts$n)
    best <- if (!is.null(f)) f$a else max(pts$est)
    list(
      status = kind,
      message = paste0(
        if (kind == "unreachable") {
          paste(
            "The target looks unreachable: the fitted learning curve levels",
            "off below it."
          )
        } else if (any(pts$est >= target)) {
          paste(
            "Some sample sizes searched met the target, but the fitted",
            "learning curve does not cross it within the sample sizes allowed."
          )
        } else {
          "No sample size searched reached the target."
        },
        sprintf(
          " Performance at n = %s was %s, against a target of %s.",
          format(pts$n[top], big.mark = ",", scientific = FALSE),
          fmt_perf(pts$est[top]),
          fmt_perf(target)
        ),
        extra,
        if (kind == "unreachable") {
          sprintf(
            paste(
              " The best achievable here is about %s; consider a less strict",
              "target, or a setting with higher achievable performance."
            ),
            fmt_perf(best)
          )
        } else {
          ""
        }
      ),
      max_achievable_perf = best
    )
  }

  # Too many failed replicates to fit a curve: every one of at least three
  # sample sizes has curve_max_failed or more of its replicates failed. (The
  # evaluator's own check, failure_status(), fires only at half.)
  too_many_failed <- function(pts) nrow(pts) >= 3L && !any(pts$usable)
  failed_status <- function() {
    failed <- sum(unlist(store$failed))
    err <- evaluator$failures()$first_error
    err <- err[!is.na(err)][1]
    list(
      status = "replicates_failed",
      message = sprintf(
        paste(
          "%d of %d simulation replicates failed to fit or score the",
          "model%s. At every sample size tried, %d%% or more failed, so no",
          "learning curve can be fitted."
        ),
        failed,
        used,
        if (is.na(err)) "" else paste0(" (first error: ", err, ")"),
        round(100 * curve_max_failed)
      )
    )
  }

  # Stopping rules for the pilot while no sample size has reached the target:
  # - out of reach: clearly below target at the largest n so far, and either
  #   the curve's ceiling is clearly below the target, or 97.5% of bootstrap
  #   refits put the crossing beyond max_n (or never). Reach is judged against
  #   max_n, not against how far the search has got: a search that started far
  #   below the answer is still far from it.
  # - cost guard: once samples get large (beyond 50,000 and 32x the first
  #   sample size), continue only if the curve more likely than not reaches
  #   the target within max_n. Without it, targets just above the ceiling
  #   climbed to max_n (1e6 rows), which for ML models means hours per batch.
  # Returns NULL to continue, or list(status, reason).
  reach_check <- function(pts, f, nxt) {
    decide <- function(pts, f) {
      if (
        sum(pts$usable) < 4L ||
          is.null(f) ||
          !clearly_below_at_top(pts) ||
          !top_is_flat(pts)
      ) {
        return(NULL)
      }
      bf <- bootstrap_fit(pts, NULL, B = 100L)
      a_ub <- stats::quantile(bf[, "a"], 0.975, na.rm = TRUE, names = FALSE)
      if (isTRUE(a_ub < target)) {
        return(list(
          reason = "ceiling_below_target",
          status = stop_status(
            pts,
            "unreachable",
            f,
            sprintf(
              " (95%% upper bound of the curve's ceiling: %s.)",
              fmt_perf(a_ub)
            )
          )
        ))
      }
      if (isTRUE(mean(bf[, "n"] > hi_limit, na.rm = TRUE) > 0.975)) {
        return(list(
          reason = "beyond_max_n",
          status = stop_status(
            pts,
            "not_bracketed",
            f,
            sprintf(
              paste(
                " The fitted learning curve puts the crossing beyond %s, the",
                "largest sample size allowed (%s), if at all."
              ),
              format(hi_limit, big.mark = ",", scientific = FALSE),
              hi_name
            )
          )
        ))
      }
      if (
        nxt > max(32 * first_n, 5e4) &&
          isTRUE(stats::median(bf[, "n"], na.rm = TRUE) > hi_limit)
      ) {
        return(list(
          reason = "cost_guard",
          status = stop_status(
            pts,
            "not_bracketed",
            f,
            sprintf(
              paste(
                " Most bootstrap refits of the learning curve put the crossing",
                "beyond %s (%s), or never; the search stopped rather than",
                "simulate ever larger samples."
              ),
              format(hi_limit, big.mark = ",", scientific = FALSE),
              hi_name
            )
          )
        ))
      }
      NULL
    }
    first <- decide(pts, f)
    if (is.null(first)) {
      return(NULL)
    }
    # A rule would stop the search: confirm it on a more precise estimate at
    # the largest sample size first (only now, so that an ordinary climb does
    # not spend its budget on top-ups).
    confirmed <- confirm_top(pts)
    if (identical(confirmed$reps, pts$reps)) {
      return(first)
    }
    decide(confirmed, fit_points(confirmed))
  }

  # Before any stopping rule fires, make sure the largest sample size has
  # enough replicates: a 20-replicate 20th percentile is noisy enough that a
  # slowly rising curve looks flat (ridge p5: -0.0132 at n = 37,632 from 20
  # replicates, against about -0.0101 from 160 at n = 40,000, where the
  # target of -0.01 is in fact met). Tops the point up to 4 x n_reps_per and
  # returns the refreshed summaries.
  confirm_top <- function(pts) {
    it <- which.max(pts$n)
    if (pts$reps[it] < reps_precise && used < n_reps_total) {
      add(pts$n[it], reps_precise - pts$reps[it])
      pts <- summarise_points()
    }
    pts
  }
  clearly_below_at_top <- function(pts) {
    it <- which.max(pts$n)
    isTRUE(pts$est[it] + 2 * pts$se[it] < target)
  }

  # Is the curve flat at the top? Needed before a fitted ceiling is trusted:
  # with only a few points on a slowly rising curve, the fit chooses a large
  # c, the ceiling collapses onto the largest observed value and its
  # bootstrap upper bound falls below a reachable target. Requires at least
  # five usable points spanning at least 16x in n, and no significant gain
  # over the last doubling. Used by every stopping rule: a rule that
  # extrapolates far beyond a few points (e.g. "beyond max_n" after reaching
  # only n = 1,024) is otherwise easily fooled.
  top_is_flat <- function(pts) {
    p <- pts[pts$usable, ]
    if (nrow(p) < 5L || max(p$n) / min(p$n) < 16) {
      return(FALSE)
    }
    top <- which.max(p$n)
    prev <- which(p$n <= p$n[top] / 1.9)
    if (!length(prev)) {
      return(FALSE)
    }
    prev <- prev[which.max(p$n[prev])]
    gain <- p$est[top] - p$est[prev]
    gain < 2 * sqrt(p$se[top]^2 + p$se[prev]^2)
  }

  # ---- pilot ------------------------------------------------------------------
  cli::cli_alert_info("Estimating the learning curve... (pilot)")
  t_pilot <- Sys.time()
  pilot_budget <- 0.5 * n_reps_total
  status <- NULL
  pilot_reason <- "bracketed"
  if (user_bounds) {
    pilot_ns <- unique(round(exp(seq(
      log(min_sample_size),
      log(max_sample_size),
      length.out = 4
    ))))
    for (n in pilot_ns) {
      add(n, n_reps_per)
    }
  } else {
    add(start_n %||% start$start_min_sample_size %||% (10 * npar), n_reps_per)
  }
  first_n <- min(as.numeric(names(store$y)))
  repeat {
    pts <- summarise_points()
    if (too_many_failed(pts)) {
      status <- failed_status()
      pilot_reason <- "replicates_failed"
      break
    }
    above <- any(pts$est >= target)
    below <- any(pts$est < target)
    if (above && below) {
      break
    }
    if (used >= pilot_budget) {
      pilot_reason <- "budget"
      break
    }
    if (!above) {
      top <- max(pts$n)
      if (top >= hi_limit) {
        pilot_reason <- if (user_bounds) "upper_bound" else "max_n_reached"
        break
      }
      f <- if (sum(pts$usable) >= 3L) fit_points(pts) else NULL
      pred <- curve_crossing(f, target)
      nxt <- if (is.finite(pred) && pred > top) {
        min(4 * top, max(2 * top, 1.25 * pred))
      } else {
        2 * top
      }
      stop_here <- reach_check(pts, f, nxt)
      if (!is.null(stop_here)) {
        status <- stop_here$status
        pilot_reason <- stop_here$reason
        break
      }
      add(nxt, n_reps_per)
    } else {
      bottom <- min(pts$n)
      if (bottom <= lo_limit) {
        pilot_reason <- "lower_bound"
        break
      }
      add(bottom / 2, n_reps_per)
    }
  }
  pilot_secs <- as.numeric(difftime(Sys.time(), t_pilot, units = "secs"))
  pts <- summarise_points()

  if (is.null(status) && identical(pilot_reason, "max_n_reached")) {
    if (clearly_below_at_top(pts)) {
      status <- stop_status(
        pts,
        "not_bracketed",
        NULL,
        " The search stopped at the largest sample size it may try (max_n)."
      )
    }
  }
  if (!is.null(status)) {
    return(curve_output(
      store,
      pts,
      fit_points(pts),
      NA_real_,
      status,
      pilot_reason,
      NULL,
      used
    ))
  }

  warn_if_long_run(
    stage_1_secs = pilot_secs,
    track = lapply(pts$n, function(n) list(n = n)),
    n_reps_per = n_reps_per,
    n_reps_total = n_reps_total - used,
    min_sample_size = min(pts$n),
    max_sample_size = max(pts$n),
    model = attr(model_function, "model", exact = TRUE)
  )

  # ---- refinement -----------------------------------------------------------
  cli::cli_alert_info("Refining the learning curve near the target...")
  # Batches cycle through the predicted crossing and 0.8x and 1.25x of it, so
  # the points around the answer also fix the curve's local slope.
  factors <- c(1, 0.8, 1.25)
  step <- 0L
  near <- NA_real_
  while (used < n_reps_total) {
    pts <- summarise_points()
    pred <- curve_crossing(fit_points(pts, near), target)
    if (!is.finite(pred)) {
      pred <- curve_crossing(fit_points(pts), target)
    }
    if (!is.finite(pred)) {
      pred <- 2 * max(pts$n)
    }
    # Never jump more than 4x beyond the data in either direction; once the
    # target has been seen on both sides, stay within 1/2x below and 2x above
    # that evidence, so a noisy, flat curve does not send the search to
    # ever more expensive sample sizes.
    pred <- min(max(pred, min(pts$n) / 4), max(pts$n) * 4)
    above <- pts$n[pts$est >= target]
    below <- pts$n[pts$est < target]
    if (length(above) && length(below)) {
      pred <- min(max(pred, min(below) / 2), 2 * max(above))
    }
    near <- pred
    step <- step + 1L
    add(pred * factors[(step - 1L) %% length(factors) + 1L], 2L * n_reps_per)
  }

  # ---- answer ---------------------------------------------------------------
  pts <- summarise_points()
  f <- fit_points(pts, near)
  if (is.null(f)) {
    # No curve (fewer than two usable sample sizes, e.g. min_sample_size ==
    # max_sample_size): return the smallest observed n that meets the target,
    # which check_result() then verifies.
    met <- pts$n[pts$usable & pts$est >= target]
    if (length(met)) {
      out <- curve_output(
        store,
        pts,
        NULL,
        min(met),
        NULL,
        pilot_reason,
        NULL,
        used,
        lo_limit,
        hi_limit
      )
      out$perf_n <- pts$est[pts$n == min(met)]
      return(out)
    }
  }
  n_star <- curve_crossing(f, target)
  bf <- bootstrap_fit(pts, if (is.finite(n_star)) n_star else near)
  ci <- stats::quantile(bf[, "n"], c(0.025, 0.975), na.rm = TRUE, names = FALSE)
  if (!is.finite(n_star)) {
    status <- stop_status(
      pts,
      "not_bracketed",
      f,
      if (is.null(f) && nrow(pts) < 2L) {
        " No learning curve could be fitted: only one sample size was simulated."
      } else if (is.null(f)) {
        sprintf(
          paste(
            " No learning curve could be fitted: it needs two sample sizes",
            "where fewer than %d%% of replicates failed."
          ),
          round(100 * curve_max_failed)
        )
      } else {
        " The fitted learning curve does not reach the target."
      }
    )
    return(curve_output(
      store,
      pts,
      f,
      NA_real_,
      status,
      "final_fit_below_target",
      ci,
      used
    ))
  }
  if (n_star > hi_limit) {
    # The crossing lies beyond the largest sample size allowed: say so rather
    # than returning that limit as if it were the answer.
    status <- stop_status(
      pts,
      "not_bracketed",
      f,
      sprintf(
        paste(
          " The fitted learning curve crosses the target at about %s, beyond",
          "the largest sample size allowed (%s)."
        ),
        format(round(n_star), big.mark = ",", scientific = FALSE),
        hi_name
      )
    )
    return(curve_output(
      store,
      pts,
      f,
      NA_real_,
      status,
      "crossing_beyond_limit",
      ci,
      used
    ))
  }
  min_n <- clamp(ceiling(n_star))
  out <- curve_output(
    store,
    pts,
    f,
    min_n,
    NULL,
    pilot_reason,
    ci,
    used,
    lo_limit,
    hi_limit
  )
  # Flags for the reported answer: a curve that is flat above the target
  # crosses it at n = 0, below the smallest sample size allowed.
  flags <- answer_flags(
    f,
    pts,
    max(n_star, lo_limit),
    ci,
    reps_precise,
    target
  )
  out$search[names(flags)] <- flags
  out
}

# Diagnostics for the answer n_star:
# - near_ceiling: doubling n from the answer gains less than two standard
#   errors of the criterion (for a point of `reps` replicates), so the curve
#   barely rises there and n is poorly determined by the data (ridge p5: the
#   criterion is at the target from n = 40,000 to 150,000);
# - poorly_determined: the interval for the answer spans more than 2x;
# - crosscheck: the crossing of a monotone (isotonic) fit to the observed
#   points, which does not assume the learning-curve shape; flagged when it
#   differs from the answer by more than 10%.
answer_flags <- function(fit, pts, n_star, ci, reps, target) {
  gain <- curve_value(fit, 2 * n_star) - curve_value(fit, n_star)
  p <- pts[pts$usable, ]
  nearest <- which.min(abs(log(p$n / n_star)))
  se_star <- p$se[nearest] * sqrt(p$reps[nearest] / reps)
  iso_n <- isotonic_crossing(p$n, p$est, p$reps, target)
  list(
    near_ceiling = isTRUE(gain < 2 * se_star),
    poorly_determined = !is.finite(ci[2]) ||
      isTRUE(ci[2] / max(ci[1], 1) > 2),
    gain_per_doubling = gain,
    crosscheck_n = iso_n,
    crosscheck_disagrees = is.finite(iso_n) &&
      abs(log(iso_n / n_star)) > log(1.1)
  )
}

# Where a weighted isotonic (non-decreasing) fit of the observed criterion
# reaches the target, interpolated on log n; Inf if it never does.
isotonic_crossing <- function(n, est, w, target) {
  o <- order(n)
  n <- n[o]
  iso <- weighted_isotonic(est[o], w[o])
  i <- which(iso >= target)[1]
  if (is.na(i)) {
    return(Inf)
  }
  if (i == 1L) {
    return(n[1])
  }
  frac <- (target - iso[i - 1L]) / (iso[i] - iso[i - 1L])
  exp(log(n[i - 1L]) + frac * (log(n[i]) - log(n[i - 1L])))
}

# Weighted isotonic (non-decreasing) regression by pool-adjacent-violators.
weighted_isotonic <- function(y, w) {
  v <- y
  wt <- w
  len <- rep(1L, length(y))
  i <- 1L
  while (i < length(v)) {
    if (v[i] > v[i + 1L]) {
      v[i] <- (v[i] * wt[i] + v[i + 1L] * wt[i + 1L]) / (wt[i] + wt[i + 1L])
      wt[i] <- wt[i] + wt[i + 1L]
      len[i] <- len[i] + len[i + 1L]
      v <- v[-(i + 1L)]
      wt <- wt[-(i + 1L)]
      len <- len[-(i + 1L)]
      if (i > 1L) i <- i - 1L
    } else {
      i <- i + 1L
    }
  }
  rep(v, len)
}

# Engine output in the shape simulate_custom() and the print/plot methods use.
curve_output <- function(
  store,
  pts,
  fit,
  min_n,
  status,
  pilot_reason,
  ci,
  used,
  lo_limit = NA,
  hi_limit = NA
) {
  ns <- as.numeric(names(store$y))
  o <- order(ns)
  dat <- lapply(o, function(i) list(x = c(n = ns[i]), y = store$y[[i]]))
  max_len <- max(lengths(store$y))
  results <- matrix(nrow = length(dat), ncol = max_len)
  rownames(results) <- ns[o]
  for (i in seq_along(dat)) {
    results[i, seq_along(dat[[i]]$y)] <- dat[[i]]$y
  }

  perf_n <- if (!is.null(fit) && is.finite(min_n)) {
    curve_value(fit, min_n)
  } else {
    NA_real_
  }
  at_bound <- NA_character_
  if (is.finite(min_n)) {
    if (isTRUE(min_n <= lo_limit)) {
      at_bound <- "lower"
    }
    if (isTRUE(min_n >= hi_limit)) at_bound <- "upper"
  }

  list(
    results = dat,
    summaries = get_summaries(results),
    min_n = min_n,
    perf_n = perf_n,
    search = list(
      status = status$status,
      status_message = status$message,
      max_achievable_perf = status$max_achievable_perf %||%
        (fit$a %||% NA_real_),
      adaptive_stop_reason = pilot_reason,
      bounds = range(ns),
      at_bound = at_bound,
      curve = if (!is.null(fit)) {
        list(
          a = fit$a,
          b = fit$b,
          c = fit$c,
          n_ci = ci,
          replicates = used,
          points = nrow(pts),
          se_factor = attr(pts, "se_factor")
        )
      }
    )
  )
}
