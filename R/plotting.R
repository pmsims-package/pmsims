#' Plot sample-size learning curves for `pmsims` outputs
#'
#' Produces a ggplot showing the simulated points and the fitted learning
#' curve (the learning-curve fit for `method = "curve"`, the Gaussian-process
#' surrogate for the mlpwr engines) stored inside a `pmsims` object.
#' Optionally returns the underlying data instead of drawing the plot.
#'
#' @param x A `pmsims` object returned by `simulate_binary()`,
#'   `simulate_continuous()`, `simulate_survival()`, or `simulate_custom()`.
#' @param metric_label Optional string used for the y-axis label when the object
#'   does not already record the metric name.
#' @param plot Logical; if `TRUE` (default) the function prints the plot.
#'   If `FALSE`, the data used to build the plot are returned instead of
#'   drawing anything.
#' @param ... Currently unused.
#'
#' @return Invisibly returns the `ggplot` object when `plot = TRUE`. When
#'   `plot = FALSE`, returns a list with two data frames: for the mlpwr
#'   engines, `observed_data` (simulated points) and `predicted_data`
#'   (Gaussian-process predictions); for `method = "curve"`, `observed_data`
#'   (the criterion at each simulated sample size, with replicates, standard
#'   error and a +/- 2 SE interval) and `fitted_curve` (the fitted learning
#'   curve, or `NULL` when none was fitted).
#' @keywords internal
#' @importFrom ggplot2 .data
#' @export
plot.pmsims <- function(x, metric_label = NULL, plot = TRUE, ...) {
  if (identical(x$method, "curve")) {
    return(plot_learning_curve(x, metric_label = metric_label, plot = plot))
  }
  ds <- x$mlpwr_ds
  # The learning curve comes from the mlpwr search; a search that stopped
  # early, or the bisection engine, has none to draw.
  if (is.null(ds) || !length(ds$data %||% ds$dat)) {
    stop(
      "No learning curve to plot: ",
      if (!is.null(x$status) && !identical(x$status, "ok")) {
        sprintf("the search stopped (status '%s').", x$status)
      } else {
        "this result has no Gaussian-process search data (e.g. method = 'bisection')."
      },
      call. = FALSE
    )
  }
  design <- NULL

  dat <- if (!is.null(ds$data)) ds$data else ds$dat
  fit <- ds$fit
  aggregate_fun <- ds$aggregate_fun

  dat_obs <- mlpwr_results_to_dataframe(
    dat,
    aggregate = TRUE,
    aggregate_fun = aggregate_fun
  )

  boundaries <- ds$boundaries
  if (!is.null(design)) {
    namesx <- names(boundaries)
    specified <- !sapply(design, is.na)
    boundariesx <- unlist(boundaries[!specified])
    ns <- seq(boundariesx[1], boundariesx[2])
    nsx <- lapply(ns, function(x) {
      a <- c()
      a[specified] <- as.numeric(design[specified])
      a[!specified] <- x
      a
    })
    ind <- dat_obs[c(specified, FALSE, FALSE)] == as.numeric(design[specified])
    dat_obs <- dat_obs[ind, ]
    a1 <- names(ds$final$design)[!specified]
    a2 <- paste(
      names(design)[specified],
      "=",
      design[specified],
      sep = " ",
      collapse = ","
    )
    xlab <- paste0(a1, " (", a2, ")")
  }
  if (is.null(design)) {
    boundariesx <- unlist(boundaries)
    xlab <- names(ds$final$design)
    ns <- seq(boundariesx[1], boundariesx[2])
    nsx <- ns
  }

  obs_n_col <- setdiff(names(dat_obs), "y")[1]
  if (is.na(obs_n_col) || is.null(obs_n_col)) {
    obs_n_col <- names(dat_obs)[1]
  }

  dat_pred <- data.frame(
    n = ns,
    y = sapply(nsx, fit$fitfun),
    type = "Prediction"
  )

  # A search routed through CSSE internally leaves its curve on the CSSE scale,
  # while perf_n, target_performance and the metric name have already been
  # translated back by restore_calibration_slope_scale(). Left alone, the target
  # line and the min_n marker would sit around 0.9 above a curve living near 0.
  # Convert after aggregation, matching how perf_n was derived from the
  # aggregated CSSE rather than from individual replicates.
  if (isTRUE(x$internal_csse)) {
    csse_direction <- if (is.null(x$csse_direction)) {
      "below"
    } else {
      x$csse_direction
    }
    to_slope <- function(v) {
      vapply(
        v,
        csse_to_calibration_slope,
        numeric(1),
        direction = csse_direction
      )
    }
    dat_obs$y <- to_slope(dat_obs$y)
    dat_pred$y <- to_slope(dat_pred$y)
  }

  # Plot annotations
  min_n <- if (!is.null(x$min_n)) as.numeric(x$min_n) else NA_real_
  perf_n <- if (!is.null(x$perf_n)) {
    as.numeric(x$perf_n)
  } else {
    if (
      !is.na(min_n) && nrow(dat_obs) > 0 && any(dat_obs[[obs_n_col]] == min_n)
    ) {
      dat_obs$y[dat_obs[[obs_n_col]] == min_n][1]
    } else {
      NA_real_
    }
  }

  target_perf <- if (!is.null(x$target_performance)) {
    as.numeric(x$target_performance)
  } else {
    NA_real_
  }
  metric_name <- if (!is.null(metric_label)) {
    metric_label
  } else if (!is.null(x$metric)) {
    as.character(x$metric)
  } else {
    "performance"
  }
  metric_summary <- if (!is.null(x$mean_or_assurance)) {
    as.character(x$mean_or_assurance)
  } else {
    "performance"
  }

  p <- ggplot2::ggplot()

  p <- p +
    ggplot2::geom_line(ggplot2::aes(x = dat_pred$n, y = dat_pred$y)) +
    ggplot2::geom_point(ggplot2::aes(x = dat_obs[[obs_n_col]], y = dat_obs$y)) +
    ggplot2::theme_bw() +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab("Power") +
    ggplot2::theme(legend.position = "bottom")

  p <- p +
    ggplot2::geom_point(
      ggplot2::aes(x = min_n, y = perf_n),
      data = data.frame(n = min_n, mean = perf_n),
      size = 3
    )
  p <- p +
    ggplot2::annotate(
      "text",
      x = min_n,
      y = perf_n,
      label = sprintf("min_n = %s\nperf = %.3f", min_n, perf_n),
      hjust = -0.05,
      vjust = -0.5,
      size = 3.5
    )

  if (!is.na(target_perf) && nrow(dat_obs) > 0) {
    x_right <- max(dat_pred$n, na.rm = TRUE)
    p <- p +
      ggplot2::annotate(
        "text",
        x = x_right,
        y = target_perf,
        label = sprintf("target = %.3f", target_perf),
        hjust = 1.05,
        vjust = -0.5,
        size = 3.5
      )
  }

  p <- p +
    ggplot2::labs(
      x = "Sample size (n)",
      y = paste0("Performance (", metric_summary, "[", metric_name, "]", ")"),
      title = "Sample size vs performance"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  if (!is.na(target_perf)) {
    p <- p + ggplot2::geom_hline(yintercept = target_perf, linetype = "dashed")
  }

  if (!is.na(min_n)) {
    p <- p + ggplot2::geom_vline(xintercept = min_n, linetype = "dotted")
  }

  if (plot) {
    invisible(print(p))
  } else {
    observed_data <- dat_obs[, c(obs_n_col, "y"), drop = FALSE]
    predicted_data <- dat_pred[, -3]
    colnames(observed_data) <- colnames(predicted_data) <- c("n", metric_name)
    plot_data <- list(
      observed_data = observed_data,
      predicted_data = predicted_data
    )
    plot_data
  }
}

#' Convert stored mlpwr results to a plotting data frame
#'
#' @param dat List of sampled designs and associated performance values.
#' @param aggregate Logical; if `TRUE`, reduce each `y` vector to one value.
#' @param aggregate_fun Summary function used when `aggregate = TRUE`.
#'
#' @return A data frame with one row per sampled design.
#' @keywords internal
#' @noRd
mlpwr_results_to_dataframe <- function(dat, aggregate = TRUE, aggregate_fun) {
  rows <- lapply(dat, function(entry) {
    x_vals <- entry$x
    if (is.null(names(x_vals))) {
      names(x_vals) <- if (length(x_vals) == 1) {
        "n"
      } else {
        paste0("x", seq_along(x_vals))
      }
    }

    y_vals <- entry$y
    y_out <- if (aggregate) {
      aggregate_fun(y_vals)
    } else {
      y_vals
    }

    data.frame(
      as.list(x_vals),
      y = y_out,
      check.names = FALSE
    )
  })

  do.call(rbind, rows)
}

# Plot for the learning-curve engine (method = "curve"): the criterion at
# each simulated sample size (point size = replicates, bars = +/- 2 SE), the
# fitted learning curve, the target, and the answer with its interval and the
# shape-free cross-check. Also works when the search stopped without an
# answer, where it shows where the curve levels off.
plot_learning_curve <- function(
  x,
  metric_label = NULL,
  plot = TRUE,
  subtitle = NULL
) {
  curve <- x$diagnostics$curve
  moa <- tolower(as.character(x$mean_or_assurance %||% "assurance")[1])
  crit <- criterion_function(moa, type = 8L)
  se_factor <- if (identical(moa, "mean")) 1 else curve$se_factor %||% 1.4

  obs <- do.call(
    rbind,
    lapply(x$data, function(p) {
      data.frame(
        n = as.numeric(p$x),
        reps = length(p$y),
        y = crit(p$y),
        se = se_factor * stats::sd(p$y) / sqrt(length(p$y))
      )
    })
  )
  ns <- exp(seq(log(min(obs$n) / 1.2), log(max(obs$n) * 1.2), length.out = 200))
  fit <- if (!is.null(curve$a)) {
    data.frame(n = ns, y = curve$a - curve$b * ns^(-curve$c))
  }
  target <- as.numeric(x$csse_target_performance %||% x$target_performance)
  min_n <- suppressWarnings(as.numeric(x$min_n))

  # Searches run on the CSSE scale are shown on the calibration slope scale.
  if (isTRUE(x$internal_csse)) {
    direction <- x$csse_direction %||% "below"
    to_slope <- function(v) {
      vapply(v, csse_to_calibration_slope, numeric(1), direction = direction)
    }
    lo <- to_slope(obs$y - 2 * obs$se)
    hi <- to_slope(pmin(obs$y + 2 * obs$se, 0))
    obs$y <- to_slope(obs$y)
    obs$lo <- pmin(lo, hi)
    obs$hi <- pmax(lo, hi)
    if (!is.null(fit)) {
      fit$y <- to_slope(fit$y)
    }
    target <- as.numeric(x$target_performance)
  } else {
    obs$lo <- obs$y - 2 * obs$se
    obs$hi <- obs$y + 2 * obs$se
  }

  if (!isTRUE(plot)) {
    return(list(observed_data = obs, fitted_curve = fit))
  }

  label <- metric_label %||%
    pmsims_metric_label(x$metric, x$outcome) %||%
    "Performance"
  subtitle <- subtitle %||%
    if (is.finite(min_n)) {
      ci <- curve$n_ci
      sprintf(
        "Minimum sample size %s (interval %s to %s)",
        format(round(min_n), big.mark = ","),
        format(round(ci[1]), big.mark = ","),
        if (is.finite(ci[2])) format(round(ci[2]), big.mark = ",") else "Inf"
      )
    } else {
      sprintf(
        "No sample size found (status: %s)",
        gsub("_", " ", x$status %||% "unknown")
      )
    }

  p <- ggplot2::ggplot(obs, ggplot2::aes(x = .data$n, y = .data$y)) +
    ggplot2::geom_hline(
      yintercept = target,
      linetype = "dashed",
      colour = "grey40"
    )
  if (
    is.finite(min_n) && length(curve$n_ci) == 2L && all(is.finite(curve$n_ci))
  ) {
    p <- p +
      ggplot2::annotate(
        "rect",
        xmin = curve$n_ci[1],
        xmax = curve$n_ci[2],
        ymin = -Inf,
        ymax = Inf,
        alpha = 0.12,
        fill = "steelblue"
      )
  }
  if (!is.null(fit)) {
    p <- p +
      ggplot2::geom_line(data = fit, colour = "steelblue", linewidth = 0.8)
  }
  p <- p +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$lo, ymax = .data$hi),
      width = 0,
      colour = "grey50"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(size = .data$reps),
      shape = 21,
      fill = "white"
    ) +
    ggplot2::scale_x_log10(labels = function(b) {
      format(b, big.mark = ",", scientific = FALSE)
    }) +
    ggplot2::scale_size_area(max_size = 4, name = "Replicates") +
    ggplot2::labs(
      x = "Training sample size (log scale)",
      y = sprintf(
        "%s (%s)",
        label,
        if (identical(moa, "mean")) "mean" else "20th percentile"
      ),
      title = "Learning curve",
      subtitle = subtitle
    ) +
    ggplot2::theme_bw()
  if (is.finite(min_n)) {
    p <- p + ggplot2::geom_vline(xintercept = min_n, colour = "steelblue")
    xc <- x$diagnostics$crosscheck_n
    if (is.numeric(xc) && is.finite(xc)) {
      p <- p +
        ggplot2::geom_vline(
          xintercept = xc,
          colour = "darkorange",
          linetype = "dotted"
        )
    }
  }
  print(p)
  invisible(p)
}
