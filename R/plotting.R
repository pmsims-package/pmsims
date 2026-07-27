#' Plot sample-size learning curves for `pmsims` outputs
#'
#' Produces a ggplot showing the simulated points and the fitted
#' Gaussian-process learning curve stored inside a `pmsims` object.
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
#'   `plot = FALSE`, returns a list with two data frames: `observed_data`
#'   (simulated points) and `predicted_data` (Gaussian-process predictions).
#' @keywords internal
#' @export
plot.pmsims <- function(x, metric_label = NULL, plot = TRUE, ...) {
  ds <- x$mlpwr_ds
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
