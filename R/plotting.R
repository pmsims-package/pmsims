#' Plot the estimated Gaussian process curve from mlpwr
#'
#' @param metric_label
#' @param x pmsims object
#' @param plot Whether to plot or data only
#' @keywords internal
#' @export
plot.pmsims <- function(x, metric_label = NULL, plot = TRUE, ...) {
  ds <- x$mlpwr_ds
  design <- NULL

  dat <- ds$dat
  fit <- ds$fit
  aggregate_fun <- ds$aggregate_fun

  dat_obs <- mlpwr:::todataframe(
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
  dat_pred <- data.frame(
    n = ns,
    y = sapply(nsx, fit$fitfun),
    type = "Prediction"
  )

  #### plot annotations
  min_n <- if (!is.null(x$min_n)) as.numeric(x$min_n) else NA_real_
  perf_n <- if (!is.null(x$perf_n)) {
    as.numeric(x$perf_n)
  } else {
    if (!is.na(min_n) && any(df$n == min_n)) {
      df$mean[df$n == min_n]
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
    ggplot2::geom_point(ggplot2::aes(x = dat_obs$V1, y = dat_obs$y)) +
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
    print(p)
  } else {
    observed_data <- dat_obs
    predicted_data <- dat_pred[, -3]
    colnames(observed_data) <- colnames(predicted_data) <- c("n", metric_name)
    plot_data <- list(
      observed_data = observed_data,
      predicted_data = predicted_data
    )
    plot_data
  }
}
