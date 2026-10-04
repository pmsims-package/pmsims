# =============================================================================
# Fast calibration fits on the test set
#
# Every replicate scores its model on a 30,000-row test set, and the calibration
# metrics do so by regressing the test outcome on one score from the model:
# glm(y ~ score), glm(y ~ 1, offset = score) or lm(y ~ score). glm() and lm()
# spend most of their time on model frames, QR decompositions and summaries
# that a one- or two-parameter fit does not need; in a replicate of a GLM this
# was over two thirds of the time.
#
# The functions below give the same coefficients, to rounding error. They are
# deliberately narrow: whenever the data are not the plain case -- missing or
# infinite values, a score that is (nearly) constant, step-halving, fitted
# probabilities of 0 or 1, no convergence -- they return NULL and the caller
# falls back to glm() or lm() itself, so those cases keep exactly the old
# result and warnings.
# =============================================================================

#' Intercept-and-slope or intercept-only GLM by glm.fit()'s own iterations
#'
#' Repeats [stats::glm.fit()] for the model `y ~ x` (or `y ~ 1` when `x` is
#' `NULL`), with optional prior `weights` and `offset`: the same starting
#' values, the same iteratively reweighted least-squares steps and the same
#' convergence rule, so it stops at the same iteration as glm(). Only the
#' weighted least-squares step is solved directly (centred normal equations)
#' instead of by a QR decomposition, which changes the coefficients by rounding
#' error only. Written for the binomial family, which is all the calibration
#' metrics use.
#'
#' @param y Numeric 0/1 outcome.
#' @param x Numeric score, or `NULL` for an intercept-only model.
#' @param family A binomial family object.
#' @param weights Optional prior weights (rows with weight 0 are ignored, as in
#'   glm()). Non-integer weights do not trigger glm()'s warning.
#' @param offset Optional offset.
#' @return The coefficients (intercept, then slope), or `NULL` when glm() is
#'   needed instead (see the section header).
#' @keywords internal
#' @noRd
calibration_glm <- function(
  y,
  x = NULL,
  family = stats::binomial(),
  weights = NULL,
  offset = NULL,
  epsilon = 1e-8,
  maxit = 25L
) {
  if (is.null(weights)) {
    weights <- rep.int(1, length(y))
  }
  if (is.null(offset)) {
    offset <- 0
  }
  if (
    !(is.numeric(y) || is.logical(y)) ||
      !all(is.finite(y)) ||
      !all(is.finite(weights)) ||
      !all(is.finite(offset)) ||
      (!is.null(x) && !all(is.finite(x))) ||
      any(weights < 0) ||
      any(y < 0 | y > 1)
  ) {
    return(NULL)
  }

  # Rows with weight 0 do not enter glm()'s fit and add exactly 0 to its
  # deviance, so they are dropped; only the final check on the fitted
  # probabilities (below) sees them.
  all_rows <- NULL
  if (!all(weights > 0)) {
    all_rows <- list(x = x, offset = offset)
    keep <- weights > 0
    y <- y[keep]
    weights <- weights[keep]
    if (!is.null(x)) {
      x <- x[keep]
    }
    if (length(offset) > 1L) {
      offset <- offset[keep]
    }
  }

  # binomial()$initialize
  mustart <- (weights * y + 0.5) / (weights + 1)
  eta <- family$linkfun(mustart)
  mu <- family$linkinv(eta)
  devold <- sum(family$dev.resids(y, mu, weights))

  for (iter in seq_len(maxit)) {
    mu_eta <- family$mu.eta(eta)
    if (any(mu_eta == 0)) {
      return(NULL)
    }
    z <- eta - offset + (y - mu) / mu_eta
    w2 <- weights * mu_eta^2 / family$variance(mu)
    sw <- sum(w2)
    zm <- sum(w2 * z) / sw
    if (is.null(x)) {
      coef <- zm
      eta <- coef + offset
    } else {
      xm <- sum(w2 * x) / sw
      xc <- x - xm
      w2_xc <- w2 * xc
      sxx <- sum(w2_xc * xc)
      # glm()'s QR would drop a (nearly) collinear score; leave that to glm().
      if (!is.finite(sxx) || sxx <= 1e-12 * sum(w2 * x^2)) {
        return(NULL)
      }
      b <- sum(w2_xc * (z - zm)) / sxx
      coef <- c(zm - b * xm, b)
      eta <- coef[1] + coef[2] * x + offset
    }
    # The binomial links keep mu inside (0, 1) for any eta that is not NaN,
    # so a finite deviance is all glm()'s validity checks need here.
    mu <- family$linkinv(eta)
    dev <- sum(family$dev.resids(y, mu, weights))
    if (!is.finite(dev)) {
      return(NULL) # glm() would halve the step
    }
    if (abs(dev - devold) / (0.1 + abs(dev)) < epsilon) {
      # glm() warns about fitted probabilities of 0 or 1, over all rows.
      mu_all <- mu
      if (!is.null(all_rows)) {
        eta_all <- if (is.null(x)) coef else coef[1] + coef[2] * all_rows$x
        mu_all <- family$linkinv(eta_all + all_rows$offset)
      }
      eps <- 10 * .Machine$double.eps
      if (any(mu_all > 1 - eps) || any(mu_all < eps)) {
        return(NULL)
      }
      return(coef)
    }
    devold <- dev
  }
  NULL
}

#' Coefficients of a binary calibration model
#'
#' The coefficients (intercept, slope) of glm(y ~ x, family = binomial(link)),
#' with optional prior weights, or `NULL` when glm() fails.
#' @keywords internal
#' @noRd
binary_calibration_coef <- function(y, x, link = "logit", weights = NULL) {
  family <- stats::binomial(link = link)
  cf <- calibration_glm(y, x, family = family, weights = weights)
  if (!is.null(cf)) {
    return(cf)
  }
  fit <- try(
    if (is.null(weights)) {
      stats::glm(y ~ x, family = family)
    } else {
      stats::glm(y ~ x, weights = weights, family = family)
    },
    silent = TRUE
  )
  if (inherits(fit, "try-error")) NULL else as.numeric(stats::coef(fit))
}

#' Calibration slope of a binary outcome on a score
#'
#' The slope of glm(y ~ x, family = binomial(link)), or `NaN` when glm()
#' fails.
#' @keywords internal
#' @noRd
binary_calibration_slope <- function(y, x, link = "logit", weights = NULL) {
  cf <- binary_calibration_coef(y, x, link = link, weights = weights)
  if (is.null(cf)) NaN else cf[2]
}

#' Calibration slope of a continuous outcome on a prediction
#'
#' The slope of lm(y ~ x) in closed form, cov(x, y) / var(x). lm() is used
#' when there are missing values or when x is so nearly constant that lm()
#' would treat it as collinear with the intercept.
#' @keywords internal
#' @noRd
continuous_calibration_slope <- function(y, x) {
  if (
    is.numeric(y) && is.numeric(x) && all(is.finite(y)) && all(is.finite(x))
  ) {
    xc <- x - mean(x)
    sxx <- sum(xc^2)
    if (is.finite(sxx) && sxx > 1e-12 * sum(x^2)) {
      return(sum(xc * (y - mean(y))) / sxx)
    }
  }
  fit <- try(stats::lm(y ~ x), silent = TRUE)
  if (inherits(fit, "try-error")) NaN else as.numeric(stats::coef(fit)[2])
}

#' Cox calibration slope: the coefficient of coxph(Surv(time, event) ~ x)
#'
#' coxph() spends most of its time on the 30,000-row test set computing a
#' concordance that is not used. This calls the fitting routine it uses,
#' survival::coxph.fit(), directly, on the response coxph() would build
#' (times merged by aeqSurv(), its default timefix) with the same control
#' settings and Efron ties, so the coefficient is identical and coxph.fit()'s
#' convergence warnings are kept. coxph() itself is used when the data are
#' not finite with 0/1 events, when there are no events, or when the
#' coefficient is not estimated (coxph() then adds its own warning).
#'
#' @return The slope, or `NaN` when coxph() fails.
#' @keywords internal
#' @noRd
cox_calibration_slope <- function(time, event, x) {
  # With no events coxph.fit() returns 0 with a convergence warning, where
  # coxph() returns NA, so that case goes to coxph().
  if (
    plain_survival_data(time, event) &&
      any(event == 1) &&
      is.numeric(x) &&
      all(is.finite(x))
  ) {
    cf <- try(
      survival::coxph.fit(
        matrix(x),
        survival::aeqSurv(survival::Surv(time, event)),
        strata = NULL,
        offset = NULL,
        init = NULL,
        control = survival::coxph.control(),
        weights = NULL,
        method = "efron",
        rownames = NULL
      )$coefficients,
      silent = TRUE
    )
    if (!inherits(cf, "try-error") && length(cf) == 1L && is.finite(cf)) {
      return(as.numeric(cf))
    }
  }
  y_surv <- survival::Surv(time, event)
  cf <- try(stats::coef(survival::coxph(y_surv ~ x)), silent = TRUE)
  if (inherits(cf, "try-error") || is.null(cf)) NaN else as.numeric(cf)
}
