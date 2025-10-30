#' Tuning for a binary outcome model
#'
#' @param target_performance The desired model performance in a large sample
#' @param target_prevalence The expected outcome prevalence
#' @param tolerance Convergence parameters (TODO)
#' @returns The optimal value for the tuning parameter

invlogit <- function(x) 1 / (1 + exp(-x))

logit <- function(x) log(x / (1 - x))

binary_tuning <- function(
  target_prevalence,
  target_performance,
  min.opt = c(-10, 0),
  max.opt = c(0.02, 5),
  tolerance = 0.00001,
  proportion_noise_features,
  candidate_features
) {
  pcfun <- function(x) {
    mean <- x[1]
    variance <- x[2]

    f1 <- function(x) {
      stats::integrate(
        function(y) {
          stats::dnorm(x, mean = mean, sd = sqrt(variance)) *
            stats::dnorm(y, mean = mean, sd = sqrt(variance)) *
            (1 + exp(-x))^(-1) *
            (1 + exp(y))^(-1)
        },
        -Inf,
        x
      )$value
    }

    num <- stats::integrate(Vectorize(f1), -Inf, Inf)$value

    f2 = function(x) {
      stats::integrate(
        function(y) {
          stats::dnorm(x, mean = mean, sd = sqrt(variance)) *
            stats::dnorm(y, mean = mean, sd = sqrt(variance)) *
            (1 + exp(-x))^(-1) *
            (1 + exp(y))^(-1)
        },
        -Inf,
        Inf
      )$value
    }

    denom <- stats::integrate(Vectorize(f2), -Inf, Inf)$value

    f3 <- function(x) {
      stats::dnorm(x, mean = mean, sd = sqrt(variance)) * (1 + exp(-x))^(-1)
    }

    c <- num / denom
    prev <- stats::integrate(f3, -Inf, Inf, subdivisions = 1000L)$value

    abs(c - target_performance)^2 + abs(prev - target_prevalence)^2
  }

  if (target_performance > 0.7) {
    sigma_c <- sqrt(2) * stats::qnorm(target_performance)
    mu <- 0.5 *
      (2 * target_prevalence - 1) *
      (sigma_c^2) +
      log(target_prevalence / (1 - target_prevalence))
    out <- stats::optim(
      par = c(mu, 0.15),
      pcfun,
      c(min.opt, max.opt, tolerance = tolerance)
    )$par
  } else {
    sigma_c <- sqrt(2) * stats::qnorm(target_performance)
    mu <- 0.5 *
      (2 * target_prevalence - 1) *
      (sigma_c^2) +
      log(target_prevalence / (1 - target_prevalence))
    sigma <- sqrt(
      (sigma_c^2) *
        (1 + target_prevalence * (1 - target_prevalence) * (sigma_c^2))
    )
    out <- c(mu, sigma^2)
  }

  N <- 500000
  lp <- stats::rnorm(N, mean = out[1], sd = sqrt(out[2]))
  p <- (1 + exp(-lp))^(-1)
  y <- stats::rbinom(N, 1, prob = p)
  prev <- mean(y)
  c <- quickcstat(y, lp)

  non_noise_predictors <-
    candidate_features - round(candidate_features * proportion_noise_features)
  beta_init <- 1 / non_noise_predictors
  beta_signal <- beta_init * sqrt(out[2] / beta_init)

  c(out[1], out[2], beta_signal, prev, c)
}

quickcstat <- function(y, pred) {
  casepred <- pred[y == 1]
  conpred <- pred[y == 0]

  if (length(conpred) > length(casepred)) {
    conpred <- conpred[sample(
      length(conpred),
      length(casepred),
      replace = FALSE
    )]
    auc.true <- sum(casepred > conpred) / length(casepred)
  } else {
    casepred <- casepred[sample(
      length(casepred),
      length(conpred),
      replace = FALSE
    )]
    auc.true <- sum(casepred > conpred) / length(conpred)
  }

  return(auc.true)
}
