default_data_generators <- function(type, beta_signal, n_params) {
  if (type == "binary") {
    f <- function(n = 500,
                  .beta_signal = beta_signal,
                  .n_params = n_params,
                  .prob_p = 0.1,
                  .baseline_probability = 0.3) {
      # n: sample size
      # beta_signal:
      # prob_p:
      # baseline_probability: baseline probability of a positive outcome
      X <- rbinom(n * .n_params, 1, .prob_p)
      X <- matrix(X, nrow = n, ncol = .n_params)
      W_ <- rep(.beta_signal, .n_params)
      b0 <- log(.baseline_probability / (1 - .baseline_probability))
      lp <- X %*% W_ + b0
      y_prob <- 1 / (1 + exp(-lp))
      y <- rbinom(n, 1, y_prob)
      data <- cbind(y, X)
      # colnames(data) <- c("y", paste0("x", 1:(ncol(data) - 1)))
      return(as.data.frame(data))
    }
  } else if (type == "survival") {
    f <- function(params = n_params,
                  n = 500,
                  lambda = 0.01) {
      # n: sample size
      # n_params: number of predictors
      # lambda: baseline hazard rate

      # Generate vector of coefficients for the covariates
      beta <- rnorm(params, 0.2)

      # Generate covariates
      X <- data.frame(replicate(length(beta), rnorm(n)))
      colnames(x) <- paste0("cov", 1:length(beta))

      # Generate survival times
      eta <- as.matrix(x) %*% beta
      T <- rexp(n, rate = lambda * exp(eta))

      # Generate censoring indicators
      C <- ifelse(runif(n) < 0.2, 1, 0) # 20% censoring rate

      # Return survival data as a data frame
      return(data.frame(id = 1:n, time = T, status = 1 - C, x))
    }
  }
  return(f)
}
