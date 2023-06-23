test_n = 10000

# default binary data generator
data_generating_function <-   function(n = 500,
                                       beta_signal = 0.5,
                                       n_params = 10,
                                       prob_p = 0.1,
                                       baseline_probability = 0.3) {
  X <- rbinom(n * n_params, 1, prob_p)
  X <- matrix(X, nrow = n, ncol = n_params)
  W_ <- rep(beta_signal, n_params)
  b0 <- log(baseline_probability / (1 - baseline_probability))
  lp <- X %*% W_ + b0
  y_prob <- 1 / (1 + exp(-lp))
  y <- rbinom(n, 1, y_prob)
  data <- cbind(y, X)
  # colnames(data) <- c("y", paste0("x", 1:(ncol(data) - 1)))
  return(as.data.frame(data))
}

# default model function
model <- function(data) {
  logistic_model <- glm("y ~ .", data = data, family = "binomial")
}

#default metric function
metric <- function(data, model) {
  y <- data[, 1]
  x <- data[, -1]
  y_hat <- predict(model, x, type = "response")
  auc <- pROC::auc(y, as.numeric(y_hat), quiet = TRUE)
  return(auc[1])
}

#simfun copied from calc_sample_size2
simfun <- function(n) {
  results <- run_simulations(
    data.frame(
      train_size = n,
      n_sims = 1
    ),
    test_n = test_n,
    data_generating_function = data_generating_function,
    model_function = model,
    performance_function = metric,
    tune_param = NULL
  )
  return(as.numeric(results))
}

aggregate_fun <- function(x) quantile(x, probs = .2)
# To estimate the variance of the estimated quantile, we use a bootstrap
var_bootstrap <- function(x) var(replicate(20, aggregate_fun(sample(x, length(x), replace = T))))
noise_fun <- function(x) var_bootstrap(x$y) # This is the bootstrapped quantile variance
evaluations <- 100 # Total number of evaluations
boundaries <- c(50, 3000) # Edge Sample Sizes
surrogate <- "gpr" # Gaussian Process Regression as surrogate Model
setsize <- 10 
n.startsets <- 4

# If we set target power to be 0.6 we get no error
power <- 0.6
set.seed(134524)
ds <-
  result <- mlpwr::find.design(
    simfun = simfun,
    aggregate_fun = aggregate_fun,
    noise_fun = noise_fun,
    boundaries = boundaries,
    power = power,
    surrogate = surrogate,
    setsize = setsize,
    evaluations = evaluations,
  )
min_n <- as.numeric(ds$final$design)
min_n
# If we set target power to be 0.8 we get an error - this is not expected behaviour
power <- 0.8
set.seed(134524)
ds2 <-
  result <- mlpwr::find.design(
    simfun = simfun,
    aggregate_fun = aggregate_fun,
    noise_fun = noise_fun,
    boundaries = boundaries,
    power = power,
    surrogate = surrogate,
    setsize = setsize,
    evaluations = evaluations,
  )
min_n2 <- as.numeric(ds2$final$design)
min_n2

# if we run simfun on a large smaple we see that the AUC is close to 0.6 - i suspect the error is occurring as the target sample size is out of range
simfun(100000)
