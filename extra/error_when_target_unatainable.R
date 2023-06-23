# error in calculate_sample_size
set.seed(16516)
calculate_sample_size(
   data = list(type = "binary"),
   target_performance = 0.8,
   test_n = 10000,
   tune_param = 0.5,
   min_sample_size = 50,
   max_sample_size = 3000,
   n_reps = 100
)

# The error occurs when running mlpwr. Here are the relevant bits extracted

################################ Set up################################

test_n = 10000
tune_param = 0.5
# default binary data generator - copied from data_generators script
data_generating_function <-  default_data_generators(list(type = "binary"))
m <- default_model_generators(type = "binary")


# default model function
model <- m$model

#default metric function
metric <- m$metric

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
    tune_param = tune_param
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

################################ reproducing the error ################################

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

# If we set target power to be 0.8 we get an error - this is not expected behavior
# The way calc_sample_size2 is written we would expect a ds object to be returned which results in an na for min sample size.
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

# if we run simfun on a large smaple we see that the AUC is close to 0.6 
# I suspect the error is occurring as the target sample size is out of range
simfun(100000)
