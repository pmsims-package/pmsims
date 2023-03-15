library(glmnet)
library(pROC) # we use this library for calculating AUCs
library(stats)

source("Separate_components/calculate_sample_size.R")
source("Separate_components/tune_generate_data.R")


# User defined functions ----

#Generate data must be a fucntion which takes n as its first argument and a tuning parameter as its second argument.
generate_data <- function(n, beta_signal) {
  p_signal <- 20    #number of real predictors
  p_noise <-  100 #number of noise predictors 
  prob_p  <-  0.1 # probability of a predictor to be 1
  base_prev  <-  0.3 # baseline probability of a positive outcome if all risks are 0
  
  alldata <-  rbinom(n*(p_signal+p_noise),1,prob_p)
  X <-  matrix(alldata, nrow = n, ncol = p_signal + p_noise)
  W_ <-  c(rep(beta_signal, p_signal), rep(0, p_noise))
  b0 <-  log(base_prev/(1- base_prev))
  lp <-  X %*% W_ + b0 
  y_prob <-  1/(1+exp(-lp))
  
  #generate outcome from the probabilities calculated based on individual risk 
  y <-  rbinom(n,1,y_prob)
  data <- cbind(y, X) 
  return(data)
}

# Fit model must be a function of data which returns a model object  
fit_model <- function(data) {
  y <- data[,1]
  x <- data[,-1]
  cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
  lasso.model <- glmnet(x, y, alpha = 1, family = "binomial",
                        lambda = cv.lasso$lambda.1se)
  return(lasso.model)
}

# Get performance must be a function of data and a model objedt
get_performance <- function(data, model) {
  y <- data[,1]
  x <- data[,-1]
  y_hat = predict(model, x, type = "response")
  auc = pROC::auc(y, as.numeric(y_hat), quiet = TRUE) 
  return(auc[1])
}

# Parameters for tuning ----
large_sample_performance = 0.75
large_n = 10000


# Tuning the data generating function ----

large_sample_performance
tune_output <- tune_generate_data(data_generating_function = generate_data,
                                  large_n = large_n,
                                  min_tune_arg = 0.5,
                                  max_tune_arg = 1,
                                  model_function = fit_model,
                                  performance_function = get_performance,
                                  target_large_sample_performance = large_sample_performance)

tune_output

# Checking the tuned model
train_data_tuned <- generate_data(large_n, tune_output)
test_data_tuned <- generate_data(large_n, tune_output)
model <- fit_model(train_data_tuned)
performance_tuned <- get_performance(data = test_data_tuned, model = model)
performance_tuned


# Calculating minimum sample size ----

# this took just over a minute to run on my laptop
results <- calculate_sample_size(data_generating_function = generate_data,
                                 performance_function = get_performance,
                                 model_function = fit_model,
                                 tune_param = tune_output,
                                 target_performance = 0.7,
                                 test_n = 50000,
                                 min_sample_size = 100,
                                 max_sample_size = 3000,
                                 n_reps = 50)


results$min_n

# How does this compare to unpenalised regression

# data generating function returns dataframe not matrix as logistic regression needs df- this is something we need to consider in error checking for the function.
generate_data2 <- function(n, beta_signal) {
  data_matrix <- generate_data(n, beta_signal)
  data <- data_matrix |> data.frame()
  x_names <- paste0("x", 1:(ncol(data)-1))
  data_names <- c("y", x_names)
  colnames(data) <- data_names
  return(data)
}

fit_model2 <- function(data) {
  x_names <- paste0("x", 1:(ncol(data)-1))
  x_formula <- paste(x_names, collapse = "+")
  glm_formula <- paste("y ~", x_formula)
  
  logistic_model <- glm(glm_formula, data = data, family = "binomial")
  
}

train_data2 <- generate_data2(10000, tune_output)
test_data2 <- generate_data2(10000, tune_output)

model2 <- fit_model2(train_data2)
performance_logistic_tuned <- get_performance(data = test_data2, model = model2)
performance_logistic_tuned

results2 <- calculate_sample_size(data_generating_function = generate_data2,
                                 performance_function = get_performance,
                                 model_function = fit_model2,
                                 tune_param = tune_output,
                                 target_performance = 0.7,
                                 test_n = 50000,
                                 min_sample_size = 100,
                                 max_sample_size = 3000,
                                 n_reps = 50)


results2$min_n


# rename 2nd result object
# validate inputs so that they are internally c