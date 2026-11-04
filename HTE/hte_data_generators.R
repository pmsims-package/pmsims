# HTE data generators.
#
# Every generator returns a data frame with Y in column 1, treatment (0/1) in
# column 2 and covariates after that: the models and metrics rely on this order.
# True individual effects are in attr(data, "tau"), for metrics that need them.

# Reorder benchtm output (run with include_truth = TRUE) to the layout above
as_hte_data <- function(dat) {
  x_names <- setdiff(names(dat), c("Y", "trt", "trt_effect", "prob_diff"))
  out <- dat[, c("Y", "trt", x_names)]
  attr(out, "tau") <- dat$trt_effect
  out
}

hte_gen1 <- function(n) {
  # 1. Generate 5 standard normal baseline predictors
  X <- matrix(stats::rnorm(n * 5), nrow = n, ncol = 5)
  colnames(X) <- paste0("X", 1:5)
  
  # 2. Assign binary treatment randomly with 50% probability
  W <- stats::rbinom(n, size = 1, prob = 0.5)
  
  # 3. Parameters:
  beta_0 <- 1.0 # intercept
  gamma  <- 2 # treatment
  beta_x <- c(1, 1, 1, 1, 1) # covariate main effects
  
  # Treatment interactions for X1 and X2 only
  theta_x <- c(0.5, 0.5) # 
  
  # 4. Construct linear outcome Y
  tau <- gamma + theta_x[1] * X[, 1] + theta_x[2] * X[, 2]
  y_main <- beta_0 + as.vector(X %*% beta_x)
  noise  <- stats::rnorm(n, mean = 0, sd = 3)
  
  Y <- y_main + W * tau + noise
  
  # 5. Assemble data frame (Col 1 = Y, Col 2 = W, Cols 3:7 = X1..X5)
  df <- data.frame(
    Y = Y,
    W = W,
    X
  )
  attr(df, "tau") <- tau
  
  return(df)
}


hte_gen2 <- function(n) {
  
  # 1. Generate X distribution
  x_data <- tryCatch({
    benchtm::generate_X_dist(n = n, p = 10, rho = 0.5)
  }, error = function(e) {
    stop(sprintf("Error in generate_X_dist at n = %d: %s", n, e$message), call. = FALSE)
  })
  x_data <- x_data[, names(x_data) != "X6"] # removing X6 as catagorical variable simulated with varying numbers of levels which causes errors when differences occurs between test and train.
  
  # Process X data
  x_data_numeric <- model.matrix(~., data = x_data)[,-1] |> data.frame()
  
  # 2. Generate Treatment: randomised 1:1
  trt <- benchtm::generate_trt(n = nrow(x_data_numeric), p_trt = 0.5)
  
  # 3. Generate Outcome (Y)
  y_data <- tryCatch({
    benchtm::generate_y(
      x_data_numeric, 
      trt = trt,
      prog = "0.5*(X1Y+X3)", # X1Y is a 0/1 dummy here
      pred = "X3>0", # this defines how treatment effect heterogeneity is introduced.
      b0 = 0, 
      b1 = 1,
      type = "continuous", 
      sigma_error = 3,
      include_truth = TRUE
    )
  }, error = function(e) {
    stop(sprintf("Error in generate_y at n = %d: %s", n, e$message), call. = FALSE)
  })
  
  return(as_hte_data(y_data))
}

# X_pool: e.g. benchtm::generate_X_syn(100000). n can't exceed nrow(X_pool).
make_hte_gen3_sun_scen4 <- function(X_pool) {
  force(X_pool)
  function(n) {
    X <- X_pool[sample(nrow(X_pool), n), ]
    trt <- benchtm::generate_trt(n = nrow(X), p_trt = 0.5)
    dat <- benchtm::generate_y(X, trt,
                      prog = "2.90*(X11-X14)",
                      pred = "(X14 > 0.3)|(X4 == 'Y')",
                      b0 = -0.11770, b1 = 0.4071,
                      type = "continuous",
                      include_truth = TRUE)
    as_hte_data(dat)
  }
}
