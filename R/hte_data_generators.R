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
  y_main <- beta_0 + gamma * W + as.vector(X %*% beta_x)
  y_int  <- W * (theta_x[1] * X[, 1] + theta_x[2] * X[, 2])
  noise  <- stats::rnorm(n, mean = 0, sd = 3)
  
  Y <- y_main + y_int + noise
  
  # 5. Assemble data frame (Col 1 = Y, Col 2 = W, Cols 3:7 = X1..X5)
  df <- data.frame(
    Y = Y,
    W = W,
    X
  )
  
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
  
  # 2. Generate Treatment
  trt <- tryCatch({
    benchtm::generate_trt(n = nrow(x_data_numeric), type = "random", X = x_data_numeric, prop = "X2")
  }, error = function(e) {
    stop(sprintf("Error in generate_trt at n = %d: %s", n, e$message), call. = FALSE)
  })
  
  # 3. Generate Outcome (Y)
  y_data <- tryCatch({
    benchtm::generate_y(
      x_data_numeric, 
      trt = trt,
      prog = "0.5*((X1Y=='Y')+X3)",
      pred = "X3>0", # this defines how treatment effect heterogeneity is introduced.
      b0 = 0, 
      b1 = 1,
      type = "continuous", 
      sigma_error = 3
    )
  }, error = function(e) {
    stop(sprintf("Error in generate_y at n = %d: %s", n, e$message), call. = FALSE)
  })
  
  return(y_data)
}

make_hte_gen3_sun_scen4 <- function(X_pool) {
  force(X_pool)
  function(n) {
    X <- X_pool[sample(nrow(X_pool), n), ]
    trt <- benchtm::generate_trt(n = nrow(X), p_trt = 0.5)
    dat <- benchtm::generate_y(X, trt,
                      prog = "2.90*(X11-X14)",
                      pred = "(X14 > 0.3)|(X4 == 'Y')",
                      b0 = -0.11770, b1 = 0.4071,
                      type = "continuous")
    dat[, c("Y", setdiff(names(dat), "Y"))]
  }
}

hte_gen3_sun_scen4 <- make_hte_gen3_sun_scen4(benchtm::generate_X_syn(100000))


hte_gen4 <- function(n) {
  
  # 1. Generate X distribution
  x_data <- tryCatch({
    benchtm::generate_X_dist(n = n, p = 10, rho = 0.5)
  }, error = function(e) {
    stop(sprintf("Error in generate_X_dist at n = %d: %s", n, e$message), call. = FALSE)
  })
  x_data <- x_data[, names(x_data) != "X6"] # removing X6 as catagorical variable simulated with varying numbers of levels which causes errors when differences occurs between test and train.
  
  # Process X data
  x_data_numeric <- model.matrix(~., data = x_data)[,-1] |> data.frame()
  
  # 2. Generate Treatment
  trt <- tryCatch({
    benchtm::generate_trt(n = nrow(x_data_numeric), type = "random", X = x_data_numeric, prop = "X2")
  }, error = function(e) {
    stop(sprintf("Error in generate_trt at n = %d: %s", n, e$message), call. = FALSE)
  })
  
  # 3. Generate Outcome (Y)
  y_data <- tryCatch({
    benchtm::generate_y(
      x_data_numeric, 
      trt = trt,
      prog = "0.5*((X1Y=='Y')+X3)",
      pred = "X3>0", # this defines how treatment effect heterogeneity is introduced.
      b0 = 0, 
      b1 = 1,
      type = "continuous", 
      sigma_error = 3
    )
  }, error = function(e) {
    stop(sprintf("Error in generate_y at n = %d: %s", n, e$message), call. = FALSE)
  })
  
  return(y_data)
}
