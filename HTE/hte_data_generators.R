# HTE data generators.
#
# A data generator can be any function(n) that returns n rows of a data frame:
#   column 1: outcome Y (numeric)
#   column 2: treatment (0/1), randomised
#   other columns: baseline covariates (numeric, character or factor)
# It could be a parametric simulation or a synthetic data model fitted to real
# data. No other information is needed: models and metrics only use these columns,
# so the true treatment effects don't need to be known.

check_hte_generator <- function(data_function, n = 1000) {
  d <- data_function(n)
  stopifnot(
    "must return a data frame" = is.data.frame(d),
    "must return n rows" = nrow(d) == n,
    "needs at least one covariate" = ncol(d) >= 3,
    "column 1 must be a numeric outcome" = is.numeric(d[[1]]),
    "column 2 must be a 0/1 treatment" = setequal(unique(d[[2]]), 0:1),
    "no missing values allowed" = !anyNA(d)
  )
  invisible(TRUE)
}

# Put benchtm output in the layout above
as_hte_data <- function(dat) {
  dat[, c("Y", "trt", setdiff(names(dat), c("Y", "trt")))]
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
      sigma_error = 3
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
                      type = "continuous")
    as_hte_data(dat)
  }
}
