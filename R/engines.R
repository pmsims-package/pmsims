calculate_mlpwr <- function(
    test_n,
    tune_param,
    n_reps_total,
    n_reps_per,
    se_final,
    min_sample_size,
    max_sample_size,
    target_performance,
    n_init,
    verbose,
    data_function,
    model_function,
    metric_function,
    value_on_error) {
  mlpwr_simulation_function <- function(n) {
    tryCatch(
      {
        test_data <- data_function(test_n, tune_param)
        train_data <- data_function(n, tune_param)
        fit <- model_function(train_data)
        model <- attr(model_function, "model")
        metric_function(test_data, fit, model)
      },
      error = function(e) {
        return(value_on_error)
      }
    )
  }

  aggregate_fun <- function(x) quantile(x, probs = .2)

  # Use a bootstrap to estimate the variance of the estimated quantile
  var_bootstrap <- function(x) {
    var(replicate(20, aggregate_fun(sample(x, length(x), replace = TRUE))))
  }

  # Calculate bootstrapped quantile variance
  noise_fun <- function(x) var_bootstrap(x$y)

  # processing final_estimate_se
  # Auto-stopping or not
  if (!(is.null(se_final))) {
    ci <- se_final * qnorm(0.975) * 2
    n_reps_total <- 10000 # setting large nreps so ci dominates.
  } else {
    ci <- NULL
  }
  # Perform search using mlpwr
  ds <-
    mlpwr::find.design(
      simfun = mlpwr_simulation_function,
      aggregate_fun = aggregate_fun,
      noise_fun = noise_fun,
      boundaries = c(min_sample_size, max_sample_size),
      power = target_performance,
      surrogate = "gpr",
      setsize = n_reps_per,
      evaluations = n_reps_total,
      ci = ci,
      n.startsets = n_init,
      silent = !verbose
    )

  # Process results from mlpwr
  perfs <- ds$dat
  perfs <- perfs[order(sapply(perfs, "[[", "x"))]
  max_len <- max(sapply(perfs, \(x) length(x$y)))
  results <- matrix(nrow = length(perfs), ncol = max_len)
  rownames(results) <- sapply(perfs, \(x) x$x)
  for (i in seq_along(perfs)) {
    results[i, seq(1, length(perfs[[i]]$y), 1)] <- perfs[[i]]$y
  }
 
  get_perf <- function(results, p) {
    apply(results, FUN = stats::quantile, MARGIN = 1, probs = p, na.rm = TRUE)
  }

  mlpwr_summaries <- list(
    median_performance = get_perf(results, 0.5),
    quant20_performance = get_perf(results, 0.2),
    quant5_performance = get_perf(results, 0.05),
    quant95_performance = get_perf(results, 0.95)
  )

  return(list(
    results = perfs,
    summaries = mlpwr_summaries,
    min_n = as.numeric(ds$final$design)
  ))
}

calculate_crude <- function(
  data_function,
  tune_param,
  model_function,
  metric_function,
  value_on_error,
  min_sample_size,
  max_sample_size,
  n_reps_per,
  target_performance) {

  # Make sure n_reps_per is 10 or over
  n_reps_per <- pmax(10, n_reps_per)

  # Specify grid
  sample_grid <- c(
    round(seq(min_sample_size, max_sample_size, length.out = 25)),
    max(30000, 3 * max_sample_size)
  )
  # Generate data and compute metric for sizes_to_check, n_reps_per times
  performance_matrix <-
    matrix(
      nrow = length(sample_grid),
      ncol = n_reps_per
    )
  colnames(performance_matrix) <- 1:n_reps_per
  rownames(performance_matrix) <- sample_grid

  test_n <- pmax(3 * max_sample_size, 30000)
  test_data <- data_function(test_n, tune_param)

  metric_calculation <- function(n) {
    tryCatch(
      {
        train_data <- data_function(n, tune_param)
        fit <- model_function(train_data)
        model <- attr(model_function, "model")
        metric_function(test_data, fit, model)
      },
      error = function(e) {
        return(value_on_error)
      }
    )
  }
  
  #progress bar
  pb <- utils::txtProgressBar(0, length(sample_grid)+1, style = 3)
  utils::setTxtProgressBar(pb, 1)
  
  # Compute performance metrics across sizes and simulations
  for (i in seq_along(sample_grid)) {
    utils::setTxtProgressBar(pb, 1+i)
    for (j in 1:n_reps_per) {
      performance_matrix[i, j] <- metric_calculation(sample_grid[i])
    }
  }
  close(pb)
  get_perf <- function(results, p) {
    apply(results, FUN = stats::quantile, MARGIN = 1, probs = p, na.rm = TRUE)
  }

  crude_summaries <- list(
    median_performance = get_perf(performance_matrix, 0.5),
    quant20_performance = get_perf(performance_matrix, 0.2),
    quant5_performance = get_perf(performance_matrix, 0.05),
    quant95_performance = get_perf(performance_matrix, 0.95)
  )

  if (is.na(
    which(crude_summaries$quant20_performance > target_performance)[1])
  ) {
    crude_min_n <- NA
  } else {
    crude_min_n <-
      sample_grid[
        which(crude_summaries$quant20_performance > target_performance)[1]
      ]
  }
  return(list(results = performance_matrix,
              summaries = crude_summaries,
              min_n = crude_min_n))
}
