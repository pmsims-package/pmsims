get_perf <- function(results, p = NULL, mean = FALSE) {
  if (is.null(p) && !mean) {
    stop("Either p or mean must be specified")
  }
  if (mean) {
    results <- apply(results, FUN = mean, MARGIN = 1, na.rm = TRUE)
  } else {
    results <- apply(
      results,
      FUN = stats::quantile,
      MARGIN = 1,
      probs = p,
      na.rm = TRUE
    )
  }
  return(results)
}


get_summaries <- function(performance_matrix) {
  list(
    mean_performance = get_perf(results = performance_matrix, mean = TRUE),
    median_performance = get_perf(performance_matrix, p = 0.5),
    quant20_performance = get_perf(performance_matrix, p = 0.2),
    quant5_performance = get_perf(performance_matrix, p = 0.05),
    quant95_performance = get_perf(performance_matrix, p = 0.95)
  )
}

get_ga_solution <- function(
  gaobject,
  mean_or_assurance = c("mean", "assurance")
) {
  mean_or_assurance <- match.arg(mean_or_assurance)

  # Select fitness threshold based on mean or assurance (20th percentile)
  fitness_threshold <- if (mean_or_assurance == "mean") {
    mean(gaobject@fitness, na.rm = TRUE)
  } else {
    quantile(gaobject@fitness, probs = 0.20, na.rm = TRUE)
  }

  # Get indices where fitness meets/exceeds threshold
  valid_idx <- which(gaobject@fitness >= fitness_threshold)
  candidate_population <- gaobject@population[valid_idx, , drop = FALSE]

  # If multiple rows, compute summary statistic based on context
  if (nrow(candidate_population) > 1) {
    solution_values <- rowMeans(candidate_population, na.rm = TRUE)
    if (mean_or_assurance == "mean") {
      solution <- mean(solution_values, na.rm = TRUE)
    } else {
      solution <- quantile(solution_values, probs = 0.2, na.rm = TRUE)
    }
  } else {
    solution <- candidate_population
  }

  return(round(as.numeric(solution)))
}


#' MLPWR Engine
#' @inheritParams simulate_custom
#' @param n_init The number of initial sample sizes simualted before the gausian process search begins.
#' @param verbose Whether to run mlpwr with verbose output
#' @param value_on_error The value used if there is an error in fitting the model or calculating performance.
#'
#' @returns
#' @export
#'
#' @examples
calculate_mlpwr <- function(
  test_n,
  n_reps_total,
  n_reps_per,
  se_final,
  min_sample_size,
  max_sample_size,
  target_performance,
  mean_or_assurance,
  n_init,
  verbose,
  data_function,
  model_function,
  metric_function,
  value_on_error
) {
  # calculate the metrics for a sample size n
  mlpwr_simulation_function <- function(n) {
    tryCatch(
      {
        test_data <- data_function(test_n)
        train_data <- data_function(n)
        fit <- model_function(train_data)
        model <- attr(model_function, "model")
        metric_function(test_data, fit, model)
      },
      error = function(e) {
        return(value_on_error)
      }
    )
  }

  if (mean_or_assurance == "mean") {
    aggregate_fun <- function(x) mean(x, na.rm = TRUE)
  } else if (mean_or_assurance == "assurance") {
    aggregate_fun <- function(x) quantile(x, probs = .2, na.rm = TRUE)
  } else {
    stop("mean_or_assurance must be either 'mean' or 'assurance'")
  }

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

  mlpwr_summaries <- get_summaries(results)

  # list(
  #   median_performance = get_perf(results, 0.5),
  #   quant20_performance = get_perf(results, 0.2),
  #   quant5_performance = get_perf(results, 0.05),
  #   quant95_performance = get_perf(results, 0.95)
  # )

  return(list(
    results = perfs,
    summaries = mlpwr_summaries,
    min_n = as.numeric(ds$final$design)
  ))
}

#' The Crude Engine
#' @inheritParams calculate_mlpwr
#' @param value_on_error
#' @param parallel Whether to use parallel processing. Default is FALSE
#' @param cores If parallel processing, how many cores to pass to parallel::makeCluster(cores) Default is 20.
#'
#' @returns
#' @export
#'
#' @examples
calculate_crude <- function(
  data_function,
  model_function,
  metric_function,
  value_on_error,
  min_sample_size,
  max_sample_size,
  test_n,
  n_reps_total,
  n_reps_per,
  target_performance,
  mean_or_assurance,
  parallel = FALSE,
  cores = 20
) {
  n_steps <- round(n_reps_total / n_reps_per)

  # Make sure n_reps_per is 10 or over
  n_reps_per <- pmax(10, n_reps_per)

  # Specify grid
  sample_grid <- c(
    round(seq(min_sample_size, max_sample_size, length.out = n_steps)),
    max(30000, 3 * max_sample_size)
  )

  # Generate data and compute metric for sizes_to_check, n_reps_per times
  test_data <- data_function(test_n)

  metric_calculation <- function(n) {
    tryCatch(
      {
        train_data <- data_function(n)
        fit <- model_function(train_data)
        model <- attr(model_function, "model")
        return(metric_function(test_data, fit, model))
      },
      error = function(e) {
        return(value_on_error)
      }
    )
  }

  if (parallel) {
    cat("\nRunning in parallel...")
    require(foreach)
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    performance_matrix <-
      foreach(a = sample_grid, .combine = rbind) %:%
      foreach(b = 1:n_reps_per, .combine = c) %dopar%
      {
        return(metric_calculation(a))
      }
    colnames(performance_matrix) <- 1:n_reps_per
    rownames(performance_matrix) <- sample_grid
    parallel::stopCluster(cl)
  } else {
    performance_matrix <-
      matrix(
        nrow = length(sample_grid),
        ncol = n_reps_per
      )
    colnames(performance_matrix) <- 1:n_reps_per
    rownames(performance_matrix) <- sample_grid
    # progress bar
    pb <- utils::txtProgressBar(0, length(sample_grid) + 2, style = 3)
    utils::setTxtProgressBar(pb, 1)

    # Compute performance metrics across sizes and simulations
    for (i in seq_along(sample_grid)) {
      utils::setTxtProgressBar(pb, 1 + i)
      for (j in 1:n_reps_per) {
        performance_matrix[i, j] <- metric_calculation(sample_grid[i])
      }
    }
    utils::setTxtProgressBar(pb, length(sample_grid) + 2)
    close(pb)
  }

  crude_summaries <- get_summaries(performance_matrix)

  if (mean_or_assurance == "mean") {
    target_summaries <- crude_summaries$mean_performance
  } else if (mean_or_assurance == "assurance") {
    target_summaries <- crude_summaries$quant20_performance
  } else {
    stop("mean_or_assurance must be either 'mean' or 'assurance'")
  }

  if (
    is.na(
      which(target_summaries > target_performance)[1]
    )
  ) {
    crude_min_n <- NA
  } else {
    crude_min_n <-
      sample_grid[
        which(target_summaries > target_performance)[1]
      ]
  }
  return(list(
    results = performance_matrix,
    summaries = crude_summaries,
    min_n = crude_min_n
  ))
}

#' The GA Engine
#' @inheritParams calculate_mlpwr
#' @param value_on_error
#'
#' @returns
#' @export
#'
#' @examples
calculate_ga <- function(
  data_function = data_function,
  model_function = model_function,
  metric_function = metric_function,
  value_on_error = value_on_error,
  min_sample_size = min_sample_size,
  max_sample_size = max_sample_size,
  test_n = test_n,
  n_reps_total = n_reps_total,
  n_reps_per = n_reps_per,
  target_performance = target_performance,
  mean_or_assurance = mean_or_assurance,
  penalty_weight = 1
) {
  maxiter <- n_reps_per
  popSize <- round(n_reps_total / n_reps_per)

  # Set seed for reproducibility
  #set.seed(seed)

  # Generate test data once
  test_data <- data_function(test_n)

  # Define the objective function for the genetic algorithm
  calc_objective_function <- function(n) {
    n <- round(n)
    if (n < min_sample_size) {
      return(-Inf)
    } # Enforce minimum sample size
    tryCatch(
      {
        # Generate training data
        train_data <- data_function(n)

        # Fit model
        fit <- model_function(train_data)
        model <- attr(model_function, "model")

        # Calculate performance metric
        performance <- metric_function(test_data, fit, model)

        # Calculate penalty term (normalized by max sample size)
        penalty <- penalty_weight * (n / max_sample_size)

        # Objective value (minimize difference between performance and target while minimizing sample size)
        # objective_value <- -abs(performance - target_performance  - penalty)
        objective_value <- 1 /
          (abs(performance - target_performance) + 1) -
          penalty
        return(objective_value)
      },
      error = function(e) {
        return(value_on_error)
      }
    )
  }

  # Configure and run genetic algorithm
  # Load GA package
  require(GA)
  ga_result <- GA::ga(
    type = "real-valued",
    fitness = calc_objective_function,
    lower = min_sample_size,
    upper = max_sample_size,
    popSize = popSize,
    maxiter = maxiter,
    keepBest = TRUE,
    parallel = FALSE,
    seed = NULL,
    monitor = FALSE
  )

  metric_calculation <- function(n) {
    tryCatch(
      {
        train_data <- data_function(n)
        fit <- model_function(train_data)
        model <- attr(model_function, "model")
        return(metric_function(test_data, fit, model))
      },
      error = function(e) {
        return(value_on_error)
      }
    )
  }

  # Extract results
  # This returns n that maximizes the mean/q20 fitness value
  best_n <- get_ga_solution(ga_result, mean_or_assurance)
  best_performance <- metric_calculation(best_n)

  # Process results from GA
  sample_size_iterations <- round(unlist(lapply(ga_result@bestSol, mean)))
  perfs <- sapply(sample_size_iterations, function(x) metric_calculation(x))
  ga_summaries <- list(
    mean_performance = mean(perfs, na.rm = TRUE),
    median_performance = quantile(perfs, 0.5, na.rm = TRUE),
    quant20_performance = quantile(perfs, 0.2, na.rm = TRUE),
    quant5_performance = quantile(perfs, 0.05, na.rm = TRUE),
    quant95_performance = quantile(perfs, 0.95, na.rm = TRUE)
  )
  return(list(
    results = perfs,
    summaries = ga_summaries,
    min_n = best_n
  ))
}


#' The Bisection Engine
#' @inheritParams calculate_mlpwr
#' @param value_on_error
#'
#' @returns
#' @export
#'
#' @examples

calculate_bisection <- function(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    value_on_error = value_on_error,
    min_sample_size = min_sample_size,
    max_sample_size = max_sample_size,
    test_n = test_n,
    n_reps_total = n_reps_total,
    n_reps_per = n_reps_per,
    target_performance = target_performance,
    mean_or_assurance = mean_or_assurance,
    tol = 1e-3,
    parallel = FALSE,
    cores = 20,
    verbose = FALSE,
    track  = FALSE
) {
  
  max_iter <- round(n_reps_total / n_reps_per)
  
  # Generate fixed test set once
  test_data <- data_function(test_n)
  
  # Helper: run 1 simulation
  single_run <- function(n) {
    tryCatch({
      dat <- data_function(n)
      fit <- model_function(dat)
      metric_function(test_data, fit, attr(model_function, "model"))
    }, error = function(e) value_on_error)
  }
  
  # Helper: summary of metric for n_reps_per repetitions
  summary_at_n <- function(n) {
    if (parallel) {
      require(foreach); require(doParallel)
      cl <- parallel::makeCluster(cores); doParallel::registerDoParallel(cl)
      vals <- foreach(i = 1:n_reps_per, .combine = c) %dopar% single_run(n)
      parallel::stopCluster(cl)
    } else {
      vals <- vapply(seq_len(n_reps_per), function(i) single_run(n), FUN.VALUE = numeric(1))
    }
    s <- get_summaries(matrix(vals, nrow = 1))
    if (mean_or_assurance == "mean") {
      list(y_summary=s$mean_performance, y=vals)
    } else {
      list(y_summary=s$quant20_performance,y=vals)
    }
  }
  

  # Initial bounds
  p_lo <- summary_at_n(min_sample_size)$y_summary
  p_hi <- summary_at_n(max_sample_size)$y_summary
  
  iter <- 0
  history <- list()  # Store mid and p_mid at each iteration
  track_bisection <- list()  # Store mid and all  p_mid at each iteration
  
  if(track){
    max_iter_track <- 4
    while (iter < max_iter_track) { # auto stopping
      #while (iter < max_iter) { # budget stopping
      mid <- floor((min_sample_size + max_sample_size) / 2)
      p_mid <- summary_at_n(mid)$y_summary
      
      track_bisection[[iter + 1]] = list(x=mid, y=summary_at_n(mid)$y)
      
      # Track iteration
      if (verbose) {
        history[[iter + 1]] <- list(iter = iter + 1, mid = mid, p_mid = p_mid)
      }
      
      if (p_mid >= target_performance) {
        max_sample_size <- mid
        p_hi <- p_mid
      } else {
        min_sample_size <- mid
        p_lo <- p_mid
      }
      
      iter <- iter + 1
    }
    
  }else{
    
    while ((p_hi - p_lo) >= tol && iter < max_iter) { # auto stopping
      #while (iter < max_iter) { # budget stopping
      mid <- floor((min_sample_size + max_sample_size) / 2)
      p_mid <- summary_at_n(mid)$y_summary
      
      track_bisection[[iter + 1]] = list(x=mid, y=summary_at_n(mid)$y)
      
      # Track iteration
      if (verbose) {
        history[[iter + 1]] <- list(iter = iter + 1, mid = mid, p_mid = p_mid)
      }
      
      if (p_mid >= target_performance) {
        max_sample_size <- mid
        p_hi <- p_mid
      } else {
        min_sample_size <- mid
        p_lo <- p_mid
      }
      
      iter <- iter + 1
    }
    
  }
  
    
  result <- list(
    min_n = max_sample_size,
    performance = p_hi,
    min_sample_size_bound = min_sample_size,
    min_sample_size_perf = p_lo,
    max_sample_size_bound = max_sample_size,
    max_sample_size_perf = p_hi,
    iterations = iter,
    track_bisection = track_bisection
  )
  
  if (verbose) {
    result$history <- history
  }
  
  return(result)
}


#' MLPWR_BS (MLPWR with Bisection for initial) Engine
#' @inheritParams simulate_custom
#' @param n_init The number of initial sample sizes simualted before the gausian process search begins.
#' @param verbose Whether to run mlpwr with verbose output
#' @param value_on_error The value used if there is an error in fitting the model or calculating performance.
#'
#' @returns
#' @export
#'
#' @examples
calculate_mlpwr_bs <- function(
    test_n,
    n_reps_total,
    n_reps_per,
    se_final,
    min_sample_size,
    max_sample_size,
    target_performance,
    mean_or_assurance,
    verbose,
    data_function,
    model_function,
    metric_function,
    value_on_error
) {
  
  # calculate the first stage bisection
  
  npar <- dim(data_function(1))[2]-1
  
  
  prev <- calculate_bisection(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    target_performance = target_performance,
    min_sample_size = 3*npar,
    max_sample_size = max_sample_size,
    #n_reps_total = floor(0.4*n_reps_total),
    n_reps_total = n_reps_total,
    n_reps_per = n_reps_per,
    mean_or_assurance = mean_or_assurance, 
    value_on_error = value_on_error, 
    verbose = FALSE,
    track = TRUE,
    test_n = test_n
  )
  
  # calculate the second stage mlpwr
  
  # calculate the metrics for a sample size n
  mlpwr_simulation_function <- function(n) {
    tryCatch(
      {
        test_data <- data_function(test_n)
        train_data <- data_function(n)
        fit <- model_function(train_data)
        model <- attr(model_function, "model")
        metric_function(test_data, fit, model)
      },
      error = function(e) {
        return(value_on_error)
      }
    )
  }
  
  if (mean_or_assurance == "mean") {
    aggregate_fun <- function(x) mean(x, na.rm = TRUE)
  } else if (mean_or_assurance == "assurance") {
    aggregate_fun <- function(x) quantile(x, probs = .2, na.rm = TRUE)
  } else {
    stop("mean_or_assurance must be either 'mean' or 'assurance'")
  }
  
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
      boundaries = c(3*npar, max_sample_size),
      power = target_performance,
      surrogate = "gpr",
      setsize = n_reps_per,
      evaluations = ceiling(0.6*n_reps_total),
      ci = ci,
      n.startsets = 0,
      silent = !verbose,
      dat = prev$track_bisection
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
  
  mlpwr_summaries <- get_summaries(results)
  
  # list(
  #   median_performance = get_perf(results, 0.5),
  #   quant20_performance = get_perf(results, 0.2),
  #   quant5_performance = get_perf(results, 0.05),
  #   quant95_performance = get_perf(results, 0.95)
  # )
  
  return(list(
    results = perfs,
    summaries = mlpwr_summaries,
    min_n = as.numeric(ds$final$design)
  ))
}