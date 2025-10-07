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


#' get_min_sample_size: Heuristic starting-n for binary/continuous/survival prediction
#'
#' @param npar             Integer. Number of predictors in the model.
#' @param prevalence       Numeric [0,1]. Event rate or case‐fraction (binary/survival).
#'                         (optional; used for EPV calculations)
#' @param c_stat           Numeric >0.5. Anticipated c‐statistic (discrimination).
#'                         (optional; factor >1/c_stat)
#' @param calib_slope      Numeric. Anticipated calibration slope.
#'                         (optional; factor 1/calib_slope if <1)
#' @param outcome_type     One of "binary", "survival", or "continuous".
#' @return Integer: recommended starting minimum sample size.
#' @examples
#' get_min_sample_size(npar = 5, prevalence = 0.2, c_stat = 0.75,
#'                     calib_slope = 0.9, outcome_type = "binary")
get_min_sample_size <- function(
    npar,
    prevalence   = NULL,
    c_stat       = NULL,
    calib_slope  = NULL,
    outcome_type = c("binary","survival","continuous")
) {
  outcome_type <- match.arg(outcome_type)
  
  # --- 1) Base rule: 3 * npar (absolute minimum)
  n0 <- 3 * npar
  
  # --- 2) Outcome-specific rules ---
  if (outcome_type == "binary") {
    # Recommended: ≥10 EPV (Riley et al., 2020)
    epv <- 10
    if (!is.null(prevalence) && prevalence > 0 && prevalence < 1) {
      n_epv <- round(epv * npar / prevalence)
      # Optional adjustments:
      if (!is.null(c_stat)) {
        if (c_stat <= 0 || c_stat > 1) warning("c_stat should be between 0 and 1.")
        # Lower c-statistic → require more data (simple heuristic)
        adj <- 1 / max(c_stat, 0.5)  # avoid extreme inflation
        n_epv <- round(n_epv * adj)
      }
    } else {
      warning("Prevalence not provided or invalid; assuming 50% events.")
      n_epv <- round(epv * npar / 0.5)
      # Optional adjustments:
      if (!is.null(c_stat)) {
        if (c_stat <= 0 || c_stat > 1) warning("c_stat should be between 0 and 1.")
        # Lower c-statistic → require more data (simple heuristic)
        adj <- 1 / max(c_stat, 0.5)  # avoid extreme inflation
        n_epv <- round(n_epv * adj)
      }
    }
    n0 <- max(n0, n_epv)
    
  } else if (outcome_type == "survival") {
    # Recommended: ≥20 EPV (Riley et al., 2020)
    epv <- 10
    if (!is.null(prevalence) && prevalence > 0 && prevalence < 1) {
      n_epv <- round(epv * npar / prevalence)
      
      # Optional adjustments:
      if (!is.null(c_stat)) {
        if (c_stat <= 0 || c_stat > 1) warning("c_stat should be between 0 and 1.")
        # Lower c-statistic → require more data (simple heuristic)
        adj <- 1 / max(c_stat, 0.5)  # avoid extreme inflation
        n_epv <- round(n_epv * adj)
      }
      
    } else {
      warning("Event proportion not provided; assuming 50% events.")
      n_epv <- round(epv * npar / 0.5)
      # Optional adjustments:
      if (!is.null(c_stat)) {
        if (c_stat <= 0 || c_stat > 1) warning("c_stat should be between 0 and 1.")
        # Lower c-statistic → require more data (simple heuristic)
        adj <- 1 / max(c_stat, 0.5)  # avoid extreme inflation
        n_epv <- round(n_epv * adj)
      }
    }
    
    
    n0 <- max(n0, n_epv)
    
  } else if (outcome_type == "continuous") {
    # Continuous outcome: ≥20 obs per predictor (Steyerberg, 2019)
    n_cont <- 3 * npar
    
    # Optional adjustments:
    if (!is.null(c_stat)) {
      if (c_stat <= 0 || c_stat > 1) warning("c_stat should be between 0 and 1.")
      # Lower c-statistic → require more data (simple heuristic)
      adj <- 1 / max(c_stat, 0.2)  # avoid extreme inflation
      n_cont <- round(n_cont * adj)
    }
    
    if (!is.null(calib_slope)) {
      if (calib_slope > 0 && calib_slope < 1) {
        # Lower slope means more shrinkage needed → increase N slightly
        if( npar > 10){
          adj <- 1 + (1 - calib_slope)
        }else{
          #adj <- 1 / (1 - calib_slope)
          adj <- 1 + (1 - calib_slope)
        }
        n_cont <- round(n_cont * adj)
      }
    }
    
    n0 <- max(n0, n_cont)
  }
  
  return(as.integer(n0))
}


#' adaptive_startvalues
#'
#' @param output 
#' @param aggregate_fun 
#' @param var_bootstrap 
#' @param target 
#' @param ci_q 
#' 
adaptive_startvalues <- function(output, aggregate_fun, var_bootstrap, target, ci_q = 0.975) {
  bisection_output <- output$track_bisection
  n_iter <- length(bisection_output)
  
  # Matrix: n, est, se, ll, ul
  bisection_summary <- matrix(NA, nrow = n_iter, ncol = 5,
                              dimnames = list(NULL, c("n", "est", "se", "ll", "ul")))
  
  for (i in seq_len(n_iter)) {
    results <- bisection_output[[i]]
    n <- results$x
    performance_data <- results$y
    
    est <- aggregate_fun(performance_data)
    se <- sqrt(var_bootstrap(performance_data))
    
    ll <- est - se * qnorm(ci_q)
    ul <- est + se * qnorm(ci_q)
    
    bisection_summary[i, ] <- c(n, est, se, ll, ul)
  }
  
  ## --- Find min value ---
  ordered_by_ul <- bisection_summary[order(bisection_summary[, "ul"], decreasing = TRUE), ]
  below_target  <- ordered_by_ul[ordered_by_ul[, "ul"] < target, , drop = FALSE]
  
  if (nrow(below_target) == 0) {
    min_value <- min(bisection_summary[, "n"] * 0.8)
  } else {
    min_value <- max(below_target[, "n"])
  }
  
  ## --- Find max value ---
  ordered_by_ll <- bisection_summary[order(bisection_summary[, "ll"], decreasing = TRUE), ]
  above_target  <- ordered_by_ll[ordered_by_ll[, "ll"] > target, , drop = FALSE]
  
  if (nrow(above_target) == 0) {
    max_value <- max(bisection_summary[, "n"] * 1.2)
  } else {
    max_value <- min(above_target[, "n"])
  }
  
  return(list(
    summary = bisection_summary,
    min_value = min_value,
    max_value = max_value
  ))
}






#' mlpwr engine
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
  # Determine number of predictors (excluding outcome column)
  npar <- dim(data_function(1))[2] - 1
  
  # Infer the outcome type and compute data-driven minimum sample size
  # Determine which outcome type applies
  formals_list <- formals(data_function)
  args_names <- names(formals_list)
  
  if ("censoring_rate" %in% args_names) {
    censoring_rate <- eval(formals_list[["censoring_rate"]], environment(data_function))
    min_sample_size <- get_min_sample_size(
      npar          = npar,
      prevalence    = 1 - censoring_rate,
      c_stat        = target_performance,
      calib_slope   = NULL,
      outcome_type  = "survival"
    )
  } else if ("baseline_prob" %in% args_names) {
    baseline_prob <- eval(formals_list[["baseline_prob"]], environment(data_function))
    min_sample_size <- get_min_sample_size(
      npar          = npar,
      prevalence    = baseline_prob,
      c_stat        = target_performance,
      calib_slope   = NULL,
      outcome_type  = "binary"
    )
  } else {
    min_sample_size <- get_min_sample_size(
      npar          = npar,
      prevalence    = NULL,
      c_stat        = target_performance,
      calib_slope   = NULL,
      outcome_type  = "continuous"
    )
  }
  
  
  

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
  
  # Determine number of predictors (excluding outcome column)
  npar <- dim(data_function(1))[2] - 1
  
  # Infer the outcome type and compute data-driven minimum sample size
  # Determine which outcome type applies
  formals_list <- formals(data_function)
  args_names <- names(formals_list)
  
  if ("censoring_rate" %in% args_names) {
    censoring_rate <- eval(formals_list[["censoring_rate"]], environment(data_function))
    min_sample_size <- get_min_sample_size(
      npar          = npar,
      prevalence    = 1 - censoring_rate,
      c_stat        = target_performance,
      calib_slope   = NULL,
      outcome_type  = "survival"
    )
  } else if ("baseline_prob" %in% args_names) {
    baseline_prob <- eval(formals_list[["baseline_prob"]], environment(data_function))
    min_sample_size <- get_min_sample_size(
      npar          = npar,
      prevalence    = baseline_prob,
      c_stat        = target_performance,
      calib_slope   = NULL,
      outcome_type  = "binary"
    )
  } else {
    min_sample_size <- get_min_sample_size(
      npar          = npar,
      prevalence    = NULL,
      c_stat        = target_performance,
      calib_slope   = NULL,
      outcome_type  = "continuous"
    )
  }
  
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
  
  # Determine number of predictors (excluding outcome column)
  npar <- dim(data_function(1))[2] - 1
  
  # Infer the outcome type and compute data-driven minimum sample size
  # Determine which outcome type applies
  formals_list <- formals(data_function)
  args_names <- names(formals_list)
  
  if ("censoring_rate" %in% args_names) {
    censoring_rate <- eval(formals_list[["censoring_rate"]], environment(data_function))
    min_sample_size <- get_min_sample_size(
      npar          = npar,
      prevalence    = 1 - censoring_rate,
      c_stat        = target_performance,
      calib_slope   = NULL,
      outcome_type  = "survival"
    )
  } else if ("baseline_prob" %in% args_names) {
    baseline_prob <- eval(formals_list[["baseline_prob"]], environment(data_function))
    min_sample_size <- get_min_sample_size(
      npar          = npar,
      prevalence    = baseline_prob,
      c_stat        = target_performance,
      calib_slope   = NULL,
      outcome_type  = "binary"
    )
  } else {
    min_sample_size <- get_min_sample_size(
      npar          = npar,
      prevalence    = NULL,
      c_stat        = target_performance,
      calib_slope   = NULL,
      outcome_type  = "continuous"
    )
  }
  
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
  budget = FALSE # <- new parameter
) {
  
  # Determine number of predictors (excluding outcome column)
  npar <- dim(data_function(1))[2] - 1
  
  # Infer the outcome type and compute data-driven minimum sample size
  # Determine which outcome type applies
  formals_list <- formals(data_function)
  args_names <- names(formals_list)
  
  if ("censoring_rate" %in% args_names) {
    censoring_rate <- eval(formals_list[["censoring_rate"]], environment(data_function))
    min_sample_size <- get_min_sample_size(
      npar          = npar,
      prevalence    = 1 - censoring_rate,
      c_stat        = target_performance,
      calib_slope   = NULL,
      outcome_type  = "survival"
    )
  } else if ("baseline_prob" %in% args_names) {
    baseline_prob <- eval(formals_list[["baseline_prob"]], environment(data_function))
    min_sample_size <- get_min_sample_size(
      npar          = npar,
      prevalence    = baseline_prob,
      c_stat        = target_performance,
      calib_slope   = NULL,
      outcome_type  = "binary"
    )
  } else {
    min_sample_size <- get_min_sample_size(
      npar          = npar,
      prevalence    = NULL,
      c_stat        = target_performance,
      calib_slope   = NULL,
      outcome_type  = "continuous"
    )
  }
  
  

  max_iter <- round(n_reps_total / n_reps_per)

  # Generate fixed test set once
  test_data <- data_function(test_n)

  # Helper: run 1 simulation
  single_run <- function(n) {
    tryCatch(
      {
        dat <- data_function(n)
        fit <- model_function(dat)
        metric_function(test_data, fit, attr(model_function, "model"))
      },
      error = function(e) value_on_error
    )
  }

  # Helper: summary of metric for n_reps_per repetitions
  summary_at_n <- function(n) {
    if (parallel) {
      require(foreach)
      require(doParallel)
      cl <- parallel::makeCluster(cores)
      doParallel::registerDoParallel(cl)
      vals <- foreach(i = 1:n_reps_per, .combine = c) %dopar% single_run(n)
      parallel::stopCluster(cl)
    } else {
      vals <- vapply(
        seq_len(n_reps_per),
        function(i) single_run(n),
        FUN.VALUE = numeric(1)
      )
    }
    s <- get_summaries(matrix(vals, nrow = 1))
    if (mean_or_assurance == "mean") {
      list(y_summary = s$mean_performance, y = vals)
    } else {
      list(y_summary = s$quant20_performance, y = vals)
    }
  }

  # Initial bounds
  p_lo <- summary_at_n(min_sample_size)$y_summary
  p_hi <- summary_at_n(max_sample_size)$y_summary

  iter <- 0
  history <- list()
  track_bisection <- list()

  # Bisection loop with condition depending on 'budget'
  while (
    (budget && iter < max_iter) ||
      (!budget && (p_hi - p_lo) >= tol && iter < max_iter)
  ) {
    mid <- floor((min_sample_size + max_sample_size) / 2)
    mid_result <- summary_at_n(mid)
    p_mid <- mid_result$y_summary

    track_bisection[[iter + 1]] <- list(x = mid, y = mid_result$y)

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

  # Determine number of predictors (excluding outcome column)
  npar <- dim(data_function(1))[2] - 1
  
  # Infer the outcome type and compute data-driven minimum sample size
  # Determine which outcome type applies
  formals_list <- formals(data_function)
  args_names <- names(formals_list)
  
  if ("censoring_rate" %in% args_names) {
    censoring_rate <- eval(formals_list[["censoring_rate"]], environment(data_function))
    min_sample_size <- get_min_sample_size(
      npar          = npar,
      prevalence    = 1 - censoring_rate,
      c_stat        = target_performance,
      calib_slope   = NULL,
      outcome_type  = "survival"
    )
    
    prev_max_sample_size <- 10 * min_sample_size
  } else if ("baseline_prob" %in% args_names) {
    baseline_prob <- eval(formals_list[["baseline_prob"]], environment(data_function))
    min_sample_size <- get_min_sample_size(
      npar          = npar,
      prevalence    = baseline_prob,
      c_stat        = target_performance,
      calib_slope   = NULL,
      outcome_type  = "binary"
    )
    
    prev_max_sample_size <- 10 * min_sample_size
  } else {
  
  metric_used <- attr(metric_function, "metric")
  if(metric_used == "calib_slope"){
    min_sample_size <- get_min_sample_size(
      npar          = npar,
      prevalence    = NULL,
      c_stat        = NULL,
      calib_slope   = target_performance,
      outcome_type  = "continuous"
    )
    
    prev_max_sample_size <- 100 * npar
    
  }else{
    
    min_sample_size <- get_min_sample_size(
      npar          = npar,
      prevalence    = NULL,
      c_stat        = target_performance,
      calib_slope   = NULL,
      outcome_type  = "continuous"
    )
    if(target_performance <= 0.5){
    prev_max_sample_size <- 200 * npar
    } else {
    prev_max_sample_size <- 100 * npar 
    }
  }
    
  }
  
 

  prev <- calculate_bisection(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function,
    target_performance = target_performance,
    min_sample_size = min_sample_size,
    max_sample_size = prev_max_sample_size,
    #max_sample_size = 10 * min_sample_size,
    #n_reps_total = floor(0.2*n_reps_total),
    #n_reps_total = 4 * n_reps_per,
    n_reps_total = 200,
    n_reps_per = n_reps_per,
    mean_or_assurance = mean_or_assurance,
    value_on_error = value_on_error,
    verbose = FALSE,
    budget = TRUE,
    test_n = test_n
  )
  

  

  # calculate the second stage mlpwr
  
  test_data <- data_function(test_n)
  # calculate the metrics for a sample size n
  mlpwr_simulation_function <- function(n) {
    tryCatch(
      {
        #test_data <- data_function(test_n)
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
    var(replicate(100, aggregate_fun(sample(x, length(x), replace = TRUE))))
  }
  
 # var_bootstrap <- function(x) {
 #   var(x) / (100*length(x))
 # }

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
  
  # get starting min_sample from previous bisection in stage 1
  #a.lo = prev$track_bisection[length(prev$track_bisection)][[1]]$x
  
  
  get_start_bounds = adaptive_startvalues(output = prev, 
                                          aggregate_fun = aggregate_fun,
                                          var_bootstrap = var_bootstrap,
                                          target = target_performance,
                                          ci_q = 0.975)
  

  mlpwrbs_min_sample_size <- get_start_bounds$min_value
  mlpwrbs_max_sample_size <- get_start_bounds$max_value
  
  ds <-
    mlpwr::find.design(
      simfun = mlpwr_simulation_function,
      aggregate_fun = aggregate_fun,
      noise_fun = noise_fun,
      boundaries = c(mlpwrbs_min_sample_size, mlpwrbs_max_sample_size),
      power = target_performance,
      surrogate = "gpr",
      setsize = n_reps_per,
      #evaluations = ceiling(0.8*n_reps_total),
      #evaluations = n_reps_total - (4 * n_reps_per),
      evaluations = n_reps_total,
      ci = ci,
      n.startsets = 4,
      silent = !verbose #,
      #dat = prev$track_bisection
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
