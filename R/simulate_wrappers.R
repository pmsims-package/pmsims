#' Calculate the minimum sample size required to develop a prediction model
#'
#' Minimum working example using the mlpwr package
#'
#' @param data_generating_function A function of two parameters, n and a tuning
#'   parameter, that returns data for the model function
#' @param model_function A function which takes the object returned by the data
#'   generating function and fits the analysis model of interest.
#' @param metric_function A function which takes a a test dataset and model
#'   object as arguments and returns a performance metricß
#' @param target_performance The desired performance of the prediction model
#' @param test_n The sample size used for testing model performance
#' @param tune_param A tuning parameter to be passed to the data generating
#'   function
#' @param large_sample_performance The desired performance in a large sample
#'   (for the metric defined by tune_metric_function). This may be specified in
#'   place of tune_param. The data generating model is tuned so the desired
#'   performance is obtained when n is equal to the max_sample_size.
#' @param tune_args A named list of arguments to be passed to
#'   tune_generate_data.R. Possible arguments are large_n, min_tune_arg,
#'   max_tune_arg, max_interval_expansion, and tolerance. See
#'   \code{\link{tune_generate_data}} for more details.
#' @param tune_metric_function The metric_function used when tuning the model.
#'   This may differ from the metric function used for determining the sample
#'   size.
#' @param min_sample_size The minimum sample size used in simulations
#' @param max_sample_size The maximum sample size used in simulations
#' @param n_reps_totalThe number of simulation reps
#' @param final_estimate_se The standard error for the estimate of model
#'   performance at the minimum sample size. Either this or nreps should be
#'   specified.
#' @param n_sample_sizes The number of different sample sizes simulations are
#'   carried out at each sample size
#' @param n_init The number of different sample sizes for initialization
#'   (before updates)
#' @return A list of results form the simulation
#' @export
#'
#' @examples
#' # TODO

#' Calculate the minimum sample size required for a binary outcome
#'
#' @inheritParams generate_binary_data
#' @param ... Other options passed to [simulate_custom()]
#'
#' @return
#' @export
#'
#' @examples

simulate_binary <- function(
    # Predictors
  signal_parameters,
  noise_parameters = 0,
  predictor_type = "continuous",
  binary_predictor_prevalence = NULL,
  # Outcome
  outcome_prevalence,
  large_sample_cstatistic,
  # Model
  model = "glm",
  # Performance
  metric = "calibration_slope",
  minimum_acceptable_performance,
  # engine control
  n_reps_total = 1000,
  mean_or_assurance = "assurance",
  ...
){
  
  
   # Tune for data function
    tune_param <- binary_tuning(
      target_prevalence = outcome_prevalence,
      target_performance = large_sample_cstatistic,
      candidate_features = signal_parameters,
      proportion_noise_features = noise_parameters
    )[c(1, 3)] # extract mean of linear predictor as new intercept and beta_signal scaled by var of lp

  
  data_spec <- list(
    type = "binary",
    args = list(
      mu_lp = tune_param[1],
      beta_signal = tune_param[2],
      n_signal_parameters = signal_parameters,
      noise_parameters = noise_parameters,
      predictor_type = predictor_type,
      predictor_prop = binary_predictor_prevalence,
      baseline_prob = outcome_prevalence
    )
  )

  data_function <- default_data_generators(data_spec)
  outcome_type <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome_type, model)

  
  # Redefine metrics to internal syntax lang
  
  metric = ifelse(metric == "calibration_slope","calib_slope", metric)
  
  
  # main pmsims
  
  suppressWarnings(output <-  simulate_custom(
      metric_function = default_metric_generator(metric, data_function),
      target_performance = minimum_acceptable_performance,
      c_statistic = large_sample_cstatistic,
      # Common arguments
      data_function = data_function,
      model_function = model_function,
      min_sample_size = NULL,
      max_sample_size = NULL,
      se_final = NULL,
      n_reps_total = n_reps_total,
      n_reps_per = 20,
      method = "mlpwr-bs",
      mean_or_assurance = mean_or_assurance,
      test_n = 30000)
  )
 
 ## print immediately (will show in console even if user assigns the result)
 #print(output)
 
 ## still return it invisibly so callers can assign/use it without extra console noise
 #invisible(output)
  
  
  ## append input parameters
 
  
  # Predictors
  output$parameters <- signal_parameters
  output$noise_parameters <- noise_parameters
  output$predictor_type <- predictor_type
  output$binary_predictor_prevalence <- output$predictor_type
  # Outcome
  output$prevalence <- outcome_prevalence
  output$cstatistic <- large_sample_cstatistic
  # Model
  output$model <- model
  # Performance
  output$metric <- metric
  # engine control
  output$n_reps_total <- n_reps_total
  output$mean_or_assurance <- mean_or_assurance

 est <- output
 class(est) <- "pmsims"
 est
 
}



#' Calculate the minimum sample size required for a continous outcome
#'
#' @inheritParams generate_continuous_data
#' @export
#'
#' @examples
simulate_continuous <- function(
    # Predictors
  signal_parameters,
  noise_parameters = 0,
  predictor_type = "continuous",
  binary_predictor_prevalence = NULL,
  # Outcome
  large_sample_rsquared,
  # Model
  model = "lm",
  # Performance
  metric = "calibration_slope",
  minimum_acceptable_performance,
  # engine control
  n_reps_total = 1000,
  mean_or_assurance = "assurance",
  ...
)
{
  
  
  # Tune for data function
  tune_param <- continuous_tuning(
    r2 = large_sample_rsquared,
    candidate_features = signal_parameters,
    proportion_noise_features = noise_parameters
  ) # extract beta_signal
  
  
  data_spec <- list(
    type = "continuous",
    args = list(
      beta_signal = tune_param,
      n_signal_parameters = signal_parameters,
      noise_parameters = noise_parameters,
      predictor_type = predictor_type,
      predictor_prop = binary_predictor_prevalence
    )
  )
  
  data_function <- default_data_generators(data_spec)
  outcome_type <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome_type, model)
  
  
  # Redefine metrics to internal syntax lang
  
  metric = ifelse(metric == "calibration_slope","calib_slope", metric)
  
  
  # main pmsims
  
  suppressWarnings(output <-  simulate_custom(
    metric_function = default_metric_generator(metric, data_function),
    target_performance = minimum_acceptable_performance,
    c_statistic = large_sample_rsquared,
    # Common arguments
    data_function = data_function,
    model_function = model_function,
    min_sample_size = NULL,
    max_sample_size = NULL,
    se_final = NULL,
    n_reps_total = n_reps_total,
    n_reps_per = 20,
    method = "mlpwr-bs",
    mean_or_assurance = mean_or_assurance,
    test_n = 30000)
  )
  
  
  ## append input parameters
  
  
  # Predictors
  output$parameters <- signal_parameters
  output$noise_parameters <- noise_parameters
  output$predictor_type <- predictor_type
  output$binary_predictor_prevalence <- output$predictor_type
  # Outcome
  output$r2 <- large_sample_rsquared
  # Model
  output$model <- model
  # Performance
  output$metric <- metric
  # engine control
  output$n_reps_total <- n_reps_total
  output$mean_or_assurance <- mean_or_assurance
  
  est <- output
  class(est) <- "pmsims"
  est
  
}

#' Calculate the minimum sample size required for a survival outcome
#'
#' @inheritParams generate_survival_data
#'
#' @return
#' @export
#'
#' @examples
simulate_survival <- function(
    # Predictors
  signal_parameters,
  noise_parameters = 0,
  predictor_type = "continuous",
  binary_predictor_prevalence = NULL,
  # Outcome
  large_sample_cindex,
  baseline_hazard = 1,
  censoring_rate,
  # Model
  model = "coxph",
  # Performance
  metric = "calibration_slope",
  minimum_acceptable_performance,
  # engine control
  n_reps_total = 1000,
  mean_or_assurance = "assurance",
  ...
){
  
  
  # Tune for data function
  tune_param <- binary_tuning(
    target_prevalence = 1 - censoring_rate,
    target_performance = large_sample_cindex,
    candidate_features = signal_parameters,
    proportion_noise_features = noise_parameters
  )[c(1, 3)] # extract mean of linear predictor as new intercept and beta_signal scaled by var of lp
  
  
  data_spec <- list(
    type = "survival",
    args = list(
      baseline_hazard = baseline_hazard,
      beta_signal = tune_param[2],
      n_signal_parameters = signal_parameters,
      noise_parameters = noise_parameters,
      predictor_type = predictor_type,
      predictor_prop = binary_predictor_prevalence,
      censoring_rate = censoring_rate
    )
  )
  
  data_function <- default_data_generators(data_spec)
  outcome_type <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome_type, model)
  
  

  # Redefine metrics to internal syntax lang
  
  metric = ifelse(metric == "calibration_slope","calib_slope", metric)
  
  
  # main pmsims
  
  suppressWarnings(output <-  simulate_custom(
    metric_function = default_metric_generator(metric, data_function),
    target_performance = minimum_acceptable_performance,
    c_statistic = large_sample_cindex,
    # Common arguments
    data_function = data_function,
    model_function = model_function,
    min_sample_size = NULL,
    max_sample_size = NULL,
    se_final = NULL,
    n_reps_total = n_reps_total,
    n_reps_per = 20,
    method = "mlpwr-bs",
    mean_or_assurance = mean_or_assurance,
    test_n = 30000)
  )

  
  ## append input parameters
  
  
  # Predictors
  output$parameters <- signal_parameters
  output$noise_parameters <- noise_parameters
  output$predictor_type <- predictor_type
  output$binary_predictor_prevalence <- output$predictor_type
  output$baseline_hazard <- baseline_hazard
  # Outcome
  output$censoring_rate <- censoring_rate
  output$cstatistic <- large_sample_cindex
  # Model
  output$model <- model
  # Performance
  output$metric <- metric
  # engine control
  output$n_reps_total <- n_reps_total
  output$mean_or_assurance <- mean_or_assurance
  
  est <- output
  class(est) <- "pmsims"
  est
  
}



#' @export
print.pmsims <- function(x, ...) {
  if (!inherits(x, "pmsims")) {
    stop("Object is not of class 'pmsims'")
  }
  
  #cat("# Things to report in print.pmsims\n\n")
  
  
  ## 1) Input parameters - print a compact list of commonly useful fields if present
  cat("Input parameters:\n")
  fields_to_show <- c("outcome", "predictor_type", "parameters", "noise_parameters", 
                      "prevalence", "baseline_hazard", "censoring_rate",  
                      "cstatistic", "r2", "target_performance", "model", "metric",
                      "n_reps_total", "mean_or_assurance")
  
  # show those available plus any small scalar items commonly present
  shown_any <- FALSE
  for (nm in fields_to_show) {
    if (!is.null(x[[nm]])) {
      val <- x[[nm]]
      # format the value nicely depending on its type
      if (length(val) > 5) {
        val_str <- paste(head(val, 5), collapse = ", ")
        val_str <- paste0(val_str, " ...")
      } else if (is.atomic(val)) {
        val_str <- paste(val, collapse = ", ")
      } else if (is.list(val)) {
        val_str <- paste0("<list of length ", length(val), ">")
      } else {
        val_str <- as.character(val)
      }
      
      cat("  - ", nm, ": ", val_str, "\n", sep = "")
      shown_any <- TRUE
    }
  }
  # if none of the above printed, print top-level names and small values
  if (!shown_any) {
    top_names <- names(x)
    # print only scalar-ish entries
    scalar_names <- top_names[sapply(x, function(el) {
      (is.atomic(el) && length(el) <= 5) || is.null(el)
    })]
    if (length(scalar_names) > 0) {
      for (nm in scalar_names) {
        if (!is.null(x[[nm]])) {
          cat("  - ", nm, ": ", paste0(capture.output(str(x[[nm]])), collapse = " "), "\n", sep = "")
        }
      }
    } else {
      cat("  <no compact scalar input parameters found on object — inspect object manually>\n")
    }
  }
  cat("\n")
  
  ## 2) Final estimate of minimum sample size
  min_n <- if (!is.null(x$min_n)) x$min_n else NA
  cat("Final estimate of minimum sample size: ", min_n, "\n\n", sep = "")
  
  ## 3) Estimated performance at that sample size — try to extract mlpwr / mean performance
  perf_val <- if (!is.null(x$perf_n)) x$perf_n else NA
  cat("Estimated performance at sample size: ", round(perf_val,3), "\n\n", sep = "")
  
  
  ## 4) Running time
  if (!is.null(x$simulation_time)) {
    simt <- x$simulation_time
    # print nicely if it's difftime
    if (inherits(simt, "difftime")) {
      cat("Running time: ", format(simt), "\n", sep = "")
    } else {
      cat("Running time: ", paste0(capture.output(str(simt)), collapse = " "), "\n", sep = "")
    }
  } else {
    cat("Running time: <not available>\n")
  }
  
}


#' @export
plot.pmsims <- function(x, metric_label = NULL, plot = TRUE, ...) {
    
    ds <- ex2$mlpwr_ds
    design <- NULL
    
    dat <- ds$dat
    fit <- ds$fit
    aggregate_fun <- ds$aggregate_fun
    
    dat_obs <- mlpwr:::todataframe(dat, aggregate = TRUE, aggregate_fun = aggregate_fun)
    
    boundaries <- ds$boundaries
    if (!is.null(design)) {
      namesx <- names(boundaries)
      specified <- !sapply(design, is.na)
      boundariesx <- unlist(boundaries[!specified])
      ns <- seq(boundariesx[1], boundariesx[2])
      nsx <- lapply(ns, function(x) {
        a <- c()
        a[specified] <- as.numeric(design[specified])
        a[!specified] <- x
        a
      })
      ind <- dat_obs[c(specified, FALSE, FALSE)] == as.numeric(design[specified])
      dat_obs <- dat_obs[ind, ]
      a1 <- names(ds$final$design)[!specified]
      a2 <- paste(names(design)[specified], "=", design[specified], 
                  sep = " ", collapse = ",")
      xlab <- paste0(a1, " (", a2, ")")
    }
    if (is.null(design)) {
      boundariesx <- unlist(boundaries)
      xlab <- names(ds$final$design)
      ns <- seq(boundariesx[1], boundariesx[2])
      nsx <- ns
    }
    dat_pred <- data.frame(n = ns, y = sapply(nsx, fit$fitfun), 
                           type = "Prediction")
    
    
    #### plot annotations
    min_n <- if (!is.null(x$min_n)) as.numeric(x$min_n) else NA_real_
    perf_n <- if (!is.null(x$perf_n)) as.numeric(x$perf_n) else {
      if (!is.na(min_n) && any(df$n == min_n)) df$mean[df$n == min_n] else NA_real_
    }
    
    
    target_perf <- if (!is.null(x$target_performance)) as.numeric(x$target_performance) else NA_real_
    metric_name <- if (!is.null(metric_label)) metric_label else if (!is.null(x$metric)) as.character(x$metric) else "performance"
    metric_summary <- if (!is.null(x$mean_or_assurance)) as.character(x$mean_or_assurance) else "performance"
    
    
    
    
    p <- ggplot2::ggplot()
    
    p <- p + ggplot2::geom_line(ggplot2::aes(x = dat_pred$n, 
                                                 y = dat_pred$y)) + ggplot2::geom_point(ggplot2::aes(x = dat_obs$V1, 
                                                                                                     y = dat_obs$y)) + ggplot2::theme_bw() + ggplot2::scale_color_brewer(palette = "Set1") + 
      ggplot2::theme(legend.title = ggplot2::element_blank()) + 
      ggplot2::xlab(xlab) + ggplot2::ylab("Power") + ggplot2::theme(legend.position = "bottom")
  
    
    p <- p + ggplot2::geom_point(ggplot2::aes(x = min_n, y = perf_n), data = data.frame(n = min_n, mean = perf_n),
                                 size = 3)
    p <- p + ggplot2::annotate("text", x = min_n, y = perf_n,
                               label = sprintf("min_n = %s\nperf = %.3f", min_n, perf_n),
                               hjust = -0.05, vjust = -0.5, size = 3.5)
    
    if (!is.na(target_perf) && nrow(dat_obs) > 0) {
      x_right <- max(dat_pred$n, na.rm = TRUE)
      p <- p + ggplot2::annotate("text", x = x_right, y = target_perf,
                                 label = sprintf("target = %.3f", target_perf),
                                 hjust = 1.05, vjust = -0.5, size = 3.5)
    }
    
    p <- p + ggplot2::labs(
      x = "Sample size (n)",
      y = paste0("Performance (", metric_summary , "[", metric_name, "]" , ")"),
      title = "Sample size vs performance"
    ) + ggplot2::theme_bw() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    
    
    if (!is.na(target_perf)) {
      p <- p + ggplot2::geom_hline(yintercept = target_perf, linetype = "dashed")
    }
    
    if (!is.na(min_n)) {
      p <- p + ggplot2::geom_vline(xintercept = min_n, linetype = "dotted")
    }
    
    if(plot){
    print(p)   
    } else {
      observed_data = dat_obs #[order(dat_obs$V1), , drop = FALSE]
      predicted_data = dat_pred[, -3]
      colnames(observed_data) <-  colnames(predicted_data) <- c("n", metric_name)
      plot_data <- list(observed_data = observed_data, predicted_data = predicted_data)
      plot_data
    } 
}
