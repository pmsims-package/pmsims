simulate_custom <- function(data_function = NULL,
                            model_function = NULL,
                            metric_function = NULL,
                            target_performance,
                            test_n = 30000,
                            min_sample_size,
                            max_sample_size,
                            n_reps_total = NULL,
                            n_reps_per = 50,
                            se_final = NULL,
                            n_init = 4,
                            method = "mlpwr",
                            verbose = FALSE,
                            ...) {
  if (is.null(data_function)) {
    stop("data_function missing")
  }


  if (sum(c(
    is.null(n_reps_total),
    is.null(se_final)
  )) != 1) {
    stop("Exactly one of 'n_reps_total' or 'se_final' must be specified.")
  }

  if (min_sample_size > max_sample_size) {
    stop("min_sample_size must be less than max_sample_size")
  }


  # Define a default metric value if calculations fail; 0.5 for default
  metric_name <- attr(metric_function, "metric")
  error_values <- list(
    auc = 0.5,
    cindex = 0.5,
    r2 = 0,
    brier_score_scaled = 0,
    brier_score = 1,
    IBS = 1,
    calib_slope = 0
  )
  value_on_error <- ifelse(metric_name %in% names(error_values),
    error_values[[metric_name]],
    0.5
  )
  time_1 <- Sys.time()

  if (method == "mlpwr") {
    output <- calculate_mlpwr(
      test_n = test_n,
      n_reps_total = n_reps_total,
      n_reps_per = n_reps_per,
      se_final = se_final,
      min_sample_size = min_sample_size,
      max_sample_size = max_sample_size,
      target_performance = target_performance,
      n_init = n_init,
      verbose = verbose,
      data_function = data_function,
      model_function = model_function,
      metric_function = metric_function,
      value_on_error = value_on_error
    )
  } else if (method == "crude") {
    output <- calculate_crude(
      data_function,
      model_function,
      metric_function,
      value_on_error,
      min_sample_size,
      max_sample_size,
      n_reps_per,
      target_performance,
      ...
    )
  } else {
    stop("Method not found")
  }
  time_2 <- Sys.time()
  results_list <- list(
    outcome = attr(data_function, "outcome"),
    min_n = ifelse(
      is.na(output$min_n),
      "Not possible. Increase sample or lower performance",
      output$min_n
    ),
    target_performance = target_performance,
    summaries = output$summaries,
    data = output$results,
    train_size = rownames(output$results),
    data_function = data_function,
    simulation_time = difftime(time_2, time_1, units = "secs")
  )
  attr(results_list, "class") <- "pmsims"
  return(results_list)
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
parse_inputs <- function(data_spec,
                         metric,
                         model) {
  if (is.null(metric)) stop("metric is missing")
  if (is.null(data_spec)) stop("data_spec missing")
  # Set data generating function
  data_function <- default_data_generators(data_spec)
  # Set model function, based on outcome type and chosen model
  model_function <- default_model_generators(
    attr(data_function, "outcome"),
    model
  )
  # Set a metric, based on outcome type
  # TODO: handle multiple metrics. Currently selecting first element in
  # 'metric' only.
  metric_function <- default_metric_generator(metric[[1]], data_function)
  return(list(
    data_function = data_function,
    model_function = model_function,
    metric_function = metric_function
  ))
}
