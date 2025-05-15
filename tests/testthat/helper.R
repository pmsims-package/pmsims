get_binary_data_model_metric <- function(){
  

    data_opts <- list(type = "binary", 
                      args = list(
                        n_signal_parameters = 5,
                        noise_parameters = 5,
                        predictor_type = "continuous",
                        baseline_prob = 0.2,
                        beta_signal = 0.6124837 # hard coded for speed from tuning code commented out below
                      ))
    data_function <- default_data_generators(data_opts)
    outcome_type <- attr(data_function, "outcome")
    model_function <- default_model_generators(outcome_type, model = "glm")
    
    metric_function = default_metric_generator(
      "auc",
      data_function
    )
    
    # tuning_parameter <- default_tune(tune_param = beta_signal, 
    #                                  max_sample_size = 10000,
    #                                  large_sample_performance = 0.8,
    #                                  data_function = data_function,
    #                                  model_function = model_function,
    #                                  metric_function = metric_function)
    
    return(list(
      data_function = data_function,
      model_function = model_function,
      metric_function = metric_function
    ))

}

