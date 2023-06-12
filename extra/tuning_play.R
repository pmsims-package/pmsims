data_options <- list(type = "binary",
                     args = list(n_params = 20))
dgf <- default_data_generators(data_options)
m <- default_model_generators(type = "binary")

tune_generate_data(data_generating_function = dgf,
                   large_n = 100000,
                   min_tune_arg = 0,
                   max_tune_arg = 1,
                   model_function = m$model,
                   performance_function = m$metric,
                   target_large_sample_performance = 0.7)

