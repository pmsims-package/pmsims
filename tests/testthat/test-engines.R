test_that("calculate_mlpwr", {
  set.seed = 1234
  
  functions <- get_binary_data_model_metric()
  output <- calculate_mlpwr(
    test_n = 1000,
    n_reps_total = 20,
    n_reps_per = 5,
    se_final = NULL,
    min_sample_size = 50,
    max_sample_size = 200,
    target_performance = 0.75,
    mean_or_assurance = "mean",
    n_init = 4,
    data_function = functions$data_function,
    model_function = functions$model_function,
    metric_function = functions$metric_function,
    value_on_error = 0.5) 
  
  expect_true(is.numeric(output$min_n))
  
  output <- calculate_mlpwr(
    test_n = 1000,
    n_reps_total = 20,
    n_reps_per = 5,
    se_final = NULL,
    min_sample_size = 50,
    max_sample_size = 200,
    target_performance = 0.75,
    mean_or_assurance = "assurance",
    n_init = 4,
    data_function = functions$data_function,
    model_function = functions$model_function,
    metric_function = functions$metric_function,
    value_on_error = 0.5) 
  
  expect_true(is.numeric(output$min_n))
 
})

test_that("calculate_crude", {
  
  functions <- get_binary_data_model_metric()
  


  
  output <- calculate_crude(
    data_function = functions$data_function,
    model_function = functions$model_function,
    metric_function = functions$metric_function,
    value_on_error = 0.5,
    min_sample_size = 50,
    max_sample_size = 200,
    test_n = 1000,
    n_reps_total =  100,
    n_reps_per = 10,
    target_performance = 0.75,
    mean_or_assurance = "mean") 
  
  expect_true(is.numeric(output$min_n))
  
  
  output <- calculate_crude(
    data_function = functions$data_function,
    model_function = functions$model_function,
    metric_function = functions$metric_function,
    value_on_error = 0.5,
    min_sample_size = 50,
    max_sample_size = 200,
    test_n = 1000,
    n_reps_total =  100,
    n_reps_per = 10,
    target_performance = 0.75,
    mean_or_assurance = "assurance") 
  
  expect_true(is.numeric(output$min_n))
  
  
})


test_that("calculate_ga", {
  
  functions <- get_binary_data_model_metric()
  

  output <- calculate_ga(
    data_function = functions$data_function,
    model_function = functions$model_function,
    metric_function = functions$metric_function,
    value_on_error = 0.5,
    min_sample_size = 50,
    max_sample_size = 200,
    test_n = 1000,
    n_reps_total =  100,
    n_reps_per = 10,
    target_performance = 0.75,
    mean_or_assurance = "mean",
    penalty_weight = 1,
    seed = 123) 
  
  expect_true(is.numeric(output$min_n))
  
  output <- calculate_ga(
    data_function = functions$data_function,
    model_function = functions$model_function,
    metric_function = functions$metric_function,
    value_on_error = 0.5,
    min_sample_size = 50,
    max_sample_size = 200,
    test_n = 1000,
    n_reps_total =  100,
    n_reps_per = 10,
    target_performance = 0.75,
    mean_or_assurance = "assurance",
    penalty_weight = 1,
    seed = 123) 
  
  expect_true(is.numeric(output$min_n))
  
})