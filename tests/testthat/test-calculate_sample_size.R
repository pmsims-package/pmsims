  test_that("parse_inputs", {
    #Binary
    inputs <- parse_inputs(data_spec = list(type = "binary",
                                              args = list(
                                                signal_parameters = 5, 
                                                noise_parameters = 5, 
                                                predictor_type = "continuous", 
                                                beta_signal = 0.1,
                                                baseline_prob = 0.1
                                              )
                ),
    metric="auc")
    expect_equal(length(inputs), 3)
    
    # Continuous

    inputs <- parse_inputs(data_spec = list(type = "continuous",
                                            args = list(
                                              signal_parameters = 5, 
                                              noise_parameters = 5, 
                                              predictor_type = "continuous", 
                                              beta_signal = 0.1
                                            )
    ),
    metric="r2")
    expect_equal(length(inputs), 3)
    
    #Survival
    inputs <- parse_inputs(data_spec = list(type = "survival",
                                            args = list(
                                              signal_parameters = 5, 
                                              noise_parameters = 5, 
                                              predictor_type = "continuous", 
                                              beta_signal = 0.1
                                            )
    ),
    metric="auc")
    expect_equal(length(inputs), 3)
    
    
  })

test_that("simulate_binary", {
  set.seed(1234)
  
  output <- simulate_binary(
      signal_parameters = 5, 
      noise_parameters = 5, 
      predictor_type = "continuous", 
      baseline_prob = 0.1,
      min_sample_size = 100,
      max_sample_size = 3000,
      n_reps = 50,
      minimum_threshold = 0.05
  )
  expect_equal(length(output), 8)
})


test_that("simulate_continuous", {
  set.seed(1234)
  output <- simulate_continuous(
    signal_parameters = 10, 
    noise_parameters = 10, 
    predictor_type = "continuous", 
    min_sample_size = 100,
    max_sample_size = 3000,
    n_reps = 10,
    minimum_threshold = 0.05
  )
  expect_equal(length(output), 8)
})


test_that("simulate_survival", {
  set.seed(1234)
  
  output <- simulate_survival(
    signal_parameters = 10, 
    noise_parameters = 10, 
    predictor_type = "continuous", 
    min_sample_size = 100,
    max_sample_size = 3000,
    n_reps = 10,
    minimum_threshold = 0.05,
    large_sample_performance = 0.7
  )
  expect_equal(length(output), 8)
})
