  test_that("parse_inputs", {
    signal_parameters <- 5
    noise_parameters <- 5
    #Binary
    inputs <- parse_inputs(data_spec = list(type = "binary",
                                              args = list(
                                                signal_parameters = signal_parameters, 
                                                noise_parameters = noise_parameters, 
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
                                              signal_parameters = signal_parameters, 
                                              noise_parameters = noise_parameters, 
                                              predictor_type = "continuous", 
                                              beta_signal = 0.1
                                            )
    ),
    metric="r2")
    expect_equal(length(inputs), 3)
    
    #Survival
    inputs <- parse_inputs(data_spec = list(type = "survival",
                                            args = list(
                                              signal_parameters = signal_parameters, 
                                              noise_parameters = noise_parameters, 
                                              predictor_type = "continuous", 
                                              beta_signal = 0.1
                                            )
    ),
    metric="auc")
    expect_equal(length(inputs), 3)
    
    
  })

test_that("simulate_binary", {
  signal_parameters <- 5
  noise_parameters <- 5
  output <- simulate_binary(
      signal_parameters = signal_parameters, 
      noise_parameters = noise_parameters, 
      predictor_type = "continuous", 
      baseline_prob = 0.1,
      min_sample_size = 100,
      max_sample_size = 3000,
      n_reps = 10
  )
  expect_equal(length(output), 6)
})


test_that("simulate_continuous", {
  signal_parameters <- 5
  noise_parameters <- 5
  output <- simulate_continuous(
    signal_parameters = signal_parameters, 
    noise_parameters = noise_parameters, 
    predictor_type = "continuous", 
    min_sample_size = 100,
    max_sample_size = 3000,
    n_reps = 10
  )
  expect_equal(length(output), 6)
})


test_that("simulate_survival", {
  signal_parameters <- 5
  noise_parameters <- 5
  output <- simulate_continuous(
    signal_parameters = signal_parameters, 
    noise_parameters = noise_parameters, 
    predictor_type = "continuous", 
    min_sample_size = 100,
    max_sample_size = 3000,
    n_reps = 10
  )
  expect_equal(length(output), 6)
})
