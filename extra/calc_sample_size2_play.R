set.seed(123)
sample_size <-  pmsims::calculate_sample_size2(
  data = list(type = "binary"),
  large_sample_performance = 0.8,
  target_performance = 0.78,
  min_sample_size = 100,
  max_sample_size = 3000,
  n_reps = 100,
  test_n = 10000
)
print(sample_size$min_n)

sample_size <-  pmsims::calculate_sample_size2(
  data = list(type = "binary"),
  tune_param = 0.7,
  target_performance = 0.7,
  min_sample_size = 100,
  max_sample_size = 3000,
  n_reps = 100,
  test_n = 10000
)
print(sample_size$min_n)


set.seed(123)
sample_size2 <-  pmsims::calculate_sample_size2(data = list(type = "binary"),
                                               large_sample_performance = 0.9,
                                               target_performance = 0.88,
                                               min_sample_size = 50,
                                               max_sample_size = 3000,
                                               n_reps = 100,
                                               test_n = 10000)
print(sample_size2$min_n)

set.seed(123)
sample_size3 <-  pmsims::calculate_sample_size2(data = list(type = "binary"),
                                                target_performance = 0.8,
                                                min_sample_size = 50,
                                                max_sample_size = 3000,
                                                n_reps = 100,
                                                test_n = 10000)
print(sample_size3$min_n)



data_options <- list(type = "binary",
                     args = list(n_params = 30))
data_generating_function <- default_data_generators(data_options)
m <- default_model_generators(type = "binary")

tune_generate_data