set_test_n <- function(max_sample_size) {
  # This function...
  return(max(30000, 3 * max_sample_size))
}

set_tolerance <- function(large_sample_performance) {
  # This function...
  return(large_sample_performance * 0.005)
}
