set_test_n <- function(max_sample_size) {
  # This sets the...
  return(max(30000, 3 * max_sample_size))
}

set_tolerance <- function(large_sample_performance) {
  return(large_sample_performance * 0.005)
}
