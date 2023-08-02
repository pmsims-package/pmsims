
#devtools::install_github("flxzimmer/mlpwr",ref="pmsims")

devtools::load_all(".")
library(mlpwr)
library(pmsims)

# testing survival data generator
n=1000
beta = 0.5
d1 <-
  pmsims::generate_survival_data(
    n,
    beta,
    signal_parameters = 5,
    noise_parameters = 5,
    predictor_type = "continuous",
    predictor_prop = 0.2,
    baseline_hazard = 0.01,
    censoring_rate = 0.2
  )
head(d1) #ok

test_survival <-
  simulate_survival(
    signal_parameters = 50,
    min_sample_size = 500,
    max_sample_size= 2000,
    noise_parameters = 5,
    predictor_type = "continuous",
    predictor_prop = NULL,
    baseline_hazard = 0.01,
    censoring_rate = 0.2,
    metric = "cindex",
    large_sample_performance = 0.8,
    minimum_threshold = 0.10,
    se_final = 0.005,
    n_reps_total = NULL
  )

test_survival$min_n #1103
test_survival$tune_param #0.211
test_survival$data[[1]]$x
pls(test_survival)

test_survival_crude <-
  simulate_survival(
    signal_parameters = 50,
    min_sample_size = 300,
    max_sample_size= 1000,
    noise_parameters = 5,
    predictor_type = "continuous",
    predictor_prop = NULL,
    baseline_hazard = 0.01,
    censoring_rate = 0.2,
    metric = "cindex",
    large_sample_performance = 0.8,
    minimum_threshold = 0.10,
    se_final = 0.005,
    n_reps_total = NULL,
    method="crude"
  )

#test_survival_crude$data
test_survival_crude$min_n #854
test_survival_crude$summaries
test_survival_crude$tune_param #0.2139
pls(test_survival_crude)

pls <- function(x){
  plot(x= as.numeric(names(x$summaries$median_performance)), 
     y = x$summaries$median_performance, xlim = c(100,3000))
  lines(x= as.numeric(names(x$summaries$median_performance)), 
     y = x$summaries$quant20_performance, xlim = c(100,3000), col = "red")
}
pls(test_survival_crude)
#clipr::write_clip(test_survival_crude$tune_param) #0.209


#################################################

test_bin <- simulate_binary( 
  signal_parameters = 100, noise_parameters = 5,
  min_sample_size = 100,max_sample_size = 1000, baseline_prob = 0.1,  
  predictor_type = "continuous",  predictor_prop = NULL,
  metric = "auc",  model = "glm",  large_sample_performance = 0.8,  
  minimum_threshold = 0.1,  se_final = 0.005,  n_reps_total = NULL
)

test_bin$min_n #654
test_bin$tune_param #0.13635
pls(test_bin)
test_bin$summaries$median_performance
test_bin$summaries$quant20_performance

test_bin_crude <- simulate_binary(
  signal_parameters = 100,  min_sample_size = 200,  max_sample_size = 800,
  baseline_prob = 0.1,  noise_parameters = 5,  predictor_type = "continuous",
  predictor_prop = NULL,  
  metric = "auc",  model = "glm",  large_sample_performance = 0.8,
  minimum_threshold = 0.1,  se_final = 0.005,n_reps_total = NULL,
  method = "crude"
)

test_bin_crude$min_n #600
test_bin_crude$tune_param #0.13676
pls(test_bin_crude)
