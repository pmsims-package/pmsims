
#devtools::install_github("flxzimmer/mlpwr",ref="pmsims")

devtools::load_all(".")
library(mlpwr)
library(pmsims)

# testing survival data generator
n=1000
beta = 0.5
d1 <-
  pmsims::generate_survival_data(
    n,  beta, signal_parameters = 5,  noise_parameters = 5,
    predictor_type = "continuous",  predictor_prop = 0.2,
    baseline_hazard = 0.01,censoring_rate = 0.2
  )
head(d1) #ok
paste("Surv(d1$time, d1$event)", cat(names(d1)[-c(1:2)], sep="+"), sep="~")
m <- survival::coxph(Surv(d1$time, d1$event)~ ., data = d1[,-c(1:3)])
summary(m) #ok, the first 5 params are estimated at 0.5 as modeled with beta = 0.5

test_survival <-
  simulate_survival(
    signal_parameters = 50,   min_sample_size = 500,    max_sample_size= 2000,
    noise_parameters = 5,    predictor_type = "continuous",    predictor_prop = NULL,
    baseline_hazard = 0.01,    censoring_rate = 0.2,    metric = "cindex",
    large_sample_performance = 0.8,    minimum_threshold = 0.10,    se_final = 0.005,
    n_reps_total = NULL
  )
test_survival$min_n #1103
test_survival$tune_param #0.211 - beta for 0.8 large performance

a1 <-
  simulate_survival(
    signal_parameters = 50,   min_sample_size = 500,    max_sample_size= 2000,
    noise_parameters = 5,    predictor_type = "continuous",    predictor_prop = NULL,
    baseline_hazard = 0.01,    censoring_rate = 0.2,    metric = "cindex",
    large_sample_performance = 0.8,    minimum_threshold = 0.10,    se_final = 0.005,
    n_reps_total = NULL, tune_param = 0.211
  )

test_survival_crude <-
  simulate_survival(
    signal_parameters = 50,    min_sample_size = 300,    max_sample_size= 1000,
    noise_parameters = 5,    predictor_type = "continuous",    predictor_prop = NULL,
    baseline_hazard = 0.01,    censoring_rate = 0.2,    metric = "cindex",
    large_sample_performance = 0.8,    minimum_threshold = 0.10,    se_final = 0.005,
    n_reps_total = NULL,
    method="crude"
  )

#test_survival_crude$data
test_survival_crude$min_n #854
test_survival_crude$summaries
test_survival_crude$tune_param #0.2139

# ok - similar min_n 

pls <- function(x){
  plot(x= as.numeric(names(x$summaries$median_performance)), 
     y = x$summaries$median_performance, xlim = c(100,1000))
  lines(x= as.numeric(names(x$summaries$median_performance)), 
     y = x$summaries$quant20_performance, xlim = c(100,10000), col = "red")
}

pls(test_survival_crude)
pls(test_survival)

#################################################

test_survival_auc <-
  simulate_survival(
    signal_parameters = 50,   min_sample_size = 500,    max_sample_size= 2000,
    noise_parameters = 5,    predictor_type = "continuous",    predictor_prop = NULL,
    baseline_hazard = 0.01,    censoring_rate = 0.2,    metric = "auc",
    large_sample_performance = 0.8,    minimum_threshold = 0.10,    se_final = 0.005,
    n_reps_total = NULL
  )
test_survival_auc$min_n #1494
test_survival_auc$tune_param #0.13788

test_survival_crude_auc <-
  simulate_survival(
    signal_parameters = 50,    min_sample_size = 750,    max_sample_size= 2000,
    noise_parameters = 5,    predictor_type = "continuous",    predictor_prop = NULL,
    baseline_hazard = 0.01,    censoring_rate = 0.2,    metric = "auc",
    large_sample_performance = 0.8,    minimum_threshold = 0.10,    se_final = 0.005,
    n_reps_total = NULL,  method="crude"
  )

#test_survival_crude$data
test_survival_crude_auc$min_n #1271
test_survival_crude_auc$summaries
test_survival_crude_auc$tune_param #0.13459
pls(test_survival_crude_auc,750, 2000)

##################################################

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
  signal_parameters = 100,  noise_parameters = 5, 
  min_sample_size = 200,  max_sample_size = 800,
  baseline_prob = 0.1,   predictor_type = "continuous",  predictor_prop = NULL,  
  metric = "auc",  model = "glm",  large_sample_performance = 0.8,
  minimum_threshold = 0.1,  se_final = 0.005,n_reps_total = NULL,
  method = "crude"
)
test_bin_crude$data
test_bin_crude$min_n #635
test_bin_crude$tune_param #0.13676
pls(test_bin_crude)

#################################################


test_bin_lasso <- simulate_binary( 
  signal_parameters = 100, noise_parameters = 5,
  min_sample_size = 100,max_sample_size = 1000, baseline_prob = 0.1,  
  predictor_type = "continuous",  predictor_prop = NULL,
  metric = "auc",  model = "glm",  large_sample_performance = 0.8,  
  minimum_threshold = 0.1,  se_final = 0.005,  n_reps_total = NULL
)





