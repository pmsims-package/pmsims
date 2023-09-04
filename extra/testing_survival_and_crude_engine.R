
#devtools::install_github("flxzimmer/mlpwr",ref="pmsims")

devtools::load_all(".")
library(mlpwr)
library(pmsims)

################################################
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
    signal_parameters = 50,   min_sample_size = 10,    max_sample_size= 1000,
    noise_parameters = 5,    predictor_type = "continuous",    predictor_prop = NULL,
    baseline_hazard = 0.01,    censoring_rate = 0.2,    metric = "cindex",
    large_sample_performance = 0.8,    minimum_threshold = 0.10,    se_final = 0.005,
    n_reps_total = NULL, tune_param = 0.211
  )
test_survival$min_n #153
test_survival$tune_param #0.211 - beta for 0.8 large performance
test_survival$summaries

# adding tune_param=0.211 from previous calculations 

test_survival_crude <-
  simulate_survival(
    signal_parameters = 50,    min_sample_size = 10,    max_sample_size= 1000,
    noise_parameters = 5,    predictor_type = "continuous",    predictor_prop = NULL,
    baseline_hazard = 0.01,    censoring_rate = 0.2,    metric = "cindex",
    large_sample_performance = 0.8,    minimum_threshold = 0.10,    se_final = 0.005,
    n_reps_total = NULL,tune_param = 0.211,
    method="crude"
  )

#test_survival_crude$data
test_survival_crude$min_n #175
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
test_bin$tune_param #0.1342822
pls(test_bin)
test_bin$summaries$median_performance
test_bin$summaries$quant20_performance
test_bin$simulation_time

test_bin_crude <- simulate_binary(
  signal_parameters = 100,  noise_parameters = 5, 
  min_sample_size = 300,  max_sample_size = 800,
  baseline_prob = 0.1,   predictor_type = "continuous",  predictor_prop = NULL,  
  metric = "auc",  model = "glm",  large_sample_performance = 0.8,
  minimum_threshold = 0.1,  se_final = 0.005,n_reps_total = NULL,
  method = "crude", tune_param = 0.136
)
test_bin_crude$data
test_bin_crude$summaries
test_bin_crude$min_n #675
test_bin_crude$simulation_time

test_bin_crude$tune_param #0.136
pls(test_bin_crude)

#################################################

test_bin_lasso <- simulate_binary( 
  signal_parameters = 100, noise_parameters = 10,
  min_sample_size = 100,max_sample_size = 1000, baseline_prob = 0.1,  
  predictor_type = "continuous",  predictor_prop = NULL,
  metric = "auc",  model = "glm",  large_sample_performance = 0.8,  
  minimum_threshold = 0.1,  se_final = 0.005,  n_reps_total = NULL
)
test_bin_lasso$min_n #715
test_bin_lasso$tune_param #0.1317444
test_bin_lasso$summaries$quant20_performance

qnorm(0.2) #-0.8416212

library("doParallel")
library("doFuture") #for parallel calculations
num.cores <- detectCores()-2
cluztrr <- makeCluster(num.cores)

#pre-trained params 
g<- expand.grid("auc" = c(0.7,0.8,0.9),"p" = c(0.05, 0.1,0.2))
g<- g[, c("p", "auc")]

get_tune <- function(x){
    temp<- simulate_binary(
      signal_parameters = 100, noise_parameters = 10,
      min_sample_size = 100,max_sample_size = 1000, baseline_prob = x["p"],
      predictor_type = "continuous",  predictor_prop = NULL,
      metric = "auc",  model = "glm",  large_sample_performance = as.numeric(x["auc"]),
      minimum_threshold = 0.1,  se_final = 0.005,  n_reps_total = NULL
    )
    return(temp$tune_param)
}

envir <- .GlobalEnv
parallel::clusterExport(cluztrr, varlist = ls(envir), envir = envir)
#z=apply(g, 1, FUN=get_tune) #0.0810118 0.1308143 0.2473303
z2=parallel::parApply(cluztrr,g, 1, FUN=get_tune) 
g$tune_param = z2
stopCluster(cluztrr)

#####################################################
################# start here ######################
g<- expand.grid("auc" = c(0.7,0.8,0.9),"p" = c(0.05, 0.1,0.2))
g<- g[, c("p", "auc")]
g$tune_param <- c(0.08,0.137,0.256, 0.08, 0.1325, 0.250, 0.08,0.131,0.0255)
rownames(g)= apply(g,1, FUN = function(x) paste(x[1], x[2], sep="_"))
# g
#      p auc tune_param
# 1 0.05 0.7 0.07965088
# 2 0.05 0.8 0.13711883
# 3 0.05 0.9 0.25562877
# 4 0.10 0.7 0.07945359
# 5 0.10 0.8 0.13252773
# 6 0.10 0.9 0.24909897
# 7 0.20 0.7 0.08053274
# 8 0.20 0.8 0.13075023
# 9 0.20 0.9 0.25465110

source("R/model_generators.R")
source("R/engines.R")
source("R/calculate_sample_size.R")
source("R/metric_generators.R")
source("R/decisions.R")
source("R/tune_generate_data.R")
source("R/surrogate_models.R")
source("R/data_generators.R")

gg <- expand.grid(
  trial = 1:50,
  baseline_prob = c(0.1),
  large_sample_performance = c(0.7, 0.8),
  n_reps_total = 1000,
  n_reps_per = 10,
  signal_parameters = 100,
  noise_parameters = 10,
  minimum_threshold = 0.05,
  min_sample_size = 1000,
  max_sample_size = 3000
)
gg$tune_param =
  g[paste(gg$baseline_prob, gg$large_sample_performance, sep = "_"), "tune_param"]
dim(gg) #[150 11]

gg2 <- expand.grid(
  trial = 1:50,
  baseline_prob = c(0.1),
  large_sample_performance = c(0.7, 0.8),
  n_reps_total = 500,
  n_reps_per = 50,#<- this has changes
  signal_parameters = 100,
  noise_parameters = 10,
  minimum_threshold = 0.05,
  min_sample_size = 100,
  max_sample_size = 3000
)
gg2$tune_param = 
  g[paste(gg2$baseline_prob, gg2$large_sample_performance, sep ="_"), "tune_param"]
dim(gg2) #[100 11]

get_minn <- function(params){
  param_list<- setNames(as.list(as.numeric(params)), names(params))
  temp<- do.call(simulate_binary, param_list)
}


#z2=parallel::parApply(cluztrr,g, 1, FUN=get_tune) 
num.cores <- detectCores()-2
cluztrr <- makeCluster(num.cores)
envir <- .GlobalEnv
parallel::clusterExport(cluztrr, varlist = ls(envir), envir = envir)

############ rep = 50 simulation to get the std of n_min
time_0<- Sys.time()
sim_results_0.1 = parallel::parApply(cluztrr,gg2, 1, FUN=get_minn) 
round(difftime(Sys.time(),time_0, "sec"),1)

length(sim_results) #6
ts<- round(unlist(lapply(sim_results_0.1, function(x) x$simulation_time)),3)
# 2740 2790 1616 1459  873  905
# 2369 2394 1676 1671  856  865
min_n_0.1 <- unlist(lapply(sim_results_0.1, function(x) x$min_n))
gg2$sim_time <- round(ts[seq(2,200,2)],1)
gg2$n_min <- min_n_0.1
results_reps_per_50 <- gg2
write_csv(results_reps_per_50, "extra/results_reps_per_50")
sd(gg2[gg2$large_sample_performance==0.7 , "n_min"])
sd(gg2[gg2$large_sample_performance==0.7 , "n_min"])

############ rep = 10 as before simulation to get the std of n_min
time_0<- Sys.time()
sim_results_0.1_as_before = parallel::parApply(cluztrr,gg, 1, FUN=get_minn) 
print(round(difftime(Sys.time(),time_0, "sec"),1))

ts0<- round(unlist(lapply(sim_results_0.1_as_before, function(x) x$simulation_time)),3)
print(ts0)
# 2740 2790 1616 1459  873  905
# 2369 2394 1676 1671  856  865
min_n_0.1_0 <- unlist(lapply(sim_results_0.1_as_before, function(x) x$min_n))
gg$sim_time <- round(ts0[seq(2,200,2)],1)
gg$n_min <- min_n_0.1_0
write_csv(gg, "extra/gg_as_before.csv")

sd(gg[gg$large_sample_performance==0.7 , "n_min"])
sd(gg[gg$large_sample_performance==0.7 , "n_min"])

plot(gg2[gg2$large_sample_performance==0.7 , "n_min"])
plot(gg2[gg2$large_sample_performance==0.8 , "n_min"])

stopCluster(cluztrr)


##################################################################

plan(multisession, workers = 6)
with_progress({
  p <- progressor(steps = nrow(gg))
  result <- future_pmap(opt, \(trial, ...) {simulate_binary(...)})
})


iter_n = 2
pb1 = utils::txtProgressBar(0, iter_n+2, style = 3, char = "|||",title = "pb1")
setTxtProgressBar(pb1, 1)

parallel_stats = foreach::foreach(
  iter = 1:iter_n, 
  .packages = c("mlpwr", "pmsims", "stats"),
  .errorhandling = "pass"
)%dopar%{
  
  setTxtProgressBar(pb1, iter+1)
  
  list("model" = model_i,
       "stat" = rbind("test" = method_any_validate(y_predict_test,valuation_times, df_train_cv, df_test_cv, weighted = 1), 
                      "train" = method_any_validate(y_predict_train, valuation_times, df_train_cv, df_train_cv, weighted = 1)))
}
setTxtProgressBar(pb1, iter_n+2)
close(pb1)

model_list = foreach::foreach(iter = 1:iter_n)%do%{parallel_stats[[iter]]$model}
modelstats = foreach::foreach(iter = 1:iter_n)%do%{parallel_stats[[iter]]$stat}


# as it is now 

# with 50 trials instead of 10 per sample size

# with 10 trials and mean and std, 20quantile = mean - 0.8416212*std 
# assuming it is normal

