
library(mlpwr)
library(pmsims)
library(parallel)

g<- expand.grid("auc" = c(0.7,0.8,0.9),"p" = c(0.05, 0.1,0.2))
g$tune_param <- c(0.08,0.137,0.256, 0.08, 0.1325, 0.250, 0.08,0.131,0.0255)
rownames(g)= apply(g,1, FUN = function(x) paste(x[1], x[2], sep="_"))
g<- g[, c("p", "auc","tune_param")]
# g
# p auc tune_param
# 0.05_0.7 0.05 0.7     0.0800
# 0.05_0.8 0.05 0.8     0.1370
# 0.05_0.9 0.05 0.9     0.2560
# 0.1_0.7  0.10 0.7     0.0800
# 0.1_0.8  0.10 0.8     0.1325
# 0.1_0.9  0.10 0.9     0.2500
# 0.2_0.7  0.20 0.7     0.0800
# 0.2_0.8  0.20 0.8     0.1310
# 0.2_0.9  0.20 0.9     0.0255

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
  n_reps_total = 500,
  n_reps_per = 10,
  signal_parameters = 100,
  noise_parameters = 10,
  minimum_threshold = 0.05,
  min_sample_size = 1000,
  max_sample_size = 3000
)
gg$tune_param =
  g[paste(gg$baseline_prob, gg$large_sample_performance, sep = "_"), "tune_param"]
dim(gg) #[100 11]

gg2 <- expand.grid(
  trial = 1:50,
  baseline_prob = c(0.1),
  large_sample_performance = c(0.7, 0.8),
  n_reps_total = 500,#<- this has changed vs gg
  n_reps_per = 50,#<- this has changed vs gg
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

# set up clusters
#z2=parallel::parApply(cluztrr,g, 1, FUN=get_tune) 
num.cores <- detectCores()-2
cluztrr <- makeCluster(num.cores)
envir <- .GlobalEnv
parallel::clusterExport(cluztrr, varlist = ls(envir), envir = envir)

############ rep = 50 simulation to get the std of n_min
time_0<- Sys.time()
sim_results_0.1 = parallel::parApply(cluztrr,gg2, 1, FUN=get_minn) 
round(difftime(Sys.time(),time_0, "sec"),1)

ts<- round(unlist(lapply(sim_results_0.1, function(x) x$simulation_time)),3)
min_n_0.1 <- unlist(lapply(sim_results_0.1, function(x) x$min_n))
gg2$sim_time <- round(ts[seq(2,200,2)],1)
gg2$n_min <- min_n_0.1
write_csv(gg2, "extra/gg2.csv")

sd(gg2[gg2$large_sample_performance==0.7 , "n_min"]) #77.60
sd(gg2[gg2$large_sample_performance==0.8 , "n_min"]) #113.60

# min_n_0.1
# [1] 2400 2407 2302 2388 2374 2266 2592 2410 2350 2262 2458 2428 2329
# [14] 2430 2424 2466 2305 2336 2345 2359 2380 2430 2389 2439 2407 2352
# [27] 2395 2400 2442 2349 2365 2265 2289 2404 2428 2419 2488 2358 2339
# [40] 2414 2361 2313 2295 2220 2314 2670 2387 2392 2366 2420 1675 1598
# [53] 1633 1681 1670 1670 1675 1639 1693 1708 1685 2185 1702 1662 1690
# [66] 1826 1730 1872 1876 1658 1645 1702 1708 1616 1653 2167 1646 1658
# [79] 1603 1641 1684 1656 1648 1742 1662 1694 1752 1658 1637 1600 1621
# [92] 1683 1741 1716 1634 1630 1697 1661 1682 1699
# > gg2$sim_time
# [1] 335.3 331.6 328.0 329.1 326.9 324.5 326.2 330.9 334.4 329.5 320.7
# [12] 317.9 320.9 320.1 322.8 311.0 263.7 334.8 333.6 330.2 328.3 328.0
# [23] 321.6 323.9 326.9 337.2 334.5 328.9 328.5 325.0 327.3 324.7 305.3
# [34] 336.7 335.1 329.0 327.3 326.2 329.6 332.2 322.2 332.5 331.3 319.6
# [45] 323.5 325.6 327.4 326.2 311.0 261.2 328.9 319.0 313.9 316.5 316.8
# [56] 312.9 312.5 313.5 317.0 322.2 312.1 310.4 308.5 310.1 308.4 309.8
# [67] 274.9 325.6 321.1 317.7 313.5 315.4 316.1 308.7 315.4 312.7 317.4
# [78] 311.1 309.7 307.7 316.8 309.5 311.3 322.0 324.9 316.0 315.9 309.0
# [89] 308.9 306.9 312.6 315.5 320.6 312.9 311.1 310.1 309.1 310.3 313.8
# [100] 281.1

############ rep = 10 as before simulation to get the std of n_min

time_0<- Sys.time()
sim_results_0.1_as_before = parallel::parApply(cluztrr,gg, 1, FUN=get_minn) 
print(round(difftime(Sys.time(),time_0, "sec"),1)) # 4 hours 
stopCluster(cluztrr)


ts0<- round(unlist(lapply(sim_results_0.1_as_before, function(x) x$simulation_time)),3)
#simulation time (1000/10)
summary(ts0[seq(2,200,2)]) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 575.5   684.6   719.6   720.6   761.3   826.2 

#tuning time (auc 0.7-0.8, binary, 100+5 continuous params)
summary(ts0[seq(1,200,2)]) 
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 39.70   62.21   91.22  134.73  160.60  634.69

min_n_0.1_0 <- unlist(lapply(sim_results_0.1_as_before, function(x) x$min_n))
tune_param_0 <- unlist(lapply(sim_results_0.1_as_before, function(x) x$tune_param))
tune_param_50 <- unlist(lapply(sim_results_0.1, function(x) x$tune_param))
gg$sim_time <- round(ts0[seq(2,200,2)],1)
gg$n_min <- min_n_0.1_0
write_csv(gg, "extra/gg_as_before.csv")

sd(gg[gg$large_sample_performance==0.7 , "n_min"]) #251.06
sd(gg[gg$large_sample_performance==0.8 , "n_min"]) #113.38


plot(gg2[gg2$large_sample_performance==0.7 , "n_min"])
plot(gg2[gg2$large_sample_performance==0.8 , "n_min"])

# min_n_0.1_0
# [1] 2650 2234 2403 2333 2568 2353 2287 2739 2295 2324 2508 2252 2713
# [14] 2326 2451 2437 2739 2682 2006 2347 2549 2463 2611 2403 1933 2099
# [27] 2570 2995 2789 2799 1923 2398 2157 2592 2806 2539 2590 2676 2334
# [40] 2490 2092 2459 2895 3000 2345 2265 2673 2250 2500 2409 1520 1491
# [53] 1468 1744 1547 1556 1649 1393 1450 1557 1611 1713 1496 1487 1683
# [66] 1551 1646 1568 1690 1813 1552 1601 1496 1517 1530 1510 1591 1587
# [79] 1728 1380 1567 1919 1654 1486 1488 1409 1488 1535 1732 1431 1730
# [92] 1527 1486 1722 1480 1740 1560 1500 1548 1520
# > gg$sim_time
# [1] 784.1 730.1 794.6 734.2 763.1 788.1 743.0 776.1 730.8 770.3 774.3
# [12] 745.9 823.7 735.9 664.7 601.2 576.4 788.1 700.5 784.3 745.0 747.8
# [23] 818.9 779.7 718.6 740.5 768.6 826.2 788.2 785.0 747.1 740.6 614.4
# [34] 791.4 782.5 810.5 752.7 760.8 767.8 761.7 730.0 758.7 793.7 810.7
# [45] 761.5 772.4 759.4 653.8 596.7 575.5 720.6 661.1 698.3 690.9 674.4
# [56] 707.9 683.6 651.0 663.0 671.9 736.9 710.3 669.1 724.1 699.7 670.0
# [67] 593.0 717.4 731.8 761.2 703.8 683.5 675.6 707.3 709.1 672.1 711.8
# [78] 687.0 742.4 684.9 685.1 749.3 699.7 695.7 711.6 723.4 697.3 678.8
# [89] 696.0 682.3 696.8 673.4 676.3 698.7 716.3 734.4 686.0 712.3 683.9
# [100] 677.8


rbind(gg[, "large_sample_performance","n_reps_per", "sim_time", "n_min"])
res70 = data.frame(cbind("reps_per_10" = gg$n_min[1:50], "reps_per_50"= gg2$n_min[1:50]))
res80 = data.frame(cbind("reps_per_10" = gg$n_min[50:100], "reps_per_50"= gg2$n_min[50:100]))

library(ggplot2)

ggplot(res70, aes(x=reps_per_50, y=reps_per_10)) + 
  geom_point(size=2) + 
  xlim(c(1800,3000)) + ylim(c(1800,3000))+
  labs(title = "n_min for AUC 0.70 (100 params,5 noise, 0.1 prevalence),50 trials")

h1 = hist(res70$reps_per_10,xlim = c(1800,3000), breaks = 10)
h2 = hist(res70$reps_per_50,xlim = c(1800,3000), breaks=10)
plot(h2, col = rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue"), xlim=c(1800,3000), 
     main = "n_min histogram, AUC 0.70, reps 10 (pink) and 50 (blue)")
plot(h1, col = rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink"), add=TRUE)

ggplot(res80, aes(x=reps_per_50, y=reps_per_10)) + 
  geom_point(size=2) + 
  xlim(c(1300,2000)) + ylim(c(1300,2000))+
  labs(title = "n_min for AUC 0.70 (100 params,5 noise, 0.1 prevalence),50 trials")

h1 = hist(res80$reps_per_10,xlim = c(1300,2000), breaks = 10)
h2 = hist(res80$reps_per_50,xlim = c(1300,2000), breaks=10)
plot(h2, col = rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue"), xlim=c(1300,2000), 
     main = "n_min histogram, AUC 0.80, reps 10 (pink) and 50 (blue)")
plot(h1, col = rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink"), add=TRUE)

