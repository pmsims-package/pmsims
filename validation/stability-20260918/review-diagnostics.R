#!/usr/bin/env Rscript
pkgload::load_all('.',quiet=TRUE)
root <- 'validation/stability-20260918'
adaptive <- get('calculate_adaptive_bounds',asNamespace('pmsims'))
# A deterministic below-target curve tests the contract, without Monte Carlo ambiguity.
flat <- adaptive(data_function=function(n)data.frame(y=rep(0,as.integer(n))),model_function=structure(function(d)list(n=nrow(d)),model='glm'),metric_function=function(test,fit,model).8,value_on_error=0,start_n=10,test_n=5000,n_reps_per=20,n_reps_total=500,target_performance=.95,threshold=.0001,mean_or_assurance='assurance',seed=20240101L)
write.csv(data.frame(diagnostic='constant_below_target',stop_reason=flat$stop_reason,lower=flat$min_sample_size,upper=flat$max_sample_size,upper_perf=flat$max_sample_size_perf,observed_above=any(vapply(flat$track,function(x)x$call=='above',logical(1))),reps_used=flat$reps_used),file.path(root,'review-diagnostics.csv'),row.names=FALSE)
saveRDS(flat,file.path(root,'review-diagnostics.rds'))
# Compare caller-stream preservation including the no-existing-seed case.
set.seed(812); before <- .Random.seed
invisible(adaptive(data_function=function(n)data.frame(y=seq_len(n)),model_function=structure(function(d)list(n=nrow(d)),model='glm'),metric_function=function(test,fit,model)fit$n/100,value_on_error=0,start_n=20,test_n=5000,n_reps_per=20,n_reps_total=100,target_performance=.6,threshold=0,mean_or_assurance='mean',seed=20240101L))
restored <- identical(before,.Random.seed)
rm('.Random.seed',envir=globalenv())
invisible(adaptive(data_function=function(n)data.frame(y=seq_len(n)),model_function=structure(function(d)list(n=nrow(d)),model='glm'),metric_function=function(test,fit,model)fit$n/100,value_on_error=0,start_n=20,test_n=5000,n_reps_per=20,n_reps_total=100,target_performance=.6,threshold=0,mean_or_assurance='mean',seed=20240101L))
write.csv(data.frame(existing_stream_restored=restored,absent_stream_restored=!exists('.Random.seed',envir=globalenv(),inherits=FALSE)),file.path(root,'rng-diagnostics.csv'),row.names=FALSE)
