#!/usr/bin/env Rscript
root <- 'validation/stability-20260918'
files <- list.files(file.path(root,'results'),pattern='search.csv$',recursive=TRUE,full.names=TRUE)
stopifnot(length(files)>0)
search <- do.call(rbind,lapply(files,function(f){r<-read.csv(f,stringsAsFactors=FALSE);obj<-sub('search.csv$','search.rds',f);if(file.exists(obj)){x<-readRDS(obj);r$actual_gp_reps<-sum(vapply(x$data,function(z)length(z$y),integer(1)))};r}))
checks_files <- list.files(file.path(root,'results'),pattern='validation.csv$',recursive=TRUE,full.names=TRUE)
checks <- do.call(rbind,lapply(checks_files,read.csv,stringsAsFactors=FALSE))
write.csv(search,file.path(root,'search-summary.csv'),row.names=FALSE)
write.csv(checks,file.path(root,'validation-summary.csv'),row.names=FALSE)
scenarios <- jsonlite::fromJSON(file.path(root,'scenarios.json'),simplifyVector=FALSE)
pairs <- list()
for(role in c('cache','shared'))for(sl in unique(search$slice)) {
  z <- search[search$role==role & search$slice==sl,];z<-z[order(z$p),]
  if(nrow(z)<2L)next
  for(i in seq_len(nrow(z)-1L)) {
    a<-z[i,];b<-z[i+1L,]
    old<-max(a$cached_n,b$cached_n)/min(a$cached_n,b$cached_n)
    new<-if(all(c(a$status,b$status)=='estimated'))max(a$n,b$n)/min(a$n,b$n)else NA_real_
    pairs[[length(pairs)+1L]]<-data.frame(slice=sl,role=role,p_from=a$p,p_to=b$p,cached_from=a$cached_n,cached_to=b$cached_n,cached_ratio=old,new_from=a$n,new_to=b$n,new_ratio=new,historical_jump=old>=3,new_jump=if(is.finite(new))new>=3 else NA,smoothed=if(is.finite(new))old>=3 & new<3 else NA)
  }
}
pairs <- do.call(rbind,pairs);write.csv(pairs,file.path(root,'adjacent-comparisons.csv'),row.names=FALSE)
mdtable <- function(d){if(nrow(d)==0L)return('(none)');d[]<-lapply(d,function(x){if(is.numeric(x))ifelse(is.na(x),'—',format(round(x,3),trim=TRUE,scientific=FALSE))else ifelse(is.na(x),'—',as.character(x))});paste(c(paste0('| ',paste(names(d),collapse=' | '),' |'),paste0('| ',paste(rep('---',ncol(d)),collapse=' | '),' |'),apply(d,1,function(x)paste0('| ',paste(x,collapse=' | '),' |'))),collapse='\n')}
reported <- subset(checks,label=='reported_n')
half <- subset(checks,label=='half_n');upper<-subset(checks,label=='upper_bound')
smallabove <- subset(half,evidence=='above')
unsupported <- subset(search,status=='estimated' & (!is.finite(upper_perf) | stage1_stop!='target_bracketed'))
# Stop labels alone are not evidence: inspect classifications at the returned bound.
search$upper_observed_above <- vapply(seq_len(nrow(search)),function(i){f<-file.path(root,'results',paste0(search$id[i],'-',search$role[i],'-seed',search$seed[i]),'stage1.csv');if(!file.exists(f))return(FALSE);z<-read.csv(f);any(z$n==search$upper[i]&z$call=='above')},logical(1))
write.csv(search,file.path(root,'search-summary.csv'),row.names=FALSE)
count <- function(x)sum(x,na.rm=TRUE)
ratio_table<-pairs[,c('slice','role','p_from','p_to','cached_ratio','new_from','new_to','new_ratio','smoothed')]
result_table<-merge(search[,c('id','p','role','seed','cached_n','target','lower','upper','n','stage1_stop','upper_observed_above')],reported[,c('id','role','seed','slope','slope_ci_low','slope_ci_high','evidence')],by=c('id','role','seed'),all.x=TRUE,sort=FALSE)
write.csv(result_table,file.path(root,'results-with-evidence.csv'),row.names=FALSE)
lines<-c('# Ridwan dev stability validation','',paste('Tested package: **d00a137640d47a9f1a75482ead8109b1ae1e463d**. Report generated',format(Sys.time(),tz='UTC'), 'UTC.'),'',sprintf('The study completed %d/%d planned cases; %d returned numeric sample sizes and %d returned errors or administrative limits. Four problematic predictor-count slices were tested with original cache seeds and a second shared seed. Each successful search requested 1,000 GP reps; every independent check used 1,000 fresh training/test/model draws.',sum(file.exists(file.path(dirname(files),'DONE'))),28,count(search$status=='estimated'),count(search$status!='estimated')),'',sprintf('Of %d historical >=3x adjacent-step comparisons across the two seed roles, %d now fall below 3x, %d remain >=3x, and %d are unavailable. These counts repeat each historical pair once per new seed role; the study contains nine unique historical adjacent pairs.',count(pairs$historical_jump),count(pairs$smoothed),count(pairs$historical_jump & pairs$new_jump),count(pairs$historical_jump & is.na(pairs$new_ratio))),'',sprintf('Independent checks at the returned N: **%d above target, %d below target, %d overlap target** (pointwise 95%% intervals). Half the reported N was already above target in **%d** cases. The adaptive upper bound checked above target in %d cases, below in %d, and overlapped in %d. These checks distinguish a smoother curve from an independently supported sample size.',count(reported$evidence=='above'),count(reported$evidence=='below'),count(reported$evidence=='overlaps'),nrow(smallabove),count(upper$evidence=='above'),count(upper$evidence=='below'),count(upper$evidence=='overlaps')),'','## Adjacent predictor-count steps','',mdtable(ratio_table),'','![Historical and new estimates across predictor counts](neighbour-estimates.png)','','## Returned sample sizes and independent evidence','',mdtable(result_table),'','![Independent target checks](independent-checks.png)','','## Search errors and bound handoff','',mdtable(search[search$status!='estimated',c('id','role','seed','status')]),'',sprintf('%d estimated cases had no observed above-target classification at their returned upper bound. The detailed stage-1 CSV traces preserve classifications, standard errors, replication counts, failures and stop reasons. An untested fallback bound is not supported merely because it is returned.',count(search$status=='estimated' & !search$upper_observed_above)),'','## Interpretation and limits','','The cache is historical, not a new matched pre-fix run; changes include simulation noise and possible cache-build environment differences. This purposive study covers 14 scenarios and cannot establish a failure rate for all 13,680 cached scenarios or justify clearing all 900 exclusions. Two seeds give a local sensitivity check, not a comprehensive stability distribution.','','Ridge/lasso assurance is assessed on the original internal CSSE scale. Displayed equivalent slope is 1-sqrt(-q20(CSSE)); GLM uses raw slope q20. Confidence intervals are binomial order-statistic intervals for individual performance quantiles, not intervals for the minimum N. Overlap is inconclusive. Below is a target shortfall; above at half N establishes an independently tested smaller alternative. Answers have not been corrected.','',sprintf('Independent checks recorded %d fit errors and %d nonfinite metric values. Search model observers recorded %d fitting errors; stage-1 n_fail itself only counts nonfinite values.',sum(checks$fit_errors),sum(checks$reps-checks$finite),sum(search$search_fit_errors)),'','The review also reproduced a below-target plateau handing untested bounds to stage 2, an absent RNG stream not being restored, and two existing bisection/hybrid test errors from adaptive_seed forwarding. See [FIXES-AND-PROVENANCE.md](FIXES-AND-PROVENANCE.md), [existing-tests.txt](existing-tests.txt), and [review-diagnostics.csv](review-diagnostics.csv). Those findings are separate from the simulation outcomes.','','## Reproduction and consolidated history','','See [PROTOCOL.md](PROTOCOL.md), [scenarios.json](scenarios.json), [jobs.json](jobs.json), [search-summary.csv](search-summary.csv), [validation-summary.csv](validation-summary.csv), and [adjacent-comparisons.csv](adjacent-comparisons.csv). Run python3 validation/stability-20260918/run-study.py from the repository root, then analyse.R and verify.R. Raw RDS draws remain local; all report summaries and session information are preserved. Historical artifacts are indexed in [history/manifest.json](history/manifest.json).')
writeLines(lines,file.path(root,'REPORT.md'))
library(ggplot2)
plotdata<-subset(search,slice!='binary_glm_seed_failure' & status=='estimated')
old<-plotdata[!duplicated(plotdata$id),];old$n<-old$cached_n;old$role<-'historical cache'
pdat<-rbind(plotdata,old)
p<-ggplot(pdat,aes(p,n,colour=role,group=role))+geom_line()+geom_point(size=2)+facet_wrap(~slice,scales='free_y',ncol=2)+scale_y_log10(labels=scales::comma)+scale_x_continuous(breaks=c(5,10,15,20))+labs(x='Signal predictors',y='Reported sample size (log scale)',colour=NULL,title="Adjacent predictor-count estimates",subtitle='Historical cache versus Ridwan dev; smoothness does not establish target attainment')+theme_minimal(base_size=12)+theme(legend.position='bottom')
ggsave(file.path(root,'neighbour-estimates.png'),p,width=11,height=7,dpi=150)
qdat<-merge(checks,search[,c('id','seed','role','slice','target')],by=c('id','seed','role'))
qdat$case<-paste0(qdat$id,' / ',qdat$role)
qdat$delta<-qdat$slope-qdat$target;qdat$delta_low<-qdat$slope_ci_low-qdat$target;qdat$delta_high<-qdat$slope_ci_high-qdat$target
p<-ggplot(qdat,aes(delta,case,colour=label))+geom_vline(xintercept=0,linetype=2)+geom_errorbar(aes(xmin=delta_low,xmax=delta_high),orientation='y',width=.2,position=position_dodge(width=.65))+geom_point(position=position_dodge(width=.65),size=1.6)+labs(x='Independent equivalent slope minus target (pointwise 95% interval)',y=NULL,colour=NULL,title='Independent performance checks',subtitle='Fresh training and test draws; 1,000 replicates per point')+theme_minimal(base_size=10)+theme(legend.position='bottom')
ggsave(file.path(root,'independent-checks.png'),p,width=12,height=11,dpi=150)
print(table(reported$evidence));print(pairs)
