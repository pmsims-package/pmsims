#' Tuning for a binary outcome model
#'
#' @param target_performance The desired model performance in a large sample
#' @param target_prevalence The desired model performance in a large sample
#' @param tolerance The tolerance in the large sample performance
#' @returns The optimal value for the tuning parameter
#' @export
#'
#' @examples


invlogit <- function(x) 1/(1+exp(-x))

logit <-function(x) log(x/(1-x))


binary_tuning <- function(target_prevalence, 
                          target_performance, 
                          min.opt = c(-10,0), 
                          max.opt = c(0.02,5), 
                          tolerance = 0.00001){
  
  
  pcfun <- function(x){
    
    #target_prevalence = 0.2; target_performance=0.7; min.opt = c(-7,0.5); max.opt = c(0,14)
    
    mean      <- x[1]
    variance  <- x[2]
    
    f1 = function(x) {
      stats::integrate(function(y) {stats::dnorm(x, mean = mean, sd = sqrt(variance)) * stats::dnorm(y, mean = mean, sd = sqrt(variance)) * (1  + exp(-x)) ^ (-1) * (1  + exp(y)) ^ (-1) },
                       -Inf, x)$value
    }
    
    num = stats::integrate(Vectorize(f1), -Inf, Inf)$value
    
    f2 = function(x) {
      stats::integrate(function(y) {stats::dnorm(x, mean = mean, sd = sqrt(variance)) * stats::dnorm(y, mean = mean, sd = sqrt(variance)) * (1  + exp(-x)) ^ (-1) * (1  + exp(y)) ^ (-1) },
                       -Inf, Inf)$value
    }
    
    denom <- stats::integrate(Vectorize(f2), -Inf, Inf)$value
    
    f3     <- function(x) stats::dnorm(x, mean=mean, sd = sqrt(variance)) * (1  + exp(-x)) ^ (-1)
    
    c      <- num/denom
    prev   <- stats::integrate(f3, - Inf, Inf, subdivisions = 1000L)$value
    
    abs( c - target_performance)^2 + abs(prev - target_prevalence )^2
  }
  
  if (target_performance>0.7) {
    sigma_c <- sqrt(2) * stats::qnorm(target_performance)
    mu      <- 0.5 * (2 * target_prevalence - 1) * (sigma_c^2) + log(target_prevalence / (1 - target_prevalence))
    out      <- stats::optim(par=c(mu,0.15), pcfun, c(min.opt, max.opt, tolerance = tolerance))$par} else
      
    {
      sigma_c <- sqrt(2) * stats::qnorm(target_performance)
      mu      <- 0.5 * (2 * target_prevalence - 1) * (sigma_c^2) + log(target_prevalence / (1 - target_prevalence))
      sigma   <- sqrt((sigma_c^2) * (1 + target_prevalence * (1 - target_prevalence) * (sigma_c^2)))
      out     <- c(mu, sigma^2)
    }
  
  N        <- 500000
  #Better 2000000 to check
  lp       <- stats::rnorm(N, mean = out[1], sd = sqrt(out[2]))
  p        <- (1 + exp(-lp)) ^ (-1)
  y        <- stats::rbinom(N, 1, prob = p)
  prev     <- mean(y)
  c        <- quickcstat(y, lp)
  c(out[1], out[2], prev, c)
}

# Check
round(binary_tuning(0.05, 0.75, tolerance=0.0001),4)

#round(binary_tuning(0.05, 0.7, tolerance=0.00001),4)

#round(binary_tuning(0.2, 0.7, tolerance=0.00001),4)


#–– REQUIREMENTS ––
# install.packages("statmod")
# quickcstat() must be on your path (as in your original script)


quickcstat <- function(y, pred){

  casepred=pred[y == 1]
  conpred=pred[y == 0]
  
  if (length(conpred)>length(casepred)){
    conpred=conpred[sample(length(conpred),length(casepred),replace=FALSE)]
    auc.true=sum(casepred>conpred)/length(casepred)} else 
    {
      casepred=casepred[sample(length(casepred),length(conpred),replace=FALSE)]
      auc.true=sum(casepred>conpred)/length(conpred)
    }
  
  return(auc.true)
}

