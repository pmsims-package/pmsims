# Generate survival data
# Similar to cross-sectional data with binary outcome,
# it must be a fucntion which takes n as its first argument and a tuning
# parameter as its second argument - this is beta_signal in the example


generate_survival_data <- function(n, beta_signal) {
  percentcensored <- 0.5 # we stop observing after (1-percentcensored)% of population had an event
  observe_time <- 10 # observation time will be normalised to this number
  drop_out <- 0.3 # expected drop out rate
  lambda <- 0.1 # weibull distribution scale parameter
  shape_rho <- 1.0 # weibull distribution shape parameter
  p_signal <- 5 # number of real predictors
  p_noise <- 20 # number of noise predictors
  prob_p <- 0.1 # probability of a predictor to be 1
  base_prev <- 0.3 # baseline probability of a positive outcome if all risks are 0

  alldata <- rbinom(n * (p_signal + p_noise), 1, prob_p)
  X <- matrix(alldata, nrow = n, ncol = p_signal + p_noise)
  W_ <- c(rep(beta_signal, p_signal), rep(0, p_noise))
  b0 <- log(base_prev / (1 - base_prev))
  lp <- X %*% W_ + b0

  # Generating Weibull survival times
  exp_beta <- exp(lp)
  v <- runif(n = n)
  generated_times <- (-log(v) / (lambda * exp_beta))^(1 / shape_rho) # Weibull density

  # re-scale the time to have 1-percentcensored of events=1
  # by the observe_time
  final_time <- quantile(generated_times, 1 - percentcensored)

  # scale to observe_time
  event_time <- 0.001 + pmin(
    round(generated_times / final_time * observe_time, 3),
    observe_time
  )

  # generate drop-out times for random drop_out % observations
  if (drop_out > 0) {
    cens_obs <- sample.int(n, round(n * drop_out, 0)) # which subjects will drop out
    randcentime <- runif(round(n * drop_out, 0), 0, observe_time) # and at which time (uniformly distr between 0 and observe_time)
    censored_time <- rep(NaN, n)
    censored_time[cens_obs] <- randcentime
    censored_time[-cens_obs] <- observe_time
  } else {
    censored_time <- observe_time
  }

  # final time and event definition
  # event =1 if event time < cens_time and observe_time
  time <- pmin(event_time, censored_time, observe_time)
  event <- ifelse(event_time == time, 1, 0)

  data <- cbind(time, event, X)

  return(data)
}

# Example
library(survival)

d1 <- generate_survival_data(10000, 0.5)
time <- d1[, 1]
event <- d1[, 2]
x <- d1[, (3:dim(d1)[2])]

summary(coxph(Surv(time, event) ~ x[, ]))

# coxph(formula = Surv(time, event) ~ x[, ])
#
# n= 10000, number of events= 4367
#
# coef exp(coef)  se(coef)      z Pr(>|z|)
#   x[, ]1   0.559175  1.749229  0.045147 12.386   <2e-16 ***
#   x[, ]2   0.539455  1.715073  0.044118 12.227   <2e-16 ***
#   x[, ]3   0.484915  1.624037  0.044843 10.814   <2e-16 ***
#   x[, ]4   0.505968  1.658590  0.044383 11.400   <2e-16 ***
#   x[, ]5   0.554517  1.741099  0.043466 12.758   <2e-16 ***
#   x[, ]6  -0.049136  0.952052  0.051466 -0.955   0.3397
#   x[, ]7  -0.033527  0.967029  0.050533 -0.663   0.5070
#   x[, ]8   0.034263  1.034857  0.051492  0.665   0.5058
#   x[, ]9  -0.015266  0.984850  0.050481 -0.302   0.7623
#   x[, ]10  0.027910  1.028303  0.050044  0.558   0.5771
#   x[, ]11 -0.004977  0.995036  0.051271 -0.097   0.9227
#   x[, ]12 -0.100801  0.904113  0.051099 -1.973   0.0485 *
#   x[, ]13 -0.048333  0.952817  0.050616 -0.955   0.3396
# x[, ]14  0.020262  1.020469  0.049351  0.411   0.6814
# x[, ]15  0.005037  1.005050  0.049041  0.103   0.9182
# x[, ]16 -0.029073  0.971345  0.050418 -0.577   0.5642
# x[, ]17  0.028490  1.028899  0.049453  0.576   0.5646
# x[, ]18  0.021110  1.021334  0.049807  0.424   0.6717
# x[, ]19  0.104995  1.110706  0.049517  2.120   0.0340 *
# x[, ]20  0.027284  1.027659  0.050523  0.540   0.5892
# x[, ]21 -0.055754  0.945771  0.051590 -1.081   0.2798
# x[, ]22 -0.014730  0.985378  0.049825 -0.296   0.7675
# x[, ]23 -0.067351  0.934867  0.052203 -1.290   0.1970
# x[, ]24  0.019791  1.019989  0.050030  0.396   0.6924
# x[, ]25  0.046846  1.047960  0.050069  0.936   0.3495
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
