library(glmnet)
library(pROC)
library(stats)

# population generating function 
# 20 real predictors 
# 100 noise predictors 
# all predictors are binary, with probability of 1 = prob_p
PGF = function(n = 100,  #sample size
               rs = NULL, #random state 
               p = 20,    #number of real predictors
               p_noise = 100, #number of noise predictors 
               prob_p = 0.1, # probability of a predictor to be 1
               w = 0.5,      # log-odds of real predictors (beta in logistic regression), i.e.increases baseline log-odds by w
               base_prev = 0.3, # baseline probability of a positive outcome if all risks are 0
               fitmodel = TRUE, #whether to fit Lasso logistic regression to the data or not 
               X_test = NULL,   # if not null, function will calculate test AUC on this testing data 
               y_test = NULL    # 
){
  if (!is.null(rs)) {set.seed(rs)} 
  alldata = rbinom(n*(p+p_noise),1,prob_p)
  X = matrix(alldata, nrow = n, ncol = p + p_noise)
  W_ = c(rep(w, p), rep(0, p_noise))
  b0 = log(base_prev*0.8/(1- base_prev*0.8))
  lp = X %*% W_ + b0 
  y_prob = 1/(1+exp(-lp))
  
  #generate outcome from the probabilities calculated based on individual risk 
  if (!is.null(rs)) {set.seed(rs+1)} 
  y = rbinom(n,1,y_prob)
  
  #fit the model and compute apparent AUC if requested 
  if (fitmodel){
    if (!is.null(rs)) {set.seed(rs+2)} 
    cv.lasso <- cv.glmnet(X, y, alpha = 1, family = "binomial")
    lasso.model <- glmnet(X, y, alpha = 1, family = "binomial",
                          lambda = cv.lasso$lambda.1se)
    y_hat = predict(lasso.model, X, type = "response")
    auc = pROC::auc(y, as.numeric(y_hat))
    }else{
      lasso.model = NaN
      auc = NaN
    }
  
  # AUC test if X_test and y_test are provided 
  auc_test = NaN
  if(fitmodel & !is.null(X_test) & !is.null(y_test)){
    y_test_hat = predict(lasso.model, X_test, type = "response")
    auc_test = pROC::auc(y_test, as.numeric(y_test_hat))
  }
  
  #outcome object 
  outcome = list()
  outcome$X = X;   outcome$y = y
  outcome$y_prob = y_prob;   outcome$model = lasso.model
  outcome$auc = auc
  outcome$auc_test = auc_test
  return(outcome)  
}


PGF_validate = function(
  n_large = 10000,
  rs = NULL,
  p = 20,
  p_noise = 100,
  prob_p = 0.1,
  w = 0.5,
  base_prev = 0.3 
  #auc_target = 0.75
  ){
  if (is.null(rs)){rs = round(rnorm(1)*1000)}
  d1 = GPF(n=n_large, rs = rs, p= p,p_noise = p_noise, prob_p = prob_p,
           w=w, base_prev = base_prev, fitmodel = TRUE)
  X = d1$X; y = d1$y
  d_test = GPF(n=n_large, rs = rs+1, p= p,p_noise = p_noise, prob_p = prob_p, 
               w=w, base_prev = base_prev, fitmodel = FALSE)
  X_test = d_test$X; y_test = d_test$y
  y_test_hat = predict(d1$model, X_test) 
  auc_test = pROC::auc(y_test, as.numeric(y_test_hat))
  return (auc_test[1])
}

#optim takes ages
# res = optim(par = c(0.5,0.1), fn = GPF_validate, lower = c(0.1, 0.05), 
#       upper = c(2,0.4), auc_target = 0.75, 
#       control = list(maxit = 10), method = "L-BFGS-B")

################# Testing AUC for various simulation parameter combinations ###########################

######## combination 1
prob_p_net = seq(0.1,0.5, 0.1)
n_parameters = c(10,100)
w_net = seq(0.1,1.1,0.2)

auc_p = matrix(nrow = length(w_net), ncol = length(n_parameters))
colnames(auc_p) = n_parameters
auc_p_test = auc_p

i=0; j=0
for (w in w_net){
  i=i+1; j=0
  for (p in n_parameters){
    j=j+1
    try({auc_p[i, j] = PGF_validate(n_large = 10000,rs = NULL,p = p,
               p_noise = 100,prob_p = 0.1,w = w,base_prev = 0.3)})
  }
}
res = data.frame("w" = w_net, auc_p , row.names = seq(6))
colnames(res) = c("w", n_parameters)
res

############ combination 2
prob_p_net = seq(0.2,0.5, 0.1)
w_net = seq(0.1,1.1,0.2)

auc_p = matrix(nrow = length(w_net), ncol = length(prob_p_net))
colnames(auc_p) = prob_p_net
rownames(auc_p) = w_net
auc_p_test = auc_p

i=0; j=0
for (w in w_net){
  i=i+1; j=0
  for (prob_p in prob_p_net){
    j=j+1
    try({auc_p[i, j] = PGF_validate(n_large = 10000,rs = NULL,p = 10,
                                    p_noise = 100,prob_p = prob_p,w = w,base_prev = 0.3)})
  }
}
res2 = data.frame("w" = w_net, auc_p)
colnames(res2) = c("w", prob_p_net)
res2
cbind(res, res2)

#### results : 
#AUC matrix for w (Odds ratio for a binary parameter)

# --- param prev prob_p = 0.1--||------ param prev prob_p 0.2-0.5------
#     OR  #params   #params   OR 
#     w        10       100   w       0.2       0.3       0.4       0.5
# 1 0.1 0.5038888 0.5380374 0.1 0.5000000 0.5086710 0.5000000 0.5000000
# 2 0.3 0.5640752 0.6990831 0.3 0.6017084 0.6148433 0.6264868 0.6247846
# 3 0.5 0.6200792 0.7836084 0.5 0.6693719 0.6757703 0.6900653 0.7055298
# 4 0.7 0.6636300 0.7940785 0.7 0.7117563 0.7421767 0.7544156 0.7536258
# 5 0.9 0.6943719 0.6816898 0.9 0.7538350 0.7836953 0.8134192 0.8222497
# 6 1.1 0.7259680 0.5000000 1.1 0.7887443 0.8205901 0.8508228 0.8476016

# Manually picking params for a given AUC 
#' AUC = 0.60 => w = 0.3, prob_p = 0.2, p = 10, p_noise = 100, base_prev = 0.3
#' AUC = 0.65 => w = 0.4, prob_p = 0.3, p = 10, p_noise = 100, base_prev = 0.3
#' AUC = 0.70 => w = 0.5, prob_p = 0.5, p = 10, p_noise = 100, base_prev = 0.3
#' AUC = 0.75 => w = 0.7, prob_p = 0.35, p = 10, p_noise = 100, base_prev = 0.3
#' AUC = 0.80 => w = 0.9, prob_p = 0.35, p = 10, p_noise = 100, base_prev = 0.3
#' AUC = 0.85 => w = 1.1, prob_p = 0.4, p = 10, p_noise = 100, base_prev = 0.3
#' 
#' AUC = 0.70 => w = 0.3, prob_p = 0.1, p = 100, p_noise = 100, base_prev = 0.3
#' AUC = 0.75 => w = 0.4, prob_p = 0.1, p = 100, p_noise = 100, base_prev = 0.3
#' AUC = 0.80 => w = 0.7, prob_p = 0.1, p = 100, p_noise = 100, base_prev = 0.3
#

######## Example: find min sample size for a target of AUC = 0.75, and aceptable AUC of 0.70 ######
# we look for n_min such that training sample of size n_min will deliver a model with 
# expected AUC on unseen data above 0.70 with probability of 80% 

# For that, we run 50 simulations for various sample sizes 
# and pick the smallest size, in which 80% of simulations had test AUC > 0.70
# we test all the models on the same very large test set (test_size =50 000) 
# 

#Target AUC = 0.75 => using params w = 0.7, prob_p = 0.35, p = 10, p_noise = 100, base_prev = 0.3

target_auc = 0.75
acceptable_auc = target_auc- 0.05

train_size = c(100,200,300,400,500,600,700,1000,3000)
test_size = 50000
n_sims =     c(50, 50, 50, 50, 40, 30, 30, 20, 10)

# creating the large test sample 
test_df = GPF(n=test_size, w = 0.7, prob_p = 0.35, p = 10, p_noise = 100, base_prev = 0.3, fitmodel=TRUE)
test_df$auc #0.74779 => OK
X_test = test_df$X; y_test= test_df$y

#running n_sims simulations and test them on the large test set 

#place holder for AUC across trials (columns) for each train_size(rows)
auc_for_train_size = matrix(nrow = length(train_size), ncol = max(n_sims))

for (i in 1:length(train_size)) {
  for (j in seq(n_sims[i])){
    #only changing random seed 
    stat =PGF(n=train_size[i], 
        rs = 100+j, w = 0.7, prob_p = 0.35, p = 10, 
        p_noise = 100, base_prev = 0.3, 
        fitmodel=TRUE, X_test = X_test, y_test = y_test)
    auc_for_train_size[i,j] = stat$auc_test
 }
}
auc_for_train_size
mean_auc = apply(auc_for_train_size, FUN = mean, MARGIN = 1, na.rm= TRUE)
quant_auc = apply(auc_for_train_size, FUN = quantile, MARGIN = 1, probs = 0.2, na.rm = TRUE)
CIlow_auc = apply(auc_for_train_size, FUN = quantile, MARGIN = 1, probs = 0.05, na.rm = TRUE)
CIhigh_auc = apply(auc_for_train_size, FUN = quantile, MARGIN = 1, probs = 0.95, na.rm = TRUE)
rbind(mean_auc, quant_auc)

#min training size where 80% of AUC are > 0.70:
# We linearly approximate values between the train sizes and get the min_n from there:
min_size_func = approxfun(quant_auc, train_size, method = "linear")
min_n = min_size_func(0.7)
# 929  => we need more than 930 participants 

#Plot 
plot(train_size, quant_auc, type = "l" , lty = 2, col = "red", 
     main = "AUC by train size with 95% CI and 80% quantile")
lines(train_size, mean_auc, col = "black")
lines(train_size, CIlow_auc, col = "grey", lty = 1)
lines(train_size, CIhigh_auc, col = "grey", lty = 1)
#abline(h = target_auc, col = 3)
abline(h = acceptable_auc, col = "green", lty = 3)
abline(v = min_n, col = "green", lty = 3)
legend("bottomright", legend = c("AUC mean", "AUC 95% CI", "AUC 20% quantile", "Acceptable AUC"), 
       col = c("black", "grey", "red", "green"), lty = c(1,1,2,3))


