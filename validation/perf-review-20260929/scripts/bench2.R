here <- Sys.getenv("PMSIMS_PERF_DIR")
suppressMessages(pkgload::load_all(file.path(here, "pmsims-fc"), quiet = TRUE))
tm <- function(expr, reps = 20) { e <- substitute(expr); pf <- parent.frame(); eval(e, pf)
  1000 * system.time(for (i in seq_len(reps)) eval(e, pf))[["elapsed"]] / reps }
row <- function(l, ms) cat(sprintf("%-60s %8.2f ms\n", l, ms))
set.seed(3)
df <- default_data_generators(list(type = "binary", args = list(n_signal_parameters = 20, noise_parameters = 0,
  complexity = 1, predictor_type = "continuous", binary_prevalence = 0, correlation = 0.3,
  distribution = "normal", mu_lp = -1, beta_signal = 0.2, baseline_prob = 0.3)))
tr <- df(1000); te <- df(30000)
fit <- glm("y ~ .", data = tr, family = "binomial")
lp <- drop(cbind(1, as.matrix(te[, -1])) %*% coef(fit))
a <- coef(glm(te$y ~ lp, family = binomial()))[2]
b <- glm.fit(cbind(1, lp), te$y, family = binomial(), start = c(0, 1))
cat(sprintf("slope glm %.8f | glm.fit start=c(0,1) %.8f (iter %d)\n", a, b$coefficients[2], b$iter))
row("glm(y ~ lp)", tm(glm(te$y ~ lp, family = binomial())))
row("glm.fit(cbind(1,lp), start = c(0,1))", tm(glm.fit(cbind(1, lp), te$y, family = binomial(), start = c(0, 1))))
row("full alt: matrix predict + glm.fit(start)", tm({l <- drop(cbind(1, as.matrix(te[, -1])) %*% coef(fit)); glm.fit(cbind(1, l), te$y, family = binomial(), start = c(0, 1))}))
cat(sprintf("AUC pROC %.10f | cstat_full %.10f\n", pROC::auc(te$y, lp, quiet = TRUE)[1], cstat_full(te$y, lp)))
X <- generate_predictors(30000, 10, 0); Xs <- X
Nraw <- rowSums(Xs^2)
r1 <- residuals(lm(Nraw ~ Xs)); r2 <- .lm.fit(cbind(1, Xs), Nraw)$residuals
cat(sprintf("max |resid diff| %.2e\n", max(abs(r1 - r2))))
row("residuals(lm(Nraw ~ Xs))", tm(residuals(lm(Nraw ~ Xs))))
row(".lm.fit(cbind(1, Xs), Nraw)$residuals", tm(.lm.fit(cbind(1, Xs), Nraw)$residuals))
