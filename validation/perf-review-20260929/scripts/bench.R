# Micro-benchmarks for candidate hotspots identified in the profiles.
here <- Sys.getenv("PMSIMS_PERF_DIR")
suppressMessages(pkgload::load_all(file.path(here, "pmsims-fc"), quiet = TRUE))

tm <- function(expr, reps = 20) {
  e <- substitute(expr)
  pf <- parent.frame()
  invisible(eval(e, pf)) # warm-up
  t <- system.time(for (i in seq_len(reps)) eval(e, pf))[["elapsed"]]
  1000 * t / reps # ms per call
}
row <- function(label, ms) cat(sprintf("%-62s %9.2f ms\n", label, ms))

set.seed(1)
p <- 20
mk <- function(type, extra) {
  default_data_generators(list(type = type, args = c(list(
    n_signal_parameters = p, noise_parameters = 0, complexity = 1,
    predictor_type = "continuous", binary_prevalence = 0, correlation = 0.3,
    distribution = "normal"), extra)))
}
df_bin <- mk("binary", list(mu_lp = -1, beta_signal = 0.2, baseline_prob = 0.3))
df_con <- mk("continuous", list(beta_signal = 0.2))
df_sur <- mk("survival", list(beta_signal = 0.2, baseline_hazard = 0.01, censoring_rate = 0.3))

cat("\n## 1. Cost of one data_function() call (p = 20)\n")
for (n in c(500, 30000)) {
  row(sprintf("binary     data_function(%d)", n), tm(df_bin(n)))
  row(sprintf("continuous data_function(%d)", n), tm(df_con(n)))
  row(sprintf("survival   data_function(%d)", n), tm(df_sur(n)))
}

cat("\n## 2. Breakdown of generate_binary_data(30000), p = 20\n")
n <- 30000
R <- matrix(0.3, p, p); diag(R) <- 1
row("rnorm(n*p)", tm(stats::rnorm(n * p)))
Z0 <- matrix(stats::rnorm(n * p), n, p)
row("eigen(R) check", tm(eigen(R, symmetric = TRUE, only.values = TRUE)))
row("chol(R)", tm(chol(R)))
U <- chol(R)
row("Z %*% chol(R)  (reference BLAS)", tm(Z0 %*% U))
Z <- Z0 %*% U
row("normal_to_family(Z, 'normal')", tm(normal_to_family(Z, "normal")))
X <- generate_predictors(n, p, 0)
row("generate_linear_predictor (C1)", tm(generate_linear_predictor(X, p, 0, 0, 0.2, 1)))
row("generate_linear_predictor (C3, lm residualisation)",
    tm(generate_linear_predictor(X, p, 0, 0, 0.2, 3), reps = 5))
y <- stats::rbinom(n, 1, 0.3)
row("as.data.frame(cbind(y, X))", tm(as.data.frame(cbind(y, X))))

cat("\n## 3. Metric evaluation on a 30,000-row test set (binary glm, p = 20)\n")
train <- df_bin(1000); test <- df_bin(30000)
fit <- stats::glm("y ~ .", data = train, family = "binomial")
row("binary_calib_slope(test, fit, 'glm')  [total]", tm(binary_calib_slope(test, fit, "glm")))
x_df <- test[, names(test) != "y", drop = FALSE]
row("  data[, names(data) != 'y']", tm(test[, names(test) != "y", drop = FALSE]))
row("  predict_custom(... 'link')  [predict.glm]", tm(predict_custom(x_df, NULL, fit, "glm", type = "link")))
xm <- as.matrix(x_df)
row("  cbind(1, X) %*% coef(fit)  [alternative]", tm(drop(cbind(1, xm) %*% stats::coef(fit))))
lp <- drop(cbind(1, xm) %*% stats::coef(fit))
row("  glm(y ~ y_link, binomial)", tm(stats::glm(test$y ~ lp, family = stats::binomial())))
row("  glm.fit(cbind(1, lp), y, binomial)  [alternative]",
    tm(stats::glm.fit(cbind(1, lp), test$y, family = stats::binomial())))
row("binary_auc_metric(test, fit, 'glm') [pROC]", tm(binary_auc_metric(test, fit, "glm")))
row("  cstat_full(y, lp)  [rank-based alternative]", tm(cstat_full(test$y, lp)))

cat("\n## 4. Model fits at training size n = 1000 (binary, p = 20)\n")
row("glm('y ~ .', binomial)", tm(stats::glm("y ~ .", data = train, family = "binomial")))
row("glm.fit(cbind(1, X), y)  [alternative]",
    tm(stats::glm.fit(cbind(1, as.matrix(train[, -1])), train$y, family = stats::binomial())))

cat("\n## 5. Continuous metric on 30,000 test rows (lm, p = 20)\n")
trc <- df_con(1000); tec <- df_con(30000)
fitc <- stats::glm("y ~ .", data = trc, family = "gaussian")
row("continuous_calib_slope(test, fit, 'lm')", tm(continuous_calib_slope(tec, fitc, "lm")))

cat("\n## 6. Survival (coxph, p = 20)\n")
trs <- df_sur(1000); tes <- df_sur(30000)
fits <- default_model_generators("survival", "coxph")(trs)
row("coxph fit, n = 1000", tm(default_model_generators("survival", "coxph")(trs)))
row("survival_csse (PH path) on 30,000 test", tm(survival_csse(tes, fits, "coxph"), reps = 5))
row("survival_cindex on 30,000 test", tm(survival_cindex(tes, fits, "coxph"), reps = 5))

cat("\n## 7. mlpwr per-replicate wrapper overhead\n")
hush <- get("hush", asNamespace("mlpwr"))
row("mlpwr:::hush(NULL)  [sink open/close + Sys.info]", tm(hush(NULL), reps = 2000))
row("parallel::detectCores(logical = FALSE)", tm(parallel::detectCores(logical = FALSE), reps = 200))

cat("\n## 8. t-distribution transform on 30,000 x 10\n")
Zt <- matrix(stats::rnorm(30000 * 10), 30000, 10)
row("normal_to_family(Z, 't')  [qt(pnorm(-|Z|))]", tm(normal_to_family(Zt, "t"), reps = 3))
row("normal_to_family(Z, 'normal')", tm(normal_to_family(Zt, "normal")))
row("  stats::pnorm(-abs(Z)) alone", tm(stats::pnorm(-abs(Zt))))

cat("\n## 9. Survival calibration-slope pieces on 30,000 test rows (coxph, p = 10)\n")
p <- 10
df_s10 <- default_data_generators(list(type = "survival", args = list(
  n_signal_parameters = 10, noise_parameters = 0, complexity = 1,
  predictor_type = "continuous", binary_prevalence = 0, correlation = 0.3,
  distribution = "normal", beta_signal = 0.2, baseline_hazard = 0.01, censoring_rate = 0.3)))
tr10 <- df_s10(600); te10 <- df_s10(30000)
fit10 <- default_model_generators("survival", "coxph")(tr10)
mcs <- default_metric_generator("calibration_slope", df_s10)
row("metric used by simulate_survival(coxph, calibration_slope) [total]", tm(mcs(te10, fit10, "coxph"), reps = 5))
d <- te10[order(te10$time), ]
ev <- d$time[d$event == 1]; et <- stats::median(ev)
row("  predicted_survival_at_time [coxph offset + basehaz]", tm(predicted_survival_at_time(d, fit10, "coxph", et), reps = 5))
lp10 <- predict_custom(d[, -(1:2)], NULL, fit10, "coxph", type = "lp")
row("    predict_custom lp (predict.coxph)", tm(predict_custom(d[, -(1:2)], NULL, fit10, "coxph", type = "lp"), reps = 5))
breslow_H0 <- function(time, event, lp, t) {
  o <- order(time); time <- time[o]; event <- event[o]; r <- exp(lp[o])
  risk <- rev(cumsum(rev(r)))
  keep <- event == 1 & time <= t
  sum(1 / risk[keep])  # ties handled Breslow-style when times distinct
}
row("    direct Breslow H0(t*) [alternative]", tm(breslow_H0(d$time, d$event, lp10, et)))
H0a <- breslow_H0(d$time, d$event, lp10, et)
bh <- survival::basehaz(survival::coxph(survival::Surv(d$time, d$event) ~ offset(lp10)), centered = FALSE)
cat(sprintf("    check: basehaz H0 = %.6f, direct = %.6f\n", stats::approx(bh$time, bh$hazard, xout = et, rule = 2)$y, H0a))
row("  ipcw_binary_at_time", tm(ipcw_binary_at_time(d, et), reps = 5))
S <- exp(-H0a * exp(lp10)); eta <- log(-log(S)); iw <- ipcw_binary_at_time(d, et)
row("  glm(y ~ eta, cloglog, weights)", tm(suppressWarnings(stats::glm(iw$y ~ eta, weights = iw$w, family = stats::binomial(link = "cloglog"))), reps = 5))
row("  glm.fit(cbind(1,eta), ...)  [alternative]", tm(suppressWarnings(stats::glm.fit(cbind(1, eta), iw$y, weights = iw$w, family = stats::binomial(link = "cloglog"))), reps = 5))

cat("\n## 10. Random forest (wall clock), p = 10, 30,000 test rows\n")
df_b10 <- default_data_generators(list(type = "binary", args = list(
  n_signal_parameters = 10, noise_parameters = 0, complexity = 1,
  predictor_type = "continuous", binary_prevalence = 0, correlation = 0.3,
  distribution = "normal", mu_lp = -1, beta_signal = 0.3, baseline_prob = 0.3)))
trb <- df_b10(80); teb <- df_b10(30000)
rf_b <- default_model_generators("binary", "rf")
row("binary rf fit, n = 80 (16 threads)", tm(rf_b(trb), reps = 10))
fb <- rf_b(trb)
row("binary_auc_metric(rf) on 30,000", tm(binary_auc_metric(teb, fb, "rf"), reps = 5))
xb <- teb[, -1]
for (nt in c(1, 2, 4, 16)) row(sprintf("  predict.ranger prob, num.threads = %d", nt), tm(stats::predict(fb, data = xb, num.threads = nt), reps = 5))
trs10 <- df_s10(95)
rf_s <- default_model_generators("survival", "rf")
row("survival rf fit, n = 95", tm(rf_s(trs10), reps = 10))
fs <- rf_s(trs10)
row("survival_cindex(rf) on 30,000", tm(survival_cindex(te10, fs, "rf"), reps = 3))
xs <- te10[, -(1:2)]
for (nt in c(1, 2, 4, 16)) row(sprintf("  predict.ranger survival, num.threads = %d", nt), tm(stats::predict(fs, data = xs, num.threads = nt), reps = 3))
cat(sprintf("  unique death times in fit: %d; chf matrix %d x %d\n", length(fs$unique.death.times), nrow(xs), length(fs$unique.death.times)))
