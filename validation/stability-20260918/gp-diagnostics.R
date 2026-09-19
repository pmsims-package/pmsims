#!/usr/bin/env Rscript
# Inspect saved GP predictions; no fitting and no random draws.
root <- 'validation/stability-20260918'
pkgload::load_all('.', quiet = TRUE)
writeLines(
  trimws(
    c(paste('mlpwr', packageVersion('mlpwr')), deparse(mlpwr:::get.pred)),
    which = 'right'
  ),
  file.path(root, 'mlpwr-get-pred-source.txt')
)
files <- list.files(
  file.path(root, 'results'),
  pattern = 'search.rds$',
  recursive = TRUE,
  full.names = TRUE
)
rows <- lapply(files, function(file) {
  x <- readRDS(file)
  out <- dirname(file)
  a <- readRDS(file.path(out, 'prepared.rds'))$args
  meta <- read.csv(file.path(out, 'search.csv'), stringsAsFactors = FALSE)
  n <- as.numeric(x$min_n)
  fit <- x$mlpwr_ds$fit
  mu <- as.numeric(fit$fitfun(n))
  sd <- as.numeric(fit$fitfun.sd(n))
  score <- mu + .3 * sd
  display <- function(z) {
    if (attr(a$metric_function, 'metric') == 'csse') {
      1 - sqrt(pmax(0, -z))
    } else {
      z
    }
  }
  reps <- vapply(x$data, function(z) length(z$y), integer(1))
  data.frame(
    id = meta$id,
    role = meta$role,
    seed = meta$seed,
    n = n,
    metric = attr(a$metric_function, 'metric'),
    internal_target = a$target_performance,
    gp_mean = mu,
    gp_sd = sd,
    optimistic_score = score,
    mean_reaches = mu >= a$target_performance,
    optimistic_reaches = score >= a$target_performance,
    equivalent_slope = display(mu),
    minimum_point_reps = min(reps),
    median_point_reps = median(reps),
    maximum_point_reps = max(reps),
    at_lower = n == meta$lower,
    at_upper = n == meta$upper
  )
})
z <- do.call(rbind, rows)
write.csv(z, file.path(root, 'gp-diagnostics.csv'), row.names = FALSE)
print(z)
