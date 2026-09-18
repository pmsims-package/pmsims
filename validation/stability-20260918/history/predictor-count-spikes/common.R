pkgload::load_all('.', quiet = TRUE)
options(pmsims.confirm_long_runs = FALSE)
study_root <- 'validation/predictor-count-spikes'
for (directory in c('raw', 'results', 'logs')) {
  dir.create(
    file.path(study_root, directory),
    recursive = TRUE,
    showWarnings = FALSE
  )
}
raw_path <- function(...) file.path(study_root, 'raw', ...)
result_path <- function(...) file.path(study_root, 'results', ...)
display_slope <- function(v) 1 - sqrt(pmax(0, -v))
summarise_values <- function(vals, target = -0.01) {
  stopifnot(length(vals) == 1000L, all(is.finite(vals)))
  ordered <- sort(vals)
  ci <- ordered[c(qbinom(.025, 1000, .2), qbinom(.975, 1000, .2) + 1)]
  q <- unname(quantile(vals, .2, type = 7))
  data.frame(
    reps = length(vals),
    internal_target = target,
    internal_q20 = q,
    internal_ci_low = ci[1],
    internal_ci_high = ci[2],
    equivalent_slope = display_slope(q),
    slope_ci_low = display_slope(ci[1]),
    slope_ci_high = display_slope(ci[2]),
    evidence = if (ci[2] < target) {
      'below'
    } else if (ci[1] > target) {
      'above'
    } else {
      'overlaps'
    }
  )
}
evaluate_values <- function(args, n, seed, label) {
  set.seed(seed)
  errors <- character()
  started <- proc.time()[['elapsed']]
  vals <- vapply(
    seq_len(1000L),
    function(i) {
      v <- tryCatch(
        {
          test <- args$data_function(args$test_n)
          train <- args$data_function(n)
          fit <- args$model_function(train)
          args$metric_function(test, fit, attr(args$model_function, 'model'))
        },
        error = function(e) {
          errors <<- c(errors, conditionMessage(e))
          args$value_on_error
        }
      )
      if (i %% 100L == 0L) {
        message(
          label,
          ': ',
          i,
          '/1000; elapsed ',
          round(proc.time()[['elapsed']] - started),
          's'
        )
      }
      v
    },
    numeric(1)
  )
  list(
    values = vals,
    errors = errors,
    seconds = proc.time()[['elapsed']] - started,
    summary = summarise_values(vals, args$target_performance)
  )
}
