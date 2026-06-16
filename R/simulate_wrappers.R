# =============================================================================
# Internal wrappers shared by simulate_binary(), simulate_continuous() and
# simulate_survival().
#
# These helpers translate the user-facing `complexity` + `data_control`
# interface into the arguments expected by (a) the tuning functions and
# (b) the data generators, keeping the two consistent. Validation lives in
# input_validation.R (validate_complexity(), validate_data_control()).
# =============================================================================

#' Resolve a `data_control` list into generator/tuner arguments
#'
#' Validates `data_control` (via [validate_data_control()]) and maps the
#' user-facing `predictor_distribution` onto the generator's internal
#' `predictor_type` + `distribution` + `binary_prevalence`:
#' `"binary"` selects binary predictors (with the supplied prevalence); any
#' other value selects continuous predictors drawn from that family.
#'
#' @param data_control A named list or `NULL`.
#' @param complexity A single value in 1:4.
#' @return A list with `nonlinear_strength`, `correlation`, `predictor_type`,
#'   `distribution`, `binary_prevalence`, and the original
#'   `predictor_distribution` (for reporting).
#' @keywords internal
#' @noRd
resolve_data_control <- function(data_control, complexity) {
  validate_data_control(data_control, complexity)
  
  ctrl <- utils::modifyList(
    list(
      nonlinear_strength          = NULL,
      correlation                 = 0.3,
      predictor_distribution      = "normal",
      binary_predictor_prevalence = NULL
    ),
    if (is.null(data_control)) list() else data_control
  )
  
  if (identical(ctrl$predictor_distribution, "binary")) {
    predictor_type    <- "binary"
    distribution      <- "normal" # ignored by the generator for binary predictors
    binary_prevalence <- ctrl$binary_predictor_prevalence
  } else {
    predictor_type    <- "continuous"
    distribution      <- ctrl$predictor_distribution
    binary_prevalence <- 0
  }
  
  list(
    nonlinear_strength     = ctrl$nonlinear_strength,
    correlation            = ctrl$correlation,
    predictor_type         = predictor_type,
    distribution           = distribution,
    binary_prevalence      = binary_prevalence,
    predictor_distribution = ctrl$predictor_distribution
  )
}

#' Call a tuning function with only the data-config arguments it accepts
#'
#' The tuner must build its unit linear predictor with the *same* data-
#' generating configuration that will be used to simulate data, otherwise the
#' tuned effect size will not recover the requested performance. This helper
#' passes `complexity`, `nonlinear_strength`, `correlation`, `distribution`,
#' `predictor_type` and `binary_prevalence` to the tuner, silently dropping any
#' the tuner does not declare (so it works with both the current and older
#' tuner signatures). If a non-default `nonlinear_strength` cannot be passed,
#' a warning is raised because C2/C3 tuning would then be inconsistent with the
#' generated data.
#'
#' @param tuner A tuning function (e.g. `binary_tuning`).
#' @param required A named list of the tuner's required arguments. Must include
#'   `.complexity`, which is forwarded as `complexity`.
#' @param dc The list returned by [resolve_data_control()].
#' @return The tuner's return value.
#' @keywords internal
#' @noRd
call_tuner <- function(tuner, required, dc) {
  fmls       <- names(formals(tuner))
  complexity <- required$.complexity
  required$.complexity <- NULL
  
  data_config <- list(
    complexity         = complexity,
    nonlinear_strength = dc$nonlinear_strength,
    correlation        = dc$correlation,
    distribution       = dc$distribution,
    predictor_type     = dc$predictor_type,
    binary_prevalence  = dc$binary_prevalence
  )
  
  if (!is.null(dc$nonlinear_strength) && !("nonlinear_strength" %in% fmls)) {
    warning(
      "The tuning function does not accept `nonlinear_strength`; the tuned ",
      "effect size may not match the generated data for complexity 2/3. ",
      "Update the tuner to the `nonlinear_strength` interface.",
      call. = FALSE
    )
  }
  
  optional <- data_config[names(data_config) %in% fmls]
  do.call(tuner, c(required, optional))
}

#' Extract a named element from a tuning result, with a scalar fallback
#'
#' @param tp A tuning result (named vector/list, or a bare scalar).
#' @param name The element to extract (e.g. `"beta_signal"`, `"mu_lp"`).
#' @keywords internal
#' @noRd
get_param <- function(tp, name) {
  if (!is.null(names(tp)) && name %in% names(tp)) {
    return(unname(tp[[name]]))
  }
  if (name == "beta_signal") {
    return(unname(tp[[1]])) # continuous_tuning may return a scalar
  }
  stop("Tuning result is missing `", name, "`.", call. = FALSE)
}

#' Assemble the generator argument list for a data_spec
#'
#' Uses the generators' real formal names. `nonlinear_strength` is included
#' only when non-`NULL`: `default_data_generators()` bakes args via
#' `formals(fn)[[key]] <- value`, and assigning `NULL` would *delete* the
#' formal rather than leave its default, so a `NULL` entry must be omitted.
#'
#' @keywords internal
#' @noRd
make_data_args <- function(signal_parameters, noise_parameters, complexity,
                           dc, extra = list()) {
  args <- c(
    list(
      n_signal_parameters = signal_parameters,
      noise_parameters    = noise_parameters,
      complexity          = complexity,
      predictor_type      = dc$predictor_type,
      binary_prevalence   = dc$binary_prevalence,
      correlation         = dc$correlation,
      distribution        = dc$distribution
    ),
    extra
  )
  if (!is.null(dc$nonlinear_strength)) {
    args$nonlinear_strength <- dc$nonlinear_strength
  }
  args
}


#' Minimum sample size for binary-outcome prediction models
#'
#' Compute the minimum sample size required to develop a prediction model with a
#' binary outcome. The function wraps a simulation-based engine that combines a
#' bisection search with Gaussian-process curve fitting. From user inputs
#' (outcome prevalence, maximum achievable performance, target performance, etc.) it
#' constructs a data-generating function, a model-fitting
#' function, and a metric function, then searches for the smallest \eqn{n} that
#' meets the chosen performance criterion.
#'
#' @section Criteria:
#' Two formulations are supported.
#'
#' - **Mean-based**: find the smallest \eqn{n} such that the expected model
#'   performance exceeds the target \eqn{M^*}, i.e.
#'   \deqn{\min_n \; \mathbb{E}_{D_n}\{ M \mid D_n \} \ge M^*.}
#'
#' - **Assurance-based**: find the smallest \eqn{n} such that the probability
#'   the performance exceeds \eqn{M^*} is at least \eqn{\delta} (e.g. 0.80),
#'   i.e.
#'   \deqn{\min_n \; \mathbb{P}_{D_n}\!\left( M \mid D_n \ge M^* \right) \ge \delta.}
#'
#' Here, \eqn{M} is the chosen performance metric and the probability/expectation
#' is over repeated samples of training data of size \eqn{n}. The assurance
#' criterion explicitly accounts for variability across training sets; models
#' with higher variance typically require larger \eqn{n} to satisfy it.
#'
#' @section Data control:
#' `complexity` selects the signal structure of the data-generating mechanism:
#' `1` purely linear, `2` linear + quadratic, `3` linear + quadratic +
#' interaction, `4` the Friedman function. `data_control` is an optional list
#' fine-tuning the predictors:
#' \describe{
#'   \item{`nonlinear_strength`}{Numeric in `[0, 1)`. Fraction of the signal
#'     variance carried by the nonlinear, linearly-inaccessible component.
#'     Applies to complexity 2 and 3 only; ignored (with a warning) for 1 and 4.
#'     If omitted, the generator's per-complexity default is used.}
#'   \item{`correlation`}{Numeric in `[-1, 1]`. Pairwise correlation among the
#'     candidate predictors. Default `0.3`.}
#'   \item{`predictor_distribution`}{One of `"normal"`, `"uniform"`, `"binary"`,
#'     `"exponential"`, `"lognormal"`, `"t"`, `"laplace"`. `"binary"` selects
#'     0/1 predictors and requires `binary_predictor_prevalence`; any other
#'     value selects continuous predictors from that family. Default `"normal"`.}
#'   \item{`binary_predictor_prevalence`}{Numeric in `(0, 1)`. Prevalence of the
#'     binary predictors; required when `predictor_distribution = "binary"`,
#'     ignored (with a warning) otherwise. Note: binary predictors are
#'     incompatible with complexity 2/3 because squaring a 0/1 variable returns
#'     itself.}
#' }
#'
#' @param signal_parameters Integer. Number of candidate predictors associated
#'   with the outcome (i.e., true signal features).
#' @param noise_parameters Integer. Number of candidate predictors not
#'   associated with the outcome (noise features). Default is 0.
#' @param complexity Integer in 1:4 selecting the data-generating signal
#'   structure (see *Data control*). Default `1`.
#' @param data_control Optional named list controlling the predictors (see
#'   *Data control*). Default `NULL` (generator defaults).
#' @param outcome_prevalence Numeric in (0, 1). Target prevalence of the binary
#'   outcome in the intended modelling context.
#' @param maximum_achievable_cstatistic Numeric in (0, 1). Maximum achievable
#'   C-statistic with effectively unlimited data. This is used to calibrate the
#'   data-generating mechanism and is not the minimum acceptable threshold.
#' @param model Character string specifying the modelling algorithm. One of
#'   `"glm"` (logistic regression), `"lasso"`, `"ridge"`, `"rf"` (random
#'   forest), or `"xgboost"` (gradient-boosted trees). The machine-learning
#'   options are experimental.
#' @param metric Character string naming the performance metric used to assess
#'   the sample size; defaults to `"calibration_slope"`. (Internally mapped to
#'   the engine's metric identifiers.)
#' @param target_performance Numeric. Minimum acceptable value of the selected
#'   performance metric \eqn{M^*}; the algorithm searches for the smallest
#'   \eqn{n} meeting the chosen criterion with respect to this threshold.
#' @param n_reps_total Integer. Total number of simulation replications used by
#'   the engine across the search.
#' @param mean_or_assurance Character string, either `"mean"` or `"assurance"`.
#'   Controls whether the minimum \eqn{n} is defined by the mean-based criterion
#'   or the assurance-based criterion (with the assurance level \eqn{\delta}
#'   controlled by the engine's defaults or additional arguments in `...`).
#' @param ... Additional options passed to [simulate_custom()] (e.g., assurance
#'   level \eqn{\delta}, per-iteration settings).
#'
#' @return An object of class `"pmsims"` containing the estimated minimum sample
#'   size and simulation diagnostics (inputs, fitted GP curve, intermediate
#'   evaluations, and summary metrics).
#'
#' @seealso [simulate_continuous()], [simulate_survival()], [simulate_custom()]
#'
#' @examples
#' \dontrun{
#' est <- simulate_binary(
#'   signal_parameters = 10,
#'   noise_parameters = 10,
#'   complexity = 2,
#'   data_control = list(nonlinear_strength = 0.4, correlation = 0.2),
#'   outcome_prevalence = 0.2,
#'   maximum_achievable_cstatistic = 0.75,
#'   model = "glm",
#'   metric = "calibration_slope",
#'   target_performance = 0.9,
#'   n_reps_total = 1000,
#'   mean_or_assurance = "assurance"
#' )
#' est
#' }
#' @export
simulate_binary <- function(
    # Predictors
  signal_parameters,
  noise_parameters = 0,
  complexity = 1,
  data_control = NULL,
  # Outcome
  outcome_prevalence,
  # Performance
  maximum_achievable_cstatistic,
  # Model
  model = c("glm", "lasso", "ridge", "rf", "xgboost"),
  metric = "calibration_slope",
  target_performance,
  # Engine
  n_reps_total = 1000,
  mean_or_assurance = "assurance",
  ...
) {
  model <- check_pmsims_args(model, c("glm", "lasso", "ridge", "rf", "xgboost"))
  validate_metric_constraints(
    metric = metric,
    target_performance = target_performance,
    maximum_achievable_performance = maximum_achievable_cstatistic
  )
  validate_complexity(complexity)
  validate_outcome_prevalence(outcome_prevalence)
  dc <- resolve_data_control(data_control, complexity)
  
  candidate_features        <- signal_parameters + noise_parameters
  proportion_noise_features <- noise_parameters / candidate_features
  
  # Tune the data-generating function under the SAME data configuration.
  tune_param <- call_tuner(
    binary_tuning,
    required = list(
      target_prevalence         = outcome_prevalence,
      target_performance        = maximum_achievable_cstatistic,
      candidate_features        = candidate_features,
      proportion_noise_features = proportion_noise_features,
      .complexity               = complexity
    ),
    dc = dc
  )
  
  data_spec <- list(
    type = "binary",
    args = make_data_args(
      signal_parameters, noise_parameters, complexity, dc,
      extra = list(
        mu_lp         = get_param(tune_param, "mu_lp"),
        beta_signal   = get_param(tune_param, "beta_signal"),
        baseline_prob = outcome_prevalence
      )
    )
  )
  
  data_function  <- default_data_generators(data_spec)
  outcome_type   <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome_type, model)
  
  # Redefine metrics to internal syntax lang
  metric <- ifelse(metric == "calibration_slope", "calib_slope", metric)
  
  simulate_custom_args <- utils::modifyList(
    list(
      metric_function = default_metric_generator(metric, data_function),
      target_performance = target_performance,
      c_statistic = maximum_achievable_cstatistic,
      data_function = data_function,
      model_function = model_function,
      n_reps_total = n_reps_total,
      n_reps_per = 20,
      method = "mlpwr",
      mean_or_assurance = mean_or_assurance,
      test_n = 30000
    ),
    list(...)
  )
  
  suppressWarnings(
    output <- do.call(simulate_custom, simulate_custom_args)
  )
  
  metric_2 <- if (metric %in% c("csse","calib_slope")) "auc" else "calib_slope"
  
  test_n <- 30000
  metric_function_2 <- default_metric_generator(metric_2, data_function)
  
  data_2 <- data_function(output$min_n)
  test_data_2 <- data_function(test_n)
  fit_2 <- model_function(data_2)
  metric_2_at_n <- metric_function_2(test_data_2, fit_2, model)
  
  output$metric_2_at_n <- metric_2_at_n
  output$metric_2 <- metric_2
  
  output$parameters <- signal_parameters
  output$noise_parameters <- noise_parameters
  output$complexity <- complexity
  output$nonlinear_strength <- dc$nonlinear_strength
  output$correlation <- dc$correlation
  output$predictor_distribution <- dc$predictor_distribution
  output$predictor_type <- dc$predictor_type
  output$binary_predictor_prevalence <- dc$binary_prevalence
  output$prevalence <- outcome_prevalence
  output$cstatistic <- maximum_achievable_cstatistic
  output$model <- model
  output$metric <- metric
  output$n_reps_total <- n_reps_total
  output$mean_or_assurance <- mean_or_assurance
  est <- output
  class(est) <- "pmsims"
  est
}

#' Minimum sample size for continuous‐outcome prediction models
#'
#' Compute the minimum sample size required to develop a prediction model with a
#' **continuous** outcome. This wraps the same simulation engine as
#' [simulate_binary()], combining bisection search with Gaussian-process
#' learning-curve modelling. From user inputs (maximum achievable performance, target
#' performance, etc.) it constructs a
#' data-generating function, model-fitting function, and metric function, then
#' searches for the smallest \eqn{n} meeting the chosen criterion.
#'
#' @inheritSection simulate_binary Criteria
#' @inheritSection simulate_binary Data control
#'
#' @inheritParams simulate_binary
#' @param maximum_achievable_rsquared Numeric in (0, 1). Maximum achievable
#'   \eqn{R^2} with effectively unlimited data. This is used to calibrate the
#'   data-generating mechanism and is not the minimum acceptable threshold.
#' @param model Character string specifying the modelling algorithm. One of
#'   `"lm"` (linear regression), `"lasso"`, `"ridge"`, `"rf"` (random forest),
#'   or `"xgboost"` (gradient-boosted trees). The machine-learning options are
#'   experimental.
#'
#' @return An object of class `"pmsims"` containing the estimated minimum sample
#'   size and simulation diagnostics (inputs, fitted GP curve, intermediate
#'   evaluations, and summary metrics).
#'
#' @seealso [simulate_binary()], [simulate_survival()], [simulate_custom()]
#'
#' @examples
#' \dontrun{
#' est <- simulate_continuous(
#'   signal_parameters = 8,
#'   noise_parameters = 8,
#'   complexity = 3,
#'   maximum_achievable_rsquared = 0.50,
#'   model = "lm",
#'   metric = "calibration_slope",
#'   target_performance = 0.9,
#'   n_reps_total = 1000,
#'   mean_or_assurance = "assurance"
#' )
#' est
#' }
#' @export
simulate_continuous <- function(
    signal_parameters,
    noise_parameters = 0,
    complexity = 1,
    data_control = NULL,
    maximum_achievable_rsquared,
    model = c("lm", "lasso", "ridge", "rf", "xgboost"),
    metric = "calibration_slope",
    target_performance,
    n_reps_total = 1000,
    mean_or_assurance = "assurance",
    ...
) {
  model <- check_pmsims_args(model, c("lm", "lasso", "ridge", "rf", "xgboost"))
  validate_metric_constraints(
    metric = metric,
    target_performance = target_performance,
    maximum_achievable_performance = maximum_achievable_rsquared
  )
  validate_complexity(complexity)
  dc <- resolve_data_control(data_control, complexity)
  
  candidate_features        <- signal_parameters + noise_parameters
  proportion_noise_features <- noise_parameters / candidate_features
  
  # Tune the data-generating function under the SAME data configuration.
  tune_param <- call_tuner(
    continuous_tuning,
    required = list(
      r2                        = maximum_achievable_rsquared,
      candidate_features        = candidate_features,
      proportion_noise_features = proportion_noise_features,
      .complexity               = complexity
    ),
    dc = dc
  )
  
  data_spec <- list(
    type = "continuous",
    args = make_data_args(
      signal_parameters, noise_parameters, complexity, dc,
      extra = list(beta_signal = get_param(tune_param, "beta_signal"))
    )
  )
  
  data_function  <- default_data_generators(data_spec)
  outcome_type   <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome_type, model)
  
  metric <- ifelse(metric == "calibration_slope", "calib_slope", metric)
  
  simulate_custom_args <- utils::modifyList(
    list(
      metric_function = default_metric_generator(metric, data_function),
      target_performance = target_performance,
      c_statistic = maximum_achievable_rsquared,
      data_function = data_function,
      model_function = model_function,
      n_reps_total = n_reps_total,
      n_reps_per = 20,
      method = "mlpwr",
      mean_or_assurance = mean_or_assurance,
      test_n = 30000
    ),
    list(...)
  )
  
  suppressWarnings(
    output <- do.call(simulate_custom, simulate_custom_args)
  )
  
  metric_2 <- if (metric %in% c("csse","calib_slope")) "r2" else "calib_slope"
  
  metric_function_2 <- default_metric_generator(metric_2, data_function)
  
  test_n <- 30000
  data_2 <- data_function(output$min_n)
  test_data_2 <- data_function(test_n)
  fit_2 <- model_function(data_2)
  metric_2_at_n <- metric_function_2(test_data_2, fit_2, model)
  
  output$metric_2_at_n <- metric_2_at_n
  output$metric_2 <- metric_2
  
  output$parameters <- signal_parameters
  output$noise_parameters <- noise_parameters
  output$complexity <- complexity
  output$nonlinear_strength <- dc$nonlinear_strength
  output$correlation <- dc$correlation
  output$predictor_distribution <- dc$predictor_distribution
  output$predictor_type <- dc$predictor_type
  output$binary_predictor_prevalence <- dc$binary_prevalence
  output$r2 <- maximum_achievable_rsquared
  output$model <- model
  output$metric <- metric
  output$n_reps_total <- n_reps_total
  output$mean_or_assurance <- mean_or_assurance
  class(output) <- "pmsims"
  output
}

#' Minimum sample size for survival‐outcome prediction models
#'
#' Compute the minimum sample size required to develop a prediction model with a
#' **time-to-event (survival)** outcome. As with the other wrappers, this uses a
#' simulation-based learning-curve approach with Gaussian-process surrogate
#' modelling to locate the smallest \eqn{n} meeting the chosen performance
#' criterion.
#'
#' @inheritSection simulate_binary Criteria
#' @inheritSection simulate_binary Data control
#'
#' @inheritParams simulate_binary
#' @param maximum_achievable_cindex Numeric in (0, 1). Maximum achievable
#'   C-index with effectively unlimited data. This is used to calibrate the
#'   data-generating mechanism and is not the minimum acceptable threshold.
#' @param baseline_hazard Numeric greater than 0. Baseline hazard level used by the
#'   data-generating mechanism (e.g., the constant hazard in an exponential
#'   baseline). Larger values imply shorter event times, all else equal.
#' @param censoring_rate Numeric in [0, 1). Proportion of individuals expected
#'   to be censored in the simulated datasets (administrative or random
#'   censoring). Higher values imply fewer observed events for a fixed \eqn{n}.
#' @param model Character string specifying the modelling algorithm. One of
#'   `"coxph"` (Cox proportional hazards), `"lasso"`, `"ridge"`, `"rf"` (random
#'   survival forest), or `"xgboost"` (gradient boosting with a Cox objective).
#'   The machine-learning options are experimental.
#'
#' @return An object of class `"pmsims"` containing the estimated minimum sample
#'   size and simulation diagnostics (inputs, fitted GP curve, intermediate
#'   evaluations, and summary metrics).
#'
#' @seealso [simulate_binary()], [simulate_continuous()], [simulate_custom()]
#'
#' @examples
#' \dontrun{
#' est <- simulate_survival(
#'   signal_parameters = 10,
#'   noise_parameters = 10,
#'   complexity = 2,
#'   data_control = list(nonlinear_strength = 0.5),
#'   maximum_achievable_cindex = 0.70,
#'   baseline_hazard = 0.01,
#'   censoring_rate = 0.30,
#'   model = "coxph",
#'   metric = "calibration_slope",
#'   target_performance = 0.9,
#'   n_reps_total = 1000,
#'   mean_or_assurance = "assurance"
#' )
#' est
#' }
#' @export
simulate_survival <- function(
    signal_parameters,
    noise_parameters = 0,
    complexity = 1,
    data_control = NULL,
    maximum_achievable_cindex,
    baseline_hazard = 1,
    censoring_rate,
    model = c("coxph", "lasso", "ridge", "rf", "xgboost"),
    metric = "calibration_slope",
    target_performance,
    n_reps_total = 1000,
    mean_or_assurance = "assurance",
    ...
) {
  model <- check_pmsims_args(model, c("coxph", "lasso", "ridge", "rf", "xgboost"))
  validate_metric_constraints(
    metric = metric,
    target_performance = target_performance,
    maximum_achievable_performance = maximum_achievable_cindex
  )
  validate_complexity(complexity)
  dc <- resolve_data_control(data_control, complexity)
  
  candidate_features        <- signal_parameters + noise_parameters
  proportion_noise_features <- noise_parameters / candidate_features
  
  # Tune the data-generating function under the SAME data configuration.
  tune_param <- call_tuner(
    survival_tuning,
    required = list(
      target_prevalence         = 1 - censoring_rate,
      target_performance        = maximum_achievable_cindex,
      candidate_features        = candidate_features,
      proportion_noise_features = proportion_noise_features,
      .complexity               = complexity
    ),
    dc = dc
  )
  
  data_spec <- list(
    type = "survival",
    args = make_data_args(
      signal_parameters, noise_parameters, complexity, dc,
      extra = list(
        baseline_hazard = baseline_hazard,
        beta_signal     = get_param(tune_param, "beta_signal"),
        censoring_rate  = censoring_rate
      )
    )
  )
  
  data_function  <- default_data_generators(data_spec)
  outcome_type   <- attr(data_function, "outcome")
  model_function <- default_model_generators(outcome_type, model)
  
  metric <- ifelse(metric == "calibration_slope", "calib_slope", metric)
  
  simulate_custom_args <- utils::modifyList(
    list(
      metric_function = default_metric_generator(metric, data_function),
      target_performance = target_performance,
      c_statistic = maximum_achievable_cindex,
      data_function = data_function,
      model_function = model_function,
      n_reps_total = n_reps_total,
      n_reps_per = 20,
      method = "mlpwr",
      mean_or_assurance = mean_or_assurance,
      test_n = 30000
    ),
    list(...)
  )
  
  suppressWarnings(
    output <- do.call(simulate_custom, simulate_custom_args)
  )
  
  metric_2 <- if (metric %in% c("csse","calib_slope")) "cindex" else "calib_slope"
  
  test_n <- 30000
  metric_function_2 <- default_metric_generator(metric_2, data_function)
  
  data_2 <- data_function(output$min_n)
  test_data_2 <- data_function(test_n)
  fit_2 <- model_function(data_2)
  metric_2_at_n <- metric_function_2(test_data_2, fit_2, model)
  
  output$metric_2_at_n <- metric_2_at_n
  output$metric_2 <- metric_2
  
  # Append input parameters
  output$parameters <- signal_parameters
  output$noise_parameters <- noise_parameters
  output$complexity <- complexity
  output$nonlinear_strength <- dc$nonlinear_strength
  output$correlation <- dc$correlation
  output$predictor_distribution <- dc$predictor_distribution
  output$predictor_type <- dc$predictor_type
  output$binary_predictor_prevalence <- dc$binary_prevalence
  output$baseline_hazard <- baseline_hazard
  output$censoring_rate <- censoring_rate
  output$cstatistic <- maximum_achievable_cindex
  output$model <- model
  output$metric <- metric
  output$n_reps_total <- n_reps_total
  output$mean_or_assurance <- mean_or_assurance
  class(output) <- "pmsims"
  output
}