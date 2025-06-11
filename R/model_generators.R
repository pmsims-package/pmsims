#' default_model_generators
#'
#' @param outcome type of outcome, possible options are: "binary".
#'
#' @return
#' @export
#'
#' @examples default_model_generators("binary")
default_models <- list(
  binary = list(
    glm = function(d) {
      glm("y ~ .", data = d, family = "binomial")
    },
    lasso = function(d) {
      d <- as.matrix(d)
      x <- d[, -1]
      y <- d[, 1]
      glmnet::cv.glmnet(
        x,
        y,
        family = "binomial"
      )
    },
    rf = function(d) {
      x <- d[, -1]
      y <- d[, 1]
      ranger::ranger(
        x = x,
        y = y,
        probability = TRUE
      )
    }
  ),
  continuous = list(
    lm = function(d) {
      glm("y ~ .", data = d, family = "gaussian")
    }
  ),
  survival = list(
    coxph = function(d) {
      formula <- as.formula("survival::Surv(time, event) ~ .")
      survival::coxph(formula, data = d)
    }
  )
)

default_model_generators <- function(outcome, model) {
  if (model %in% names(default_models[[outcome]])) {
    model_function <- default_models[[outcome]][[model]]
    attr(model_function, "model") <- model
    return(model_function)
  } else {
    stop(paste0(
      "Model \"",
      model,
      "\" not found for outcome \"",
      outcome,
      "\""
    ))
  }
}
