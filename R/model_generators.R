#' default_model_generators
#'
#' @param outcome type of outcome, possible options are: "binary".
#'
#' @return
#' @export
#'
#' @examples default_model_generators("binary")
default_model_generators <- function(outcome) {
  if (outcome == "binary") {
    model_function <- function(data) {
      glm("y ~ .", data = data, family = "binomial")
    }
  } else if (outcome == "continuous") {
    model_function <- function(data) {
      glm("y ~ .", data = data, family = "gaussian")
    }
  } else if (outcome == "survival") {
    model_function <- function(data) {
      formula <- as.formula("survival::Surv(time, event) ~ .")
      survival::coxph(formula, data = data)
      }
    }
  return(model_function)
}

