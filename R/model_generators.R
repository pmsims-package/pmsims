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
    model <- function(data) {
      logistic_model <- glm("y ~ .", data = data, family = "binomial")
    }
  } else if (opts$outcome == "linear") {
    model <- function(data) {
      linear_model <- glm("y ~ .", data = data, family = "gaussian")
    }
  } else if (opts$outcome == "survival") {
    model <- function(data) {
      cox_model <- survival::cox.ph("Surv(time, event) ~ .", data = data)
      }
    }
  return(list(model = model))
}

