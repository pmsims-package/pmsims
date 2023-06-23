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

    # Get performance must be a function of data and a model object
    metric <- function(data, model) {
      y <- data[, 1]
      x <- data[, -1]
      y_hat <- predict(model, x, type = "response")
      auc <- pROC::auc(y, as.numeric(y_hat), quiet = TRUE)
      return(auc[1])
    }
  } else if (opts$outcome == "linear") {
    f <- function() {
      return(data.frame())
    }
  } else if (opts$outcome == "survival") {
    f <- function(params = parameters)
                  

    return(list(
      model = model,
      metric = metric
    ))
  }
}
