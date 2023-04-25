default_model_generators <- function(type) {
  if (type == "binary") {
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

    return(list(
      model = model,
      metric = metric
    ))
  }
}
