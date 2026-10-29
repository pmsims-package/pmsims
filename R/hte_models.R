fit_linear_regression <- function(train_data) {
  y_name <- colnames(train_data)[1]
  w_name <- colnames(train_data)[2]
  x_names <- colnames(train_data)[3:ncol(train_data)]
  
  formula_str <- paste0(y_name, " ~ ", w_name, " * (", paste(x_names, collapse = " + "), ")")
  fit <- stats::lm(stats::as.formula(formula_str), data = train_data)
  
  structure(
    list(
      model = fit,
      predict = function(new_data) {
        data_w1 <- new_data; data_w1[[w_name]] <- 1
        data_w0 <- new_data; data_w0[[w_name]] <- 0
        
        pred_1 <- stats::predict(fit, newdata = data_w1)
        pred_0 <- stats::predict(fit, newdata = data_w0)
        return(as.numeric(pred_1 - pred_0))
      }
    ),
    class = "hte_linear"
  )
}

# 1. Penalized Regression
fit_penalized_regression <- function(train_data, alpha = 1) {
  y_name <- colnames(train_data)[1]
  w_name <- colnames(train_data)[2]
  x_names <- colnames(train_data)[3:ncol(train_data)]
  
  formula_str <- paste0("~ ", w_name, " * (", paste(x_names, collapse = " + "), ")")
  x_matrix <- stats::model.matrix(stats::as.formula(formula_str), data = train_data)[, -1]
  y_vector <- train_data[[y_name]]
  
  cv_fit <- glmnet::cv.glmnet(x = x_matrix, y = y_vector, alpha = alpha, nfolds = 10)
  
  structure(
    list(
      model = cv_fit,
      predict = function(new_data) {
        data_w1 <- new_data; data_w1[[w_name]] <- 1
        data_w0 <- new_data; data_w0[[w_name]] <- 0
        
        m1 <- stats::model.matrix(stats::as.formula(formula_str), data = data_w1)[, -1]
        m0 <- stats::model.matrix(stats::as.formula(formula_str), data = data_w0)[, -1]
        
        pred_1 <- stats::predict(cv_fit, newx = m1, s = "lambda.min")
        pred_0 <- stats::predict(cv_fit, newx = m0, s = "lambda.min")
        return(as.numeric(pred_1 - pred_0))
      }
    ),
    class = "hte_penalized"
  )
}

# 2. MOB Tree (Fixed coef extraction via standard S3 coef())
fit_mob_l_tree <- function(train_data, alpha = 0.10, minsize = 100) {
  # 1. Ensure data is a data frame and extract column names
  train_df <- as.data.frame(train_data)
  y_name <- colnames(train_df)[1]
  w_name <- colnames(train_df)[2]
  x_names <- colnames(train_df)[3:ncol(train_df)]
  
  # 2. Convert treatment explicitly to numeric 0/1 to eliminate factor contrast errors
  train_df[[w_name]] <- as.numeric(as.character(train_df[[w_name]]))
  
  y <- train_df[[y_name]]
  w <- train_df[[w_name]]
  x_mat <- as.matrix(train_df[, x_names])
  
  # 3. LASSO Prognostic Pre-Selection (separate regression per treatment arm)
  sel0 <- character(0)
  sel1 <- character(0)
  
  if (sum(w == 0) > 10) {
    cv0 <- glmnet::cv.glmnet(x_mat[w == 0, , drop = FALSE], y[w == 0], alpha = 1)
    coef0 <- as.matrix(glmnet::coef.glmnet(cv0, s = "lambda.1se"))
    sel0 <- rownames(coef0)[coef0[, 1] != 0 & rownames(coef0) != "(Intercept)"]
  }
  
  if (sum(w == 1) > 10) {
    cv1 <- glmnet::cv.glmnet(x_mat[w == 1, , drop = FALSE], y[w == 1], alpha = 1)
    coef1 <- as.matrix(glmnet::coef.glmnet(cv1, s = "lambda.1se"))
    sel1 <- rownames(coef1)[coef1[, 1] != 0 & rownames(coef1) != "(Intercept)"]
  }
  
  # Union of selected prognostic biomarkers
  x_star <- unique(c(sel0, sel1))
  
  # 4. Construct Node Model Formula
  node_terms <- if (length(x_star) > 0) {
    paste(c(w_name, x_star), collapse = " + ")
  } else {
    w_name
  }
  
  mob_formula <- stats::as.formula(
    paste0(y_name, " ~ ", node_terms, " | ", paste(x_names, collapse = " + "))
  )
  
  # 5. Fit MOB via partykit::glmtree
  mob_fit <- partykit::glmtree(
    formula = mob_formula,
    data = train_df,
    alpha = alpha,
    minsize = minsize,
    parm = 2,
    bonferroni = TRUE
  )
  
  # 6. Standardized Wrapper Output with Dimension-Safe Predictor
  structure(
    list(
      model = mob_fit,
      predict = function(new_data) {
        new_df <- as.data.frame(new_data)
        new_df[[w_name]] <- as.numeric(as.character(new_df[[w_name]]))
        
        # Get terminal node predictions using partykit's built-in predict method
        # type = "response" or coef matrix extraction
        leaf_coefs <- stats::coef(mob_fit)
        node_ids <- partykit::predict.party(mob_fit, newdata = new_df, type = "node")
        
        # Handle 1-node tree (stump) vs multi-node tree matrix dimensionality
        if (is.null(dim(leaf_coefs))) {
          # Stump tree: leaf_coefs is a 1D vector
          tau_val <- leaf_coefs[w_name]
          tau_hat <- rep(tau_val, length(node_ids))
        } else {
          # Multi-node tree: leaf_coefs is a 2D matrix
          tau_hat <- sapply(node_ids, function(id) {
            node_row <- leaf_coefs[as.character(id), ]
            return(node_row[w_name])
          })
        }
        
        # Always return as a 1D unclassed numeric vector
        return(as.vector(as.numeric(tau_hat)))
      }
    ),
    class = "hte_mob_l"
  )
}

# 3. Causal Forest
make_encoder <- function(x_df) {
  x_df <- as.data.frame(x_df)
  x_df[] <- lapply(x_df, function(v) if (is.character(v)) factor(v) else v)
  vars <- names(x_df)
  levs <- lapply(Filter(is.factor, x_df), levels)
  
  function(new_df) {
    new_df <- as.data.frame(new_df)[vars]
    for (v in names(levs)) new_df[[v]] <- factor(new_df[[v]], levels = levs[[v]])
    stats::model.matrix(
      ~ ., data = new_df,
      contrasts.arg = lapply(new_df[names(levs)], stats::contrasts, contrasts = FALSE)
    )[, -1, drop = FALSE]
  }
}

fit_causal_forest <- function(train_data) {
  train_df <- as.data.frame(train_data)
  x_names <- colnames(train_df)[3:ncol(train_df)]
  
  Y <- train_df[[1]]
  W <- as.numeric(as.character(train_df[[2]]))
  
  encode <- make_encoder(train_df[x_names])
  X <- encode(train_df)
  
  cf_fit <- grf::causal_forest(X = X, Y = Y, W = W, tune.parameters = "all")
  
  structure(
    list(
      model = cf_fit,
      predict = function(new_data) {
        X_new <- encode(new_data)
        as.numeric(stats::predict(cf_fit, newdata = X_new)$predictions)
      }
    ),
    class = "hte_causal_forest"
  )
}