# Turns covariates into a numeric matrix using the levels seen in the training
# data, so new data with missing or unseen categories still works: synthetic data
# may not reproduce rare categories. An unseen category gets zeros on all dummies.
# Covariates constant in the training data are dropped.
make_encoder <- function(x_df, full_dummies = TRUE) {
  x_df <- as.data.frame(x_df)
  x_df[] <- lapply(x_df, function(v) if (is.character(v)) factor(v) else v)
  x_df <- x_df[, vapply(x_df, function(v) length(unique(v)) > 1, logical(1)), drop = FALSE]
  vars <- names(x_df)
  levs <- lapply(Filter(is.factor, x_df), levels)
  
  function(new_df) {
    new_df <- as.data.frame(new_df)[vars]
    for (v in names(levs)) new_df[[v]] <- factor(new_df[[v]], levels = levs[[v]])
    contrast_list <- if (full_dummies) {
      lapply(new_df[names(levs)], stats::contrasts, contrasts = FALSE)
    }
    frame <- stats::model.frame(~ ., data = new_df, na.action = stats::na.pass)
    X <- stats::model.matrix(~ ., data = frame, contrasts.arg = contrast_list)[, -1, drop = FALSE]
    X[is.na(X)] <- 0
    colnames(X) <- make.names(colnames(X), unique = TRUE)
    X
  }
}

fit_linear_regression <- function(train_data) {
  train_df <- as.data.frame(train_data)
  y_name <- colnames(train_df)[1]
  w_name <- colnames(train_df)[2]
  encode <- make_encoder(train_df[-(1:2)], full_dummies = FALSE)
  X <- encode(train_df)
  
  formula_str <- paste0(y_name, " ~ ", w_name, " * (", paste(colnames(X), collapse = " + "), ")")
  fit <- stats::lm(stats::as.formula(formula_str), data = data.frame(train_df[1:2], X))
  
  structure(
    list(
      model = fit,
      predict = function(new_data) {
        X_new <- encode(new_data)
        data_w1 <- data.frame(1, X_new); names(data_w1)[1] <- w_name
        data_w0 <- data.frame(0, X_new); names(data_w0)[1] <- w_name
        
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
  train_df <- as.data.frame(train_data)
  w_name <- colnames(train_df)[2]
  encode <- make_encoder(train_df[-(1:2)], full_dummies = FALSE)
  X <- encode(train_df)
  
  formula_str <- paste0("~ ", w_name, " * (", paste(colnames(X), collapse = " + "), ")")
  design <- function(w, X) {
    d <- data.frame(w, X); names(d)[1] <- w_name
    stats::model.matrix(stats::as.formula(formula_str), data = d)[, -1]
  }
  
  x_matrix <- design(train_df[[2]], X)
  cv_fit <- glmnet::cv.glmnet(x = x_matrix, y = train_df[[1]], alpha = alpha, nfolds = 10)
  
  structure(
    list(
      model = cv_fit,
      predict = function(new_data) {
        X_new <- encode(new_data)
        pred_1 <- stats::predict(cv_fit, newx = design(1, X_new), s = "lambda.min")
        pred_0 <- stats::predict(cv_fit, newx = design(0, X_new), s = "lambda.min")
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