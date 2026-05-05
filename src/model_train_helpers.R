#' Helper functions for Random Forest model training workflow.
#'
#' This file contains reusable utility functions used in
#' `scripts/03_model_train.R` to:
#' - calculate model performance metrics,
#' - build model formulas,
#' - fit Random Forest models,
#' - perform automated predictor subset selection, and
#' - create validation plots.

#' Calculate Random Forest performance metrics.
#'
#' Computes common regression metrics from observed and predicted values.
#'
#' @param observed numeric. Vector of observed response values.
#' @param predicted numeric. Vector of predicted response values.
#'
#' @return data.frame with number of observations, MSE, RMSE, RMSE ratio,
#'   R-squared, and bias.
calc_rf_metrics <- function(observed, predicted) {
  keep <- complete.cases(observed, predicted)
  observed <- observed[keep]
  predicted <- predicted[keep]

  mse <- mean((observed - predicted)^2)
  rmse <- sqrt(mse)
  rmse_pct <- rmse / mean(observed)
  rss <- sum((observed - predicted)^2)
  tss <- sum((observed - mean(observed))^2)
  r_squared <- 1 - rss / tss
  bias <- mean(predicted - observed)

  data.frame(
    n = length(observed),
    mse = mse,
    rmse = rmse,
    rmse_pct = rmse_pct,
    r_squared = r_squared,
    bias = bias
  )
}

#' Create a model formula from response and predictors.
#'
#' @param response character. Name of the response variable.
#' @param predictors character vector. Names of predictor variables.
#'
#' @return formula object used in model fitting.
make_rf_formula <- function(response, predictors) {
  as.formula(paste(response, '~', paste(predictors, collapse = ' + ')))
}

#' Fit a Random Forest regression model.
#'
#' @param train_data data.frame. Training data containing response and predictors.
#' @param predictors character vector. Predictor variable names.
#' @param response character. Response variable name.
#' @param ntree numeric. Number of trees to grow.
#' @param mtry numeric. Number of predictors sampled at each split.
#' @param nodesize numeric. Minimum terminal node size.
#' @param seed numeric. Random seed for reproducibility.
#'
#' @return randomForest model object.
fit_rf <- function(
  train_data,
  predictors,
  response,
  ntree = 500,
  mtry = 3,
  nodesize = 10,
  seed = 123
) {
  if (missing(response) || is.null(response) || !nzchar(response)) {
    stop('fit_rf(): response must be provided explicitly.')
  }

  set.seed(seed)
  randomForest::randomForest(
    formula = make_rf_formula(response, predictors),
    data = train_data[, c(response, predictors), drop = F],
    importance = T,
    ntree = ntree,
    mtry = mtry,
    nodesize = nodesize
  )
}

#' Select predictor subset for Random Forest model.
#'
#' Uses permutation importance from a full model and evaluates reduced
#' top-k predictor sets. Selects the simplest model whose test RMSE is
#' within a tolerance of the best RMSE.
#'
#' @param train_data data.frame. Training dataset.
#' @param test_data data.frame. Testing dataset.
#' @param full_predictors character vector. Candidate predictor names.
#' @param response character. Response variable name.
#' @param subset_sizes numeric vector. Candidate predictor subset sizes.
#' @param rmse_tolerance numeric. Relative RMSE tolerance above best model.
#' @param ntree numeric. Number of trees for selection models.
#' @param mtry numeric. mtry value for selection models.
#' @param nodesize numeric. Minimum terminal node size.
#' @param seed numeric. Random seed for reproducibility.
#'
#' @return list with full model, variable importance, selection results,
#'   selected model row, and selected predictor names.
select_rf_variables <- function(
  train_data,
  test_data,
  full_predictors,
  response,
  subset_sizes = c(4, 5, 6, 7, 8, 10, 12, 15),
  rmse_tolerance = 0.02,
  ntree = 500,
  mtry = 3,
  nodesize = 10,
  seed = 123
) {
  if (missing(response) || is.null(response) || !nzchar(response)) {
    stop('select_rf_variables(): response must be provided explicitly.')
  }

  # 1) fit full Random Forest model
  max_rf <- fit_rf(
    train_data = train_data,
    predictors = full_predictors,
    response = response,
    ntree = ntree,
    mtry = mtry,
    nodesize = nodesize,
    seed = seed
  )

  # 2) rank predictors by permutation importance (%IncMSE)
  importance_mat <- randomForest::importance(max_rf, type = 1)
  importance_df <- data.frame(
    predictor = rownames(importance_mat),
    inc_mse = importance_mat[, 1],
    row.names = NULL
  ) %>%
    dplyr::arrange(dplyr::desc(inc_mse))

  ranked_predictors <- importance_df$predictor

  # 3) fit reduced models using top-k predictors
  subset_sizes <- sort(unique(pmin(subset_sizes, length(full_predictors))))
  subset_sizes <- subset_sizes[subset_sizes > 0]

  selection_results <- lapply(subset_sizes, function(k) {
    predictors_k <- ranked_predictors[seq_len(k)]

    rf_k <- fit_rf(
      train_data = train_data,
      predictors = predictors_k,
      response = response,
      ntree = ntree,
      mtry = min(mtry, length(predictors_k)),
      nodesize = nodesize,
      seed = seed
    )

    train_pred <- predict(rf_k, newdata = train_data)
    test_pred <- predict(rf_k, newdata = test_data)

    train_metrics <- calc_rf_metrics(train_data[[response]], train_pred)
    test_metrics <- calc_rf_metrics(test_data[[response]], test_pred)

    data.frame(
      n_predictors = k,
      predictors = paste(predictors_k, collapse = ', '),
      train_rmse = train_metrics$rmse,
      train_rmse_pct = train_metrics$rmse_pct,
      train_r_squared = train_metrics$r_squared,
      test_rmse = test_metrics$rmse,
      test_rmse_pct = test_metrics$rmse_pct,
      test_r_squared = test_metrics$r_squared,
      test_bias = test_metrics$bias
    )
  }) %>%
    dplyr::bind_rows()

  # 4) select simplest model within RMSE tolerance
  best_rmse <- min(selection_results$test_rmse, na.rm = T)
  rmse_cutoff <- best_rmse * (1 + rmse_tolerance)

  selected_model_row <- selection_results %>%
    dplyr::filter(test_rmse <= rmse_cutoff) %>%
    dplyr::arrange(n_predictors, test_rmse) %>%
    dplyr::slice(1)

  selected_predictors <- strsplit(selected_model_row$predictors, ', ')[[1]]

  list(
    max_rf = max_rf,
    variable_importance = importance_df,
    selection_results = selection_results,
    selected_model_row = selected_model_row,
    selected_predictors = selected_predictors
  )
}

#' Plot validation diagnostics for Random Forest predictions.
#'
#' Produces:
#' - observed vs predicted scatter plot with 1:1 line, and
#' - residual plot against predicted values.
#'
#' @param observed numeric. Observed response values.
#' @param predicted numeric. Predicted response values.
#' @param model_label character. Label used in axis titles.
#'
#' @return No returned object; creates base R plots.
plot_rf_validation <- function(
  observed,
  predicted,
  model_label = 'Global Random Forest'
) {
  par(
    mfrow = c(1, 2),
    mar = c(5, 5, 2, 2),
    las = 1,
    pch = 19,
    cex = 1.2
  )

  plot(
    observed, predicted,
    xlab = 'Observed',
    ylab = paste(model_label, 'Predicted'),
    main = '',
    xlim = range(observed, predicted, na.rm = T),
    ylim = range(observed, predicted, na.rm = T),
    cex.lab = 1.2,
    cex.axis = 1,
    col = 'black'
  )
  abline(0, 1, col = 'red', lwd = 2, lty = 2)

  plot(
    predicted, observed - predicted,
    xlab = paste(model_label, 'Predicted'),
    ylab = 'Residuals (Observed - Predicted)',
    main = '',
    cex.lab = 1.2,
    cex.axis = 1,
    col = 'black'
  )
  abline(h = 0, col = 'red', lwd = 2, lty = 2)
}
