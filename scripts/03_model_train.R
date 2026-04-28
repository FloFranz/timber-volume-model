#------------------------------------------------------------------------------
# Name:         03_model_train.R
# Description:  Script models the growing stock (GS) based on previously
#               derived metrics in terrestrial sample plots using a
#               Random Forest regression model. The workflow includes
#               automated predictor selection, mtry tuning, and model validation.
# Authors:      Georgia Reeves, Florian Franz, Ryan Carroll
# Contact:      georgia.reeves@nw-fva.de, florian.franz@nw-fva.de
#------------------------------------------------------------------------------


# source setup script
source('src/setup.R', local = TRUE)



# 01 - user settings
#-------------------------------------

# input data
rs_plot_metrics_path <- file.path(processed_data_dir, 'plot_metrics_pc_solling_incl_forest_type.RDS')

# model output
final_model_path <- file.path(processed_data_dir, 'global_rf_model.rds')

# reproducibility and split
set_seed <- 123
train_fraction <- 0.70

# full set of candidate predictor variables
full_rf_predictors <- c(
  'zmean', 'zsd', 'zmin', 'zmax',
  'zq1', 'zq5', 'zq10', 'zq20', 'zq25', 'zq30', 'zq40', 'zq50',
  'zq60', 'zq70', 'zq75', 'zq80', 'zq90', 'zq95', 'zq99',
  'zskew', 'zkurt', 'zcv', 'zcrr', 'ndvi',
  'dominant_species'
)

# variable-selection settings
subset_sizes_to_test <- c(4, 5, 6, 7, 8, 10, 12, 15, length(full_rf_predictors))
rmse_tolerance <- 0.02

# RF settings for variable-selection models
selection_ntree <- 500
selection_nodesize <- 10
selection_mtry <- 3

# final global RF settings
final_ntree <- 500
final_nodesize <- 10
mtry_tune_ntree <- 400


# 02 - helper functions
#-------------------------------------

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

make_rf_formula <- function(response, predictors) {
  as.formula(paste(response, '~', paste(predictors, collapse = ' + ')))
}

fit_rf <- function(train_data,
                   predictors,
                   response = 'vol_ha',
                   ntree = 500,
                   mtry = 3,
                   nodesize = 10,
                   seed = 123) {
  set.seed(seed)
  randomForest::randomForest(
    formula = make_rf_formula(response, predictors),
    data = train_data[, c(response, predictors), drop = FALSE],
    importance = TRUE,
    ntree = ntree,
    mtry = mtry,
    nodesize = nodesize
  )
}

select_rf_variables <- function(train_data,
                                test_data,
                                full_predictors,
                                response = 'vol_ha',
                                subset_sizes = c(4, 5, 6, 7, 8, 10, 12, 15),
                                rmse_tolerance = 0.02,
                                ntree = 500,
                                mtry = 3,
                                nodesize = 10,
                                seed = 123) {

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
  best_rmse <- min(selection_results$test_rmse, na.rm = TRUE)
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

plot_rf_validation <- function(observed, predicted, model_label = 'Global Random Forest') {
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
    xlim = range(observed, predicted, na.rm = TRUE),
    ylim = range(observed, predicted, na.rm = TRUE),
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


# 03 - read and prepare data
#-------------------------------------

# plot-level metrics already include key, kspnr and dominant_species
plot_metrics_dataset <- readr::read_rds(rs_plot_metrics_path)

# dominant_species as factor
# in plot metrics this can be encoded as 1 (LB) and 2 (NB)
plot_metrics_dataset$dominant_species <- dplyr::case_when(
  plot_metrics_dataset$dominant_species == 1 ~ 'LB',
  plot_metrics_dataset$dominant_species == 2 ~ 'NB',
  TRUE ~ as.character(plot_metrics_dataset$dominant_species)
)

plot_metrics_dataset$dominant_species <- factor(
  plot_metrics_dataset$dominant_species,
  levels = c('NB', 'LB')
)

# modelling table with response + full candidate predictors
global_model_data <- plot_metrics_dataset %>%
  sf::st_drop_geometry() %>%
  dplyr::select(vol_ha, dplyr::all_of(full_rf_predictors)) %>%
  na.omit()


# 04 - train/test split
#-------------------------------------

set.seed(set_seed)
global_train_indices <- sample(
  seq_len(nrow(global_model_data)),
  size = train_fraction * nrow(global_model_data)
)

global_train <- global_model_data[global_train_indices, ]
global_test <- global_model_data[-global_train_indices, ]

cat('Training observations:', nrow(global_train), '\n')
cat('Testing observations:', nrow(global_test), '\n')
cat('Dominant species counts in training data:\n')
print(table(global_train$dominant_species))
cat('Dominant species counts in testing data:\n')
print(table(global_test$dominant_species))


# 05 - automated variable selection (RF_2 to RF_4 replacement)
#-------------------------------------

rf_selection <- select_rf_variables(
  train_data = global_train,
  test_data = global_test,
  full_predictors = full_rf_predictors,
  response = 'vol_ha',
  subset_sizes = subset_sizes_to_test,
  rmse_tolerance = rmse_tolerance,
  ntree = selection_ntree,
  mtry = selection_mtry,
  nodesize = selection_nodesize,
  seed = set_seed
)

# keep object names used in existing workflow
Global_train_RF_max <- rf_selection$max_rf
RF_variable_importance <- rf_selection$variable_importance
RF_selection_results <- rf_selection$selection_results
RF_selected_model_row <- rf_selection$selected_model_row
selected_predictors <- rf_selection$selected_predictors

cat('\nVariable importance from full/max RF model:\n')
print(RF_variable_importance)

cat('\nAutomated RF subset-selection results:\n')
print(RF_selection_results)

cat('\nSelected predictor set:\n')
print(selected_predictors)

cat('\nSelected model row:\n')
print(RF_selected_model_row)

randomForest::varImpPlot(Global_train_RF_max, type = 1)


# 06 - final global RF mtry tuning and model fitting
#-------------------------------------

global_train_df <- as.data.frame(global_train)
global_train_df$dominant_species <- as.factor(global_train_df$dominant_species)

x_train <- global_train_df[, selected_predictors, drop = FALSE]
y_train <- global_train_df$vol_ha

set.seed(set_seed)
mtry_tune <- randomForest::tuneRF(
  x = x_train,
  y = y_train,
  stepFactor = 1.5,
  improve = 0.01,
  ntreeTry = mtry_tune_ntree,
  trace = TRUE
)

best_mtry <- mtry_tune[which.min(mtry_tune[, 'OOBError']), 'mtry']

selected_with_response <- c('vol_ha', selected_predictors)
x_train <- global_train_df[, selected_with_response, drop = FALSE]

global_rf_model <- randomForest::randomForest(
  formula = make_rf_formula('vol_ha', selected_predictors),
  data = x_train,
  importance = TRUE,
  ntree = final_ntree,
  mtry = best_mtry,
  nodesize = final_nodesize
)

cat('\nFinal global RF model:\n')
print(global_rf_model)
cat('\nBest mtry for final global RF model:', best_mtry, '\n')
cat('\nFinal global RF variable importance:\n')
print(randomForest::importance(global_rf_model))
randomForest::varImpPlot(global_rf_model, type = 1)


# 07 - model performance (training and testing)
#-------------------------------------

# training predictions and metrics
RF_Global_train_pred <- predict(global_rf_model, newdata = global_train)
RF_Global_train_metrics <- calc_rf_metrics(
  observed = global_train$vol_ha,
  predicted = RF_Global_train_pred
)

cat('\nFinal global RF training metrics:\n')
print(RF_Global_train_metrics)

# testing predictions and metrics
RF_Global_pred <- predict(global_rf_model, newdata = global_test)
summary(RF_Global_pred)

RF_Global_test_metrics <- calc_rf_metrics(
  observed = global_test$vol_ha,
  predicted = RF_Global_pred
)

cat('\nFinal global RF testing metrics:\n')
print(RF_Global_test_metrics)

# object names for continuity
RF_Global_rmse <- RF_Global_test_metrics$rmse
RF_Global_rmse_pct <- RF_Global_test_metrics$rmse_pct
RF_Global_rsq <- RF_Global_test_metrics$r_squared
RF_Global_bias <- RF_Global_test_metrics$bias

# validation plots
plot_rf_validation(
  observed = global_test$vol_ha,
  predicted = RF_Global_pred,
  model_label = 'Global RF'
)


# 08 - save final model
#-------------------------------------

if (!file.exists(final_model_path)) {
  saveRDS(global_rf_model, final_model_path)
  cat('\nFinal RF model written to:\n')
  cat(final_model_path, '\n')
} else {
  cat('\nFinal RF model already exists at:\n')
  cat(final_model_path, '\n')
}


# 09 - optional exports
#-------------------------------------

write.csv(
  RF_variable_importance,
  file.path(processed_data_dir, 'RF_full_model_variable_importance.csv'),
  row.names = FALSE
)

write.csv(
  RF_selection_results,
  file.path(processed_data_dir, 'RF_automated_variable_selection_results.csv'),
  row.names = FALSE
)

write.csv(
  data.frame(selected_predictors = selected_predictors),
  file.path(processed_data_dir, 'global_rf_selected_predictors.csv'),
  row.names = FALSE
)


