#------------------------------------------------------------------------------
# Name:         03_model_train.R
# Description:  Script models the growing stock (GS) based on previously
#               derived metrics in terrestrial sample plots using a
#               Random Forest regression model. The workflow includes
#               automated predictor selection, mtry tuning, and model validation.
# Authors:      Georgia Reeves, Florian Franz, Ryan Carroll
# Contact:      georgia.reeves@nw-fva.de, florian.franz@nw-fva.de
#------------------------------------------------------------------------------


# source setup and helper scripts
source('src/setup.R', local = T)
source('src/model_train_helpers.R', local = T)



# 01 - user settings
#-------------------------------------

# input data
rs_plot_metrics_path <- file.path(processed_data_dir, 'plot_metrics_pc_solling_incl_forest_type.RDS')

# dataset identifier used in output file names
dataset_id <- 'solling'

# response variable (e.g. 'vol_ha' or 'sum_vol_ha')
response_var <- 'vol_ha'

# model output
final_model_path <- file.path(processed_data_dir, paste0('global_rf_model_', dataset_id, '.rds'))

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


# 02 - read and prepare data
#-------------------------------------

# plot-level metrics already include key, kspnr and dominant_species
if (!file.exists(rs_plot_metrics_path)) {
  stop('Input file does not exist: ', rs_plot_metrics_path)
}
plot_metrics_dataset <- readr::read_rds(rs_plot_metrics_path)

# dominant_species as factor (if available)
# in plot metrics this can be encoded as 1 (LB) and 2 (NB)
if ('dominant_species' %in% names(plot_metrics_dataset)) {
  plot_metrics_dataset$dominant_species <- dplyr::case_when(
    plot_metrics_dataset$dominant_species == 1 ~ 'LB',
    plot_metrics_dataset$dominant_species == 2 ~ 'NB',
    T ~ as.character(plot_metrics_dataset$dominant_species)
  )
  
  plot_metrics_dataset$dominant_species <- factor(
    plot_metrics_dataset$dominant_species,
    levels = c('NB', 'LB')
  )
}

# validate response and available predictors
plot_metrics_no_geom <- sf::st_drop_geometry(plot_metrics_dataset)
if (!response_var %in% names(plot_metrics_no_geom)) {
  stop(
    'Configured response_var (\'', response_var,
    '\') not found in input data. Available columns: ',
    paste(names(plot_metrics_no_geom), collapse = ', ')
  )
}

available_predictors <- intersect(full_rf_predictors, names(plot_metrics_no_geom))
missing_predictors <- setdiff(full_rf_predictors, available_predictors)

if (length(missing_predictors) > 0) {
  warning(
    'The following candidate predictors are missing and will be skipped: ',
    paste(missing_predictors, collapse = ', ')
  )
}

if (length(available_predictors) == 0) {
  stop('None of the configured predictors are available in input data.')
}

cat('\n--- Model run settings ---\n')
cat('dataset_id: ', dataset_id, '\n', sep = '')
cat('response_var: ', response_var, '\n', sep = '')
cat('n_predictors_available: ', length(available_predictors), '\n', sep = '')
cat('predictors_available: ', paste(available_predictors, collapse = ', '), '\n', sep = '')
cat('--------------------------\n\n')

# modelling table with response + full candidate predictors
global_model_data <- plot_metrics_dataset %>%
  sf::st_drop_geometry() %>%
  dplyr::select(dplyr::all_of(c(response_var, available_predictors))) %>%
  na.omit()


# 03 - train/test split
#-------------------------------------

if (!is.numeric(train_fraction) || length(train_fraction) != 1 ||
    train_fraction <= 0 || train_fraction >= 1) {
  stop('train_fraction must be a single numeric value in (0, 1).')
}

if (nrow(global_model_data) < 2) {
  stop('Not enough observations after filtering/NA removal. Need at least 2 rows.')
}

train_size <- floor(train_fraction * nrow(global_model_data))
train_size <- max(1, min(train_size, nrow(global_model_data) - 1))

set.seed(set_seed)
global_train_indices <- sample(
  seq_len(nrow(global_model_data)),
  size = train_size
)

global_train <- global_model_data[global_train_indices, ]
global_test <- global_model_data[-global_train_indices, ]

cat('Training observations:', nrow(global_train), '\n')
cat('Testing observations:', nrow(global_test), '\n')
cat('Dominant species counts in training data:\n')
print(table(global_train$dominant_species))
cat('Dominant species counts in testing data:\n')
print(table(global_test$dominant_species))


# 04 - automated variable selection
#-------------------------------------

rf_selection <- select_rf_variables(
  train_data = global_train,
  test_data = global_test,
  full_predictors = available_predictors,
  response = response_var,
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

cat('\n--- Selected model predictors ---\n')
cat('n_predictors_selected: ', length(selected_predictors), '\n', sep = '')
cat('predictors_selected: ', paste(selected_predictors, collapse = ', '), '\n', sep = '')
cat('---------------------------------\n\n')

cat('\nVariable importance from full/max RF model:\n')
print(RF_variable_importance)

cat('\nAutomated RF subset-selection results:\n')
print(RF_selection_results)

cat('\nSelected predictor set:\n')
print(selected_predictors)

cat('\nSelected model row:\n')
print(RF_selected_model_row)

randomForest::varImpPlot(Global_train_RF_max, type = 1)


# 05 - final global RF mtry tuning and model fitting
#-------------------------------------

global_train_df <- as.data.frame(global_train)
if ('dominant_species' %in% names(global_train_df)) {
  global_train_df$dominant_species <- as.factor(global_train_df$dominant_species)
}

x_train <- global_train_df[, selected_predictors, drop = F]
y_train <- global_train_df[[response_var]]

set.seed(set_seed)
mtry_tune <- randomForest::tuneRF(
  x = x_train,
  y = y_train,
  stepFactor = 1.5,
  improve = 0.01,
  ntreeTry = mtry_tune_ntree,
  trace = T
)

best_mtry <- mtry_tune[which.min(mtry_tune[, 'OOBError']), 'mtry']

selected_with_response <- c(response_var, selected_predictors)
x_train <- global_train_df[, selected_with_response, drop = F]

global_rf_model <- randomForest::randomForest(
  formula = make_rf_formula(response_var, selected_predictors),
  data = x_train,
  importance = T,
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


# 06 - model performance (training and testing)
#-------------------------------------

# training predictions and metrics
RF_Global_train_pred <- predict(global_rf_model, newdata = global_train)
RF_Global_train_metrics <- calc_rf_metrics(
  observed = global_train[[response_var]],
  predicted = RF_Global_train_pred
)

cat('\nFinal global RF training metrics:\n')
print(RF_Global_train_metrics)

# testing predictions and metrics
RF_Global_pred <- predict(global_rf_model, newdata = global_test)
summary(RF_Global_pred)

RF_Global_test_metrics <- calc_rf_metrics(
  observed = global_test[[response_var]],
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
  observed = global_test[[response_var]],
  predicted = RF_Global_pred,
  model_label = 'Global RF'
)


# 07 - save final model
#-------------------------------------

if (!file.exists(final_model_path)) {
  saveRDS(global_rf_model, final_model_path)
  cat('\nFinal RF model written to:\n')
  cat(final_model_path, '\n')
} else {
  cat('\nFinal RF model already exists at:\n')
  cat(final_model_path, '\n')
}


# 08 - optional exports
#-------------------------------------

write.csv(
  RF_variable_importance,
  file.path(processed_data_dir, paste0('RF_full_model_variable_importance_', dataset_id, '.csv')),
  row.names = F
)

write.csv(
  RF_selection_results,
  file.path(processed_data_dir, paste0('RF_automated_variable_selection_results_', dataset_id, '.csv')),
  row.names = F
)

write.csv(
  data.frame(selected_predictors = selected_predictors),
  file.path(processed_data_dir, paste0('global_rf_selected_predictors_', dataset_id, '.csv')),
  row.names = F
)


