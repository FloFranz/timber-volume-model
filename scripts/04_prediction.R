#------------------------------------------------------------------------------
# Name:         04_prediction.R
# Description:  Script loads the trained global Random Forest model,
#               predicts wall-to-wall growing stock (GS) per hectare from
#               the Solling metrics raster, and extracts mean predicted GS
#               for all terrestrial sample plots.
# Author:       Georgia Reeves, Florian Franz
# Contact:      georgia.reeves@nw-fva.de, florian.franz@nw-fva.de
#------------------------------------------------------------------------------


# source setup script
source('src/setup.R', local = TRUE)



# 01 - user settings
#-------------------------------------

# input model and wall-to-wall metrics
model_path <- file.path(processed_data_dir, 'global_rf_model.rds')
w2w_metrics_path <- file.path(processed_data_dir, 'metrics_w2w_solling_incl_forest_type.tif')

# output prediction raster
w2w_prediction_output_path <- file.path(output_dir, 'global_rf_prediction.tif')


# 02 - read model and derive predictor names
#-------------------------------------

if (!file.exists(model_path)) {
  stop('Model file does not exist: ', model_path)
}

global_rf_model <- readRDS(model_path)

# derive predictors from fitted RF formula
rf_terms <- all.vars(formula(global_rf_model))
response_var <- rf_terms[1]
model_predictors <- setdiff(rf_terms, response_var)

cat('Loaded RF model from:\n')
cat(model_path, '\n')
cat('Model predictors used for prediction:\n')
print(model_predictors)


# 03 - prepare wall-to-wall predictor raster
#-------------------------------------

Model_Based_Global_RF <- terra::rast(w2w_metrics_path)

# rename forest_type to dominant_species if needed
if ('forest_type' %in% names(Model_Based_Global_RF) &&
    !('dominant_species' %in% names(Model_Based_Global_RF))) {
  names(Model_Based_Global_RF)[names(Model_Based_Global_RF) == 'forest_type'] <- 'dominant_species'
}

# check that all required model predictors exist
missing_raster_vars <- setdiff(model_predictors, names(Model_Based_Global_RF))
if (length(missing_raster_vars) > 0) {
  stop(
    'The following model predictors are missing from the wall-to-wall raster: ',
    paste(missing_raster_vars, collapse = ', ')
  )
}

# select only variables used in the trained model
Model_Based_Global_RF <- Model_Based_Global_RF[[model_predictors]]

# recode dominant species if part of model
if ('dominant_species' %in% model_predictors) {
  ft_RF <- Model_Based_Global_RF[['dominant_species']]
  ft_RF[ft_RF == 0] <- NA
  ft_RF <- as.factor(ft_RF)

  levels(ft_RF) <- data.frame(
    value = c(1, 2),
    label = c('LB', 'NB')
  )

  Model_Based_Global_RF[['dominant_species']] <- ft_RF
}

cat('Raster predictors used:\n')
print(names(Model_Based_Global_RF))
print(Model_Based_Global_RF)


# 04 - wall-to-wall prediction
#-------------------------------------

global_rf_prediction <- terra::predict(
  object = Model_Based_Global_RF,
  model = global_rf_model,
  na.rm = TRUE,
  type = 'response'
)

# write prediction raster to output directory
if (!file.exists(w2w_prediction_output_path)) {
  terra::writeRaster(
    global_rf_prediction,
    w2w_prediction_output_path
  )
} else {
  cat('\nPrediction raster already exists at:\n')
  cat(w2w_prediction_output_path, '\n')
}

cat('\nWall-to-wall prediction available at:\n')
cat(w2w_prediction_output_path, '\n')
print(global_rf_prediction)


# 05 - extract predicted GS in terrestrial sample plots
#-------------------------------------

# read administrative forestry data of Lower Saxony
wefl <- sf::st_read(file.path(raw_data_dir, 'orga', 'WEFL_2025.shp'))

# read terrestrial sample plots
bi_plots_path <- file.path(processed_data_dir, 'vol_stp.gpkg')
if (!file.exists(bi_plots_path)) {
  stop('Plot file does not exist: ', bi_plots_path)
}
bi_plots <- sf::st_read(bi_plots_path)


# 05.1 - join plots with forest organization units
file_bi_plots_wefl <- file.path(processed_data_dir, 'vol_stp_joined_with_wefl.gpkg')

if (file.exists(file_bi_plots_wefl)) {
  cat('Loading existing joined BI plots with WEFL data...\n')
  bi_plots_wefl <- sf::st_read(file_bi_plots_wefl)
} else {
  cat('Join BI plots with WEFL data...\n')

  bi_plots_projected <- sf::st_transform(bi_plots, sf::st_crs(25832))

  wefl_selected <- wefl %>%
    dplyr::select(FORSTAMT, REVIER, ABTEILUNG, UABT)

  bi_plots_wefl <- sf::st_join(bi_plots_projected, wefl_selected)

  sf::st_write(
    bi_plots_wefl,
    file_bi_plots_wefl
  )
}


# 05.2 - buffer plots and extract mean predicted GS
file_vol_stp_vs_pred_vol <- file.path(processed_data_dir, 'vol_stp_vs_pred_vol.gpkg')

if (file.exists(file_vol_stp_vs_pred_vol)) {
  cat('Loading existing buffered BI plots with RF predictions...\n')
  vol_stp_vs_pred_vol <- sf::st_read(file_vol_stp_vs_pred_vol)
} else {
  cat('Extract predicted GS per pixel in terrestrial sample plots...\n')

  vol_stp_vs_pred_vol <- sf::st_buffer(bi_plots_wefl, dist = 13)

  # use prediction raster from disk if available
  # otherwise use in-memory object
  if (file.exists(w2w_prediction_output_path)) {
    prediction_raster_for_extract <- terra::rast(w2w_prediction_output_path)
  } else {
    prediction_raster_for_extract <- global_rf_prediction
  }

  mean_pred_vol_rf_plots <- exactextractr::exact_extract(
    prediction_raster_for_extract,
    vol_stp_vs_pred_vol,
    fun = function(values, coverage_fraction) {
      valid_values <- values[!is.na(values) & values != 0]
      if (length(valid_values) > 0) {
        mean(valid_values)
      } else {
        NA_real_
      }
    }
  )

  vol_stp_vs_pred_vol$mean_pred_vol_rf <- mean_pred_vol_rf_plots

  sf::st_write(
    vol_stp_vs_pred_vol,
    file_vol_stp_vs_pred_vol
  )
}

vol_stp_vs_pred_vol
