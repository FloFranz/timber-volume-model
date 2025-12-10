#------------------------------------------------------------------------------
# Name:         gsv_aggregation.R
# Description:  Script aggregates growing stock volume (GSV) predictions on
#               different forest organization levels, to calculate mean GSV on
#               different units.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#------------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01 - data reading
#-------------------------------------------------------------------------------

# read predictions
# linear model
lm_pred_gsv <- terra::rast(file.path(output_dir, 'PredictionPlusForestClassification.tif'))
lm_pred_gsv <- lm_pred_gsv$PredictionPlusForestClassification_1
lm_pred_gsv

# random forest
rf_pred_gsv <- terra::rast(file.path(output_dir, 'FirstRFRaster.tif'))
rf_pred_gsv

# read Revier geometries
reviere <- sf::st_read(file.path(raw_data_dir, 'orga', 'Rfö.shp'))
reviere

# read WEFL geometries
wefl <- sf::st_read(file.path(raw_data_dir, 'orga', 'WEFL_2025.shp'))
wefl

terra::plot(lm_pred_gsv, main = 'Linear Model Predictions')
terra::plot(reviere$geometry, border = 'white', add = T)

terra::plot(rf_pred_gsv, main = 'Random Forest Predictions')
terra::plot(reviere$geometry, border = 'white', add = T)



# 02 - data preparation
#-------------------------------------------------------------------------------

# filter Reviere to those covering the core Solling region
reviere_solling <- reviere %>%
  dplyr::filter(
    FORSTAMT %in% c(254, 268),
    !((FORSTAMT == 268 & REVIER == 10) |
      (FORSTAMT == 254 & REVIER == 9) |
      (FORSTAMT == 254 & REVIER == 8) |
      (FORSTAMT == 254 & REVIER == 10))
  )

reviere_solling

wefl_solling <- wefl %>%
  dplyr::filter(
    FORSTAMT %in% c(254, 268)
  )

terra::plot(lm_pred_gsv, main = 'Linear Model - Solling Reviere')
terra::plot(reviere_solling$geometry, border = 'white', add = T)

terra::plot(rf_pred_gsv, main = 'Random Forest - Solling Reviere')
terra::plot(reviere_solling$geometry, border = 'white', add = T)

terra::plot(lm_pred_gsv, main = 'Linear Model - Solling WEFL')
terra::plot(wefl_solling$geometry, border = 'white', add = T)

terra::plot(rf_pred_gsv, main = 'Random Forest - Solling WEFL')
terra::plot(wefl_solling$geometry, border = 'white', add = T)


# 03 - aggregation of GSV values
#-------------------------------------------------------------------------------

# calculate mean GSV per Revier for both prediction types
# keep only valid values (not NA and not 0)

# linear model predictions
reviere_mean_lm <- exactextractr::exact_extract(
  lm_pred_gsv,
  reviere_solling,
  fun = function(values, coverage_fraction) {
    valid_values <- values[!is.na(values) & values != 0]
    mean(valid_values)
  }
)

# random forest predictions
reviere_mean_rf <- exactextractr::exact_extract(
  rf_pred_gsv,
  reviere_solling,
  fun = function(values, coverage_fraction) {
    valid_values <- values[!is.na(values) & values != 0]
    mean(valid_values)
  }
)

# aggregate WEFL geometries from SE level to UABT level
# group by organizational hierarchy: FORSTAMT, REVIER, ABTEILUNG, UABT
wefl_uabt <- wefl_solling %>%
  dplyr::group_by(FORSTAMT, REVIER, ABTEILUNG, UABT) %>%
  dplyr::summarise(
    .groups = 'drop'
  ) %>%
  # union geometries for each UABT unit
  sf::st_union(by_feature = TRUE)

wefl_uabt

sf::st_write(wefl_uabt, file.path(processed_data_dir, 'wefl_uabt.gpkg'))

# calculate mean GSV per UABT unit for both prediction types
# keep only valid values (not NA and not 0)

# linear model predictions
uabt_mean_lm <- exactextractr::exact_extract(
  lm_pred_gsv,
  wefl_uabt,
  fun = function(values, coverage_fraction) {
    valid_values <- values[!is.na(values) & values != 0]
    mean(valid_values)
  }
)

# random forest predictions
uabt_mean_rf <- exactextractr::exact_extract(
  rf_pred_gsv,
  wefl_uabt,
  fun = function(values, coverage_fraction) {
    valid_values <- values[!is.na(values) & values != 0]
    mean(valid_values)
  }
)

# add results to the respective sf objects
# create separate copies for each prediction type
reviere_solling_lm <- reviere_solling
reviere_solling_rf <- reviere_solling
wefl_uabt_lm <- wefl_uabt
wefl_uabt_rf <- wefl_uabt

# add mean GSV values
reviere_solling_lm$mean_gsv <- reviere_mean_lm
reviere_solling_rf$mean_gsv <- reviere_mean_rf
wefl_uabt_lm$mean_gsv <- uabt_mean_lm
wefl_uabt_rf$mean_gsv <- uabt_mean_rf

reviere_solling_lm
reviere_solling_rf
wefl_uabt_lm
wefl_uabt_rf

# save layers for both prediction types
sf::st_write(
  reviere_solling_lm, file.path(output_dir, 'mean_pred_gsv_reviere_lm.gpkg')
  )
sf::st_write(
  reviere_solling_rf, file.path(output_dir, 'mean_pred_gsv_reviere_rf.gpkg')
  )
sf::st_write(
  wefl_uabt_lm, file.path(output_dir, 'mean_pred_gsv_uabt_lm.gpkg')
  )
sf::st_write(
  wefl_uabt_rf, file.path(output_dir, 'mean_pred_gsv_uabt_rf.gpkg')
  )

# calculate the mean GSV per Forstamt for both prediction types
forstaemter_mean_lm <- reviere_solling_lm %>%
  sf::st_drop_geometry() %>%      
  dplyr::group_by(FORSTAMT) %>%
  dplyr::summarise(mean_gsv = mean(mean_gsv, na.rm = T))

forstaemter_mean_rf <- reviere_solling_rf %>%
  sf::st_drop_geometry() %>%      
  dplyr::group_by(FORSTAMT) %>%
  dplyr::summarise(mean_gsv = mean(mean_gsv, na.rm = T))

forstaemter_mean_lm
forstaemter_mean_rf

# plot GSV for each Revier - Linear vs. Random Forest Model
terra::plot(
  reviere_solling_lm['mean_gsv'], 
  main = 'Mean GSV per Revier - Linear Model'
  )
terra::plot(
  reviere_solling_rf['mean_gsv'], 
  main = 'Mean GSV per Revier - Random Forest'
  )

# plot GSV for each UABT - Linear vs. Random Forest Model
terra::plot(
  wefl_uabt_lm['mean_gsv'],
  main = 'Mean GSV per UABT - Linear Model'
  )
terra::plot(
  wefl_uabt_rf['mean_gsv'],
  main = 'Mean GSV per UABT - Random Forest'
  )























