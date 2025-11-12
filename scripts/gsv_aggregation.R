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
#-------------------------------------

# read prediction
pred_gsv <- terra::rast(file.path(output_dir, 'PredictionPlusForestClassification.tif'))
pred_gsv

# read Revier geometries
reviere <- sf::st_read(file.path(raw_data_dir, 'orga', 'Rfö.shp'))
reviere

# read WEFL geometries
wefl <- sf::st_read(file.path(raw_data_dir, 'orga', 'WEFL_2025.shp'))
wefl

terra::plot(pred_gsv$PredictionPlusForestClassification_1)
terra::plot(reviere$geometry, add = T, border = 'white')



# 02 - data preparation
#-------------------------------------

reviere_solling <- reviere %>%
  dplyr::filter(
    FORSTAMT %in% c(254, 268),
    !((FORSTAMT == 268 & REVIER == 10) |
      (FORSTAMT == 254 & REVIER == 9) |
      (FORSTAMT == 254 & REVIER == 8) |
      (FORSTAMT == 254 & REVIER == 10))
  )

reviere_solling

terra::plot(pred_gsv$PredictionPlusForestClassification_1)
terra::plot(reviere_solling$geometry, add = T, border = 'white')



# 03 - aggregation of GSV values
#-------------------------------------

# calculate mean GSV per Revier
reviere_mean <- exactextractr::exact_extract(
  pred_gsv$PredictionPlusForestClassification_1,
  reviere_solling,
  fun = 'mean'
)

# Keep only valid values (not NA and not 0)
reviere_mean <- exactextractr::exact_extract(
  pred_gsv$PredictionPlusForestClassification_1,
  reviere_solling,
  fun = function(values, coverage_fraction) {
    valid_values <- values[!is.na(values) & values != 0]
    mean(valid_values)
  }
)


# add results to the sf object
reviere_solling$mean_gsv <- reviere_mean
reviere_solling

# save layer
sf::st_write(reviere_solling, file.path(output_dir, 'mean_pred_gsv_reviere.gpkg'))

# calculate the mean GSV per Forstamt
forstaemter_mean <- reviere_solling %>%
  sf::st_drop_geometry() %>%      
  dplyr::group_by(FORSTAMT) %>%
  dplyr::summarise(mean_gsv = mean(mean_gsv, na.rm = T))

forstaemter_mean

# plot GSV for each Revier
terra::plot(reviere_solling['mean_gsv'])
























