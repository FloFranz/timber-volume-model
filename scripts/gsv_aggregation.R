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

terra::plot(pred_gsv$PredictionPlusForestClassification_1)
terra::plot(reviere_solling$geometry, add = T, border = 'white')

terra::plot(pred_gsv$PredictionPlusForestClassification_1)
terra::plot(wefl_solling$geometry, add = T, border = 'white')


# 03 - aggregation of GSV values
#-------------------------------------

# calculate mean GSV per Revier
# keep only valid values (not NA and not 0)
reviere_mean <- exactextractr::exact_extract(
  pred_gsv$PredictionPlusForestClassification_1,
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

# calculate mean GSV per UABT unit
# keep only valid values (not NA and not 0)
uabt_mean <- exactextractr::exact_extract(
  pred_gsv$PredictionPlusForestClassification_1,
  wefl_uabt,
  fun = function(values, coverage_fraction) {
    valid_values <- values[!is.na(values) & values != 0]
      mean(valid_values)
  }
)

# add results to the respective sf objects
reviere_solling$mean_gsv <- reviere_mean
wefl_uabt$mean_gsv <- uabt_mean
reviere_solling
wefl_uabt

# save layers
sf::st_write(reviere_solling, file.path(output_dir, 'mean_pred_gsv_reviere.gpkg'))
sf::st_write(wefl_uabt, file.path(output_dir, 'mean_pred_gsv_uabt.gpkg'))

# calculate the mean GSV per Forstamt
forstaemter_mean <- reviere_solling %>%
  sf::st_drop_geometry() %>%      
  dplyr::group_by(FORSTAMT) %>%
  dplyr::summarise(mean_gsv = mean(mean_gsv, na.rm = T))

forstaemter_mean

# plot GSV for each Revier
terra::plot(reviere_solling['mean_gsv'])

# plot GSV for each UABT
terra::plot(wefl_uabt['mean_gsv'])























