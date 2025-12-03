#------------------------------------------------------------------------------
# Name:         gsv_validation.R
# Description:  Script extracts predicted growing stock volume (GSV) per pixel
#               in forest inventory plots and calculates the mean value. 
#               This extracted mean GSV is validated with the reference GSV.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#------------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01 - data reading
#------------------------------------------------------------------------------

# read administrative forestry data of Lower Saxony
wefl <- sf::st_read(file.path(raw_data_dir, 'orga', 'WEFL_2025.shp'))
wefl

# read forest inventory plots
bi_plots <- sf::st_read(file.path(processed_data_dir, 'vol_stp_092023.gpkg'))
bi_plots

# read GSV prediction
pred_gsv <- terra::rast(
  file.path(output_dir, 'PredictionPlusForestClassification.tif')
)
pred_gsv



# 02 - join forest inventory plots with forest organization units
#------------------------------------------------------------------------------

# reproject BI plots to the CRS of the point clouds
# DHDN / 3-degree Gauss-Kruger zone 3 --> ETRS89 / UTM zone 32N
bi_plots_projected <- sf::st_transform(bi_plots, sf::st_crs(25832))

# select needed variables from wefl
wefl_selected <- wefl %>%
  dplyr::select(FORSTAMT, REVIER, ABTEILUNG, UABT)

# perform spatial join
bi_plots_wefl <- sf::st_join(bi_plots_projected, wefl_selected)
bi_plots_wefl

# save to disk
sf::st_write(
  bi_plots_wefl,
  file.path(processed_data_dir, 'vol_stp_joined_with_wefl.gpkg')
  )



# 03 - extract predicted GSV per pixel in the forest inventory plots
#------------------------------------------------------------------------------

# buffer plots
bi_plots_wefl_bufferd <- sf::st_buffer(bi_plots_wefl, dist = 13)

# calculate mean predicted GSV per plot
# keep only valid values (not NA and not 0)
mean_pred_GSV_plots <- exactextractr::exact_extract(
  pred_gsv$PredictionPlusForestClassification_1,
  bi_plots_wefl_bufferd,
  fun = function(values, coverage_fraction) {
    valid_values <- values[!is.na(values) & values != 0]
    mean(valid_values)
  }
)

# add mean predicted GSV per plot to the respective sf objects
bi_plots_wefl_bufferd$mean_pred_GSV <- mean_pred_GSV_plots

# save to disk
sf::st_write(
  bi_plots_wefl_bufferd,
  file.path(processed_data_dir, 'vol_stp_vs_pred_vol.gpkg')
)



# 04 - compare predicted vs reference GSV
#------------------------------------------------------------------------------

# scatterplot predicted vs. observed
ggplot(data = bi_plots_wefl_bufferd) +
  geom_point(aes(x = vol_ha, y = mean_pred_GSV), 
             alpha = 0.6, size = 1.5, color = 'black') +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'dashed', linewidth = 1) +
  geom_smooth(aes(x = vol_ha, y = mean_pred_GSV), 
              method = 'lm', color = 'blue', se = T, alpha = 0.3) +
  labs(
    x = 'observed GSV',
    y = 'predicted GSV (mean per plot)'
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = 'bold'),
    plot.subtitle = element_text(size = 12)
  )

# calculate validation statistics
valid_data <- bi_plots_wefl_bufferd[!is.na(bi_plots_wefl_bufferd$vol_ha) & 
                                   !is.na(bi_plots_wefl_bufferd$mean_pred_GSV), ]

if(nrow(valid_data) > 0) {
  
  # correlation
  correlation <- cor(valid_data$vol_ha, valid_data$mean_pred_GSV, use = 'complete.obs')

  # RMSE
  rmse <- sqrt(mean((valid_data$vol_ha - valid_data$mean_pred_GSV)^2, na.rm = T))
  
  # relative RMSE
  mean_observed <- mean(valid_data$vol_ha, na.rm = T)
  rel_rmse <- (rmse / mean_observed) * 100
  
  # bias
  bias <- mean(valid_data$mean_pred_GSV - valid_data$vol_ha, na.rm = T)

  cat('\nValidation Statistics:\n')
  cat('Correlation (r):', round(correlation, 3), '\n')
  cat('R-squared:', round(correlation^2, 2), '\n')
  cat('RMSE:', round(rmse, 2), '\n')
  cat('Relative RMSE (%):', round(rel_rmse, 2), '\n')
  cat('Bias (Predicted - Reference):', round(bias, 2), '\n')
  cat('Number of valid pairs:', nrow(valid_data), '\n')
}













