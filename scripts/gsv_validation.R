#------------------------------------------------------------------------------
# Name:         gsv_validation.R
# Description:  Script extracts predicted growing stock volume (GSV) per pixel
#               in forest inventory plots and calculates the mean value. 
#               This is done for the original prediction and three tiles where
#               canopy gaps are masked out.
#               The extracted mean GSV (original and gaps masked out)
#               is validated with the reference GSV.
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

# read masked GSV tiles (created in script gsv_correction.R)
masked_gsv_files <- c(
  file.path(processed_data_dir, 'masked_gsv_t1.tif'),
  file.path(processed_data_dir, 'masked_gsv_t2.tif'),
  file.path(processed_data_dir, 'masked_gsv_t3.tif')
)



# 02 - join forest inventory plots with forest organization units
#------------------------------------------------------------------------------

# check if joined data already exists, if not create it
file_bi_plots_wefl <- file.path(processed_data_dir, 'vol_stp_joined_with_wefl.gpkg')

if (file.exists(file_bi_plots_wefl)) {
  
  cat('Loading existing joined BI plots with WEFL data...\n')
  bi_plots_wefl <- sf::st_read(file_bi_plots_wefl)
  
} else {
  
  cat('Join BI plots with WEFL data...\n')
  
  # reproject BI plots to the CRS of the point clouds
  # DHDN / 3-degree Gauss-Kruger zone 3 --> ETRS89 / UTM zone 32N
  bi_plots_projected <- sf::st_transform(bi_plots, sf::st_crs(25832))
  
  # select needed variables from wefl
  wefl_selected <- wefl %>%
    dplyr::select(FORSTAMT, REVIER, ABTEILUNG, UABT)
  
  # perform spatial join
  bi_plots_wefl <- sf::st_join(bi_plots_projected, wefl_selected)
  
  # save to disk
  sf::st_write(
    bi_plots_wefl,
    joined_file_path
    )
}

bi_plots_wefl



# 03 - extract predicted GSV per pixel in the forest inventory plots (entire area)
#------------------------------------------------------------------------------

# check if buffered plots with GSV predictions already exist, if not create them
file_bi_plots_buffered <- file.path(processed_data_dir, 'vol_stp_vs_pred_vol.gpkg')

if (file.exists(file_bi_plots_buffered)) {
  
  cat('Loading existing buffered BI plots with GSV predictions...\n')
  bi_plots_wefl_buffered <- sf::st_read(file_bi_plots_buffered)
  
} else {
  
  cat('Extract predicted GSV per pixel in forest inventory plots...\n')
  
  # buffer plots
  bi_plots_wefl_buffered <- sf::st_buffer(bi_plots_wefl, dist = 13)
  
  # calculate mean predicted GSV per plot
  # keep only valid values (not NA and not 0)
  mean_pred_GSV_plots <- exactextractr::exact_extract(
    pred_gsv$PredictionPlusForestClassification_1,
    bi_plots_wefl_buffered,
    fun = function(values, coverage_fraction) {
      valid_values <- values[!is.na(values) & values != 0]
      if (length(valid_values) > 0) {
        mean(valid_values)
      } else {
        NA_real_
      }
    }
  )
  
  # add mean predicted GSV per plot to the respective sf objects
  bi_plots_wefl_buffered$mean_pred_GSV <- mean_pred_GSV_plots
  
  # save to disk
  sf::st_write(
    bi_plots_wefl_buffered,
    file_bi_plots_buffered
  )
}

bi_plots_wefl_buffered



# 04 - extract predicted GSV from gap-masked tiles
#------------------------------------------------------------------------------

# check if masked files exist
existing_masked_files <- masked_gsv_files[file.exists(masked_gsv_files)]

if (length(existing_masked_files) > 0) {
  
  cat('Found', length(existing_masked_files), 'masked GSV tiles for extraction\n')
  
  # initialize list to store plots with masked predictions
  masked_plots_list <- list()
  
  for (i in seq_along(existing_masked_files)) {
    
    cat('Processing masked tile', i, '...\n')
    
    # read masked GSV tile
    masked_gsv_tile <- terra::rast(existing_masked_files[i])
    
    # extract mean predicted GSV per plot for this masked tile
    # only extract for plots that intersect with this tile
    tile_extent <- terra::ext(masked_gsv_tile)
    
    # create bbox for cropping
    tile_bbox <- sf::st_bbox(
      tile_extent, crs = sf::st_crs(bi_plots_wefl_buffered)
      )
    
      # filter plots to those intersecting with the tile extent
      plots_intersecting <- sf::st_crop(bi_plots_wefl_buffered, tile_bbox)
      
      # only keep plots that are completely within the tile (no edge effects)
      if (nrow(plots_intersecting) > 0) {
        
        # create a slightly smaller bbox (buffer inward by 15m to ensure complete containment)
        # this ensures that even the 13m buffer around plot centers is fully within the tile
        tile_bbox_shrunk <- sf::st_bbox(c(
          xmin = tile_extent$xmin + 15, 
          ymin = tile_extent$ymin + 15,
          xmax = tile_extent$xmax - 15, 
          ymax = tile_extent$ymax - 15
        ), crs = sf::st_crs(bi_plots_wefl_buffered))
        
        # check which plot centers are within the shrunk bbox
        plot_centers <- sf::st_centroid(plots_intersecting)
        plots_completely_within <- sf::st_within(plot_centers, 
                                                sf::st_as_sfc(tile_bbox_shrunk), 
                                                sparse = FALSE)
        
        plots_in_tile <- plots_intersecting[plots_completely_within[,1], ]
        
        cat('    Plots intersecting tile:', nrow(plots_intersecting), 
            '| Plots completely within (after 15m buffer):', nrow(plots_in_tile), '\n')
        
      } else {
        plots_in_tile <- plots_intersecting
      }
      
      if (nrow(plots_in_tile) > 0) {
      
      # calculate mean predicted GSV per plot from masked tile
      # treat masked areas (NA) as zero GSV (gaps have no growing stock)
      mean_pred_GSV_masked <- exactextractr::exact_extract(
        masked_gsv_tile,
        plots_in_tile,
        fun = function(values, coverage_fraction) {
          # replace NA (masked gaps) with 0, but keep original 0s as 0
          values_with_gaps_as_zero <- ifelse(is.na(values), 0, values)
          # now calculate mean including the gap areas as zero GSV
          mean(values_with_gaps_as_zero)
        }
      )
      
      # add masked prediction to plots
      plots_in_tile$mean_pred_GSV_corrected <- mean_pred_GSV_masked
      
      # store for later combination
      masked_plots_list[[i]] <- plots_in_tile
      
      cat('  Extracted GSV for', nrow(plots_in_tile), 'plots in tile', i, '\n')
      
    } else {
      cat('  No plots intersect with tile', i, '\n')
    }
  }
  
  # combine all masked validation plots
  if (length(masked_plots_list) > 0) {
    
    # combine all plots with corrected predictions
    bi_plots_wefl_buffered_corrected <- do.call(rbind, masked_plots_list)
    
    # save combined corrected validation results
    sf::st_write(
      bi_plots_wefl_buffered_corrected,
      file.path(processed_data_dir, 'vol_stp_vs_pred_vol_corrected.gpkg'),
      append = F
    )
    
      cat('Combined corrected GSV predictions saved for', nrow(bi_plots_wefl_buffered_corrected), 'plots\n')
    
  } else {
    cat('No valid corrected predictions extracted\n')
    bi_plots_wefl_buffered_corrected <- NULL
  }
  
} else {
  
  cat('No masked GSV tiles found. Skipping masked GSV extraction.\n')
  bi_plots_wefl_buffered_corrected <- NULL
  
}



# 04 - compare predicted vs reference GSV (original prediction)
#------------------------------------------------------------------------------

# scatterplot predicted vs. observed
ggplot(data = bi_plots_wefl_buffered) +
  geom_point(aes(x = vol_ha, y = mean_pred_GSV), 
             alpha = 0.6, size = 1.5, color = 'black') +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'dashed', 
              linewidth = 1) +
  geom_smooth(aes(x = vol_ha, y = mean_pred_GSV), 
              method = 'lm', color = 'blue', se = T, alpha = 0.3) +
  labs(
    title = 'Original GSV Prediction',
    x = 'observed GSV',
    y = 'predicted GSV (mean per plot)'
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = 'bold'),
    plot.subtitle = element_text(size = 12)
  )

# calculate validation statistics for original prediction
valid_data_original <- bi_plots_wefl_buffered[
  !is.na(bi_plots_wefl_buffered$vol_ha) & 
    !is.na(bi_plots_wefl_buffered$mean_pred_GSV), 
  ]

if(nrow(valid_data_original) > 0) {
  
  # correlation
  correlation_orig <- cor(
    valid_data_original$vol_ha,
    valid_data_original$mean_pred_GSV,
    use = 'complete.obs'
    )

  # RMSE
  rmse_orig <- sqrt(
    mean((valid_data_original$vol_ha - valid_data_original$mean_pred_GSV)^2,
         na.rm = T)
    )
  
  # relative RMSE
  mean_observed_orig <- mean(valid_data_original$vol_ha, na.rm = T)
  rel_rmse_orig <- (rmse_orig / mean_observed_orig) * 100
  
  # bias
  bias_orig <- mean(
    valid_data_original$mean_pred_GSV - valid_data_original$vol_ha, na.rm = T
    )

  cat('\n=== ORIGINAL GSV PREDICTION - Validation Statistics ===\n')
  cat('Correlation (r):', round(correlation_orig, 3), '\n')
  cat('R-squared:', round(correlation_orig^2, 2), '\n')
  cat('RMSE:', round(rmse_orig, 2), '\n')
  cat('Relative RMSE (%):', round(rel_rmse_orig, 2), '\n')
  cat('Bias (Predicted - Reference):', round(bias_orig, 2), '\n')
  cat('Number of valid plots:', nrow(valid_data_original), '\n\n')
   
}



# validation with gap-corrected GSV predictions
#------------------------------------------------------------------------------

# scatterplot predicted vs. observed
ggplot(data = bi_plots_wefl_buffered_corrected) +
  geom_point(aes(x = vol_ha, y = mean_pred_GSV_corrected), 
             alpha = 0.6, size = 1.5, color = 'black') +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'dashed', 
              linewidth = 1) +
  geom_smooth(aes(x = vol_ha, y = mean_pred_GSV_corrected), 
              method = 'lm', color = 'blue', se = T, alpha = 0.3) +
  labs(
    title = 'Gap-Corrected GSV Prediction',
    x = 'observed GSV',
    y = 'predicted GSV (mean per plot, gaps corrected)'
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = 'bold'),
    plot.subtitle = element_text(size = 12)
  )

# calculate validation statistics for corrected predictions
valid_data_corrected <- bi_plots_wefl_buffered_corrected[
  !is.na(bi_plots_wefl_buffered_corrected$vol_ha) & 
    !is.na(bi_plots_wefl_buffered_corrected$mean_pred_GSV_corrected), 
  ]

if(nrow(valid_data_corrected) > 0) {
  
  # correlation
  correlation_corrected <- cor(
    valid_data_corrected$vol_ha,
    valid_data_corrected$mean_pred_GSV_corrected,
    use = 'complete.obs'
    )

  # RMSE
  rmse_corrected <- sqrt(
    mean((valid_data_corrected$vol_ha - valid_data_corrected$mean_pred_GSV_corrected)^2,
         na.rm = T)
    )
  
  # relative RMSE
  mean_observed_corrected <- mean(valid_data_corrected$vol_ha, na.rm = T)
  rel_rmse_corrected <- (rmse_corrected / mean_observed_corrected) * 100
  
  # bias
  bias_corrected <- mean(
    valid_data_corrected$mean_pred_GSV_corrected - valid_data_corrected$vol_ha, na.rm = T
    )

  cat('\n=== GAP-CORRECTED GSV PREDICTION - Validation Statistics ===\n')
  cat('Correlation (r):', round(correlation_corrected, 3), '\n')
  cat('R-squared:', round(correlation_corrected^2, 2), '\n')
  cat('RMSE:', round(rmse_corrected, 2), '\n')
  cat('Relative RMSE (%):', round(rel_rmse_corrected, 2), '\n')
  cat('Bias (Predicted - Reference):', round(bias_corrected, 2), '\n')
  cat('Number of valid plots:', nrow(valid_data_corrected), '\n\n')
   
}



# 05 - comparison between original and corrected predictions
#------------------------------------------------------------------------------

if (exists('valid_data_original') && exists('valid_data_corrected')) {
  
  cat('\n=== COMPARISON: ORIGINAL vs GAP-MASKED PREDICTIONS ===\n')
  
  # find common plots between original and corrected datasets
  # (plots that have valid predictions in both datasets)
  # drop geometry to avoid sf merge issues
  original_data <- sf::st_drop_geometry(valid_data_original[, c('key', 'vol_ha', 'mean_pred_GSV')])
  corrected_data <- sf::st_drop_geometry(valid_data_corrected[, c('key', 'vol_ha', 'mean_pred_GSV_corrected')])
  
  # rename prediction columns to avoid suffix confusion
  names(original_data)[names(original_data) == 'mean_pred_GSV'] <- 'mean_pred_GSV_orig'
  names(corrected_data)[names(corrected_data) == 'mean_pred_GSV_corrected'] <- 'mean_pred_GSV_corrected'
  
  common_plots <- merge(
    original_data,
    corrected_data,
    by = c('key', 'vol_ha')  # merge by both key and vol_ha since they should be identical
  )
  
  if (nrow(common_plots) > 0) {
    
    cat('Number of plots with both original and corrected predictions:', nrow(common_plots), '\n\n')
    
    # calculate differences in validation metrics
    cat('IMPROVEMENT IN VALIDATION METRICS (Corrected vs Original):\n')
    
    if (exists('correlation_orig') && exists('correlation_corrected')) {
      corr_diff <- correlation_corrected - correlation_orig
      cat('Correlation difference:', round(corr_diff, 3), 
          ifelse(corr_diff > 0, '(improved)', '(degraded)'), '\n')
      
      r2_diff <- correlation_corrected^2 - correlation_orig^2
      cat('R-squared difference:', round(r2_diff, 3), 
          ifelse(r2_diff > 0, '(improved)', '(degraded)'), '\n')
    }
    
    if (exists('rmse_orig') && exists('rmse_corrected')) {
      rmse_diff <- rmse_corrected - rmse_orig
      cat('RMSE difference:', round(rmse_diff, 2), 
          ifelse(rmse_diff < 0, '(improved)', '(degraded)'), '\n')
      
      rel_rmse_diff <- rel_rmse_corrected - rel_rmse_orig
      cat('Relative RMSE difference (%):', round(rel_rmse_diff, 2), 
          ifelse(rel_rmse_diff < 0, '(improved)', '(degraded)'), '\n')
    }
    
    if (exists('bias_orig') && exists('bias_corrected')) {
      bias_diff <- abs(bias_corrected) - abs(bias_orig)
      cat('Absolute bias difference:', round(bias_diff, 2), 
          ifelse(bias_diff < 0, '(improved)', '(degraded)'), '\n\n')
    }
    
  } else {
    cat('No common plots found between original and corrected datasets.\n')
  }
  
} else {
  cat('Cannot perform comparison - missing original or corrected validation data.\n')
}













