#------------------------------------------------------------------------------
# Name:         gsv_validation.R
# Description:  Script extracts predicted growing stock volume (GSV) per pixel
#               in forest inventory plots and calculates the mean value. 
#               This is done for both linear model and random forest predictions,
#               including original predictions and three tiles where canopy gaps
#               are masked out. The extracted mean GSV (original and gaps masked out)
#               is validated with the reference GSV for both model types.
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

# read GSV prediction (linear model)
pred_gsv_lm <- terra::rast(
  file.path(output_dir, 'PredictionPlusForestClassification.tif')
)
pred_gsv_lm <- pred_gsv_lm$PredictionPlusForestClassification_1
pred_gsv_lm

# read GSV prediction (random forest)
pred_gsv_rf <- terra::rast(
  file.path(output_dir, 'FirstRFRaster.tif')
)
pred_gsv_rf

# read masked GSV tiles (created in script gsv_correction.R)
# linear model masked tiles
masked_gsv_lm_files <- c(
  file.path(processed_data_dir, 'masked_gsv_lm_t1.tif'),
  file.path(processed_data_dir, 'masked_gsv_lm_t2.tif'),
  file.path(processed_data_dir, 'masked_gsv_lm_t3.tif')
)

# random forest masked tiles
masked_gsv_rf_files <- c(
  file.path(processed_data_dir, 'masked_gsv_rf_t1.tif'),
  file.path(processed_data_dir, 'masked_gsv_rf_t2.tif'),
  file.path(processed_data_dir, 'masked_gsv_rf_t3.tif')
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
  
  # calculate mean predicted GSV per plot (linear model)
  # keep only valid values (not NA and not 0)
  mean_pred_GSV_lm_plots <- exactextractr::exact_extract(
    pred_gsv_lm,
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
  
  # calculate mean predicted GSV per plot (random forest)
  # keep only valid values (not NA and not 0)
  mean_pred_GSV_rf_plots <- exactextractr::exact_extract(
    pred_gsv_rf,
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
  bi_plots_wefl_buffered$mean_pred_GSV_lm <- mean_pred_GSV_lm_plots
  bi_plots_wefl_buffered$mean_pred_GSV_rf <- mean_pred_GSV_rf_plots
  
  # save to disk
  sf::st_write(
    bi_plots_wefl_buffered,
    file_bi_plots_buffered
  )
}

bi_plots_wefl_buffered



# 04 - extract predicted GSV from gap-masked tiles
#------------------------------------------------------------------------------

# check if masked files exist (linear model)
existing_masked_lm_files <- masked_gsv_lm_files[file.exists(masked_gsv_lm_files)]
# check if masked files exist (random forest)
existing_masked_rf_files <- masked_gsv_rf_files[file.exists(masked_gsv_rf_files)]

if (length(existing_masked_lm_files) > 0 || length(existing_masked_rf_files) > 0) {
  
  cat('Found', length(existing_masked_lm_files), 'masked LM GSV tiles and', 
      length(existing_masked_rf_files), 'masked RF GSV tiles for extraction\n')
  
  # initialize lists to store plots with masked predictions
  masked_lm_plots_list <- list()
  masked_rf_plots_list <- list()
  
  # process linear model masked tiles
  for (i in seq_along(existing_masked_lm_files)) {
    
    cat('Processing LM masked tile', i, '...\n')
    
    # read masked LM GSV tile
    masked_lm_gsv_tile <- terra::rast(existing_masked_lm_files[i])
    
    # extract mean predicted GSV per plot for this masked tile
    # only extract for plots that intersect with this tile
    tile_extent <- terra::ext(masked_lm_gsv_tile)
    
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
          tile_extent$xmin + 15, 
          tile_extent$ymin + 15,
          tile_extent$xmax - 15, 
          tile_extent$ymax - 15
        ), crs = sf::st_crs(bi_plots_wefl_buffered))
        
        # check which plot centers are within the shrunk bbox
        plot_centers <- sf::st_centroid(plots_intersecting)
        plots_completely_within <- sf::st_within(plot_centers, 
                                                sf::st_as_sfc(tile_bbox_shrunk), 
                                                sparse = F)
        
        plots_in_tile <- plots_intersecting[plots_completely_within[,1], ]
        
        cat('    Plots intersecting tile:', nrow(plots_intersecting), 
            '| Plots completely within (after 15m buffer):', nrow(plots_in_tile), '\n')
        
      } else {
        plots_in_tile <- plots_intersecting
      }
      
      if (nrow(plots_in_tile) > 0) {
      
      # calculate mean predicted GSV per plot from masked tile
      # treat masked areas (NA) as zero GSV (gaps have no growing stock)
      mean_pred_GSV_lm_masked <- exactextractr::exact_extract(
        masked_lm_gsv_tile,
        plots_in_tile,
        fun = function(values, coverage_fraction) {
          # replace NA (masked gaps) with 0, but keep original 0s as 0
          values_with_gaps_as_zero <- ifelse(is.na(values), 0, values)
          # now calculate mean including the gap areas as zero GSV
          mean(values_with_gaps_as_zero)
        }
      )
      
      # add masked prediction to plots
      plots_in_tile$mean_pred_GSV_lm_corrected <- mean_pred_GSV_lm_masked
      
      # store for later combination
      masked_lm_plots_list[[i]] <- plots_in_tile
      
      cat('  Extracted LM GSV for', nrow(plots_in_tile), 'plots in tile', i, '\n')
      
    } else {
      cat('  No plots intersect with LM tile', i, '\n')
    }
  }
  
  # process random forest masked tiles
  for (i in seq_along(existing_masked_rf_files)) {
    
    cat('Processing RF masked tile', i, '...\n')
    
    # read masked RF GSV tile
    masked_rf_gsv_tile <- terra::rast(existing_masked_rf_files[i])
    
    # extract mean predicted GSV per plot for this masked tile
    # only extract for plots that intersect with this tile
    tile_extent <- terra::ext(masked_rf_gsv_tile)
    
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
          tile_extent$xmin + 15, 
          tile_extent$ymin + 15,
          tile_extent$xmax - 15, 
          tile_extent$ymax - 15
        ), crs = sf::st_crs(bi_plots_wefl_buffered))
        
        # check which plot centers are within the shrunk bbox
        plot_centers <- sf::st_centroid(plots_intersecting)
        plots_completely_within <- sf::st_within(plot_centers, 
                                                sf::st_as_sfc(tile_bbox_shrunk), 
                                                sparse = F)
        
        plots_in_tile <- plots_intersecting[plots_completely_within[,1], ]
        
        cat('    Plots intersecting tile:', nrow(plots_intersecting), 
            '| Plots completely within (after 15m buffer):', nrow(plots_in_tile), '\n')
        
      } else {
        plots_in_tile <- plots_intersecting
      }
      
      if (nrow(plots_in_tile) > 0) {
      
      # calculate mean predicted RF GSV per plot from masked tile
      # for areas without gaps, the masked tile should have the same values as original
      # for areas with gaps, the masked tile should have NA (which we treat as 0)
      mean_pred_GSV_rf_masked <- exactextractr::exact_extract(
        masked_rf_gsv_tile,
        plots_in_tile,
        fun = function(values, coverage_fraction) {
          # replace NA (masked gaps) with 0, but keep original values for non-gaps
          values_with_gaps_as_zero <- ifelse(is.na(values), 0, values)
          # now calculate mean including the gap areas as zero GSV
          mean(values_with_gaps_as_zero)
        }
      )
      
      # add masked RF prediction to plots
      plots_in_tile$mean_pred_GSV_rf_corrected <- mean_pred_GSV_rf_masked
      
      # store for later combination
      masked_rf_plots_list[[i]] <- plots_in_tile
      
      cat('  Extracted RF GSV for', nrow(plots_in_tile), 'plots in tile', i, '\n')
      
    } else {
      cat('  No plots intersect with RF tile', i, '\n')
    }
  }
  
  # combine all masked validation plots (linear model)
  if (length(masked_lm_plots_list) > 0) {
    
    # combine all plots with corrected LM predictions
    bi_plots_wefl_buffered_lm_corrected <- do.call(rbind, masked_lm_plots_list)
    
      cat('Combined corrected LM GSV predictions for', nrow(bi_plots_wefl_buffered_lm_corrected), 'plots\n')
    
  } else {
    cat('No valid corrected LM predictions extracted\n')
    bi_plots_wefl_buffered_lm_corrected <- NULL
  }
  
  # combine all masked validation plots (random forest)
  if (length(masked_rf_plots_list) > 0) {
    
    # combine all plots with corrected RF predictions
    bi_plots_wefl_buffered_rf_corrected <- do.call(rbind, masked_rf_plots_list)
    
      cat('Combined corrected RF GSV predictions for', nrow(bi_plots_wefl_buffered_rf_corrected), 'plots\n')
    
  } else {
    cat('No valid corrected RF predictions extracted\n')
    bi_plots_wefl_buffered_rf_corrected <- NULL
  }
  
  # create dataset with only plots that have corrected predictions (intersect with gap tiles)
  # collect all plot IDs that have corrected predictions
  corrected_plot_ids <- c()
  
  # add LM corrected plot IDs
  if (!is.null(bi_plots_wefl_buffered_lm_corrected)) {
    lm_plot_ids <- bi_plots_wefl_buffered_lm_corrected$kspnr
    corrected_plot_ids <- c(corrected_plot_ids, lm_plot_ids)
    cat('Found', length(lm_plot_ids), 'plots with LM corrected predictions\n')
  }
  
  # add RF corrected plot IDs
  if (!is.null(bi_plots_wefl_buffered_rf_corrected)) {
    rf_plot_ids <- bi_plots_wefl_buffered_rf_corrected$kspnr
    corrected_plot_ids <- c(corrected_plot_ids, rf_plot_ids)
    cat('Found', length(rf_plot_ids), 'plots with RF corrected predictions\n')
  }
  
  # get unique plot IDs that have any corrected predictions
  corrected_plot_ids <- unique(corrected_plot_ids)
  cat('Total unique plots with corrected predictions:', length(corrected_plot_ids), '\n')
  
  # filter main dataset to only include plots with corrected predictions
  bi_plots_wefl_buffered_corrected <- bi_plots_wefl_buffered[
    bi_plots_wefl_buffered$kspnr %in% corrected_plot_ids, 
  ]
  
  cat('Filtered main dataset to', nrow(bi_plots_wefl_buffered_corrected), 'plots that intersect with gap tiles\n')
  
  # add LM corrected predictions if available
  if (!is.null(bi_plots_wefl_buffered_lm_corrected)) {
    
    # extract LM corrected predictions without geometry
    lm_corrected_data <- sf::st_drop_geometry(bi_plots_wefl_buffered_lm_corrected[, c('kspnr', 'mean_pred_GSV_lm_corrected')])
    
    # merge LM corrected predictions with filtered dataset
    bi_plots_wefl_buffered_corrected <- merge(
      bi_plots_wefl_buffered_corrected,
      lm_corrected_data,
      by = 'kspnr',
      all.x = T  # keep all plots from filtered dataset
    )
    
    cat('Added LM corrected predictions for', nrow(lm_corrected_data), 'plots\n')
  }
  
  # add RF corrected predictions if available
  if (!is.null(bi_plots_wefl_buffered_rf_corrected)) {
    
    # extract RF corrected predictions without geometry
    rf_corrected_data <- sf::st_drop_geometry(bi_plots_wefl_buffered_rf_corrected[, c('kspnr', 'mean_pred_GSV_rf_corrected')])
    
    # merge RF corrected predictions with filtered dataset
    bi_plots_wefl_buffered_corrected <- merge(
      bi_plots_wefl_buffered_corrected,
      rf_corrected_data,
      by = 'kspnr',
      all.x = T  # keep all plots from filtered dataset
    )
    
    cat('Added RF corrected predictions for', nrow(rf_corrected_data), 'plots\n')
  }
  
  # save the enhanced dataset with all predictions (original and corrected)
  if (!is.null(bi_plots_wefl_buffered_lm_corrected) || !is.null(bi_plots_wefl_buffered_rf_corrected)) {
    
    sf::st_write(
      bi_plots_wefl_buffered_corrected,
      file.path(processed_data_dir, 'vol_stp_vs_pred_vol_corrected.gpkg'),
      append = F
    )
    
    cat('Dataset with corrected predictions saved for', nrow(bi_plots_wefl_buffered_corrected), 'plots (only those intersecting with gap tiles)\n')
    cat('  - Original LM predictions: available for all', nrow(bi_plots_wefl_buffered_corrected), 'plots\n')
    cat('  - Original RF predictions: available for all', nrow(bi_plots_wefl_buffered_corrected), 'plots\n')
    if (!is.null(bi_plots_wefl_buffered_lm_corrected)) {
      cat('  - LM corrected predictions: available for', sum(!is.na(bi_plots_wefl_buffered_corrected$mean_pred_GSV_lm_corrected)), 'plots\n')
    }
    if (!is.null(bi_plots_wefl_buffered_rf_corrected)) {
      cat('  - RF corrected predictions: available for', sum(!is.na(bi_plots_wefl_buffered_corrected$mean_pred_GSV_rf_corrected)), 'plots\n')
    }
    
  } else {
    
    cat('No corrected predictions available - using original dataset\n')
    bi_plots_wefl_buffered_corrected <- bi_plots_wefl_buffered
    
  }
  
} else {
  
  cat('No masked GSV tiles found. Skipping masked GSV extraction.\n')
  bi_plots_wefl_buffered_lm_corrected <- NULL
  bi_plots_wefl_buffered_rf_corrected <- NULL
  
}



# 05 - compare predicted vs reference GSV (linear model original prediction)
#------------------------------------------------------------------------------

# scatterplot predicted vs. observed
ggplot(data = bi_plots_wefl_buffered) +
  geom_point(aes(x = vol_ha, y = mean_pred_GSV_lm), 
             alpha = 0.6, size = 1.5, color = 'black') +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'dashed', 
              linewidth = 1) +
  geom_smooth(aes(x = vol_ha, y = mean_pred_GSV_lm), 
              method = 'lm', color = 'blue', se = T, alpha = 0.3) +
  labs(
    title = 'Linear Model GSV Prediction',
    x = 'observed GSV',
    y = 'predicted GSV (mean per plot)'
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = 'bold'),
    plot.subtitle = element_text(size = 12)
  )

# calculate validation statistics for linear model prediction
valid_data_lm_original <- bi_plots_wefl_buffered[
  !is.na(bi_plots_wefl_buffered$vol_ha) & 
    !is.na(bi_plots_wefl_buffered$mean_pred_GSV_lm), 
  ]

if(nrow(valid_data_lm_original) > 0) {
  
  # correlation
  correlation_lm_orig <- cor(
    valid_data_lm_original$vol_ha,
    valid_data_lm_original$mean_pred_GSV_lm,
    use = 'complete.obs'
    )

  # RMSE
  rmse_lm_orig <- sqrt(
    mean((valid_data_lm_original$vol_ha - valid_data_lm_original$mean_pred_GSV_lm)^2,
         na.rm = T)
    )
  
  # relative RMSE
  mean_observed_lm_orig <- mean(valid_data_lm_original$vol_ha, na.rm = T)
  rel_rmse_lm_orig <- (rmse_lm_orig / mean_observed_lm_orig) * 100
  
  # bias
  bias_lm_orig <- mean(
    valid_data_lm_original$mean_pred_GSV_lm - valid_data_lm_original$vol_ha, na.rm = T
    )

  cat('\n=== LINEAR MODEL GSV PREDICTION - Validation Statistics ===\n')
  cat('Correlation (r):', round(correlation_lm_orig, 3), '\n')
  cat('R-squared:', round(correlation_lm_orig^2, 2), '\n')
  cat('RMSE:', round(rmse_lm_orig, 2), '\n')
  cat('Relative RMSE (%):', round(rel_rmse_lm_orig, 2), '\n')
  cat('Bias (Predicted - Reference):', round(bias_lm_orig, 2), '\n')
  cat('Number of valid plots:', nrow(valid_data_lm_original), '\n\n')
   
}


# 06 - compare predicted vs reference GSV (random forest original prediction)
#------------------------------------------------------------------------------

# scatterplot predicted vs. observed (RF)
ggplot(data = bi_plots_wefl_buffered) +
  geom_point(aes(x = vol_ha, y = mean_pred_GSV_rf), 
             alpha = 0.6, size = 1.5, color = 'black') +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'dashed', 
              linewidth = 1) +
  geom_smooth(aes(x = vol_ha, y = mean_pred_GSV_rf), 
              method = 'lm', color = 'blue', se = T, alpha = 0.3) +
  labs(
    title = 'Random Forest GSV Prediction',
    x = 'observed GSV',
    y = 'predicted GSV (mean per plot)'
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = 'bold'),
    plot.subtitle = element_text(size = 12)
  )

# calculate validation statistics for RF original prediction
valid_data_rf_original <- bi_plots_wefl_buffered[
  !is.na(bi_plots_wefl_buffered$vol_ha) & 
    !is.na(bi_plots_wefl_buffered$mean_pred_GSV_rf), 
  ]

if(nrow(valid_data_rf_original) > 0) {
  
  # correlation
  correlation_rf_orig <- cor(
    valid_data_rf_original$vol_ha,
    valid_data_rf_original$mean_pred_GSV_rf,
    use = 'complete.obs'
    )

  # RMSE
  rmse_rf_orig <- sqrt(
    mean((valid_data_rf_original$vol_ha - valid_data_rf_original$mean_pred_GSV_rf)^2,
         na.rm = T)
    )
  
  # relative RMSE
  mean_observed_rf_orig <- mean(valid_data_rf_original$vol_ha, na.rm = T)
  rel_rmse_rf_orig <- (rmse_rf_orig / mean_observed_rf_orig) * 100
  
  # bias
  bias_rf_orig <- mean(
    valid_data_rf_original$mean_pred_GSV_rf - valid_data_rf_original$vol_ha, na.rm = T
    )

  cat('\n=== RANDOM FOREST GSV PREDICTION - Validation Statistics ===\n')
  cat('Correlation (r):', round(correlation_rf_orig, 3), '\n')
  cat('R-squared:', round(correlation_rf_orig^2, 2), '\n')
  cat('RMSE:', round(rmse_rf_orig, 2), '\n')
  cat('Relative RMSE (%):', round(rel_rmse_rf_orig, 2), '\n')
  cat('Bias (Predicted - Reference):', round(bias_rf_orig, 2), '\n')
  cat('Number of valid plots:', nrow(valid_data_rf_original), '\n\n')
   
}



# 07 - validation with gap-corrected GSV predictions (linear model)
#------------------------------------------------------------------------------

if (!is.null(bi_plots_wefl_buffered_corrected)) {

# scatterplot predicted vs. observed
ggplot(data = bi_plots_wefl_buffered_corrected) +
  geom_point(aes(x = vol_ha, y = mean_pred_GSV_lm_corrected), 
             alpha = 0.6, size = 1.5, color = 'black') +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'dashed', 
              linewidth = 1) +
  geom_smooth(aes(x = vol_ha, y = mean_pred_GSV_lm_corrected), 
              method = 'lm', color = 'blue', se = T, alpha = 0.3) +
  labs(
    title = 'Linear Model Gap-Corrected GSV Prediction',
    x = 'observed GSV',
    y = 'predicted GSV (mean per plot, gaps corrected)'
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = 'bold'),
    plot.subtitle = element_text(size = 12)
  )

# calculate validation statistics for LM corrected predictions
valid_data_lm_corrected <- bi_plots_wefl_buffered_corrected[
  !is.na(bi_plots_wefl_buffered_corrected$vol_ha) & 
    !is.na(bi_plots_wefl_buffered_corrected$mean_pred_GSV_lm_corrected), 
  ]

if(nrow(valid_data_lm_corrected) > 0) {
  
  # correlation
  correlation_lm_corrected <- cor(
    valid_data_lm_corrected$vol_ha,
    valid_data_lm_corrected$mean_pred_GSV_lm_corrected,
    use = 'complete.obs'
    )

  # RMSE
  rmse_lm_corrected <- sqrt(
    mean((valid_data_lm_corrected$vol_ha - valid_data_lm_corrected$mean_pred_GSV_lm_corrected)^2,
         na.rm = T)
    )
  
  # relative RMSE
  mean_observed_lm_corrected <- mean(valid_data_lm_corrected$vol_ha, na.rm = T)
  rel_rmse_lm_corrected <- (rmse_lm_corrected / mean_observed_lm_corrected) * 100
  
  # bias
  bias_lm_corrected <- mean(
    valid_data_lm_corrected$mean_pred_GSV_lm_corrected - valid_data_lm_corrected$vol_ha, na.rm = T
    )

  cat('\n=== LINEAR MODEL GAP-CORRECTED GSV PREDICTION - Validation Statistics ===\n')
  cat('Correlation (r):', round(correlation_lm_corrected, 3), '\n')
  cat('R-squared:', round(correlation_lm_corrected^2, 2), '\n')
  cat('RMSE:', round(rmse_lm_corrected, 2), '\n')
  cat('Relative RMSE (%):', round(rel_rmse_lm_corrected, 2), '\n')
  cat('Bias (Predicted - Reference):', round(bias_lm_corrected, 2), '\n')
  cat('Number of valid plots:', nrow(valid_data_lm_corrected), '\n\n')
   
}

} else {
  cat('\n=== LINEAR MODEL GAP-CORRECTED GSV PREDICTION ===\n')
  cat('No LM gap-corrected predictions available for validation.\n\n')
}


# 08 - validation with RF gap-corrected GSV predictions
#------------------------------------------------------------------------------

if (!is.null(bi_plots_wefl_buffered_corrected)) {

# scatterplot predicted vs. observed (RF gap-corrected)
ggplot(data = bi_plots_wefl_buffered_corrected) +
  geom_point(aes(x = vol_ha, y = mean_pred_GSV_rf_corrected), 
             alpha = 0.6, size = 1.5, color = 'black') +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'dashed', 
              linewidth = 1) +
  geom_smooth(aes(x = vol_ha, y = mean_pred_GSV_rf_corrected), 
              method = 'lm', color = 'blue', se = T, alpha = 0.3) +
  labs(
    title = 'Random Forest Gap-Corrected GSV Prediction',
    x = 'observed GSV',
    y = 'predicted GSV (mean per plot, gaps corrected)'
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = 'bold'),
    plot.subtitle = element_text(size = 12)
  )

# calculate validation statistics for RF corrected predictions
valid_data_rf_corrected <- bi_plots_wefl_buffered_corrected[
  !is.na(bi_plots_wefl_buffered_corrected$vol_ha) & 
    !is.na(bi_plots_wefl_buffered_corrected$mean_pred_GSV_rf_corrected), 
  ]

if(nrow(valid_data_rf_corrected) > 0) {
  
  # correlation
  correlation_rf_corrected <- cor(
    valid_data_rf_corrected$vol_ha,
    valid_data_rf_corrected$mean_pred_GSV_rf_corrected,
    use = 'complete.obs'
    )

  # RMSE
  rmse_rf_corrected <- sqrt(
    mean((valid_data_rf_corrected$vol_ha - valid_data_rf_corrected$mean_pred_GSV_rf_corrected)^2,
         na.rm = T)
    )
  
  # relative RMSE
  mean_observed_rf_corrected <- mean(valid_data_rf_corrected$vol_ha, na.rm = T)
  rel_rmse_rf_corrected <- (rmse_rf_corrected / mean_observed_rf_corrected) * 100
  
  # bias
  bias_rf_corrected <- mean(
    valid_data_rf_corrected$mean_pred_GSV_rf_corrected - valid_data_rf_corrected$vol_ha, na.rm = T
    )

  cat('\n=== RANDOM FOREST GAP-CORRECTED GSV PREDICTION - Validation Statistics ===\n')
  cat('Correlation (r):', round(correlation_rf_corrected, 3), '\n')
  cat('R-squared:', round(correlation_rf_corrected^2, 2), '\n')
  cat('RMSE:', round(rmse_rf_corrected, 2), '\n')
  cat('Relative RMSE (%):', round(rel_rmse_rf_corrected, 2), '\n')
  cat('Bias (Predicted - Reference):', round(bias_rf_corrected, 2), '\n')
  cat('Number of valid plots:', nrow(valid_data_rf_corrected), '\n\n')
   
}

} else {
  cat('\n=== RANDOM FOREST GAP-CORRECTED GSV PREDICTION ===\n')
  cat('No RF gap-corrected predictions available for validation.\n\n')
}



# 09 - comparison between original and corrected predictions (linear model)
#------------------------------------------------------------------------------

if (exists('valid_data_lm_original') && exists('valid_data_lm_corrected')) {
  
  cat('\n=== COMPARISON: ORIGINAL vs GAP-MASKED PREDICTIONS ===\n')
  
  # find common plots between original and corrected datasets
  # (plots that have valid predictions in both datasets)
  # drop geometry to avoid sf merge issues
  original_lm_data <- sf::st_drop_geometry(valid_data_lm_original[, c('key', 'vol_ha', 'mean_pred_GSV_lm')])
  corrected_lm_data <- sf::st_drop_geometry(valid_data_lm_corrected[, c('key', 'vol_ha', 'mean_pred_GSV_lm_corrected')])
  
  # rename prediction columns to avoid suffix confusion
  names(original_lm_data)[names(original_lm_data) == 'mean_pred_GSV_lm'] <- 'mean_pred_GSV_lm_orig'
  names(corrected_lm_data)[names(corrected_lm_data) == 'mean_pred_GSV_lm_corrected'] <- 'mean_pred_GSV_lm_corrected'
  
  common_lm_plots <- merge(
    original_lm_data,
    corrected_lm_data,
    by = c('key', 'vol_ha')  # merge by both key and vol_ha since they should be identical
  )
  
  if (nrow(common_lm_plots) > 0) {
    
    cat('Number of plots with both original and corrected LM predictions:', nrow(common_lm_plots), '\n\n')
    
    # calculate differences in validation metrics
    cat('IMPROVEMENT IN LM VALIDATION METRICS (Corrected vs Original):\n')
    
    if (exists('correlation_lm_orig') && exists('correlation_lm_corrected')) {
      corr_lm_diff <- correlation_lm_corrected - correlation_lm_orig
      cat('Correlation difference:', round(corr_lm_diff, 3), 
          ifelse(corr_lm_diff > 0, '(improved)', '(degraded)'), '\n')
      
      r2_lm_diff <- correlation_lm_corrected^2 - correlation_lm_orig^2
      cat('R-squared difference:', round(r2_lm_diff, 3), 
          ifelse(r2_lm_diff > 0, '(improved)', '(degraded)'), '\n')
    }
    
    if (exists('rmse_lm_orig') && exists('rmse_lm_corrected')) {
      rmse_lm_diff <- rmse_lm_corrected - rmse_lm_orig
      cat('RMSE difference:', round(rmse_lm_diff, 2), 
          ifelse(rmse_lm_diff < 0, '(improved)', '(degraded)'), '\n')
      
      rel_rmse_lm_diff <- rel_rmse_lm_corrected - rel_rmse_lm_orig
      cat('Relative RMSE difference (%):', round(rel_rmse_lm_diff, 2), 
          ifelse(rel_rmse_lm_diff < 0, '(improved)', '(degraded)'), '\n')
    }
    
    if (exists('bias_lm_orig') && exists('bias_lm_corrected')) {
      bias_lm_diff <- abs(bias_lm_corrected) - abs(bias_lm_orig)
      cat('Absolute bias difference:', round(bias_lm_diff, 2), 
          ifelse(bias_lm_diff < 0, '(improved)', '(degraded)'), '\n\n')
    }
    
  } else {
    cat('No common plots found between original and corrected LM datasets.\n')
  }
  
} else {
  cat('Cannot perform LM comparison - missing original or corrected LM validation data.\n')
}



# 10 - comparison between original and corrected predictions (random forest)
#------------------------------------------------------------------------------

if (exists('valid_data_rf_original') && exists('valid_data_rf_corrected')) {
  
  cat('\n=== COMPARISON: ORIGINAL vs GAP-MASKED RF PREDICTIONS ===\n')
  
  # find common plots between original and corrected RF datasets
  # (plots that have valid predictions in both datasets)
  # drop geometry to avoid sf merge issues
  original_rf_data <- sf::st_drop_geometry(valid_data_rf_original[, c('key', 'vol_ha', 'mean_pred_GSV_rf')])
  corrected_rf_data <- sf::st_drop_geometry(valid_data_rf_corrected[, c('key', 'vol_ha', 'mean_pred_GSV_rf_corrected')])
  
  # rename prediction columns to avoid suffix confusion
  names(original_rf_data)[names(original_rf_data) == 'mean_pred_GSV_rf'] <- 'mean_pred_GSV_rf_orig'
  names(corrected_rf_data)[names(corrected_rf_data) == 'mean_pred_GSV_rf_corrected'] <- 'mean_pred_GSV_rf_corrected'
  
  common_rf_plots <- merge(
    original_rf_data,
    corrected_rf_data,
    by = c('key', 'vol_ha')  # merge by both key and vol_ha since they should be identical
  )
  
  if (nrow(common_rf_plots) > 0) {
    
    cat('Number of plots with both original and corrected RF predictions:', nrow(common_rf_plots), '\n\n')
    
    # calculate differences in validation metrics
    cat('IMPROVEMENT IN RF VALIDATION METRICS (Corrected vs Original):\n')
    
    if (exists('correlation_rf_orig') && exists('correlation_rf_corrected')) {
      corr_rf_diff <- correlation_rf_corrected - correlation_rf_orig
      cat('Correlation difference:', round(corr_rf_diff, 3), 
          ifelse(corr_rf_diff > 0, '(improved)', '(degraded)'), '\n')
      
      r2_rf_diff <- correlation_rf_corrected^2 - correlation_rf_orig^2
      cat('R-squared difference:', round(r2_rf_diff, 3), 
          ifelse(r2_rf_diff > 0, '(improved)', '(degraded)'), '\n')
    }
    
    if (exists('rmse_rf_orig') && exists('rmse_rf_corrected')) {
      rmse_rf_diff <- rmse_rf_corrected - rmse_rf_orig
      cat('RMSE difference:', round(rmse_rf_diff, 2), 
          ifelse(rmse_rf_diff < 0, '(improved)', '(degraded)'), '\n')
      
      rel_rmse_rf_diff <- rel_rmse_rf_corrected - rel_rmse_rf_orig
      cat('Relative RMSE difference (%):', round(rel_rmse_rf_diff, 2), 
          ifelse(rel_rmse_rf_diff < 0, '(improved)', '(degraded)'), '\n')
    }
    
    if (exists('bias_rf_orig') && exists('bias_rf_corrected')) {
      bias_rf_diff <- abs(bias_rf_corrected) - abs(bias_rf_orig)
      cat('Absolute bias difference:', round(bias_rf_diff, 2), 
          ifelse(bias_rf_diff < 0, '(improved)', '(degraded)'), '\n\n')
    }
    
  } else {
    cat('No common plots found between original and corrected RF datasets.\n')
  }
  
} else {
  cat('Cannot perform RF comparison - missing original or corrected RF validation data.\n')
}



# 11 - comparison between linear model and random forest predictions
#------------------------------------------------------------------------------

if (exists('valid_data_lm_original') && exists('valid_data_rf_original')) {
  
  cat('\n=== COMPARISON: LINEAR MODEL vs RANDOM FOREST PREDICTIONS ===\n')
  
  # find common plots between LM and RF datasets
  # drop geometry to avoid sf merge issues
  lm_data <- sf::st_drop_geometry(valid_data_lm_original[, c('key', 'vol_ha', 'mean_pred_GSV_lm')])
  rf_data <- sf::st_drop_geometry(valid_data_rf_original[, c('key', 'vol_ha', 'mean_pred_GSV_rf')])
  
  # rename prediction columns to avoid suffix confusion
  names(lm_data)[names(lm_data) == 'mean_pred_GSV_lm'] <- 'mean_pred_GSV_lm'
  names(rf_data)[names(rf_data) == 'mean_pred_GSV_rf'] <- 'mean_pred_GSV_rf'
  
  common_lm_rf_plots <- merge(
    lm_data,
    rf_data,
    by = c('key', 'vol_ha')  # merge by both key and vol_ha since they should be identical
  )
  
  if (nrow(common_lm_rf_plots) > 0) {
    
    cat('Number of plots with both LM and RF predictions:', nrow(common_lm_rf_plots), '\n\n')
    
    # calculate differences in validation metrics
    cat('COMPARISON OF VALIDATION METRICS (RF vs LM):\n')
    
    if (exists('correlation_lm_orig') && exists('correlation_rf_orig')) {
      corr_lm_rf_diff <- correlation_rf_orig - correlation_lm_orig
      cat('Correlation difference (RF - LM):', round(corr_lm_rf_diff, 3), 
          ifelse(corr_lm_rf_diff > 0, '(RF better)', '(LM better)'), '\n')
      
      r2_lm_rf_diff <- correlation_rf_orig^2 - correlation_lm_orig^2
      cat('R-squared difference (RF - LM):', round(r2_lm_rf_diff, 3), 
          ifelse(r2_lm_rf_diff > 0, '(RF better)', '(LM better)'), '\n')
    }
    
    if (exists('rmse_lm_orig') && exists('rmse_rf_orig')) {
      rmse_lm_rf_diff <- rmse_rf_orig - rmse_lm_orig
      cat('RMSE difference (RF - LM):', round(rmse_lm_rf_diff, 2), 
          ifelse(rmse_lm_rf_diff < 0, '(RF better)', '(LM better)'), '\n')
      
      rel_rmse_lm_rf_diff <- rel_rmse_rf_orig - rel_rmse_lm_orig
      cat('Relative RMSE difference (RF - LM) (%):', round(rel_rmse_lm_rf_diff, 2), 
          ifelse(rel_rmse_lm_rf_diff < 0, '(RF better)', '(LM better)'), '\n')
    }
    
    if (exists('bias_lm_orig') && exists('bias_rf_orig')) {
      bias_lm_rf_diff <- abs(bias_rf_orig) - abs(bias_lm_orig)
      cat('Absolute bias difference (RF - LM):', round(bias_lm_rf_diff, 2), 
          ifelse(bias_lm_rf_diff < 0, '(RF better)', '(LM better)'), '\n\n')
    }
    
  } else {
    cat('No common plots found between LM and RF datasets.\n')
  }
  
} else {
  cat('Cannot perform LM vs RF comparison - missing LM or RF validation data.\n')
}













