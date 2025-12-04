#------------------------------------------------------------------------------
# Name:         gsv_correction.R
# Description:  Script corrects growing stock volume (GSV) predictions by
#               masking out canopy gaps.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#------------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01 - data reading
#------------------------------------------------------------------------------

# read GSV prediction
pred_gsv <- terra::rast(
  file.path(output_dir, 'PredictionPlusForestClassification.tif')
  )
pred_gsv

# plot
terra::plot(
  pred_gsv$PredictionPlusForestClassification_1,
  col = cmocean::cmocean('speed')(50)
  )

# read gap prediction
# three tiles
pred_gaps_t1 <- terra::rast(file.path(processed_data_dir, 'pred_gaps_tile1.tif'))
pred_gaps_t2 <- terra::rast(file.path(processed_data_dir, 'pred_gaps_tile2.tif'))
pred_gaps_t3 <- terra::rast(file.path(processed_data_dir, 'pred_gaps_tile3.tif'))
pred_gaps_t1
pred_gaps_t2
pred_gaps_t3

# plot
par(mfrow = c(1,3))
terra::plot(pred_gaps_t1)
terra::plot(pred_gaps_t2)
terra::plot(pred_gaps_t3)



# 02 - data preparation
#------------------------------------------------------------------------------

# crop predicted GSV raster to gap raster extents
pred_gsv_t1 <- terra::crop(pred_gsv, pred_gaps_t1)
pred_gsv_t2 <- terra::crop(pred_gsv, pred_gaps_t2)
pred_gsv_t3 <- terra::crop(pred_gsv, pred_gaps_t3)
pred_gsv_t1
pred_gsv_t2
pred_gsv_t3

# plot the cropped predicted GSV rasters
par(mfrow = c(1,3))
terra::plot(
  pred_gsv_t1$PredictionPlusForestClassification_1,
  col = cmocean::cmocean('speed')(50)
  )
terra::plot(
  pred_gsv_t2$PredictionPlusForestClassification_1,
  col = cmocean::cmocean('speed')(50)
  )
terra::plot(
  pred_gsv_t3$PredictionPlusForestClassification_1,
  col = cmocean::cmocean('speed')(50)
  )

# prepare gaps for masking
# resample gap rasters to match the cropped GSV raster resolution and alignment
pred_gaps_t1_resampled <- terra::resample(
  pred_gaps_t1, pred_gsv_t1, method = 'average'
  )
pred_gaps_t2_resampled <- terra::resample(
  pred_gaps_t2, pred_gsv_t2, method = 'average'
  )
pred_gaps_t3_resampled <- terra::resample(
  pred_gaps_t3, pred_gsv_t3, method = 'average'
  )

# threshold the resampled gap rasters at 0.25 (25% gap coverage per 20x20m cell)
pred_gaps_t1_resampled_025 <- pred_gaps_t1_resampled > 0.25
pred_gaps_t2_resampled_025 <- pred_gaps_t2_resampled > 0.25
pred_gaps_t3_resampled_025 <- pred_gaps_t3_resampled > 0.25

# visualization: show GSV with thresholded gaps
par(mfrow = c(1,3))

terra::plot(
  pred_gsv_t1$PredictionPlusForestClassification_1,
  col = cmocean::cmocean('speed')(50),
  main = 'Tile 1 - gaps (threshold 0.25)'
  )
terra::plot(
  terra::ifel(
    pred_gaps_t1_resampled_025 == 0, NA, pred_gaps_t1_resampled_025),
  col = 'white', add = T
  )
terra::plot(
  pred_gsv_t2$PredictionPlusForestClassification_1,
  col = cmocean::cmocean('speed')(50), 
  main = 'Tile 2 - gaps (threshold 0.25)'
  )
terra::plot(
  terra::ifel(
    pred_gaps_t2_resampled_025 == 0, NA, pred_gaps_t2_resampled_025),
  col = 'white', add = T
  )
terra::plot(
  pred_gsv_t3$PredictionPlusForestClassification_1,
  col = cmocean::cmocean('speed')(50),
  main = 'Tile 3 - gaps (threshold 0.25)'
  )
terra::plot(
  terra::ifel(
    pred_gaps_t3_resampled_025 == 0, NA, pred_gaps_t3_resampled_025),
  col = 'white', add = T
  )



# 03 - masking out canopy gaps in GSV prediction
#------------------------------------------------------------------------------

# raster-based masking using thresholded gaps
masked_gsv_t1 <- terra::mask(
  pred_gsv_t1$PredictionPlusForestClassification_1,
  pred_gaps_t1_resampled_025,
  maskvalues = 1
  )
masked_gsv_t2 <- terra::mask(
  pred_gsv_t2$PredictionPlusForestClassification_1,
  pred_gaps_t2_resampled_025, 
  maskvalues = 1
  )
masked_gsv_t3 <- terra::mask(
  pred_gsv_t3$PredictionPlusForestClassification_1,
  pred_gaps_t3_resampled_025, 
  maskvalues = 1
  )

# compare the results
for (i in 1:3) {
  original_pixels <- sum(
    !is.na(terra::values(
      get(paste0('pred_gsv_t', i))$PredictionPlusForestClassification_1)
      )
    )
  masked_pixels <- sum(
    !is.na(terra::values(get(paste0('masked_gsv_t', i)))))
  
  cat('Tile', i, 'Results:\n')
  cat('  Original GSV pixels:', original_pixels, '\n')
  cat('  Masked GSV pixels (remaining):', masked_pixels, '\n')
  cat('  Pixels masked out:', original_pixels - masked_pixels, '\n')
  cat('  Percentage masked:', 
      round((1 - masked_pixels / original_pixels) * 100, 2), '%\n\n')
}

# visualization: show masked GSV results
par(mfrow = c(1,3))

terra::plot(
  masked_gsv_t1,
  col = cmocean::cmocean('speed')(50),
  main = 'Tile 1 - masked GSV'
)
terra::plot(
  masked_gsv_t2,
  col = cmocean::cmocean('speed')(50),
  main = 'Tile 2 - masked GSV'
)
terra::plot(
  masked_gsv_t3,
  col = cmocean::cmocean('speed')(50),
  main = 'Tile 3 - masked GSV'
)

# save masked GSV results
terra::writeRaster(
  masked_gsv_t1, 
  file.path(processed_data_dir, 'masked_gsv_t1.tif'), overwrite = T
  )
terra::writeRaster(
  masked_gsv_t2,
  file.path(processed_data_dir, 'masked_gsv_t2.tif'), overwrite = T
  )
terra::writeRaster(
  masked_gsv_t3, 
  file.path(processed_data_dir, 'masked_gsv_t3.tif'), overwrite = T
  )



# 04 - calculate mean GSV before and after masking
#------------------------------------------------------------------------------

# get values of original predicted GSV raster and 
# the masked GSV raster with gaps masked out
for (i in 1:3) {
  
  original_values <- terra::values(
    get(paste0('pred_gsv_t', i))$PredictionPlusForestClassification_1
    )
  masked_values <- terra::values(get(paste0('masked_gsv_t', i)))
  
  # replace masked pixels (NA) with 0
  masked_values_with_zeros <- masked_values
  masked_values_with_zeros[is.na(masked_values_with_zeros)] <- 0
  
  cat('=== TILE', i, '- GSV MASKING RESULTS ===\n')
  cat('Mean GSV before masking:', 
      round(mean(original_values, na.rm = T), 2), 'm³/ha\n')
  cat('Mean GSV after masking (gaps = 0):', 
      round(mean(masked_values_with_zeros, na.rm = T), 2), 'm³/ha\n')
  cat('Difference in mean GSV:', 
      round(
        mean(masked_values_with_zeros, na.rm = T) - 
          mean(original_values, na.rm = T), 2), 'm³/ha\n\n')
}
























