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
# APPROACH 1: raster solution
# resample gap rasters to match the cropped GSV raster resolution and alignment
pred_gaps_t1_resampled <- terra::resample(pred_gaps_t1, pred_gsv_t1, method = 'near')
pred_gaps_t2_resampled <- terra::resample(pred_gaps_t2, pred_gsv_t2, method = 'near')
pred_gaps_t3_resampled <- terra::resample(pred_gaps_t3, pred_gsv_t3, method = 'near')

# APPROACH 2: vector solution
# vectorize gap rasters to polygons, keep only gap polygons (gaps = 1)
gaps_poly_t1 <- terra::as.polygons(pred_gaps_t1, values = T)
gaps_poly_t1 <- gaps_poly_t1[gaps_poly_t1[[1]] == 1, ]
gaps_poly_t2 <- terra::as.polygons(pred_gaps_t2, values = T)
gaps_poly_t2 <- gaps_poly_t2[gaps_poly_t2[[1]] == 1, ]
gaps_poly_t3 <- terra::as.polygons(pred_gaps_t3, values = T)
gaps_poly_t3 <- gaps_poly_t3[gaps_poly_t3[[1]] == 1, ]

# visualization: compare raster vs vector gaps
par(mfrow = c(2,3))

# top row: raster gaps (coarse, 20m resolution)
terra::plot(
  pred_gsv_t1$PredictionPlusForestClassification_1,
  col = cmocean::cmocean('speed')(50),
  main = 'Tile 1 - raster gaps'
  )
terra::plot(
  terra::ifel(
    pred_gaps_t1_resampled == 0, NA, pred_gaps_t1_resampled),
  col = 'white', add = T
  )
terra::plot(
  pred_gsv_t2$PredictionPlusForestClassification_1,
  col = cmocean::cmocean('speed')(50), 
  main = 'Tile 2 - raster gaps'
  )
terra::plot(
  terra::ifel(
    pred_gaps_t2_resampled == 0, NA, pred_gaps_t2_resampled),
  col = 'white', add = T
  )
terra::plot(
  pred_gsv_t3$PredictionPlusForestClassification_1,
  col = cmocean::cmocean('speed')(50),
  main = 'Tile 3 - raster gaps'
  )
terra::plot(
  terra::ifel(
    pred_gaps_t3_resampled == 0, NA, pred_gaps_t3_resampled),
  col = 'white', add = T
  )

# bottom row: vector gaps (high-resolution, 0.5m boundaries)
terra::plot(
  pred_gsv_t1$PredictionPlusForestClassification_1,
  col = cmocean::cmocean('speed')(50), 
  main = 'Tile 1 - vector gaps'
  )
terra::plot(gaps_poly_t1, col = NA, border = 'white', lwd = 1, add = T)
terra::plot(
  pred_gsv_t2$PredictionPlusForestClassification_1,
  col = cmocean::cmocean('speed')(50),
  main = 'Tile 2 - vector gaps'
  )
terra::plot(gaps_poly_t2, col = NA, border = 'white', lwd = 1, add = T)
terra::plot(
  pred_gsv_t3$PredictionPlusForestClassification_1,
  col = cmocean::cmocean('speed')(50),
  main = 'Tile 3 - vector gaps'
  )
terra::plot(gaps_poly_t3, col = NA, border = 'white', lwd = 1, add = T)



# 03 - masking out canopy gaps in GSV prediction
#------------------------------------------------------------------------------

# APPROACH 1: raster-based masking
corrected_gsv_raster_t1 <- terra::mask(
  pred_gsv_t1$PredictionPlusForestClassification_1,
  pred_gaps_t1_resampled,
  maskvalues = 1
  )
corrected_gsv_raster_t2 <- terra::mask(
  pred_gsv_t2$PredictionPlusForestClassification_1,
  pred_gaps_t2_resampled, 
  maskvalues = 1
  )
corrected_gsv_raster_t3 <- terra::mask(
  pred_gsv_t3$PredictionPlusForestClassification_1,
  pred_gaps_t3_resampled, 
  maskvalues = 1
  )

# APPROACH 2: vector-based masking
corrected_gsv_vector_t1 <- terra::mask(
  pred_gsv_t1$PredictionPlusForestClassification_1,
  gaps_poly_t1,
  inverse = T
  )
corrected_gsv_vector_t2 <- terra::mask(
  pred_gsv_t2$PredictionPlusForestClassification_1, 
  gaps_poly_t2, 
  inverse = T
  )
corrected_gsv_vector_t3 <- terra::mask(
  pred_gsv_t3$PredictionPlusForestClassification_1,
  gaps_poly_t3, 
  inverse = T
  )

# compare the results
for (i in 1:3) {
  original_pixels <- sum(
    !is.na(terra::values(
      get(paste0('pred_gsv_t', i))$PredictionPlusForestClassification_1)
      )
    )
  raster_pixels <- sum(
    !is.na(terra::values(get(paste0('corrected_gsv_raster_t', i)))))
  vector_pixels <- sum(
    !is.na(terra::values(get(paste0('corrected_gsv_vector_t', i)))))
  
  cat('Tile', i, 'Results:\n')
  cat('  Original GSV pixels:', original_pixels, '\n')
  cat('  Raster approach - remaining pixels:', raster_pixels, '\n')
  cat('  Vector approach - remaining pixels:', vector_pixels, '\n')
  cat('  Raster approach - % masked:', 
      round((1 - raster_pixels / original_pixels) * 100, 2), '%\n')
  cat('  Vector approach - % masked:',
      round((1 - vector_pixels / original_pixels) * 100, 2), '%\n\n')
}

# visualization: compare raster vs vector approach
par(mfrow = c(2,3))

# top row: raster gaps removed from GSV
terra::plot(
  corrected_gsv_raster_t1,
  col = cmocean::cmocean('speed')(50),
  main = 'Tile 1 - raster approach'
)
terra::plot(
  corrected_gsv_raster_t2,
  col = cmocean::cmocean('speed')(50),
  main = 'Tile 2 - raster approach'
)
terra::plot(
  corrected_gsv_raster_t3,
  col = cmocean::cmocean('speed')(50),
  main = 'Tile 3 - raster approach'
)

# bottom row: vector gaps removed from GSV
terra::plot(
  corrected_gsv_vector_t1,
  col = cmocean::cmocean('speed')(50), 
  main = 'Tile 1 - vector approach'
)
terra::plot(
  corrected_gsv_vector_t2,
  col = cmocean::cmocean('speed')(50), 
  main = 'Tile 2 - vector approach'
)
terra::plot(
  corrected_gsv_vector_t3,
  col = cmocean::cmocean('speed')(50), 
  main = 'Tile 3 - vector approach'
)

# save both results for comparison
terra::writeRaster(corrected_gsv_raster_t1, file.path(output_dir, 'corrected_gsv_raster_t1.tif'), overwrite = TRUE)
terra::writeRaster(corrected_gsv_vector_t1, file.path(output_dir, 'corrected_gsv_vector_t1.tif'), overwrite = TRUE)

























