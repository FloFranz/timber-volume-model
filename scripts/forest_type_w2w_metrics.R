#------------------------------------------------------------------------------
# Name:         forest_type_w2w_metrics.R
# Description:  Script adds forest type information to the wall-to-wall
#               calculated metrics. Forest type information is obtained from
#               the Copernicus Land Monitoring Service (CLMS) Forest Type 2021
#               product (https://land.copernicus.eu/en/products/high-resolution-layer-forests-and-tree-cover/forest-type-2021-raster-10-m-100-m-europe-3-yearly#general_info).
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#------------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01 - data reading
#-------------------------------------

# read w2w-metrics
metrics_w2w <- terra::rast(file.path(output_dir, 'metrics_w2w_solling.tif'))
metrics_w2w

# read CLMS forest type data
# two tiles covering the Solling area
forest_type_1 <- terra::rast(
  file.path(raw_data_dir,
            'tree_species',
            'CLMS_HRLVLCC_FTY_S2021_R10m_E42N31_03035_V01_R00.tif')
  )

forest_type_2 <- terra::rast(
  file.path(raw_data_dir,
            'tree_species',
            'CLMS_HRLVLCC_FTY_S2021_R10m_E43N31_03035_V01_R00.tif')
)

forest_type_1
forest_type_2



# 02 - data processing
#-------------------------------------

# merge CLMS forest type tiles
forest_type_merg <- terra::mosaic(forest_type_1, forest_type_2)

# assign CRS (EPSG:25832) to w2w-metrics raster
terra::crs(metrics_w2w) <- 'EPSG:25832'

# reproject merged forest type raster to EPSG:25832
forest_type_merg <- terra::project(
  forest_type_merg, metrics_w2w, method = 'near'
  )

# add forest type information to w2w-metrics raster
metrics_w2w$forest_type <- forest_type_merg
metrics_w2w$forest_type
terra::plot(metrics_w2w$forest_type)

# write to disk
terra::writeRaster(
  metrics_w2w,
  file.path(output_dir, 'metrics_w2w_solling_incl_forest_type.tif')
  )

















