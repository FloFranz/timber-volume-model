#--------------------------------------------------------------------------
# Name:         pc_ctg_extraction.R
# Description:  Script extracts multiple polygons of point clouds from a
#               point cloud catalog located in a desired forestry office.
#               The extracted point clouds are written to disk.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#--------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01 - set file paths
#-------------------------------------

# input path to normalized point clouds
ndsm_pc_path <- paste0(raw_data_dir, 'nDSMs_laz/')



# 02 - data reading
#-------------------------------------

# read normalized point clouds with LAScatalog
ndsm_pc_ctg <- lidR::readLAScatalog(ndsm_pc_path)
ndsm_pc_ctg

# quick plot
lidR::plot(ndsm_pc_ctg)

# read BI data preprocessed in script vol_sample_plots.R
# contains timber volume per sample points
bi_plots <- sf::st_read(paste0(processed_data_dir, 'vol_stp.gpkg'))

# quick overview
bi_plots
str(bi_plots)



# 03 - data preparation
#-------------------------------------

# convert column vol_ha in bi_plots to numeric
bi_plots$vol_ha <- as.numeric(bi_plots$vol_ha)
str(bi_plots)

# filter plots by year (2022) and forestry office (Neuhaus = 268)
bi_plots_neuhaus <- bi_plots[grep('268-2022-', bi_plots$key),]

# assign CRS to point clouds (ETRS89 / UTM zone 32N)
lidR::crs(ndsm_pc_ctg) <- 'EPSG:25832'

# reproject BI plots to the CRS of the point clouds
# DHDN / 3-degree Gauss-Kruger zone 3 --> ETRS89 / UTM zone 32N
bi_plots_neuhaus <- sf::st_transform(bi_plots_neuhaus, sf::st_crs(25832))

# visualize locations of BI plots
lidR::plot(ndsm_pc_ctg)
lidR::plot(bi_plots_neuhaus, add = T, col = 'red')

# get polygons (squares) of point clouds in the catalog
# that are in the area of the desired forestry office
ctg_polys_neuhaus <- sf::st_crop(ndsm_pc_ctg@data[["geometry"]],
                                 bi_plots_neuhaus)

ctg_polys_neuhaus <- sf::st_sf(ctg_polys_neuhaus)

# quick overview
plot(ctg_polys_neuhaus)
plot(bi_plots_neuhaus$geom, add= T, col = 'red')

# extract the point clouds that are in the
# area of the desired forestry office
#lidR::opt_output_files(ndsm_pc_ctg) <- paste0(tempfile(tmpdir = 'Y:/FFranz/nDSM_pc_ctg_neuhaus_new'), '/{XLEFT}_{YBOTTOM}_{ID}')
lidR::opt_output_files(ndsm_pc_ctg) <- 'Y:/FFranz/nDSM_pc_ctg_neuhaus_new/{XLEFT}_{YBOTTOM}_{ID}'
ndsm_pc_ctg_neuhaus <- lidR::clip_roi(ndsm_pc_ctg, ctg_polys_neuhaus)
