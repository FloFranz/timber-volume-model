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

# input path to administrative data
orga_path <- paste0(raw_data_dir, 'orga/')



# 02 - data reading
#-------------------------------------

# read normalized point clouds with LAScatalog
ndsm_pc_ctg <- lidR::readLAScatalog(ndsm_pc_path)
ndsm_pc_ctg

# quick plot
lidR::plot(ndsm_pc_ctg)

# read BI data preprocessed in script vol_sample_plots.R
# contains timber volume per sample points
bi_plots <- sf::st_read(paste0(processed_data_dir, 'vol_stp_Kopie_GR_092023.gpkg'))

# quick overview
bi_plots
str(bi_plots)

# read administrative forestry data of Lower Saxony
nlf_org <- sf::st_read(paste0(orga_path, 'NLF_Org_2022.shp'))
nlf_org
str(nlf_org)



# 03 - data preparation
#-------------------------------------

# filter plots by year (2022) and forestry office (Neuhaus = 268)
bi_plots_neuhaus <- bi_plots[grep('268-2022-', bi_plots$key),]

# filter administrative forestry data
# by forestry office 'Neuhaus' (268) 
fa_neuhaus <- nlf_org[nlf_org$FORSTAMT == 268,]

# assign CRS to point clouds (ETRS89 / UTM zone 32N)
lidR::crs(ndsm_pc_ctg) <- 'EPSG:25832'

# reproject BI plots to the CRS of the point clouds
# DHDN / 3-degree Gauss-Kruger zone 3 --> ETRS89 / UTM zone 32N
bi_plots_neuhaus <- sf::st_transform(bi_plots_neuhaus, sf::st_crs(25832))

# visualize locations of BI plots
lidR::plot(ndsm_pc_ctg)
terra::plot(bi_plots_neuhaus$geom, col = 'red', add = T)
terra::plot(fa_neuhaus$geometry, alpha = 0, lwd = 2, add = T)

# get polygons (squares) of point clouds in the catalog
# that are in the area of the desired forestry office
ctg_polys_neuhaus <- sf::st_crop(ndsm_pc_ctg@data[["geometry"]],
                                 fa_neuhaus)

ctg_polys_neuhaus <- sf::st_sf(ctg_polys_neuhaus)

# quick overview
terra::plot(ctg_polys_neuhaus)
terra::plot(bi_plots_neuhaus$geom, add= T, col = 'red')
terra::plot(fa_neuhaus$geometry, alpha = 0, lwd = 2, add = T)

# extract the point clouds that are in the
# area of the desired forestry office
dir.create(paste0(processed_data_dir, 'nDSMs_laz_neuhaus'))
lidR::opt_output_files(ndsm_pc_ctg) <- paste0(processed_data_dir, 'nDSMs_laz_neuhaus/{XLEFT}_{YBOTTOM}_{ID}')
ndsm_pc_ctg_neuhaus <- lidR::clip_roi(ndsm_pc_ctg, ctg_polys_neuhaus)
