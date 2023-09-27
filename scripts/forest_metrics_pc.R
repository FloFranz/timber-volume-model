#-----------------------------------------------------------------------------------
# Name:         forest_metrics_pc.R
# Description:  Script calculates several metrics in terrestrial
#               sample plot points based on preprocessed point clouds 
#               generated via image matching.
#               This script is designed to test metrics for one single point cloud.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#-----------------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = T)



# 01 - set file paths
#-------------------------------------

# input path to point clouds (normalized and not normalized)
dsm_pc_path <- paste0(raw_data_dir, 'DSMs_laz/')
ndsm_pc_path <- paste0(raw_data_dir, 'nDSMs_laz/')



# 02 - data reading
#-------------------------------------

# read normalized point cloud
ndsm_pc_files <- list.files(ndsm_pc_path)
ndsm_pc <- lidR::readLAS(paste0(ndsm_pc_path, ndsm_pc_files[594]))
ndsm_pc

# quick plot
lidR::plot(ndsm_pc)

# read BI data preprocessed in script vol_sample_plots.R
# contains timber volume per sample points
bi_plots <- sf::st_read(paste0(processed_data_dir, 'vol_stp_092023.gpkg'))

# quick overview
bi_plots
str(bi_plots)



# 03 - data preparation
#-------------------------------------

# filter plots by year (2022) and forestry office
# (Neuhaus = 268, Dassel = 254)
bi_plots <- bi_plots[grep('254-2022-', bi_plots$key),]

# assign CRS to point cloud (ETRS89 / UTM zone 32N)
lidR::crs(ndsm_pc) <- 'EPSG:25832'

# reproject BI plots to the CRS of the point cloud
# DHDN / 3-degree Gauss-Kruger zone 3 --> ETRS89 / UTM zone 32N
bi_plots_projected <- sf::st_transform(bi_plots, sf::st_crs(25832))

# get extent of the point cloud and use it
# to crop the BI plots to the point cloud
ndsm_pc_ext <- lidR::extent(ndsm_pc)
bi_plots_cropped <- sf::st_crop(bi_plots_projected, ndsm_pc_ext)



# 04 - calculation of metrics
#--------------------------------------------------------

# source function for metrics calculation
source('src/calc_metrics.R', local = T)

# calculate the predefined metrics for each plot (radius = 13 m) 
# within the normalized point cloud
# 2 m height threshold according to literature
ndsm_pc <- lidR::filter_poi(ndsm_pc, Z >= 2)

plot_metrics <- lidR::plot_metrics(ndsm_pc, ~calc_metrics(Z, R, B),
                                   bi_plots_cropped, radius = 13)










