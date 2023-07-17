#--------------------------------------------------------------------------
# Name:         forest_metrics_pc.R
# Description:  script calculates several metrics in terrestrial
#               sample plot points based on preprocessed point clouds 
#               generated via image matching.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#--------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01 - set file paths
#-------------------------------------

# input path to point clouds (normalized and not normalized)
dsm_pc_path <- paste0(raw_data_dir, 'DSMs_laz/')
ndsm_pc_path <- paste0(raw_data_dir, 'nDSMs_laz/')

# input path to terrestrial data
bi_path <- paste0(raw_data_dir, 'BI/')



# 02 - data reading
#-------------------------------------

# read normalized point clouds
ndsm_pc_files <- list.files(ndsm_pc_path)

ndsm_pc <- lidR::readLAS(paste0(ndsm_pc_path, ndsm_pc_files))

# quick plot
lidR::plot(ndsm_pc)

# read BI data
bi_files <- list.files(bi_path)

bi_plots <- sf::st_read(paste0(bi_path, bi_files[4]))

# quick overview
bi_plots
str(bi_plots)



# 03 - data preperation
#-------------------------------------

# source and apply function for data formatting
# on the BI data
source('src/format_data.R', local = TRUE)

bi_plots <- format_data(bi_plots)

# select needed columns
bi_plots <- bi_plots[,c('key', 'kspnr', 'fa', 'rw', 'hw', 'stj')]

# filter plots by year (2022)
bi_plots <- bi_plots[bi_plots$stj == 2022,]

# add column with plot number
bi_plots$plot <- paste0('plot_', 1:nrow(bi_plots))

head(bi_plots)
str(bi_plots)

# assign CRS to point clouds (ETRS89 / UTM zone 32N)
# and project it to the CRS of the BI plots 
# (DHDN / 3-degree Gauss-Kruger zone 3)
lidR::crs(ndsm_pc) <- 'EPSG:25832'
ndsm_pc_projected <- sf::st_transform(ndsm_pc, sf::st_crs(31467))

# get extent of the point cloud and use it
# to crop the BI plots to the point cloud
ndsm_pc_projected_ext <- lidR::extent(ndsm_pc_projected)
bi_plots_cropped <- sf::st_crop(bi_plots, ndsm_pc_projected_ext)

# filter point cloud to ignore noise below 0 m
ndsm_pc_projected <- lidR::filter_poi(ndsm_pc_projected, Z >= 0)



# 04.1
# calculation of metrics based on the height values in
# the point clouds within the sample plots (radius 13m)
#--------------------------------------------------------


plot_metrics <- lidR::plot_metrics(ndsm_pc_projected, ~mean(Z),
                                   bi_plots_cropped, radius = 13)











