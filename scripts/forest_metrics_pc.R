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
source('src/setup.R', local = TRUE)



# 01 - set file paths
#-------------------------------------

# input path to point clouds (normalized and not normalized)
dsm_pc_path <- paste0(raw_data_dir, 'DSMs_laz/')
ndsm_pc_path <- paste0(raw_data_dir, 'nDSMs_laz/')



# 02 - data reading
#-------------------------------------

# read normalized point cloud
ndsm_pc_files <- list.files(ndsm_pc_path)
ndsm_pc <- lidR::readLAS(paste0(ndsm_pc_path, ndsm_pc_files[1]))
ndsm_pc

# quick plot
lidR::plot(ndsm_pc)

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

# filter plots by year (2022)
bi_plots <- bi_plots[grep('-2022-', bi_plots$key),]

# assign CRS to point cloud (ETRS89 / UTM zone 32N)
lidR::crs(ndsm_pc) <- 'EPSG:25832'

# reproject BI plots to the CRS of the point cloud
# DHDN / 3-degree Gauss-Kruger zone 3 --> ETRS89 / UTM zone 32N
bi_plots_projected <- sf::st_transform(bi_plots, sf::st_crs(25832))

# get extent of the point cloud and use it
# to crop the BI plots to the point cloud
ndsm_pc_ext <- lidR::extent(ndsm_pc)
bi_plots_cropped <- sf::st_crop(bi_plots_projected, ndsm_pc_ext)

# filter point cloud to ignore noise below 0 m
ndsm_pc <- lidR::filter_poi(ndsm_pc, Z >= 0)



# 04 - calculation of metrics
#--------------------------------------------------------

# create function that calculates several metrics
# --> potential explanatory variables for a timber volume model
#
# height metrics: mean, standard deviation, minimum, maximum,
# percentile values (1st, 5th, 10th, 20th, 25th, 30th, 40th, 50th,
#                    60th, 70th, 75th, 80th, 90th, 95th, 99th);
# variability metrics: skewness, kurtosis, coefficient of variation
# (all three as conventional moments and as L-moments),
# canopy relief ratio (crr) --> https://doi.org/10.1016/j.foreco.2003.09.001;
# canopy cover metrics: percentage of points above 3m and above mean height
# --> see different studies, e.g. https://doi.org/10.1139/cjfr-2014-0297
calc_metrics <- function(z) {
  
  probs <- c(0.01, 0.05, 0.1, 0.2, 0.25,
             0.3, 0.4, 0.5, 0.6, 0.7,
             0.75, 0.8, 0.9, 0.95, 0.99)
  
  zq <- stats::quantile(z, probs, na.rm = T)
  
  list(zmean = mean(z, na.rm = T), zsd = sd(z, na.rm = T),
       zmin = min(z, na.rm = T), zmax = max(z, na.rm = T),
       zq1 = zq[1], zq5 = zq[2], zq10 = zq[3], zq20 = zq[4],
       zq25 = zq[5], zq30 = zq[6], z40 = zq[7], zq50 = zq[8],
       zq60 = zq[9], zq70 = zq[10], zq75 = zq[11], zq80 = zq[12],
       zq90 = zq[13], zq95 = zq[14], zq99 = zq[15],
       zskew = moments::skewness(z, na.rm = T),
       zkurt = moments::kurtosis(z, na.rm = T),
       zcv = sd(z, na.rm = T) / mean(z, na.rm = T) * 100,
       zskew_lmom = lmom::samlmu(z)[3],
       zkurt_lmom = lmom::samlmu(z)[4],
       zcv_lmom = lmom::samlmu(z, ratios = F)[2] / lmom::samlmu(z, ratios = F)[1],
       zcrr = ((mean(z, na.rm = T) - min(z, na.rm = T)) / (max(z, na.rm = T) - min(z, na.rm = T))),
       pzabove3 = (sum(z > 3, na.rm = T) / length(z)) * 100,
       pzabovezmean = (sum(z > mean(z, na.rm = T), na.rm = T) / length(z)) * 100)
  
}

# calculate the predefined metrics for each plot (radius = 13 m) 
# within the normalized point cloud
plot_metrics <- lidR::plot_metrics(ndsm_pc, ~calc_metrics(Z),
                                   bi_plots_cropped, radius = 13)











