#--------------------------------------------------------------------------
# Name:         forest_metrics_pc_ctg.R
# Description:  Script calculates several metrics in terrestrial
#               sample plot points based on preprocessed point clouds
#               generated via image matching. 
#               LAScatalog is used to process several point clouds.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#--------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = T)



# 01 - set file paths
#-------------------------------------

# input path to point clouds (normalized and not normalized)
# point clouds were previously extracted from a larger area
# in script pc_ctg_extraction.R, now represent a forestry office
#dsm_pc_path <- paste0(raw_data_dir, 'DSMs_laz/')
ndsm_pc_path <- paste0(processed_data_dir, 'nDSMs_laz_neuhaus/')

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
bi_plots <- sf::st_read(paste0(processed_data_dir, 'vol_stp_092023.gpkg'))

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
bi_plots <- bi_plots[grep('268-2022-', bi_plots$key),]

# assign CRS to point clouds (ETRS89 / UTM zone 32N)
lidR::crs(ndsm_pc_ctg) <- 'EPSG:25832'

# reproject BI plots to the CRS of the point clouds
# DHDN / 3-degree Gauss-Kruger zone 3 --> ETRS89 / UTM zone 32N
bi_plots_projected <- sf::st_transform(bi_plots, sf::st_crs(25832))

# filter administrative forestry data
# by forestry office 'Neuhaus' (268) 
fa_neuhaus <- nlf_org[nlf_org$FORSTAMT == 268,]

# visualize locations of BI plots
lidR::plot(ndsm_pc_ctg, mapview = T, 
           map.type = 'OpenStreetMap',
           alpha.regions = 0) +
  
  mapview::mapview(bi_plots_projected, col.regions = 'red', cex = 2) +
  mapview::mapview(fa_neuhaus, alpha.regions = 0, lwd = 2)



# 04 - calculation of metrics
#--------------------------------------------------------

# source function for metrics calculation
source('src/calc_metrics.R', local = T)


# calculate the predefined metrics for each plot (radius = 13 m) 
# within the normalized point cloud
# 2 m height threshold according to literature
# save data frame with the plots and calculated metrics
# if the data frame with the metrics already exists, read it
if (!file.exists(paste0(processed_data_dir, 'plot_metrics_pc.RDS'))) {
  
  lidR::opt_filter(ndsm_pc_ctg) <- '-drop_z_below 2'
  
  plot_metrics <- lidR::plot_metrics(ndsm_pc_ctg, ~calc_metrics(Z, R, B),
                                     bi_plots_projected, radius = 13)
  
  # remove rows with NA (two plots are empty)
  plot_metrics <- na.omit(plot_metrics)
  
  saveRDS(plot_metrics, file = paste0(processed_data_dir, 'plot_metrics_pc.RDS'))
  
} else {
  
  plot_metrics <- readRDS(paste0(processed_data_dir, 'plot_metrics_pc.RDS'))
  
}

# plot correlogram of the metrics
plot_metrics_df <- as.data.frame(plot_metrics)
corrplot::corrplot(cor(plot_metrics_df[, -(c(1:4, 6, ncol(plot_metrics_df)))]),
                   method = 'circle', type= 'full')















