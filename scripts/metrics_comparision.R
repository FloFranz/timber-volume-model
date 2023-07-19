#----------------------------------------------------------------------------------
# Name:         metrics_comparision.R
# Description:  script compares forest metrics previously calculated from
#               normalized point clouds and normalized digital surface models (nDSM).
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#----------------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01 - data reading
#-------------------------------------

# read data frames with calculated metrics per sample plot
# plot_metrics_sm = metrics based on nDSM
# plot_metrics_pc = metrics based on normalized point cloud
plot_metrics_sm <- readRDS(paste0(processed_data_dir, 'plot_metrics_sm.RDS'))
plot_metrics_pc <- readRDS(paste0(processed_data_dir, 'plot_metrics_pc.RDS'))