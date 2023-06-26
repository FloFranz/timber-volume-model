#-------------------------------------------------------------
# Name:         lm_timber_vol.R
# Description:  script creates a linear model to predict 
#               timber volume based on remote sensing data and
#               terrestrial sample plot measurements.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#-------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01 - set file paths
#-------------------------------------

# input path to DSMs and nDSM
dsm_path <- paste0(raw_data_dir, 'DSMs/')
ndsm_path <- paste0(raw_data_dir, 'nDSMs/')

# input path to terrestrial data
bi_path <- paste0(raw_data_dir, 'BI/')



# 02 - data reading
#-------------------------------------

# read nDSM
ndsm_files <- list.files(ndsm_path)

ndsm <- terra::rast(paste0(ndsm_path, ndsm_files))

# quick plot
terra::plot(ndsm)

# read BI data
bi_tables <- list.files(bi_path)

bi_vorr <- read.table(paste0(bi_path, bi_tables[1]),
                      header = T, sep = ';')

bi_plots <- read.table(paste0(bi_path, bi_tables[2]),
                       header = T, sep = ';')

head(bi_vorr)
head(bi_plots)



# 03 - data preperation
#-------------------------------------

source('src/format_data.R', local = TRUE)

bi_plots <- format_data(bi_plots)
bi_vorr <- format_data(bi_vorr)

head(bi_plots)
str(bi_plots)
head(bi_vorr)
str(bi_vorr)







