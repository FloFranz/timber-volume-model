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



# 02 - data reading and preperation
#-------------------------------------

# read nDSM
ndsm_files <- list.files(ndsm_path)

ndsm <- terra::rast(paste0(ndsm_path, ndsm_files))

# quick plot
terra::plot(ndsm)

# read BI data
bi_tables <- list.files(bi_path)

bi_plots <- read.table(paste0(bi_path, bi_tables),
                       header = T, sep = ';')

head(bi_plots)

# column names to lower cases
names(bi_plots) <- tolower(names(bi_plots))

dat <- data.frame(name = names(bi_plots), pos_sep = NA)

# get position of last separator in column name strings
for(i in 1:dim(dat)[1]) {
  
  y <- dat[i,]
  sep <- unlist(gregexpr('_', y, fixed = T))
  dat[i,]$pos_sep = max(sep)
  
}

# replace column names
name_n <- substr(dat$name, dat$pos_sep + 1, nchar(dat$name))  
names(bi_plots) <- name_n
rm(dat, y, sep, i, name_n)

head(bi_plots)






