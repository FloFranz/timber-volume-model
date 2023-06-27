#--------------------------------------------------------------------------
# Name:         forest_metrics.R
# Description:  script calculates several metrics in terrestrial
#               sample plot points based on digital surface models (DSM)
#               or normalized digital surface models (nDSM), respectively.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#--------------------------------------------------------------------------



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

bi_plots <- read.table(paste0(bi_path, bi_tables[2]),
                       header = T, sep = ';')

head(bi_plots)



# 03 - data preperation
#-------------------------------------

# source and apply function for data formatting
source('src/format_data.R', local = TRUE)

bi_plots <- format_data(bi_plots)

head(bi_plots)
str(bi_plots)

# select needed columns
bi_plots <- bi_plots[,c("key", "kspnr", "fa", "rw", "hw", "stj")]

# filter plots by forestry offices Dassel (254) and Neuhaus (268) and year 2022
fa_dassel <- bi_plots[bi_plots$fa == 254 & bi_plots$stj == 2022,]
fa_neuhaus <- bi_plots[bi_plots$fa == 268 & bi_plots$stj == 2022,]

# merge them
fa_dassel_neuhaus <- merge(fa_dassel, fa_neuhaus, all = T)

# create two column matrix with easting (rw) and northing (hw) values
pts <- matrix(c(fa_dassel_neuhaus$rw, fa_dassel_neuhaus$hw), ncol = 2)

# create simple feature geometry 'multipoint' from the rw and hw
mp <- sf::st_multipoint(pts)

# create simple feature geometry column from the multipoint object
# assign CRS (DHDN / 3-degree Gauss-Kruger zone 3)
sfc <- sf::st_sfc(mp, crs = "EPSG:31467")

# assign CRS to the raster (ETRS89 / UTM zone 32N)
# and project it to the CRS of the multipoints
terra::crs(ndsm) <- "EPSG:25832"
ndsm_projected <- terra::project(ndsm, "EPSG:31467")

# plot the raster with the points
terra::plot(ndsm_projected)
terra::points(sfc[[1]])



