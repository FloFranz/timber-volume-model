#--------------------------------------------------------------------------
# Name:         dop_cropping.R
# Description:  Script crops a large digital orthophoto (DOP) to the extent
#               of a forestry office of Lower Saxony.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#--------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01 - set file paths
#-------------------------------------

# input path to DOP
dop_path <- paste0(raw_data_dir, 'DOP/')

# input path to administrative data
orga_path <- paste0(raw_data_dir, 'orga/')



# 02 - data reading
#-------------------------------------

# read DOP Solling
dop_solling <- terra::rast(paste0(dop_path, 'dop_solling.tif'))
dop_solling

# quick overview
terra::plotRGB(dop_solling,
               r = 1, g = 2, b = 3)

# read administrative forestry data of Lower Saxony
nlf_org <- sf::st_read(paste0(orga_path, 'NLF_Org_2022.shp'))
nlf_org
str(nlf_org)



# 03 - cropping
#-------------------------------------

# filter administrative forestry data
# by forestry office 'Neuhaus' (268) 
fa_neuhaus <- nlf_org[nlf_org$FORSTAMT == 268,]

# quick overview
mapview::mapview(fa_neuhaus$geometry, map.types = 'OpenStreetMap',
                 alpha.regions = 0, lwd = 2)

# crop whole DOP to extent of previously
# filtered forestry office
terra::terraOptions(memmax = 220)

n_cores <- parallel::detectCores() - 4

cl <- parallel::makeCluster(n_cores)

doParallel::registerDoParallel(cl)

dop_fa_neuhaus <- terra::crop(dop_solling, fa_neuhaus)

parallel::stopCluster(cl)

# overview
terra::plotRGB(dop_fa_neuhaus,
               r = 1, g = 2, b = 3)

terra::plot(fa_neuhaus$geometry, add = T, col = NA, border = 'gray', lwd = 2)

# write cropped DOP to disk as RDS
saveRDS(dop_fa_neuhaus, paste0(processed_data_dir, 'dop_fa_neuhaus.RDS'))
