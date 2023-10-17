#--------------------------------------------------------------------------------------
# Name:         microclimate_data.R
# Description:  Script processes microclimate data (mean annual temperature
#               at 15 cm above ground) obtained by ForestClim (see publication
#               Haesen et al. 2023, https://doi.org/10.1111/gcb.16678).
#               The microclimate data is masked to the desired forestry office.
#               Subsequently, values within the terrestrial sample plots are extracted.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#--------------------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01 - set file paths
#-------------------------------------

# input path to microclimate data
micro_clim_path <- paste0(raw_data_dir, 'microclimate/') 

# input path to administrative data
orga_path <- paste0(raw_data_dir, 'orga/')



# 02 - data reading
#-------------------------------------

# read microclimate data 
# (mean annual temperature at 15 cm above ground)
micro_clim <- terra::rast(paste0(micro_clim_path, 'ForestClim_01.tif'))
micro_clim

# read BI data preprocessed in script vol_sample_plots.R
# contains timber volume per sample points
bi_plots <- sf::st_read(paste0(processed_data_dir, 'vol_stp_092023.gpkg'))
bi_plots
str(bi_plots)

# read administrative forestry data of Lower Saxony
nlf_org <- sf::st_read(paste0(orga_path, 'NLF_Org_2022.shp'))
nlf_org
str(nlf_org)



# 03 - data preparation
#-----------------------
# 03.1: microclimate preprocessing
#-------------------------------------

# filter plots by year (2022) and forestry office (Neuhaus = 268)
bi_plots <- bi_plots[grep('268-2022-', bi_plots$key),]

# reproject BI plots
# DHDN / 3-degree Gauss-Kruger zone 3 --> ETRS89 / UTM zone 32N
bi_plots_projected <- sf::st_transform(bi_plots, sf::st_crs(25832))

# filter administrative forestry data
# by forestry office 'Neuhaus' (268) 
fa_neuhaus <- nlf_org[nlf_org$FORSTAMT == 268,]

# microclimate processing steps:
# 1. transform boundaries of forestry office Neuhaus to CRS of microclimate data
# (ETRS89-extended / LAEA Europe)
# 2. mask the microclimate data to the forestry office
# 3. reproject the result in ETRS89 / UTM zone 32N

if (!file.exists(paste0(processed_data_dir, 'micro_clim_fa_neuhaus.tif'))) {
  
  fa_neuhaus_projected <- sf::st_transform(fa_neuhaus, sf::st_crs(micro_clim))

  micro_clim_fa_neuhaus_projected <- terra::mask(micro_clim, fa_neuhaus_projected)

  micro_clim_fa_neuhaus <- terra::project(micro_clim_fa_neuhaus_projected,
                                          'epsg:25832', method = 'bilinear')
  
  terra::writeRaster(micro_clim_fa_neuhaus, paste0(processed_data_dir, 
                                                   'micro_clim_fa_neuhaus.tif'))
  
} else {
  
  micro_clim_fa_neuhaus <- terra::rast(paste0(processed_data_dir, 
                                              'micro_clim_fa_neuhaus.tif'))
}

# plot microclimate data in forestry office Neuhaus
# with the terrestrial sample plots
terra::plot(micro_clim_fa_neuhaus,
            ext = terra::ext(fa_neuhaus),
            col = colorRamps::matlab.like2(50))
terra::plot(fa_neuhaus$geometry, add = T)
terra::plot(bi_plots_projected, col = 'black', add = T)



# 03.2: microclimate data extraction
#--------------------------------------

# create buffer of 13 m around the point centroids
# --> radius 13 m
bi_plots_buf <- sf::st_buffer(bi_plots_projected, dist = 13)

# add ID column to bi_plots_buf
bi_plots_buf <- bi_plots_buf %>%
  dplyr::mutate(ID = row_number())

# extract the values from the microclimate raster
# that are within the buffered points (now circles --> sample plots)
extr_val_microclim <- exactextractr::exact_extract(micro_clim_fa_neuhaus,
                                                   bi_plots_buf)

# combine all data frames in the resulting list,
# add an ID column corresponding to the plot
extr_val_microclim_df <- dplyr::bind_rows(
  lapply(seq_along(extr_val_microclim), function(i) {
    
df <- extr_val_microclim[[i]]
df <- df %>%
  dplyr::select(value)
df$ID <- i
return(df)

})) %>%
  dplyr::select(ID, value)

# use previously assigned ID column from bi_plots_buf
# to join with the plot number (kspnr)
extr_val_microclim_df <- extr_val_microclim_df %>%
  dplyr::left_join(bi_plots_buf %>% dplyr::select(kspnr, ID), by = 'ID') %>%
  dplyr::select(kspnr, value)

# remove NAs
extr_val_microclim_df <- na.omit(extr_val_microclim_df)

# add unique row ID within each "kspnr" group
extr_val_microclim_df <- extr_val_microclim_df %>%
  dplyr::group_by(kspnr) %>%
  dplyr::mutate(row_id = row_number())

# spread the data frame, filling with NA where necessary
extr_val_microclim <- extr_val_microclim_df %>%
  tidyr::pivot_wider(id_cols = row_id, names_from = kspnr, values_from = value)

extr_val_microclim$row_id <- NULL

