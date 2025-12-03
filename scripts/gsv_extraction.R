#------------------------------------------------------------------------------
# Name:         gsv_extraction.R
# Description:  Script extracts predicted growing stock volume (GSV) per pixel
#               in forest inventory plots.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#------------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01 - data reading
#------------------------------------------------------------------------------

# read administrative forestry data of Lower Saxony
wefl <- sf::st_read(file.path(raw_data_dir, 'orga', 'WEFL_2025.shp'))
wefl

# read forest inventory plots
bi_plots <- sf::st_read(file.path(processed_data_dir, 'vol_stp_092023.gpkg'))



# 02 - join forest inventory plots with forest organization units
#------------------------------------------------------------------------------

# reproject BI plots to the CRS of the point clouds
# DHDN / 3-degree Gauss-Kruger zone 3 --> ETRS89 / UTM zone 32N
bi_plots_projected <- sf::st_transform(bi_plots, sf::st_crs(25832))

# select needed variables from wefl
wefl_selected <- wefl %>%
  dplyr::select(FORSTAMT, REVIER, ABTEILUNG, UABT)

# perform spatial join
bi_plots_wefl <- sf::st_join(bi_plots_projected, wefl_selected)
bi_plots_wefl

# save to disk
sf::st_write(
  bi_plots_wefl,
  file.path(processed_data_dir, 'vol_stp_joined_with_wefl.gpkg')
  )

















