#-----------------------------------------------------------------------------
# Name:         tree_species.R
# Description:  Script extracts tree species within terrestrial sample plots.
#               The tree species information is obtained by the map
#               'Dominant Tree Species for Germany (2017/2018)' 
#               developed by Blickensdörfer et al. (in prep.) and published
#               in the 'Thünen Waldatlas'
#               (https://atlas.thuenen.de/layers/Dominant_Species_Class:geonode:Dominant_Species_Class)
#               Eleven tree species are defined and two groups are derived:
#               'coniferous' and 'deciduous'
#               Finally, both (tree species and tree species group) are added
#               to the plot_metrics data frame as two additional columns.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#-----------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = T)



# 01 - set file paths
#-------------------------------------

# input path to tree species data
tree_species_path <- paste0(raw_data_dir, 'tree_species/')

# input path to administrative data
orga_path <- paste0(raw_data_dir, 'orga/')


# read data frame with plots and calculated metrics
# based on the normalized point clouds
# --> see script forest_metrics_pc_ctg.R
plot_metrics <- readRDS(paste0(processed_data_dir, 'plot_metrics_pc_solling.RDS'))



# 02 - data reading
#-------------------------------------

# read tree species map of whole Germany 
# https://doi.org/10.3220/DATA20221214084846
# See also upcoming publication:
# Blickensdörfer, L., Oehmichen, K., Pflugmacher, D., Kleinschmit, B., Hostert, P. (in prep.). Large-area tree species mapping across environmental gradients: Combining Sentinel-2 and Sentinel-1 time series with German National Forest Inventory data.
tree_species <- terra::rast(paste0(tree_species_path, 'Dominant_Species_Class.tif'))
tree_species

# read administrative forestry data of Lower Saxony
nlf_org <- sf::st_read(paste0(orga_path, 'NLF_Org_2022.shp'))
nlf_org
str(nlf_org)



# 03 - data preparation
#-----------------------
# 03.1: tree species preprocessing
#-------------------------------------

# filter administrative forestry data
# by forestry offices 'Neuhaus' (268) and 'Dassel' (254) 
fa_solling <- nlf_org[nlf_org$FORSTAMT == 268 | nlf_org$FORSTAMT == 254,]

# check CRS of tree_species raster and nlf_org vector
terra::crs(tree_species)
sf::st_crs(fa_solling)

# transform nlf_org to tree_species CRS
fa_solling_transformed <- sf::st_transform(fa_solling, terra::crs(tree_species))

# crop and mask the tree_species raster
tree_species_solling <- terra::crop(tree_species, 
                                    fa_solling_transformed,
                                    mask = T)

# reproject the masked tree species raster back to EPSG:25832
tree_species_solling <- terra::project(tree_species_solling, 
                                       'epsg:25832',
                                       method = 'near')
tree_species_solling


# define the reclassification matrix
# format: c(original value, new value)
reclass_matrix <- matrix(c(2, 1,
                           3, 2,
                           4, 3,
                           5, 4,
                           6, 5,
                           8, 6,
                           9, 7,
                           10, 8,
                           14, 9,
                           16, 10,
                           17, 11),
                         byrow = T,
                         ncol = 2)

# reclassify the tree species classes
tree_species_solling <- terra::classify(tree_species_solling,
                                        rcl = reclass_matrix,
                                        right=NA)
tree_species_solling

# check unique values after reclassification
unique(values(tree_species_solling))

# plotting
custom_colors <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', 
                   '#46f0f0', '#f032e6', '#800000', '#fabebe', '#008080')

terra::plot(tree_species_solling,
            col = custom_colors)
terra::plot(fa_solling$geometry, add = T)



# 03.2: tree species in plots
#--------------------------------------

# read BI data preprocessed in script vol_sample_plots.R
# contains timber volume per sample points
bi_plots <- sf::st_read(paste0(processed_data_dir, 'vol_stp_092023.gpkg'))
bi_plots
str(bi_plots)

# reproject BI plots
# DHDN / 3-degree Gauss-Kruger zone 3 --> ETRS89 / UTM zone 32N
bi_plots <- sf::st_transform(bi_plots, sf::st_crs(25832))

# add BI plots to tree species map
terra::plot(bi_plots$geom, cex = 0.8, add = T)

# create buffer of 13 m around the point centroids
# --> radius 13 m
bi_plots_buf <- sf::st_buffer(bi_plots, dist = 13)

# add ID column to bi_plots_buf
bi_plots_buf <- bi_plots_buf %>%
  dplyr::mutate(ID = row_number())

# extract the values from the tree species raster
# that are within the buffered points (now circles --> sample plots)
extr_tree_species <- exactextractr::exact_extract(tree_species_solling,
                                                  bi_plots_buf)

# combine all data frames in the resulting list,
# add an ID column corresponding to the plot
extr_tree_species_df <- dplyr::bind_rows(
  lapply(seq_along(extr_tree_species), function(i) {
    
    df <- extr_tree_species[[i]]
    df <- df %>%
      dplyr::select(value, coverage_fraction)
    df$ID <- i
    return(df)
    
  })) %>%
  dplyr::select(ID, value, coverage_fraction)

# use previously assigned ID column from bi_plots_buf
# to join with the plot number (kspnr)
extr_tree_species_df <- extr_tree_species_df %>%
  dplyr::left_join(bi_plots_buf %>% dplyr::select(kspnr, ID), by = 'ID') %>%
  dplyr::select(kspnr, value, coverage_fraction)

# add unique row ID within each "kspnr" group
extr_tree_species_df <- extr_tree_species_df %>%
  dplyr::group_by(kspnr) %>%
  dplyr::mutate(row_id = row_number())



# 04 - define dominant tree species per plot
#--------------------------------------------

# function to find the mode (most frequent tree species)
# if two tree species occur equally often,
# the individual coverage fraction is used for decision
find_mode_with_coverage_fraction <- function(values, coverage_fractions) {
  
  # combine the values and coverage fractions into a data frame
  data <- data.frame(value = values, coverage_fraction = coverage_fractions)
  
  # count the occurrence of each value and
  # capture the maximum coverage fraction for each value
  value_counts <- data %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(count = n(), 
                     max_coverage_fraction = max(coverage_fraction)) %>%
    dplyr::arrange(dplyr::desc(count), dplyr::desc(max_coverage_fraction))
  
  # get the value with the highest count
  # in case of a tie, the value with the highest coverage fraction
  mode_value <- value_counts$value[1]
  
  return(mode_value)
  
}

# apply function to each plot
tree_species_plots <- extr_tree_species_df %>%
  dplyr::group_by(kspnr) %>%
  dplyr::summarise(tree_species = find_mode_with_coverage_fraction(value, coverage_fraction))

# add the most frequent tree species per plot
# to the plot metrics data frame
plot_metrics <- cbind(plot_metrics, tree_species_plots$tree_species)
colnames(plot_metrics)[ncol(plot_metrics) - 1] <- 'tree_species'

# categorize tree species into 'coniferous' and 'deciduous'
# 3,6,7,8,9 = 'coniferous' (1) - 1,2,4,5,10,11 = 'deciduous' (2)
plot_metrics <- dplyr::mutate(plot_metrics, tree_species_group 
                              = ifelse(is.na(tree_species), NA_integer_,
                                       ifelse(tree_species %in% c(3,6,7,8,9), 1,2)))

# rearrange columns to move tree_species_group
# before the geometry column
cols_order <- c((1:(ncol(plot_metrics) - 2)), 
                ncol(plot_metrics), 
                (ncol(plot_metrics) - 1))
plot_metrics <- plot_metrics[, cols_order]

# save data frame
saveRDS(plot_metrics, paste0(processed_data_dir, 'plot_metrics_pc_solling.RDS'))





















