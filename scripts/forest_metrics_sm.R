#----------------------------------------------------------------------------------
# Name:         forest_metrics_sm.R
# Description:  Script calculates several metrics in terrestrial
#               sample plot points based on digital surface models (DSM)
#               or normalized digital surface models (nDSM) at 1 m resolution
#               and a digital orthophoto (DOP) at 20 cm resolution.
#               DSM and nDSM were previously derived from image-based point clouds.
#               This script is designed to test metrics for one single DSM/nDSM tile.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#----------------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01 - set file paths
#-------------------------------------

# input path to DSM and nDSM raster files
dsm_path <- paste0(raw_data_dir, 'DSMs/')
ndsm_path <- paste0(raw_data_dir, 'nDSMs/')

# input path to DOP raster file
dop_path <- paste0(raw_data_dir, 'DOP/')



# 02 - data reading
#-------------------------------------

# read nDSM raster file
ndsm_files <- list.files(ndsm_path)
ndsm <- terra::rast(paste0(ndsm_path, ndsm_files[593]))
ndsm

# quick plot
terra::plot(ndsm)

# read DOP
dop_solling <- terra::rast(paste0(dop_path, 'dop_solling.tif'))
dop_solling <- terra::rast(r'{Y:\FFranz\von_Berrit\mosaic_gesamt_2023_0717_gdal\dop_solling.tif}')
dop_solling

# read BI data preprocessed in script vol_sample_plots.R
# contains timber volume per sample points
bi_plots <- sf::st_read(paste0(processed_data_dir, 'vol_stp_092023.gpkg'))

# quick overview
bi_plots
str(bi_plots)



# 03 - data preparation
#-------------------------------------

# filter plots by year (2022) and forestry office 
# (Neuhaus = 268, Dassel = 254)
bi_plots <- bi_plots[grep('254-2022-', bi_plots$key),]

# assign CRS to raster nDSM (ETRS89 / UTM zone 32N)
terra::crs(ndsm) <- 'EPSG:25832'

# reproject BI plots to the CRS of the raster nDSM
# DHDN / 3-degree Gauss-Kruger zone 3 --> ETRS89 / UTM zone 32N
bi_plots_projected <- sf::st_transform(bi_plots, sf::st_crs(25832))

# get extent of the nDSM and use it
# to crop the BI plots to the nDSM
ndsm_ext <- terra::ext(ndsm)
bi_plots_cropped <- sf::st_crop(bi_plots_projected, ndsm_ext)
head(bi_plots_cropped)

# create buffer of 13 m around the point centroids
# --> radius 13 m
bi_plots_cropped_buf <- sf::st_buffer(bi_plots_cropped, dist = 13)

# quick plot
terra::plot(ndsm)
plot(bi_plots_cropped_buf$geom, pch = 16, add = T)

# extract the values from the rasters 
# (height values of the nDSM and spectral values of the DOP)
# that are within the buffered points (now circles --> sample plots)
extracted_val_ndsm <- terra::extract(ndsm, bi_plots_cropped_buf)
extracted_val_dop <- terra::extract(dop_solling, bi_plots_cropped_buf)

# split both resulting data frames into several data frames
# corresponding to the number of sample plots
extracted_val_ndsm_df_list <- split(extracted_val_ndsm, extracted_val_ndsm$ID)
extracted_val_dop_df_list <- split(extracted_val_dop, extracted_val_dop$ID)

# the resulting data frames do not all have the same length
# --> empty rows must be filled with NA
# function to pad data frames with NA rows
pad_with_na <- function(df, max_rows) {
  
  if (nrow(df) < max_rows) {
    
    extra_rows <- max_rows - nrow(df)
    
    extra_df <- data.frame(matrix(NA, nrow = extra_rows, ncol = ncol(df)))
    
    colnames(extra_df) <- colnames(df)
    
    df <- bind_rows(df, extra_df)
    
  }
  
  df
  
}

# apply padding to each data frame in the data frame lists
max_rows_ndsm <- max(sapply(extracted_val_ndsm_df_list, nrow))
max_rows_dop <- max(sapply(extracted_val_dop_df_list, nrow))

extracted_val_ndsm_df_list <- lapply(extracted_val_ndsm_df_list, 
                                     pad_with_na, 
                                     max_rows = max_rows_ndsm)

extracted_val_dop_df_list <- lapply(extracted_val_dop_df_list, 
                                    pad_with_na,
                                    max_rows = max_rows_dop)

# bind the data frames by columns
extracted_val_ndsm <- dplyr::bind_cols(extracted_val_ndsm_df_list)
head(extracted_val_ndsm)

extracted_val_dop <- dplyr::bind_cols(extracted_val_dop_df_list)
head(extracted_val_dop)

# remove data frame lists
rm(extracted_val_ndsm_df_list, extracted_val_dop_df_list)

# reduce data frames so for each plot 
# only the extracted values are shown
# this step is done separately for the nDSM and the DOP

# nDSM
#-----

# get the number of ID and value column pairs
num_pairs <- ncol(extracted_val_ndsm) / 2

# vector to store the indices of the ID columns to be removed
id_columns_to_remove <- c()

# get the unique plot names from the 'kspnr' column in bi_plots_cropped_buf
plot_names <- unique(bi_plots_cropped_buf$kspnr)

# iterate over each pair
for (i in 1:num_pairs) {
  
  id_column <- 2 * i - 1  # ID column index
  value_column <- 2 * i  # value column index
  
  # get the corresponding plot names from the bi_plots_cropped_buf data frame
  plot_name <- plot_names[i]
  
  # assign the plot names as the column names
  colnames(extracted_val_ndsm)[value_column] <- plot_name
  
  # store the index of the ID column to be removed
  id_columns_to_remove <- c(id_columns_to_remove, id_column)
  
}

# remove the ID columns
extracted_val_ndsm <- extracted_val_ndsm[, -id_columns_to_remove]
head(extracted_val_ndsm)

# DOP
#----

# number of plot names
num_plot_names <- num_pairs

# number of value columns corresponding to each plot name
# --> R-G-B-I
num_val_col_per_plot <- 4

# calculate the total number of value columns
total_val_col <- num_plot_names * num_val_col_per_plot

# get the unique plot names from the 'kspnr' column in bi_plots_cropped_buf
# (repeated for each set of value columns)
plot_names <- rep(unique(bi_plots_cropped_buf$kspnr), 
                  each = num_val_col_per_plot)

# create vector of indices for the value columns to keep
val_col_indices <- c(seq(2,5), seq(7,10), seq(12,15), seq(17,20), 
                     seq(22,25), seq(27,30), seq(32,35), seq(37,40),
                     seq(42,45), seq(47,50), seq(52,55))

# subset the data frame to keep only the value columns
extracted_val_dop <- extracted_val_dop[, val_col_indices]

# assign the plot names as column names
colnames(extracted_val_dop) <- plot_names

# create band names:
# -R = red, -G = green, -B = blue, -I = near-infrared
bands <- c('-R', '-G', '-B', '-I')

# create new column names with band names
# and assign them to extracted_val_dop
new_col_names <- paste0(colnames(extracted_val_dop), bands)
colnames(extracted_val_dop) <- new_col_names
head(extracted_val_dop)



# 04 - calculation of metrics
#--------------------------------------------------------

# --> potential explanatory variables for a timber volume model
#
# nDSM:
# height metrics: mean, standard deviation, minimum, maximum,
# percentile values (1st, 5th, 10th, 20th, 25th, 30th, 40th, 50th,
#                    60th, 70th, 75th, 80th, 90th, 95th, 99th);
# variability metrics: skewness, kurtosis, coefficient of variation
# (all three as conventional moments and as L-moments),
# canopy relief ratio (crr) --> https://doi.org/10.1016/j.foreco.2003.09.001;
# canopy cover metrics: percentage of pixels above 3m and above mean height
#
# DOP:
# spectral metrics: NDVI
#
# --> see different studies, e.g. https://doi.org/10.1139/cjfr-2014-0297
#
# metrics based on the nDSM
plot_metrics <- sapply(extracted_val_ndsm, function(z)
  c(mean = mean(z, na.rm = T),
    sd   = sd(z, na.rm = T),
    min  = min(z, na.rm = T),
    max  = max(z, na.rm = T),
    quantile(z, 
             probs = c(0.01, 0.05, 0.1, 0.2, 0.25,
                       0.3, 0.4, 0.5, 0.6, 0.7,
                       0.75, 0.8, 0.9, 0.95, 0.99),
             na.rm = T),
    skewness = moments::skewness(z, na.rm = T),
    kurtosis = moments::kurtosis(z, na.rm = T),
    cv = sd(z, na.rm = T) / mean(z, na.rm = T) * 100,
    lmom_skew = lmom::samlmu(z)[3],
    lmom_kurt = lmom::samlmu(z)[4],
    lmom_cv = lmom::samlmu(z, ratios = F)[2] / lmom::samlmu(z, ratios = F)[1],
    crr = ((mean(z, na.rm = T) - min(z, na.rm = T)) / (max(z, na.rm = T) - min(z, na.rm = T))),
    pabove3 = (sum(z > 3, na.rm = T) / length(z)) * 100,
    pabovemean = (sum(z > mean(z, na.rm = T), na.rm = T) / length(z)) * 100))

# metrics based on the DOP

# define NDVI function
calculate_ndvi <- function(red, nir) {
  
  (nir - red) / (nir + red)
  
}

# calculate mean NDVI for each plot
ndvi <- vector('numeric', length = ncol(extracted_val_dop) / 4)

for (i in seq(1, ncol(extracted_val_dop), by = 4)) {
  
  # get red and nir values for each plot
  red <- extracted_val_dop[, i]
  nir <- extracted_val_dop[, i + 3]
  
  # apply NDVI function to each plot
  plot_ndvi <- calculate_ndvi(red, nir)
  ndvi[(i + 3) / 4] <- mean(plot_ndvi, na.rm = T)
}

# create data frame with NDVI values and plot names
ndvi_plots <- data.frame(plot = unique(bi_plots_cropped_buf$kspnr),
                         NDVI = ndvi)

# add NDVI values for each plot to plot_metrics data frame
plot_metrics <- rbind(plot_metrics, ndvi)

#----------------------------------------------------------

# transpose the result for a more convenient format
plot_metrics_transposed <- t(plot_metrics)

# add volumes per sample plot
plot_metrics_transposed <- cbind(plot_metrics_transposed, bi_plots_cropped_buf$vol_ha)
plot_metrics_transposed <- as.data.frame(plot_metrics_transposed)
names(plot_metrics_transposed)[30] <- 'vol_ha'
head(plot_metrics_transposed)













