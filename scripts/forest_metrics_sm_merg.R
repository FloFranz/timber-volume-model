#--------------------------------------------------------------------------------------
# Name:         forest_metrics_sm_merg.R
# Description:  Script calculates several metrics in terrestrial
#               sample plot points based on digital surface models (DSM)
#               or normalized digital surface models (nDSM), respectively.
#               DSM and nDSM were previously derived from image-based point clouds.
#               Several raster files are merged to scale up the calculation of metrics.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#--------------------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01 - set file paths
#-------------------------------------

# input path to DSM and nDSM raster files
dsm_path <- paste0(raw_data_dir, 'DSMs/')
ndsm_path <- paste0(raw_data_dir, 'nDSMs/')



# 02 - data reading
#-------------------------------------

# read nDSM raster files
ndsm_files <- list.files(ndsm_path)
ndsms <- lapply(paste0(ndsm_path, ndsm_files), terra::rast)

# read BI data preprocessed in script vol_sample_plots.R
# contains timber volume per sample points
bi_plots <- sf::st_read(paste0(processed_data_dir, 'vol_stp.gpkg'))

# quick overview
bi_plots
str(bi_plots)



# 03 - data preparation
#-------------------------------------

# convert column vol_ha in bi_plots to numeric
bi_plots$vol_ha <- as.numeric(bi_plots$vol_ha)
str(bi_plots)

# filter plots by year (2022)
bi_plots <- bi_plots[grep('-2022-', bi_plots$key),]

# assign CRS to raster nDSMs (ETRS89 / UTM zone 32N)
ndsms <- lapply(ndsms, function(i) {
  
  terra::crs(i) <- 'EPSG:25832'
  return(i)
  
})

# reproject BI plots to the CRS of the raster nDSMs
# DHDN / 3-degree Gauss-Kruger zone 3 --> ETRS89 / UTM zone 32N
bi_plots_projected <- sf::st_transform(bi_plots, sf::st_crs(25832))

# merge nDSMs
ndsms_sprc <- terra::sprc(ndsms)
ndsms_merged <- terra::merge(ndsms_sprc)

# quick plot
terra::plot(ndsms_merged)

# get extent of the merged nDSMs and use it
# to crop the BI plots to the merged nDSMs
ndsms_merged_ext <- terra::ext(ndsms_merged)
bi_plots_cropped <- sf::st_crop(bi_plots_projected, ndsms_merged_ext)

# create buffer of 13 m around the point centroids
# --> radius 13 m
bi_plots_cropped_buf <- sf::st_buffer(bi_plots_cropped, dist = 13)

# visualize locations of BI plots
terra::plot(ndsms_merged)
plot(bi_plots_cropped_buf$geom, pch = 16, add = T)

# extract the values from the raster (height values of the merged nDSMs)
# that are within the buffered points (now circles --> sample plots)
extracted_val <- terra::extract(ndsms_merged, bi_plots_cropped_buf, raw = T)

# convert the resulting matrix to a data frame
extracted_val_df <- as.data.frame(extracted_val)
head(extracted_val_df)

# split the data frame into several data frames
# corresponding to the number of sample plots within the nDSM
extracted_val_df_list <- split(extracted_val_df, extracted_val_df$ID)

# the resulting data frames do not all have the same length
# --> empty rows must be filled with NA
# function to pad data frames with NA rows
max_rows <- max(sapply(extracted_val_df_list, nrow))

pad_with_na <- function(df, max_rows) {
  
  if (nrow(df) < max_rows) {
    
    extra_rows <- max_rows - nrow(df)
    
    extra_df <- data.frame(matrix(NA, nrow = extra_rows, ncol = ncol(df)))
    
    colnames(extra_df) <- colnames(df)
    
    df <- bind_rows(df, extra_df)
    
  }
  
  df
  
}

# apply padding to each data frame in the data frame list
extracted_val_df_list <- lapply(extracted_val_df_list, pad_with_na, 
                                max_rows = max_rows)

# bind the data frames by columns
extracted_val_df <- dplyr::bind_cols(extracted_val_df_list)
head(extracted_val_df)

# get the number of ID and value column pairs
num_pairs <- ncol(extracted_val_df) / 2

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
  colnames(extracted_val_df)[value_column] <- plot_name
  
  # store the index of the ID column to be removed
  id_columns_to_remove <- c(id_columns_to_remove, id_column)
  
}

# remove the ID columns
extracted_val_df <- extracted_val_df[, -id_columns_to_remove]

head(extracted_val_df)

# save data frame with the extracted values within the sample plots
if (!file.exists(paste0(processed_data_dir, 'extr_val_plots_nDSM.RDS'))) {
  
  saveRDS(extracted_val_df, file = paste0(processed_data_dir, 'extr_val_plots_nDSM.RDS'))
  
} else {
  
  print('File extr_val_plots_nDSM.RDS already exists.')
  
}



# 04 - calculation of metrics
#--------------------------------------------------------

# --> potential explanatory variables for a timber volume model
#
# height metrics: mean, standard deviation, minimum, maximum,
# percentile values (1st, 5th, 10th, 20th, 25th, 30th, 40th, 50th,
#                    60th, 70th, 75th, 80th, 90th, 95th, 99th);
# variability metrics: skewness, kurtosis, coefficient of variation
# (all three as conventional moments and as L-moments),
# canopy relief ratio (crr) --> https://doi.org/10.1016/j.foreco.2003.09.001;
# canopy cover metrics: percentage of pixels above 3m and above mean height
# --> see different studies, e.g. https://doi.org/10.1139/cjfr-2014-0297
plot_metrics <- sapply(extracted_val_df, function(z)
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

# transpose the result for a more convenient format
plot_metrics_transposed <- t(plot_metrics)

# add volumes per sample plot
plot_metrics_transposed <- cbind(plot_metrics_transposed, bi_plots_cropped_buf$vol_ha)
plot_metrics_transposed <- as.data.frame(plot_metrics_transposed)
names(plot_metrics_transposed)[29] <- 'vol_ha'
head(plot_metrics_transposed)

# save data frame with the plots and calculated metrics
if (!file.exists(paste0(processed_data_dir, 'plot_metrics_sm.RDS'))) {
  
  saveRDS(plot_metrics_transposed, file = paste0(processed_data_dir, 'plot_metrics_sm.RDS'))
  
} else {
  
  print('File plot_metrics_sm.RDS already exists.')
  
}

# plot correlogram of the metrics
corrplot::corrplot(cor(plot_metrics_transposed), method = 'circle', type= 'full')

### simple model test
m <- stats::lm(vol_ha ~ mean, data = plot_metrics_transposed)
summary(m)

plot(plot_metrics_transposed$mean, plot_metrics_transposed$vol_ha)
abline(m)

plot(plot_metrics_transposed$vol_ha, predict(m))
abline(0,1)
###








