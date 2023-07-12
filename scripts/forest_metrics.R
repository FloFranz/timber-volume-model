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

# input path to DSM and nDSM raster files
dsm_path <- paste0(raw_data_dir, 'DSMs/')
ndsm_path <- paste0(raw_data_dir, 'nDSMs/')

# input path to DSM and nDSM point clouds
dsm_pc_path <- paste0(raw_data_dir, 'DSMs_laz/')
ndsm_pc_path <- paste0(raw_data_dir, 'nDSMs_laz/')

# input path to terrestrial data
bi_path <- paste0(raw_data_dir, 'BI/')



# 02 - data reading
#-------------------------------------

# read nDSM raster and point clouds
ndsm_files <- list.files(ndsm_path)
ndsm_pc_files <- list.files(ndsm_pc_path)

ndsm <- terra::rast(paste0(ndsm_path, ndsm_files))
ndsm_pc <- lidR::readLAS(paste0(ndsm_pc_path, ndsm_pc_files))

# quick plot
terra::plot(ndsm)
lidR::plot(ndsm_pc)

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
bi_plots <- bi_plots[,c('key', 'kspnr', 'fa', 'rw', 'hw', 'stj')]

# filter plots by forestry offices Dassel (254) and Neuhaus (268) and year 2022
fa_dassel <- bi_plots[bi_plots$fa == 254 & bi_plots$stj == 2022,]
fa_neuhaus <- bi_plots[bi_plots$fa == 268 & bi_plots$stj == 2022,]

# merge them
fa_dassel_neuhaus <- merge(fa_dassel, fa_neuhaus, all = T)

# create two column matrix with easting (rw) and northing (hw) values
pts <- matrix(c(fa_dassel_neuhaus$rw, fa_dassel_neuhaus$hw), ncol = 2)

# create simple feature geometry 'multipoint' from the rw and hw
# mp <- sf::st_multipoint(pts)

points_dassel_neuhaus <- terra::vect(pts, type = 'points', crs = 'EPSG:31467')

# create simple feature geometry column from the multipoint object
# assign CRS (DHDN / 3-degree Gauss-Kruger zone 3)
# sfc <- sf::st_sfc(mp, crs = "EPSG:31467")

# assign CRS to the raster (ETRS89 / UTM zone 32N)
# and project it to the CRS of the multipoints
terra::crs(ndsm) <- 'EPSG:25832'
ndsm_projected <- terra::project(ndsm, 'EPSG:31467')

# plot the raster with the points
# terra::plot(ndsm_projected)
# terra::points(sfc[[1]])

terra::plot(ndsm_projected)
terra::points(points_dassel_neuhaus)



# 04 - calculation of metrics
#-------------------------------------

# 04.1
# creation of a data frame with extracted height values
# within the sample plot points
#--------------------------------------------------------

# create buffer of 13 m around the point centroids
# --> radius 13 m
points_dassel_neuhaus_buffered <- terra::buffer(points_dassel_neuhaus, 13)

terra::plot(ndsm_projected)
terra::plot(points_dassel_neuhaus_buffered, add = T)

# extract the values from the raster (height values of the nDSM)
# that are within the buffered points (now circles --> sample plots)
extracted_val <- terra::extract(ndsm_projected, points_dassel_neuhaus_buffered, raw = T)

# remove NA (= buffered points not lying within the nDSM)
extracted_val <- na.omit(extracted_val)

# convert the matrix to a data frame
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
extracted_val_df_list <- lapply(extracted_val_df_list, pad_with_na, max_rows = max_rows)

# bind the data frames by columns
extracted_val_df <- dplyr::bind_cols(extracted_val_df_list)
head(extracted_val_df)

# get the number of ID and value column pairs
num_pairs <- ncol(extracted_val_df) / 2

# vector to store the indices of the ID columns to be removed
id_columns_to_remove <- c()

# iterate over each pair
for (i in 1:num_pairs) {
  
  id_column <- 2*i - 1  # ID column index
  value_column <- 2*i  # value column index
  
  # assign the "plot_" prefix with the corresponding number as the column name
  colnames(extracted_val_df)[value_column] <- paste0('plot_', i)
  
  # store the index of the ID column to be removed
  id_columns_to_remove <- c(id_columns_to_remove, id_column)
  
}

# remove the ID columns
extracted_val_df <- extracted_val_df[, -id_columns_to_remove]

head(extracted_val_df)

# save data frame with the extracted values within the sample plots
if (!file.exists(paste0(processed_data_dir, 'extr_val_plots.RDS'))) {
  
  saveRDS(extracted_val_df, file = paste0(processed_data_dir, 'extr_val_plots.RDS'))
  
} else {
  
  invisible()
  
}


# 04.2
# calculation of metrics based on the height values
# within the sample plot points
#--------------------------------------------------------

# calculate some metrics
# --> potential explanatory variables for a timber volume model
#
# height metrics: mean, standard deviation, minimum, maximum,
# percentile values (1st, 5th, 10th, 20th, 25th, 30th, 40th, 50th,
#                    60th, 70th, 75th, 80th, 90th, 95th, 99th),
# skewness kurtosis, coefficient of variation
# (all three as conventional moments and as L-moments),
# canopy relief ratio (crr) --> https://doi.org/10.1016/j.foreco.2003.09.001
# for the rest, see different studies, e.g. https://doi.org/10.1139/cjfr-2014-0297
metrics <- sapply(extracted_val_df, function(x) c(mean = mean(x, na.rm = T),
                                                  sd   = sd(x, na.rm = T),
                                                  min  = min(x, na.rm = T),
                                                  max  = max(x, na.rm = T),
                                                  quantile(x, 
                                                           probs = c(0.01, 0.05, 0.1, 0.2, 0.25,
                                                                     0.3, 0.4, 0.5, 0.6, 0.7,
                                                                     0.75, 0.8, 0.9, 0.95, 0.99),
                                                           na.rm = T),
                                                  skewness = moments::skewness(x, na.rm = T),
                                                  kurtosis = moments::kurtosis(x, na.rm = T),
                                                  cv = sd(x, na.rm = T) / mean(x, na.rm = T) * 100,
                                                  lmom_skew = lmom::samlmu(x)[3],
                                                  lmom_kurt = lmom::samlmu(x)[4],
                                                  lmom_cv = lmom::samlmu(x, ratios = F)[2] / lmom::samlmu(x, ratios = F)[1],
                                                  crr = ((mean(x, na.rm = T) - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))))

# transpose the result for a more convenient format
metrics_new <- t(metrics)

# plot correlogram of the metrics
corrplot::corrplot(cor(metrics_new), method = 'circle', type= 'full')

#GGally::ggcorr(metrics_new, method = c('everything', 'pearson'))








