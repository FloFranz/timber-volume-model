#--------------------------------------------------------------------------
# Name:         02_forest_metrics.R
# Description:  Script calculates plot-level and pixel-level forest metrics
#               from normalized point clouds and augments them
#               with CLMS forest type information, including dominant species
#               per sample plot.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#--------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = T)



# 01 - user settings
#-------------------------------------

# input path to normalized point clouds
ndsm_pc_path <- file.path(processed_data_dir, 'nDSMs_laz_solling')

# input path to administrative data
orga_path <- file.path(raw_data_dir, 'orga')

# input files
bi_plots_path <- file.path(processed_data_dir, 'vol_stp.gpkg')
nlf_org_path <- file.path(orga_path, 'NLF_Org_2022.shp')
forest_type_1_path <- file.path(
  raw_data_dir, 'tree_species', 'CLMS_HRLVLCC_FTY_S2021_R10m_E42N31_03035_V01_R00.tif'
)
forest_type_2_path <- file.path(
  raw_data_dir, 'tree_species', 'CLMS_HRLVLCC_FTY_S2021_R10m_E43N31_03035_V01_R00.tif'
)

# forestry office filter (set to numeric(0) to keep all)
selected_forstaemter <- c(268, 254)

# output files
plot_metrics_path <- file.path(processed_data_dir, 'plot_metrics_pc_solling.RDS')
metrics_w2w_path <- file.path(processed_data_dir, 'metrics_w2w_solling.tif')
metrics_w2w_forest_type_path <- file.path(processed_data_dir, 'metrics_w2w_solling_incl_forest_type.tif')
plot_metrics_forest_type_path <- file.path(processed_data_dir, 'plot_metrics_pc_solling_incl_forest_type.RDS')


# 02 - input checks and data reading
#-------------------------------------

if (!dir.exists(ndsm_pc_path)) {
  stop('Point cloud directory does not exist: ', ndsm_pc_path)
}
if (!file.exists(bi_plots_path)) {
  stop('BI plots file does not exist: ', bi_plots_path)
}
if (!file.exists(nlf_org_path)) {
  stop('Administrative forestry file does not exist: ', nlf_org_path)
}
if (!file.exists(forest_type_1_path) || !file.exists(forest_type_2_path)) {
  stop('One or both forest type tiles do not exist.')
}

# read normalized point clouds with LAScatalog
ndsm_pc_ctg <- lidR::readLAScatalog(ndsm_pc_path)
ndsm_pc_ctg

# quick visual check
lidR::plot(ndsm_pc_ctg)

# read BI data preprocessed in script 01_vol_sample_plots.R
# contains timber volume per sample points
bi_plots <- sf::st_read(bi_plots_path)

# quick data check
bi_plots
str(bi_plots)

# read administrative forestry data of Lower Saxony
nlf_org <- sf::st_read(nlf_org_path)
nlf_org
str(nlf_org)

# read CLMS forest type data
# two tiles covering the Solling area
forest_type_1 <- terra::rast(
  forest_type_1_path
)

forest_type_2 <- terra::rast(
  forest_type_2_path
)

forest_type_1
forest_type_2



# 03 - data preparation
#-------------------------------------

# optional filtering of plots and forestry office polygons
if (length(selected_forstaemter) > 0) {
  # plot key format expected like "268-2022-002"
  key_pattern <- paste0('^(', paste(selected_forstaemter, collapse = '|'), ')-')
  bi_plots <- bi_plots[grep(key_pattern, bi_plots$key), ]
  fa_solling <- nlf_org[nlf_org$FORSTAMT %in% selected_forstaemter, ]
} else {
  fa_solling <- nlf_org
}

# assign CRS to point clouds (ETRS89 / UTM zone 32N)
lidR::crs(ndsm_pc_ctg) <- 'EPSG:25832'

# reproject BI plots to the CRS of the point clouds
# DHDN / 3-degree Gauss-Kruger zone 3 --> ETRS89 / UTM zone 32N
bi_plots_projected <- sf::st_transform(bi_plots, sf::st_crs(25832))

# visualize locations of BI plots
lidR::plot(ndsm_pc_ctg, mapview = T, 
           map.type = 'OpenStreetMap',
           alpha.regions = 0) +
  
  mapview::mapview(bi_plots_projected, col.regions = 'red', cex = 2) +
  mapview::mapview(fa_solling, alpha.regions = 0, lwd = 2)



# 04 - calculation of metrics
#--------------------------------------------------------

# source function for metrics calculation
source('src/calc_metrics.R', local = T)

# --- plot level ---
# calculate the predefined metrics for each plot (radius = 13 m) 
# within the normalized point cloud
# 2 m height threshold according to literature
# save data frame with the plots and calculated metrics
# if the data frame with the metrics already exists, read it
if (!file.exists(plot_metrics_path)) {
  
  lidR::opt_filter(ndsm_pc_ctg) <- '-drop_z_below 2'
  
  plot_metrics <- lidR::plot_metrics(
    ndsm_pc_ctg, ~calc_metrics(Z, R, B),
    bi_plots_projected, radius = 13
    )
  
  saveRDS(plot_metrics, file = plot_metrics_path)
  
} else {
  
  plot_metrics <- readRDS(plot_metrics_path)
  
}

# --- pixel level ---
# calculate the metrics for the entire collection of files
# (normalized point clouds in LAScatalog)
# output resolution of the metrics = 20 m 
if (!file.exists(metrics_w2w_path)) {
  
  lidR::opt_filter(ndsm_pc_ctg) <- '-drop_z_below 2'
  
  metrics_w2w <- lidR::pixel_metrics(
    ndsm_pc_ctg, ~calc_metrics(Z, R, B),
    res = 20,
    pkg = 'terra'
  )
  
  terra::writeRaster(
    metrics_w2w,
    metrics_w2w_path,
    overwrite = T)
  
} else {
  
  metrics_w2w <- terra::rast(metrics_w2w_path)
  
}



# 05 - addition of forest type
#--------------------------------------------------------

# --- pixel level ---

if (!file.exists(metrics_w2w_forest_type_path)) {
  
  # merge CLMS forest type tiles
  forest_type_merg <- terra::mosaic(forest_type_1, forest_type_2)
  
  # assign CRS (EPSG:25832) to w2w-metrics raster
  terra::crs(metrics_w2w) <- 'EPSG:25832'
  
  # reproject merged forest type raster to EPSG:25832
  forest_type_merg <- terra::project(
    forest_type_merg, metrics_w2w, method = 'near'
  )
  
  # add forest type information to w2w-metrics raster
  metrics_w2w$forest_type <- forest_type_merg
  metrics_w2w$forest_type
  terra::plot(metrics_w2w$forest_type)
  
  # write to disk
  terra::writeRaster(
    metrics_w2w,
    metrics_w2w_forest_type_path,
    overwrite = T
  )
  
} else {
  
  metrics_w2w <- terra::rast(metrics_w2w_forest_type_path)
  
}

# --- plot level ---

# create buffer of 13 m around the point centroids
# --> radius 13 m
if (!file.exists(plot_metrics_forest_type_path)) {
  
  plot_metrics_buf <- sf::st_buffer(plot_metrics, dist = 13)
  
  # extract forest type information from w2w-metrics raster 
  # that are within the buffered points (now circles --> sample plots)
  plot_metrics_buf <- plot_metrics_buf %>%
    dplyr::mutate(ID = dplyr::row_number())

  extr_forest_type <- exactextractr::exact_extract(
    metrics_w2w$forest_type,
    plot_metrics_buf
  )

  # combine all extracted values in one data frame
  extr_forest_type_df <- dplyr::bind_rows(
    lapply(seq_along(extr_forest_type), function(i) {
    
      df <- extr_forest_type[[i]]
      df <- df %>%
        dplyr::select(value, coverage_fraction)
      df$ID <- i
      return(df)
    
    })
  ) %>%
    dplyr::select(ID, value, coverage_fraction)

  # function to find dominant class per plot
  # if classes occur equally often, use the highest coverage fraction
  find_mode_with_coverage_fraction <- function(values, coverage_fractions) {
  
    data <- data.frame(value = values, coverage_fraction = coverage_fractions)
  
    value_counts <- data %>%
      dplyr::group_by(value) %>%
      dplyr::summarise(count = dplyr::n(),
                       max_coverage_fraction = max(coverage_fraction, na.rm = T),
                       .groups = 'drop') %>%
      dplyr::arrange(dplyr::desc(count), dplyr::desc(max_coverage_fraction))
  
    value_counts$value[1]
  
  }

  # assign dominant forest type per buffered plot
  forest_type_plots <- extr_forest_type_df %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(
      dominant_species = find_mode_with_coverage_fraction(value, coverage_fraction),
      .groups = 'drop'
      )

  # add forest type to plot metrics
  plot_metrics <- plot_metrics_buf %>%
    dplyr::left_join(forest_type_plots, by = 'ID') %>%
    dplyr::select(-ID)

  # save updated data frame
  saveRDS(plot_metrics, plot_metrics_forest_type_path)
  
} else {
  
  plot_metrics <- readRDS(plot_metrics_forest_type_path)
  
}















