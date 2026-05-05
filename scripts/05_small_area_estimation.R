#------------------------------------------------------------------------------
# Name:         05_small_area_estimation.R
# Description:  Script performs direct, model-based, and model-assisted
#               (GREG) small area estimation of growing stock (GS) in the
#               selected area, using Reviere as the small-area units.
#               It uses terrestrial sample plots, stratum information from
#               BI phase-1 data, and wall-to-wall prediction/raster metrics.
# Author:       Georgia Reeves
# Contact:      georgia.reeves@nw-fva.de
#------------------------------------------------------------------------------


# source setup script
source('src/setup.R', local = T)
source('src/small_area_estimation_helpers.R', local = T)



# 01 - user settings
#-------------------------------------

# dataset identifier used in input/output file names
dataset_id <- 'solling'

# response variable in plot-level volume input (e.g. 'vol_ha')
response_var <- 'vol_ha'

# column names in vol_stp input
plot_vol_key_col <- 'key'
plot_vol_kspnr_col <- 'kspnr'
plot_vol_rw_col <- 'rw'
plot_vol_hw_col <- 'hw'

# canonical join keys used in table joins (set to available keys only)
# examples:
# c('key', 'kspnr', 'rw', 'hw')
# c('key', 'kspnr') or c('key')
join_keys_strata <- c('key', 'kspnr', 'rw', 'hw')
join_keys_pred <- c('key', 'kspnr', 'rw', 'hw')
join_keys_metrics <- c('key', 'kspnr')

# BI Access database (phase-1 strata source)
bi_access_db_path <- file.path(raw_data_dir, 'BI', 'BI_ZE_092023.accdb')
bi_phase1_table <- 'tblDatPh1_ZE'

# selected forestry offices (Solling: Neuhaus + Dassel)
selected_datorga_keys <- c('268-2022-002', '254-2022-002')
selected_forstaemter <- c(254, 268)
excluded_revier_ids <- c('268_10', '254_9', '254_8', '254_10')

# BI data preprocessed in script 01_vol_sample_plots.R
# contains GS per plot
vol_stp_path <- file.path(processed_data_dir, 'vol_stp.RDS')

# BI data with mean predicted GS per plot
vol_stp_vs_pred_path <- file.path(
  processed_data_dir,
  paste0('vol_stp_vs_pred_vol_', dataset_id, '.gpkg')
)

# plot-level metrics from script 02_forest_metrics.R (source for GREG metrics)
plot_metrics_path <- file.path(
  processed_data_dir,
  paste0('plot_metrics_pc_', dataset_id, '_incl_forest_type.RDS')
)

# trained model used to derive final selected predictors
rf_model_path <- file.path(processed_data_dir, paste0('global_rf_model_', dataset_id, '.rds'))

# inputs for creating revier attribution raster
rf_prediction_raster_path <- file.path(output_dir, paste0('global_rf_prediction_', dataset_id, '.tif'))
metrics_w2w_path <- file.path(
  processed_data_dir,
  paste0('metrics_w2w_', dataset_id, '_incl_forest_type.tif')
)
reviere_path <- file.path(raw_data_dir, 'orga', 'Rfö.shp')

# output files
w2w_pred_small_area_metrics_raster_path <- file.path(
  output_dir,
  paste0('w2w_pred_small_area_metrics_', dataset_id, '.tif')
)
small_area_results_path <- file.path(output_dir, paste0('small_area_estimation_revier_', dataset_id, '.csv'))
strata_joined_plots_path <- file.path(
  output_dir,
  paste0('vol_stp_with_strata_and_small_area_info_', dataset_id, '.csv')
)

# strata retained for design-based estimation
strata_levels <- c('1_1', '1_2', '1_3', '1_4', '2_1', '2_2', '2_3', '2_4')


# 02 - data reading
#-------------------------------------

# read phase-1 table from BI Access DB (for strata)
con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = 'Microsoft Access Driver (*.mdb, *.accdb)',
  DBQ = bi_access_db_path
)
on.exit(DBI::dbDisconnect(con), add = TRUE)

strata_weights_tbl <- DBI::dbReadTable(con, bi_phase1_table)
required_phase1_cols <- c(
  'DatOrga_Key', 'DatPh1_KSPNr', 'DatPh1_RW', 'DatPh1_HW',
  'DatPh1_BAG', 'DatPh1_AKl'
)
missing_phase1_cols <- setdiff(required_phase1_cols, names(strata_weights_tbl))
if (length(missing_phase1_cols) > 0) {
  stop(
    'Phase-1 table is missing required columns: ',
    paste(missing_phase1_cols, collapse = ', ')
  )
}

strata_weights_tbl <- subset(
  strata_weights_tbl,
  DatOrga_Key %in% selected_datorga_keys
)

names(strata_weights_tbl)[names(strata_weights_tbl) %in% c(
  'DatOrga_Key', 'DatPh1_KSPNr', 'DatPh1_RW', 'DatPh1_HW'
)] <- c('key', 'kspnr', 'rw', 'hw')

build_strata_id <- function(datph1_bag, datph1_akl) {
  paste(datph1_bag, datph1_akl, sep = '_')
}

strata_weights_tbl$Strata_ID <- build_strata_id(
  strata_weights_tbl$DatPh1_BAG,
  strata_weights_tbl$DatPh1_AKl
)

strata_weights_tbl <- strata_weights_tbl[strata_weights_tbl$Strata_ID %in% strata_levels, ]

# phase-1 design strata weights (Solling-wide)
calc_strata_weights <- function(strata_vector, levels_keep) {
  counts <- table(factor(strata_vector, levels = levels_keep))
  weights <- counts / sum(counts)
  as.numeric(weights) |> stats::setNames(levels_keep)
}

strata_weight <- calc_strata_weights(strata_weights_tbl$Strata_ID, strata_levels)
stopifnot(abs(sum(strata_weight) - 1) < 1e-8)

# read plot-level terrestrial volume
if (!file.exists(vol_stp_path)) {
  stop('File does not exist: ', vol_stp_path)
}
plot_vol <- readRDS(vol_stp_path)
plot_vol_col_map <- c()
if (!is.na(plot_vol_key_col) && nzchar(plot_vol_key_col)) {
  plot_vol_col_map['key'] <- plot_vol_key_col
}
if (!is.na(plot_vol_kspnr_col) && nzchar(plot_vol_kspnr_col)) {
  plot_vol_col_map['kspnr'] <- plot_vol_kspnr_col
}
if (!is.na(plot_vol_rw_col) && nzchar(plot_vol_rw_col)) {
  plot_vol_col_map['rw'] <- plot_vol_rw_col
}
if (!is.na(plot_vol_hw_col) && nzchar(plot_vol_hw_col)) {
  plot_vol_col_map['hw'] <- plot_vol_hw_col
}
if (length(plot_vol_col_map) == 0) {
  stop('No plot_vol column mapping configured.')
}
plot_vol_col_map <- c(
  plot_vol_col_map,
  setNames(response_var, response_var)
)
plot_vol <- rename_columns_from_map(
  plot_vol,
  col_map = plot_vol_col_map,
  table_label = 'vol_stp input'
)
validate_join_keys(plot_vol, join_keys_strata, 'vol_stp input')
validate_join_keys(plot_vol, join_keys_pred, 'vol_stp input')
validate_join_keys(plot_vol, join_keys_metrics, 'vol_stp input')

# read plot-level prediction/small-area join file from prediction script
if (!file.exists(vol_stp_vs_pred_path)) {
  stop('File does not exist: ', vol_stp_vs_pred_path)
}
plot_pred_small_area <- sf::st_read(vol_stp_vs_pred_path)
required_pred_cols <- c(
  join_keys_pred,
  'FORSTAMT', 'REVIER', 'ABTEILUNG', 'UABT', 'mean_pred_vol_rf'
)
missing_pred_cols <- setdiff(required_pred_cols, names(plot_pred_small_area))
if (length(missing_pred_cols) > 0) {
  stop(
    'vol_stp_vs_pred_vol input is missing required columns: ',
    paste(missing_pred_cols, collapse = ', ')
  )
}

# read plot-level metrics (for GREG metric predictors at sample plots)
if (!file.exists(plot_metrics_path)) {
  stop('File does not exist: ', plot_metrics_path)
}
plot_metrics <- readRDS(plot_metrics_path)
required_plot_metrics_cols <- join_keys_metrics
missing_plot_metrics_cols <- setdiff(required_plot_metrics_cols, names(plot_metrics))
if (length(missing_plot_metrics_cols) > 0) {
  stop(
    'plot_metrics input is missing required columns: ',
    paste(missing_plot_metrics_cols, collapse = ', ')
  )
}

# derive GREG metrics automatically from final RF model predictors
if (!file.exists(rf_model_path)) {
  stop('Required model file does not exist: ', rf_model_path)
}

global_rf_model <- readRDS(rf_model_path)
rf_terms <- all.vars(formula(global_rf_model))
rf_response <- rf_terms[1]
rf_predictors <- setdiff(rf_terms, rf_response)

# GREG uses numeric metrics (exclude categorical species predictor if selected)
greg_metrics <- setdiff(rf_predictors, 'dominant_species')
if (length(greg_metrics) == 0) {
  stop('No numeric RF predictors available for GREG metrics after excluding dominant_species.')
}

# create revier attribution raster if needed
if (!file.exists(w2w_pred_small_area_metrics_raster_path)) {
  cat('Creating w2w_revier_raster...\n')

  if (!file.exists(rf_prediction_raster_path)) {
    stop('Required file does not exist: ', rf_prediction_raster_path)
  }
  if (!file.exists(metrics_w2w_path)) {
    stop('Required file does not exist: ', metrics_w2w_path)
  }
  if (!file.exists(reviere_path)) {
    stop('Required file does not exist: ', reviere_path)
  }

  rf_pred_gsv <- terra::rast(rf_prediction_raster_path)
  metrics_w2w <- terra::rast(metrics_w2w_path)
  reviere <- sf::st_read(reviere_path)

  # filter Reviere to those covering the core Solling region
  excluded_forstamt <- as.numeric(sub('_.*$', '', excluded_revier_ids))
  excluded_revier <- as.numeric(sub('^.*_', '', excluded_revier_ids))
  reviere_filtered <- reviere %>%
    dplyr::filter(
      FORSTAMT %in% selected_forstaemter,
      !(FORSTAMT %in% excluded_forstamt & REVIER %in% excluded_revier)
    )

  revier_raster <- terra::rasterize(
    x = terra::vect(reviere_filtered),
    y = rf_pred_gsv,
    field = 'REVIER'
  )

  foa_raster <- terra::rasterize(
    x = terra::vect(reviere_filtered),
    y = rf_pred_gsv,
    field = 'FORSTAMT'
  )

  names(rf_pred_gsv) <- 'pred_gsv'
  names(revier_raster) <- 'revier'
  names(foa_raster) <- 'forstamt'

  comb_raster <- c(rf_pred_gsv, revier_raster, foa_raster, metrics_w2w)

  terra::writeRaster(
    comb_raster,
    w2w_pred_small_area_metrics_raster_path,
    overwrite = TRUE
  )

  cat('Combined prediction/small-area/metric raster written to:\n')
  cat(w2w_pred_small_area_metrics_raster_path, '\n')
}

w2w_revier_df <- terra::rast(w2w_pred_small_area_metrics_raster_path) %>%
  as.data.frame() %>%
  stats::na.omit()
w2w_revier_df$revier_id <- paste(w2w_revier_df$forstamt, w2w_revier_df$revier, sep = '_')

required_w2w_cols <- c('forstamt', 'revier', 'pred_gsv', greg_metrics)
missing_w2w_cols <- setdiff(required_w2w_cols, names(w2w_revier_df))
if (length(missing_w2w_cols) > 0) {
  stop(
    'w2w_raster is missing required columns: ',
    paste(missing_w2w_cols, collapse = ', ')
  )
}


# 03 - prepare estimation tables
#-------------------------------------

plot_vol_strata <- plot_vol %>%
  dplyr::left_join(
    strata_weights_tbl %>% dplyr::select(dplyr::all_of(c(join_keys_strata, 'Strata_ID'))),
    by = join_keys_strata
  )

plot_vol_strata_small_area <- plot_vol_strata %>%
  dplyr::left_join(
    plot_pred_small_area %>%
      sf::st_drop_geometry() %>%
      dplyr::select(dplyr::all_of(c(
        join_keys_pred,
        'FORSTAMT', 'REVIER', 'ABTEILUNG', 'UABT', 'mean_pred_vol_rf'
      ))),
    by = join_keys_pred
  ) %>%
  dplyr::left_join(
    plot_metrics %>%
      sf::st_drop_geometry() %>%
      dplyr::select(dplyr::all_of(c(join_keys_metrics, greg_metrics))),
    by = join_keys_metrics
  )

plot_vol_strata_small_area$revier_id <- paste(
  plot_vol_strata_small_area$FORSTAMT,
  plot_vol_strata_small_area$REVIER,
  sep = '_'
)


# 04 - direct small area estimation
#-------------------------------------

# dataset-wide direct stratified mean
dataset_stratum_means <- plot_vol_strata %>%
  dplyr::filter(!is.na(Strata_ID), Strata_ID %in% strata_levels) %>%
  dplyr::group_by(Strata_ID) %>%
  dplyr::summarise(stratum_mean_vol_ha = mean(.data[[response_var]], na.rm = T), .groups = 'drop') %>%
  dplyr::mutate(weight = unname(strata_weight[Strata_ID]),
                weighted_component = stratum_mean_vol_ha * weight)

direct_dataset_mean <- sum(dataset_stratum_means$weighted_component, na.rm = TRUE)

# Revier-level direct stratified mean + variance
revier_direct <- calc_revier_direct_estimate(
  plot_tbl = plot_vol_strata_small_area,
  strata_weight = strata_weight,
  levels_keep = strata_levels,
  response_var = response_var
)


# 05 - model-based estimation (revier means)
#-------------------------------------

revier_model_based <- w2w_revier_df %>%
  dplyr::group_by(revier_id) %>%
  dplyr::summarise(model_based_mean_vol_ha = mean(pred_gsv, na.rm = TRUE), .groups = 'drop')


# 06 - model-assisted estimation (GREG by revier)
#-------------------------------------

missing_greg_sample_metrics <- setdiff(greg_metrics, names(plot_vol_strata_small_area))
if (length(missing_greg_sample_metrics) > 0) {
  stop(
    'plot_metrics input is missing required GREG metrics: ',
    paste(missing_greg_sample_metrics, collapse = ', ')
  )
}

# sample table for GREG
greg_sample <- plot_vol_strata_small_area %>%
  dplyr::select(revier_id, dplyr::all_of(response_var), dplyr::all_of(greg_metrics)) %>%
  sf::st_drop_geometry() %>%
  stats::na.omit() %>%
  dplyr::filter(!is.na(revier_id))

# population metric means and population size per revier
greg_xpop <- w2w_revier_df %>%
  dplyr::group_by(revier_id) %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(greg_metrics), ~mean(.x, na.rm = TRUE)), .groups = 'drop')

greg_n <- w2w_revier_df %>%
  dplyr::group_by(revier_id) %>%
  dplyr::summarise(n = dplyr::n(), .groups = 'drop')

# keep only small areas with both sample and population data
valid_small_areas <- intersect(unique(greg_sample$revier_id), unique(greg_xpop$revier_id))
greg_sample <- greg_sample[greg_sample$revier_id %in% valid_small_areas, ]
greg_xpop <- greg_xpop[greg_xpop$revier_id %in% valid_small_areas, ]
greg_n <- greg_n[greg_n$revier_id %in% valid_small_areas, ]

if (length(valid_small_areas) == 0) {
  stop('No overlapping revier small areas between GREG sample and population inputs.')
}

revier_greg <- run_revier_greg(
  sample_tbl = greg_sample,
  xpop_tbl = greg_xpop,
  n_tbl = greg_n,
  metrics = greg_metrics,
  response_var = response_var
)


# 07 - combine and export results
#-------------------------------------

revier_results <- revier_direct %>%
  dplyr::left_join(revier_model_based, by = 'revier_id') %>%
  dplyr::left_join(revier_greg, by = 'revier_id')

cat('Direct stratified dataset-wide mean volume (', response_var, '):\n', sep = '')
print(direct_dataset_mean)

cat('Revier-level small area estimation results:\n')
print(revier_results)

write.csv(revier_results, small_area_results_path, row.names = F)
write.csv(plot_vol_strata_small_area, strata_joined_plots_path, row.names = F)
