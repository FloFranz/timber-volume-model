#------------------------------------------------------------------------------
# Name:         05_small_area_estimation.R
# Description:  Script performs direct, model-based, and model-assisted
#               (GREG) small area estimation of growing stock (GS) in the
#               Solling area, using Reviere as the small-area units.
#               It uses terrestrial sample plots, stratum information from
#               BI phase-1 data, and wall-to-wall prediction/raster metrics.
# Author:       Georgia Reeves
# Contact:      georgia.reeves@nw-fva.de
#------------------------------------------------------------------------------


# source setup script
source('src/setup.R', local = TRUE)



# 01 - user settings
#-------------------------------------

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
vol_stp_vs_pred_path <- file.path(processed_data_dir, 'vol_stp_vs_pred_vol.gpkg')

# plot-level metrics from script 02_forest_metrics.R (source for GREG metrics)
plot_metrics_path <- file.path(processed_data_dir, 'plot_metrics_pc_solling_incl_forest_type.RDS')

# trained model used to derive final selected predictors
rf_model_path <- file.path(processed_data_dir, 'global_rf_model.rds')

# inputs for creating revier attribution raster
rf_prediction_raster_path <- file.path(output_dir, 'global_rf_prediction.tif')
metrics_w2w_path <- file.path(processed_data_dir, 'metrics_w2w_solling_incl_forest_type.tif')
reviere_path <- file.path(raw_data_dir, 'orga', 'Rfö.shp')

# combined wall-to-wall raster containing prediction, small-area IDs and metrics
# (must contain: forstamt, revier, pred_gsv + metrics)
w2w_pred_small_area_metrics_raster_path <- file.path(output_dir, 'w2w_pred_small_area_metrics.tif')

# output files
small_area_results_path <- file.path(output_dir, 'small_area_estimation_revier.csv')
strata_joined_plots_path <- file.path(output_dir, 'vol_stp_with_strata_and_small_area_info.csv')

# strata retained for design-based estimation
strata_levels <- c('1_1', '1_2', '1_3', '1_4', '2_1', '2_2', '2_3', '2_4')


# 02 - helper functions
#-------------------------------------

build_strata_id <- function(datph1_bag, datph1_akl) {
  paste(datph1_bag, datph1_akl, sep = '_')
}

calc_strata_weights <- function(strata_vector, levels_keep) {
  counts <- table(factor(strata_vector, levels = levels_keep))
  weights <- counts / sum(counts)
  as.numeric(weights) |> stats::setNames(levels_keep)
}

calc_revier_direct_estimate <- function(plot_tbl, strata_weight, levels_keep) {
  revier_stratum_means <- plot_tbl %>%
    dplyr::filter(!is.na(revier_id), !is.na(Strata_ID), Strata_ID %in% levels_keep) %>%
    dplyr::group_by(revier_id, Strata_ID) %>%
    dplyr::summarise(
      stratum_mean_vol_ha = mean(vol_ha, na.rm = TRUE),
      n_plots = sum(!is.na(vol_ha)),
      .groups = 'drop'
    ) %>%
    tidyr::complete(
      revier_id,
      Strata_ID = levels_keep,
      fill = list(stratum_mean_vol_ha = NA_real_, n_plots = 0L)
    ) %>%
    dplyr::mutate(
      weight = unname(strata_weight[Strata_ID]),
      weighted_mean_component = stratum_mean_vol_ha * weight
    )

  direct_mean <- revier_stratum_means %>%
    dplyr::group_by(revier_id) %>%
    dplyr::summarise(
      direct_stratified_mean_vol_ha = sum(weighted_mean_component, na.rm = TRUE),
      direct_weight_covered = sum(weight[n_plots > 0], na.rm = TRUE),
      .groups = 'drop'
    )

  direct_var <- plot_tbl %>%
    dplyr::filter(!is.na(revier_id), !is.na(Strata_ID), !is.na(vol_ha), Strata_ID %in% levels_keep) %>%
    dplyr::group_by(revier_id, Strata_ID) %>%
    dplyr::summarise(
      n_plots = dplyr::n(),
      Sh2 = ifelse(n_plots > 1, stats::var(vol_ha), 0),
      .groups = 'drop'
    ) %>%
    tidyr::complete(
      revier_id,
      Strata_ID = levels_keep,
      fill = list(n_plots = 0L, Sh2 = 0)
    ) %>%
    dplyr::mutate(
      Wh = unname(strata_weight[Strata_ID]),
      weighted_var_component = (Wh^2) * (Sh2 / pmax(n_plots, 1))
    ) %>%
    dplyr::group_by(revier_id) %>%
    dplyr::summarise(
      direct_stratified_var = sum(weighted_var_component, na.rm = TRUE),
      direct_stratified_se = sqrt(direct_stratified_var),
      .groups = 'drop'
    )

  dplyr::left_join(direct_mean, direct_var, by = 'revier_id')
}

run_revier_greg <- function(sample_tbl, xpop_tbl, n_tbl, metrics) {
  reviers <- unique(sample_tbl$revier_id)

  out <- lapply(reviers, function(d) {
    p <- subset(sample_tbl, revier_id == d)
    y <- p$vol_ha
    xsample <- p[, metrics, drop = FALSE]
    xpop <- subset(xpop_tbl, revier_id == d)[, metrics, drop = FALSE]
    N <- subset(n_tbl, revier_id == d)$n

    fit <- mase::greg(
      y = y,
      xsample = xsample,
      xpop = xpop,
      datatype = 'means',
      N = N,
      model = 'linear',
      var_est = TRUE,
      var_method = 'LinHB'
    )

    data.frame(
      revier_id = d,
      greg_pop_mean = fit$pop_mean,
      greg_pop_mean_var = fit$pop_mean_var,
      greg_pop_total = fit$pop_total,
      greg_pop_total_var = fit$pop_total_var
    )
  })

  greg_results <- do.call(rbind, out)
  greg_results$greg_se_mean <- sqrt(greg_results$greg_pop_mean_var)
  greg_results$greg_se_total <- sqrt(greg_results$greg_pop_total_var)
  greg_results
}


# 03 - data reading
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

strata_weights_tbl$Strata_ID <- build_strata_id(
  strata_weights_tbl$DatPh1_BAG,
  strata_weights_tbl$DatPh1_AKl
)

strata_weights_tbl <- strata_weights_tbl[strata_weights_tbl$Strata_ID %in% strata_levels, ]

# phase-1 design strata weights (Solling-wide)
strata_weight <- calc_strata_weights(strata_weights_tbl$Strata_ID, strata_levels)
stopifnot(abs(sum(strata_weight) - 1) < 1e-8)

# read plot-level terrestrial volume
if (!file.exists(vol_stp_path)) {
  stop('File does not exist: ', vol_stp_path)
}
plot_vol <- readRDS(vol_stp_path)
required_plot_vol_cols <- c('key', 'kspnr', 'rw', 'hw', 'vol_ha')
missing_plot_vol_cols <- setdiff(required_plot_vol_cols, names(plot_vol))
if (length(missing_plot_vol_cols) > 0) {
  stop(
    'vol_stp input is missing required columns: ',
    paste(missing_plot_vol_cols, collapse = ', ')
  )
}

# read plot-level prediction/small-area join file from prediction script
if (!file.exists(vol_stp_vs_pred_path)) {
  stop('File does not exist: ', vol_stp_vs_pred_path)
}
plot_pred_small_area <- sf::st_read(vol_stp_vs_pred_path)
required_pred_cols <- c('key', 'kspnr', 'rw', 'hw', 'FORSTAMT', 'REVIER', 'ABTEILUNG', 'UABT', 'mean_pred_vol_rf')
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
required_plot_metrics_cols <- c('key', 'kspnr')
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
  reviere_solling <- reviere %>%
    dplyr::filter(
      FORSTAMT %in% selected_forstaemter,
      !(FORSTAMT %in% excluded_forstamt & REVIER %in% excluded_revier)
    )

  revier_raster <- terra::rasterize(
    x = terra::vect(reviere_solling),
    y = rf_pred_gsv,
    field = 'REVIER'
  )

  foa_raster <- terra::rasterize(
    x = terra::vect(reviere_solling),
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


# 04 - prepare estimation tables
#-------------------------------------

plot_vol_strata <- plot_vol %>%
  dplyr::left_join(
    strata_weights_tbl %>% dplyr::select(key, kspnr, rw, hw, Strata_ID),
    by = c('key', 'kspnr', 'rw', 'hw')
  )

plot_vol_strata_small_area <- plot_vol_strata %>%
  dplyr::left_join(
    plot_pred_small_area %>%
      sf::st_drop_geometry() %>%
      dplyr::select(key, kspnr, rw, hw, FORSTAMT, REVIER, ABTEILUNG, UABT, mean_pred_vol_rf),
    by = c('key', 'kspnr', 'rw', 'hw')
  ) %>%
  dplyr::left_join(
    plot_metrics %>%
      sf::st_drop_geometry() %>%
      dplyr::select(key, kspnr, dplyr::all_of(greg_metrics)),
    by = c('key', 'kspnr')
  )

plot_vol_strata_small_area$revier_id <- paste(
  plot_vol_strata_small_area$FORSTAMT,
  plot_vol_strata_small_area$REVIER,
  sep = '_'
)


# 05 - direct small area estimation
#-------------------------------------

# Solling-wide direct stratified mean
solling_stratum_means <- plot_vol_strata %>%
  dplyr::filter(!is.na(Strata_ID), Strata_ID %in% strata_levels) %>%
  dplyr::group_by(Strata_ID) %>%
  dplyr::summarise(stratum_mean_vol_ha = mean(vol_ha, na.rm = TRUE), .groups = 'drop') %>%
  dplyr::mutate(weight = unname(strata_weight[Strata_ID]),
                weighted_component = stratum_mean_vol_ha * weight)

direct_solling_mean <- sum(solling_stratum_means$weighted_component, na.rm = TRUE)

# Revier-level direct stratified mean + variance
revier_direct <- calc_revier_direct_estimate(
  plot_tbl = plot_vol_strata_small_area,
  strata_weight = strata_weight,
  levels_keep = strata_levels
)


# 06 - model-based estimation (revier means)
#-------------------------------------

revier_model_based <- w2w_revier_df %>%
  dplyr::group_by(revier_id) %>%
  dplyr::summarise(model_based_mean_vol_ha = mean(pred_gsv, na.rm = TRUE), .groups = 'drop')


# 07 - model-assisted estimation (GREG by revier)
#-------------------------------------

missing_greg_sample_metrics <- setdiff(greg_metrics, names(plot_vol_strata_small_area))
if (length(missing_greg_sample_metrics) > 0) {
  stop(
    'plot_metrics_pc_solling_incl_forest_type input is missing required GREG metrics: ',
    paste(missing_greg_sample_metrics, collapse = ', ')
  )
}

# sample table for GREG
greg_sample <- plot_vol_strata_small_area %>%
  dplyr::select(revier_id, vol_ha, dplyr::all_of(greg_metrics)) %>%
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
  metrics = greg_metrics
)


# 08 - combine and export results
#-------------------------------------

revier_results <- revier_direct %>%
  dplyr::left_join(revier_model_based, by = 'revier_id') %>%
  dplyr::left_join(revier_greg, by = 'revier_id')

cat('Direct stratified Solling-wide mean volume (vol_ha):\n')
print(direct_solling_mean)

cat('Revier-level small area estimation results:\n')
print(revier_results)

write.csv(revier_results, small_area_results_path, row.names = FALSE)
write.csv(plot_vol_strata_small_area, strata_joined_plots_path, row.names = FALSE)
