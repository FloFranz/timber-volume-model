#' Helper functions for small area estimation workflow.
#'
#' This file contains reusable helper functions used in
#' `scripts/05_small_area_estimation.R` to:
#' - standardize and validate join keys across input tables,
#' - compute direct stratified revier-level estimates, and
#' - run model-assisted GREG estimation by revier.

#' Rename table columns using user-defined mapping.
#'
#' @param tbl data.frame or sf object. Input table whose columns are renamed.
#' @param col_map named character vector. Names are target names, values are
#'   current column names in `tbl`.
#' @param table_label character. Label used in error messages.
#'
#' @return input table with renamed columns.
rename_columns_from_map <- function(tbl, col_map, table_label = 'input table') {
  actual_cols <- unname(col_map)
  missing_actual <- setdiff(actual_cols, names(tbl))
  if (length(missing_actual) > 0) {
    stop(
      table_label, ' is missing configured columns: ',
      paste(missing_actual, collapse = ', ')
    )
  }

  for (target_name in names(col_map)) {
    source_name <- col_map[[target_name]]
    if (target_name != source_name) {
      names(tbl)[names(tbl) == source_name] <- target_name
    }
  }

  tbl
}

#' Validate that configured join keys exist in a table.
#'
#' @param tbl data.frame or sf object. Table to validate.
#' @param join_keys character vector. Join key names expected in `tbl`.
#' @param table_label character. Label used in error messages.
#'
#' @return No returned object; raises an error if validation fails.
validate_join_keys <- function(tbl, join_keys, table_label) {
  if (length(join_keys) == 0) {
    stop('No join keys configured for ', table_label, '.')
  }
  missing_keys <- setdiff(join_keys, names(tbl))
  if (length(missing_keys) > 0) {
    stop(
      table_label, ' is missing configured join keys: ',
      paste(missing_keys, collapse = ', ')
    )
  }
}

#' Compute direct stratified small-area estimates by revier.
#'
#' Calculates direct stratified mean and variance estimates per revier based on
#' sample plots and fixed strata weights.
#'
#' @param plot_tbl data.frame. Plot-level table containing `revier_id`,
#'   `Strata_ID`, and response variable.
#' @param strata_weight named numeric vector. Strata weights.
#' @param levels_keep character vector. Strata levels to include.
#' @param response_var character. Response column name in `plot_tbl`.
#'
#' @return data.frame with direct stratified revier mean, covered weight,
#'   variance, and standard error.
calc_revier_direct_estimate <- function(plot_tbl, strata_weight, levels_keep, response_var) {
  revier_stratum_means <- plot_tbl %>%
    dplyr::filter(!is.na(revier_id), !is.na(Strata_ID), Strata_ID %in% levels_keep) %>%
    dplyr::group_by(revier_id, Strata_ID) %>%
    dplyr::summarise(
      stratum_mean_vol_ha = mean(.data[[response_var]], na.rm = T),
      n_plots = sum(!is.na(.data[[response_var]])),
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
    dplyr::filter(!is.na(revier_id), !is.na(Strata_ID), !is.na(.data[[response_var]]), Strata_ID %in% levels_keep) %>%
    dplyr::group_by(revier_id, Strata_ID) %>%
    dplyr::summarise(
      n_plots = dplyr::n(),
      Sh2 = ifelse(n_plots > 1, stats::var(.data[[response_var]]), 0),
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

#' Run GREG model-assisted estimation by revier.
#'
#' Fits a GREG estimator (`mase::greg`) independently for each revier and
#' returns mean/total estimates with variances and standard errors.
#'
#' @param sample_tbl data.frame. Sample table including `revier_id`,
#'   response, and GREG metrics.
#' @param xpop_tbl data.frame. Population table with revier-level metric means.
#' @param n_tbl data.frame. Revier-level population sizes with columns
#'   `revier_id` and `n`.
#' @param metrics character vector. Predictor metric names used in GREG.
#' @param response_var character. Response column name in `sample_tbl`.
#'
#' @return data.frame with GREG estimates and uncertainties by revier.
run_revier_greg <- function(sample_tbl, xpop_tbl, n_tbl, metrics, response_var) {
  reviers <- unique(sample_tbl$revier_id)

  out <- lapply(reviers, function(d) {
    p <- subset(sample_tbl, revier_id == d)
    y <- p[[response_var]]
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
