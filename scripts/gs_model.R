#--------------------------------------------------------------------------
# Name:         gs_model.R
# Description:  Script models the growing stock (GS) based on previously
#               derived metrics in terrestrial sample plots.
#               Different model types are created and tested.
#               Finally, wall-to-wall predictions of the GS for two entire
#               forestry offices are calculated.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#--------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01 - set file paths
#-------------------------------------

# input path to point clouds (normalized and not normalized)
# point clouds were previously extracted from a larger area
# in script pc_ctg_extraction.R, now represent a forestry office
#dsm_pc_path <- paste0(raw_data_dir, 'DSMs_laz/')
ndsm_pc_path <- paste0(processed_data_dir, 'nDSMs_laz_solling/')


# 02 - data reading
#-------------------------------------

# read normalized point clouds with LAScatalog
ndsm_pc_ctg <- lidR::readLAScatalog(ndsm_pc_path)

# read data frame with plots and calculated metrics
# based on the normalized point clouds
# --> see script forest_metrics_pc_ctg.R
plot_metrics <- readRDS(paste0(processed_data_dir, 'plot_metrics_pc_solling.RDS'))
plot_metrics <- na.omit(plot_metrics)



# 03 - model creation
#-------------------------------------

# ### simple ordinary least squares (OLS) linear regression ###
# ols_model <- stats::lm(vol_ha ~ zmean, data = plot_metrics)
# summary(ols_model)
# 
# # check variance and normal distribution of residuals
# ggplot2::autoplot(ols_model, which = 1:2)
# 
# # scatterplot
# ggplot(plot_metrics, aes(x=zmean, y=vol_ha)) +
#   geom_point() +
#   geom_smooth(method=lm, color='red', se=F) +
#   xlab(expression(paste('mean height [', m, ']', sep = ''))) +
#   ylab(expression(paste('growing stock [', m^3, ha^-1, ']', sep = ''))) +
#   theme_bw()
# 
# # predicted vs. observed GS
# ggplot(plot_metrics, aes(x=vol_ha, y=predict(ols_model))) +
#   geom_point() +
#   geom_smooth(method=lm, color='red', se=F) +
#   xlab(expression(paste('observed growing stock [', m^3, ha^-1, ']', sep = ''))) +
#   ylab(expression(paste('predicted growing stock [', m^3, ha^-1, ']', sep = ''))) +
#   ylim(0,750) +
#   theme_bw()
# 
# ### generalized linear model (GLM) ###
# ### using gaussian distribution and logarithmic link ###
# glm_gaus_log <- stats::glm(vol_ha ~ zmean, 
#                            family = gaussian(link = 'log'), 
#                            data = plot_metrics)
# summary(glm_gaus_log)
# (b <- stats::coef(glm_gaus_log))
# 
# x <- seq(2,40, by = 1)
# 
# ggplot(plot_metrics, aes(x = zmean, y = vol_ha)) +
#   geom_point() +
#   #geom_smooth(method=lm, color='red', se=F) +
#   geom_line(data = data.frame(x = x, y = exp(b[1] + b[2] * x)),
#             aes(x = x, y = y), color = "blue", lwd = 1) +
#   xlab(expression(paste('mean height [', m, ']', sep = ''))) +
#   ylab(expression(paste('growing stock [', m^3, ha^-1, ']', sep = ''))) +
#   theme_bw()



# split data into training and testing
plot_metrics_df <- as.data.frame(plot_metrics)
rownames(plot_metrics_df) <- 1:nrow(plot_metrics_df)

set.seed(11)
trainIndex <- caret::createDataPartition(plot_metrics_df$vol_ha,
                                         p = 0.8, list = F)
train <- plot_metrics_df[trainIndex,]
test <- plot_metrics_df[-trainIndex,]

######################################
# random forest model using 'ranger' #
######################################

# define predictors and response
predictors <- train[,7:32]
response <- train[,'vol_ha']

# initialize leave-location-out cross-validation (LLOCV)
#indices <- CAST::CreateSpacetimeFolds(train,
#                                      spacevar = 'kspnr',
#                                      k = 10)

# control the parameters of the train function
#ctrl <- caret::trainControl(method = 'cv',
#                            index = indices$index,
#                            savePredictions = T,
#                            allowParallel = T,
#                            verboseIter = T)

ctrl <- caret::trainControl(method = 'cv',
                            number = 10,
                            savePredictions = T,
                            allowParallel = T)

# create grid for tuning features
tgrid <- expand.grid(mtry = 2:10,
                     splitrule = 'variance',
                     min.node.size = c(10,20,30,40,50))

# create parallel cluster to increase computing speed
n_cores <- parallel::detectCores() - 2 
cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

# train the model
set.seed(11)
rf_model <- caret::train(
  predictors,
  response,
  method = 'ranger',
  trControl = ctrl,
  tuneGrid = tgrid,
  num.trees = 500,
  importance = 'permutation'
  )

# stop parallel cluster
parallel::stopCluster(cl)

# save or read the model
saveRDS(rf_model, paste0(processed_data_dir, 'rf_model.RDS'))
rf_model <- readRDS(paste0(processed_data_dir, 'rf_model.RDS'))

# summary of the model
summary(rf_model)

# predicted vs. observed GS
ggplot(train, aes(x=vol_ha, y=stats::predict(rf_model))) +
  geom_point() +
  ggtitle('random forest') +
  xlab(expression(paste('observed growing stock [', m^3, ha^-1, ']', sep = ''))) +
  ylab(expression(paste('predicted growing stock [', m^3, ha^-1, ']', sep = ''))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits=c(0,1700), breaks=seq(0,1500, by=300)) +
  scale_y_continuous(limits=c(0,1500), breaks=seq(0,1500, by=300)) +
  geom_abline(slope=1, intercept=0, size=1, color='red')

#################################################
#       generalized additive model (GAM)        #
# using gamma distribution and logarithmic link #
#################################################

# train the model
gam = mgcv::gam(
  vol_ha ~ s(zmin) + s(zmax) +
    s(zq1) + s(zq5) + s(zq95) + s(zq99) +
    s(zskew) + s(zkurt) + s(zcv) + s(zcrr) +
    s(ndvi),
  family = Gamma(link = 'log'),
  data = train
  )

# save or read the model
saveRDS(gam, paste0(processed_data_dir, 'gam.RDS'))
gam <- readRDS(paste0(processed_data_dir, 'gam.RDS'))

# summary of the model
summary(gam)

# predicted vs. observed GS
ggplot(train, aes(x=vol_ha, y=mgcv::predict.gam(gam, type = 'response'))) +
  geom_point() +
  ggtitle('GAM') +
  xlab(expression(paste('observed growing stock [', m^3, ha^-1, ']', sep = ''))) +
  ylab(expression(paste('predicted growing stock [', m^3, ha^-1, ']', sep = ''))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits=c(0,1700), breaks=seq(0,1500, by=300)) +
  scale_y_continuous(limits=c(0,1500), breaks=seq(0,1500, by=300)) +
  geom_abline(slope=1, intercept=0, size=1, color='red')




# 04 - wall-to-wall modeling
#-------------------------------------

# source function for metrics calculation
source('src/calc_metrics.R', local = T)

# 1. calculate the metrics for the entire collection of files
# (normalized point clouds in LAScatalog)
# output resolution of the metrics = 23 m (13 m plot radius = 531 m²)
if (!file.exists(paste0(output_dir, 'metrics_w2w_solling.tif'))) {
  
  lidR::opt_filter(ndsm_pc_ctg) <- '-drop_z_below 2'
  
  metrics_w2w <- lidR::pixel_metrics(
    ndsm_pc_ctg, ~calc_metrics(Z, R, B),
    res = 20,
    pkg = 'terra'
    )
  
  terra::writeRaster(
    metrics_w2w,
    paste0(output_dir, 'metrics_w2w_solling.tif'),
    overwrite = T)
  
} else {
  
  metrics_w2w <- terra::rast(paste0(output_dir, 'metrics_w2w_solling.tif'))
  
}

# 2. calculate wall-to-wall predictions of GS using random forest model
if (!file.exists(paste0(output_dir, 'vol_ha_pred_rf_solling.tif'))) {
  
  if (!exists("metrics_w2w")) {
    metrics_w2w <- terra::rast(paste0(output_dir, 'metrics_w2w_solling.tif'))
  }
  
  vol_ha_pred_rf <- terra::predict(metrics_w2w, rf_model, na.rm = T)
  
  terra::writeRaster(
    vol_ha_pred_rf,
    paste0(output_dir, 'vol_ha_pred_rf_solling.tif'),
    overwrite = T)
  
} else {
  
  vol_ha_pred_rf <- terra::rast(paste0(output_dir, 'vol_ha_pred_rf_solling.tif'))
  
}

# 3. calculate wall-to-wall predictions of GS using GAM
if (!file.exists(paste0(output_dir, 'vol_ha_pred_gam_solling.tif'))) {
  
  if (!exists("metrics_w2w")) {
    metrics_w2w <- terra::rast(paste0(output_dir, 'metrics_w2w_solling.tif'))
  }
  
  vol_ha_pred_gam <- terra::predict(metrics_w2w, gam, type='response', na.rm = T)
  
  terra::writeRaster(
    vol_ha_pred_gam,
    paste0(output_dir, 'vol_ha_pred_gam_solling.tif'),
    overwrite = T)
  
} else {
  
  vol_ha_pred_gam <- terra::rast(paste0(output_dir, 'vol_ha_pred_gam_solling.tif'))
  
}






# clip_values <- function(raster_layer, min_val, max_val) {
#   raster_layer <- terra::ifel(raster_layer < min_val, min_val, raster_layer)
#   raster_layer <- terra::ifel(raster_layer > max_val, max_val, raster_layer)
#   return(raster_layer)
# }
# 
# # Define reasonable thresholds based on the summary of your training data
# thresholds <- train %>%
#   summarize(
#     zmin_min = min(zmin, na.rm = TRUE),
#     zmin_max = max(zmin, na.rm = TRUE),
#     zmax_min = min(zmax, na.rm = TRUE),
#     zmax_max = max(zmax, na.rm = TRUE),
#     zq1_min = min(zq1, na.rm = TRUE),
#     zq1_max = max(zq1, na.rm = TRUE),
#     zq5_min = min(zq5, na.rm = TRUE),
#     zq5_max = max(zq5, na.rm = TRUE),
#     zq95_min = min(zq95, na.rm = TRUE),
#     zq95_max = max(zq95, na.rm = TRUE),
#     zq99_min = min(zq99, na.rm = TRUE),
#     zq99_max = max(zq99, na.rm = TRUE),
#     zskew_min = min(zskew, na.rm = TRUE),
#     zskew_max = max(zskew, na.rm = TRUE),
#     zkurt_min = min(zkurt, na.rm = TRUE),
#     zkurt_max = max(zkurt, na.rm = TRUE),
#     zcv_min = min(zcv, na.rm = TRUE),
#     zcv_max = max(zcv, na.rm = TRUE),
#     zcrr_min = min(zcrr, na.rm = TRUE),
#     zcrr_max = max(zcrr, na.rm = TRUE),
#     ndvi_min = min(ndvi, na.rm = TRUE),
#     ndvi_max = max(ndvi, na.rm = TRUE)
#   )
# 
# 
# # Clip extreme values
# metrics_w2w$zmin <- clip_values(metrics_w2w$zmin, thresholds$zmin_min, thresholds$zmin_max)
# metrics_w2w$zmax <- clip_values(metrics_w2w$zmax, thresholds$zmax_min, thresholds$zmax_max)
# metrics_w2w$zq1 <- clip_values(metrics_w2w$zq1, thresholds$zq1_min, thresholds$zq1_max)
# metrics_w2w$zq5 <- clip_values(metrics_w2w$zq5, thresholds$zq5_min, thresholds$zq5_max)
# metrics_w2w$zq95 <- clip_values(metrics_w2w$zq95, thresholds$zq95_min, thresholds$zq95_max)
# metrics_w2w$zq99 <- clip_values(metrics_w2w$zq99, thresholds$zq99_min, thresholds$zq99_max)
# metrics_w2w$zskew <- clip_values(metrics_w2w$zskew, thresholds$zskew_min, thresholds$zskew_max)
# metrics_w2w$zkurt <- clip_values(metrics_w2w$zkurt, thresholds$zkurt_min, thresholds$zkurt_max)
# metrics_w2w$zcv <- clip_values(metrics_w2w$zcv, thresholds$zcv_min, thresholds$zcv_max)
# metrics_w2w$zcrr <- clip_values(metrics_w2w$zcrr, thresholds$zcrr_min, thresholds$zcrr_max)
# metrics_w2w$ndvi <- clip_values(metrics_w2w$ndvi, thresholds$ndvi_min, thresholds$ndvi_max)
# 
# 
# 
# vol_ha_pred_new <- terra::predict(metrics_w2w, gam_gamma_log, type='response')



# 05 - model comparison
#-------------------------------------

# extract prediction values as vectors
prediction_values_gam <- as.vector(terra::values(vol_ha_pred_gam))
prediction_values_rf <- as.vector(terra::values(vol_ha_pred_rf))

# remove NA values
prediction_values_gam <- prediction_values_gam[!is.na(prediction_values_gam)]
prediction_values_rf <- prediction_values_rf[!is.na(prediction_values_rf)]

# combine into a data frame
prediction_df <- data.frame(
  value = c(prediction_values_gam, prediction_values_rf),
  model = rep(c('GAM', 'RF'), 
              c(length(prediction_values_gam), 
                length(prediction_values_rf)))
)

# plot side-by-side boxplots of the prediction values
ggplot(prediction_df, aes(x = model, y = value)) +
  geom_boxplot() +
  ylab(expression(paste('predicted growing stock [', m^3, ha^-1, ']', sep = ''))) +
  xlab('model type') +
  ggtitle('comparison of prediction values') +
  theme_minimal()

# Create the histograms with transparency to overlay them
ggplot(prediction_df, aes(x = value, fill = model)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 500) +
  ylab('Frequency') +
  xlab(expression(paste('predicted growing stock [', m^3, ha^-1, ']', sep = ''))) +
  ggtitle('comparison of prediction values') +
  theme_minimal() +
  scale_fill_manual(values = c("GAM" = "blue", "RF" = "red"))

# Create faceted histograms to compare the two models
ggplot(prediction_df, aes(x = value, fill = model)) +
  geom_histogram(bins = 500, color = "black") +
  ylab('Frequency') +
  xlab(expression(paste('predicted growing stock [', m^3, ha^-1, ']', sep = ''))) +
  ggtitle('comparison of prediction values') +
  theme_minimal() +
  facet_wrap(~model, scales = "free_y") +
  scale_fill_manual(values = c("GAM" = "blue", "RF" = "red"))

ggplot(prediction_df, aes(x = value, fill = model)) +
  geom_histogram(bins = 500, color = "black") +
  ylab('Frequency') +
  xlab(expression(paste('Predicted Growing Stock [', m^3, ha^-1, ']', sep = ''))) +
  ggtitle('Comparison of Prediction Values') +
  theme_minimal() +
  facet_wrap(~model, scales = "free_y") +
  scale_fill_manual(values = c("GAM" = "blue", "RF" = "red")) +
  coord_cartesian(xlim = c(0, 1000))  # Set x-axis limits to 0-1000









# # Extract predictors from training dataset
# train <- as.data.frame(train)
# train <- subset(train, select = -c(geom))
# 
# train_predictors <- train %>%
#   select(zmin, zmax, zq1, zq5, zq95, zq99, zskew, zkurt, zcv, zcrr, ndvi)
# 
# # Extract predictors from wall-to-wall metrics
# wall_to_wall_df <- as.data.frame(metrics_w2w, na.rm = TRUE) %>%
#   select(zmin, zmax, zq1, zq5, zq95, zq99, zskew, zkurt, zcv, zcrr, ndvi)
# 
# # Add a source column
# train_predictors$source <- 'Training'
# wall_to_wall_df$source <- 'Wall-to-Wall'
# 
# # Combine the data frames
# combined_df <- rbind(train_predictors, wall_to_wall_df)
# 
# # Convert to long format for ggplot
# combined_long_df <- combined_df %>%
#   tidyr::pivot_longer(cols = -source, names_to = "predictor", values_to = "value")
# 
# # Plot side-by-side boxplots using ggplot2
# ggplot(combined_long_df, aes(x = source, y = value, fill = source)) +
#   geom_boxplot() +
#   facet_wrap(~ predictor, scales = "free") +
#   xlab('') +
#   ylab('') +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_blank(),
#         strip.text = element_text(size = 8),
#         legend.position = "none")





# 06 - validation
#-------------------------------------

# predict on the unseen test dataset
pred_values_rf <- stats::predict(rf_model, test)
pred_values_gam <-mgcv::predict.gam(gam, test, type = 'response')

# build data frame with predicted and observed GS
val_df_rf <- data.frame(kspnr = dplyr::pull(test, 'kspnr'),
                        observed = dplyr::pull(test, 'vol_ha'),
                        predicted = pred_values_rf)

val_df_gam <- data.frame(kspnr = dplyr::pull(test, 'kspnr'),
                         observed = dplyr::pull(test, 'vol_ha'),
                         predicted = pred_values_gam)

# calculate RMSE
rmse_rf <- round(sqrt(mean((val_df_rf$predicted - val_df_rf$observed)^2, na.rm = T)), 2)
rmse_gam <- round(sqrt(mean((val_df_gam$predicted - val_df_gam$observed)^2, na.rm = T)), 2)

# calculate relative RMSE
mean_observed_rf <- mean(val_df_rf$observed, na.rm = T)
rrmse_rf <- round((rmse_rf / mean_observed_rf) * 100, 2)
mean_observed_gam <- mean(val_df_gam$observed, na.rm = T)
rrmse_gam <- round((rmse_gam / mean_observed_gam) * 100, 2)

# calculate bias (mean error)
bias_rf <- mean(val_df_rf$predicted - val_df_rf$observed)
bias_gam <- mean(val_df_gam$predicted - val_df_gam$observed)

# calculate R-squared
ssr_rf <- sum((val_df_rf$predicted - mean(val_df_rf$observed))^2)
sst_rf <- sum((val_df_rf$observed - mean(val_df_rf$observed))^2)
rsquared_rf <- 1 - (ssr_rf / sst_rf)
ssr_gam <- sum((val_df_gam$predicted - mean(val_df_gam$observed))^2)
sst_gam <- sum((val_df_gam$observed - mean(val_df_gam$observed))^2)
rsquared_gam <- 1 - (ssr_gam / sst_gam)

# print the results
cat("RMSE Random Forest:", rmse_rf, "\n")
cat("rRMSE Random Forest:", rrmse_rf, "\n")
cat("Bias Random Forest:", bias_rf, "\n")
cat("R-squared Random Forest:", rsquared_rf, "\n")
cat("RMSE GAM:", rmse_gam, "\n")
cat("rRMSE GAM:", rrmse_gam, "\n")
cat("Bias GAM:", bias_gam, "\n")
cat("R-squared GAM:", rsquared_gam, "\n")

# predicted vs. observed GS
ggplot(val_df_rf, aes(x=observed, y=predicted)) +
  geom_point() +
  ggtitle('random forest') +
  xlab(expression(paste('observed growing stock [', m^3, ha^-1, ']', sep = ''))) +
  ylab(expression(paste('predicted growing stock [', m^3, ha^-1, ']', sep = ''))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits=c(0,1350), breaks=seq(0,1500, by=300)) +
  scale_y_continuous(limits=c(0,1350), breaks=seq(0,1500, by=300)) +
  geom_abline(slope=1, intercept=0, size=1, color='red')

ggplot(val_df_gam, aes(x=observed, y=predicted)) +
  geom_point() +
  ggtitle('GAM') +
  xlab(expression(paste('observed growing stock [', m^3, ha^-1, ']', sep = ''))) +
  ylab(expression(paste('predicted growing stock [', m^3, ha^-1, ']', sep = ''))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits=c(0,1350), breaks=seq(0,1500, by=300)) +
  scale_y_continuous(limits=c(0,1350), breaks=seq(0,1500, by=300)) +
  geom_abline(slope=1, intercept=0, size=1, color='red')





orga_path <- paste0(raw_data_dir, 'orga/')
nlf_org <- sf::st_read(paste0(orga_path, 'NLF_Org_2022.shp'))
rev_buchenberg <- nlf_org[nlf_org$REVIERNAME == 'Buchenberg',]
vol_ha_pred_gam_rev <- terra::crop(vol_ha_pred_gam, rev_buchenberg, mask = T)
vol_ha_pred_rf_rev <- terra::crop(vol_ha_pred_rf, rev_buchenberg, mask = T)
terra::writeRaster(
  vol_ha_pred_gam_rev,
  paste0(output_dir, 'vol_ha_pred_gam_rev.tif'),
  overwrite = T)
terra::writeRaster(
  vol_ha_pred_rf_rev,
  paste0(output_dir, 'vol_ha_pred_rf_rev.tif'),
  overwrite = T)

mean_gs_rf_rev <- mean(terra::values(vol_ha_pred_rf_rev), na.rm = T)
mean_gs_gam_rev <- mean(terra::values(vol_ha_pred_gam_rev), na.rm = T)

set.seed(11)
trainIndex <- caret::createDataPartition(plot_metrics$vol_ha,
                                         p = 0.8, list = F)
train <- plot_metrics[trainIndex,]
test <- plot_metrics[-trainIndex,]

bi_plots_rev <- sf::st_intersection(test, rev_buchenberg)
mean_gs_bi_rev <- mean(bi_plots_rev$vol_ha)

rmse_rf_rev <- round(sqrt((mean_gs_rf_rev - mean_gs_bi_rev)^2), 2)
rmse_gam_rev <- round(sqrt((mean_gs_gam_rev - mean_gs_bi_rev)^2), 2)

rrmse_rf_rev <- round((rmse_rf_rev / mean_gs_bi_rev) * 100, 2)
rrmse_gam_rev <- round((rmse_gam_rev / mean_gs_bi_rev) * 100, 2)







fa_solling <- nlf_org[nlf_org$FORSTAMT == 268 | nlf_org$FORSTAMT == 254,]
rev <- fa_solling[fa_solling$REVIER == 10 & fa_solling$FORSTAMT == 254 & fa_solling$SHAPE_AREA >= 100000000,] 

terra::plot(ndsm_pc_ctg)
terra::plot(rev$geometry, add = T)


ndsm_pc_ctg_rev <- lidR::catalog_intersect(ndsm_pc_ctg, terra::ext(rev))

terra::plot(ndsm_pc_ctg_rev)
terra::plot(rev$geometry, add = T)

lidR::opt_filter(ndsm_pc_ctg_rev) <- '-drop_z_below 2 -drop_z_above 50'
metrics_w2w_rev <- lidR::pixel_metrics(
  ndsm_pc_ctg_rev, ~calc_metrics(Z, R, B),
  res = 20,
  pkg = 'terra'
)
terra::writeRaster(
  metrics_w2w_rev,
  paste0(output_dir, 'metrics_w2w_rev.tif'),
  overwrite = T)




