#--------------------------------------------------------------------------
# Name:         gs_model.R
# Description:  Script models the growing stock (GS) based on previously
#               derived metrics in terrestrial sample plots.
#               Different model types are created and tested.
#               Finally, wall-to-wall predictions of the GS for an entire
#               forestry office is calculated.
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
ndsm_pc_path <- paste0(processed_data_dir, 'nDSMs_laz_neuhaus/')


# 02 - data reading
#-------------------------------------

# read normalized point clouds with LAScatalog
ndsm_pc_ctg <- lidR::readLAScatalog(ndsm_pc_path)

# read data frame with plots and calculated metrics
# based on the normalized point clouds
# --> see script forest_metrics_pc_ctg.R
plot_metrics <- readRDS(paste0(processed_data_dir, 'plot_metrics_pc.RDS'))



# 03 - model creation
#-------------------------------------

### simple ordinary least squares (OLS) linear regression ###
ols_model <- stats::lm(vol_ha ~ zmean, data = plot_metrics)
summary(ols_model)

# check variance and normal distribution of residuals
ggplot2::autoplot(ols_model, which = 1:2)

# scatterplot
ggplot(plot_metrics, aes(x=zmean, y=vol_ha)) +
  geom_point() +
  geom_smooth(method=lm, color='red', se=F) +
  xlab(expression(paste('mean height [', m, ']', sep = ''))) +
  ylab(expression(paste('growing stock [', m^3, ha^-1, ']', sep = ''))) +
  theme_bw()

# predicted vs. observed GS
ggplot(plot_metrics, aes(x=vol_ha, y=predict(ols_model))) +
  geom_point() +
  geom_smooth(method=lm, color='red', se=F) +
  xlab(expression(paste('observed growing stock [', m^3, ha^-1, ']', sep = ''))) +
  ylab(expression(paste('predicted growing stock [', m^3, ha^-1, ']', sep = ''))) +
  ylim(0,750) +
  theme_bw()

### generalized linear model (GLM) ###
### using gaussian distribution and logarithmic link ###
glm_gaus_log <- stats::glm(vol_ha ~ zmean, 
                           family = gaussian(link = 'log'), 
                           data = plot_metrics)
summary(glm_gaus_log)
(b <- stats::coef(glm_gaus_log))

x <- seq(2,40, by = 1)

ggplot(plot_metrics, aes(x = zmean, y = vol_ha)) +
  geom_point() +
  #geom_smooth(method=lm, color='red', se=F) +
  geom_line(data = data.frame(x = x, y = exp(b[1] + b[2] * x)),
            aes(x = x, y = y), color = "blue", lwd = 1) +
  xlab(expression(paste('mean height [', m, ']', sep = ''))) +
  ylab(expression(paste('growing stock [', m^3, ha^-1, ']', sep = ''))) +
  theme_bw()

### random forest model using 'ranger' ###

# split data into training and testing
plot_metrics_df <- as.data.frame(plot_metrics)
rownames(plot_metrics_df) <- 1:nrow(plot_metrics_df)

trainIndex <- caret::createDataPartition(plot_metrics_df$vol_ha,
                                         p = 0.8, list = F)
train <- plot_metrics_df[trainIndex,]
test <- plot_metrics_df[-trainIndex,]

predictors <- train[,7:35]
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
model <- caret::train(predictors,
                      response,
                      method = 'ranger',
                      trControl = ctrl,
                      tuneGrid = tgrid,
                      num.trees = 500,
                      importance = 'permutation')

# stop parallel cluster
parallel::stopCluster(cl)

# save or read the model
saveRDS(model, paste0(processed_data_dir, 'rf_model.RDS'))
model <- readRDS(paste0(processed_data_dir, 'rf_model.RDS'))



# 04 - wall-to-wall modeling
#-------------------------------------

# source function for metrics calculation
source('src/calc_metrics.R', local = T)

# 1. calculate the metrics for the entire collection of files
# (normalized point clouds in LAScatalog)
# output resolution of the metrics = 23 m (13 m plot radius = 531 m²)
# 2. calculate wall-to-wall predictions of GS
if (!file.exists(paste0(output_dir, 'metrics_w2w_neuhaus.tif')) |
    (!file.exists(paste0(output_dir, 'vol_ha_pred_neuhaus.tif')))) {
  
  metrics_w2w <- lidR::pixel_metrics(ndsm_pc_ctg, ~calc_metrics(Z, R, B),
                                     res = 23, pkg = 'terra')
  
  vol_ha_pred <- terra::predict(metrics_w2w, ols_model)
  
  terra::writeRaster(metrics_w2w, paste0(output_dir, 'metrics_w2w_neuhaus.tif'),
                     overwrite = T)
  
  terra::writeRaster(vol_ha_pred, paste0(output_dir, 'vol_ha_pred_neuhaus.tif'),
                     overwrite = T)
  
} else {
  
  metrics_w2w <- terra::rast(paste0(output_dir, 'metrics_w2w_neuhaus.tif'))
  vol_ha_pred <- terra::rast(paste0(output_dir, 'vol_ha_pred_neuhaus.tif'))
  
}

# prediction with the rf model
vol_ha_pred_rf <- terra::predict(metrics_w2w, model, na.rm = T)



# 05 - validation
#-------------------------------------

# validation of the rf model
pred_values <- stats::predict(model, test)

val_df <- data.frame(kspnr = dplyr::pull(test, 'kspnr'),
                     observed = dplyr::pull(test, 'vol_ha'),
                     predicted = pred_values)

# RMSE
rmse <- round(sqrt(mean((val_df$predicted - val_df$observed)^2, na.rm = T)), 2)

# R-squared
ssr <- sum((val_df$predicted - mean(val_df$observed))^2)
sst <- sum((val_df$observed - mean(val_df$observed))^2)
rsquared <- 1 - (ssr / sst)

# print the results
cat("RMSE:", rmse, "\n")
cat("R-squared:", rsquared, "\n")














