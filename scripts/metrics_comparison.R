#----------------------------------------------------------------------------------
# Name:         metrics_comparison.R
# Description:  script compares forest metrics previously calculated from
#               normalized point clouds and normalized digital surface models (nDSM).
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#----------------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01 - data reading
#-------------------------------------

# read data frames with calculated metrics per sample plot
# plot_metrics_sm = metrics based on nDSM
# plot_metrics_pc = metrics based on normalized point cloud
plot_metrics_sm <- readRDS(paste0(processed_data_dir, 'plot_metrics_sm.RDS'))
plot_metrics_pc <- readRDS(paste0(processed_data_dir, 'plot_metrics_pc.RDS'))


 
# 02 - forest metrics comparison
# 02.1: plots
#-------------------------------------

#-----------------------------
# plots of mean, sd, min, max
#-----------------------------

par_org <- par()
par(mfrow = c(2,2))

# generate the x-axis tick labels
tick_labels <- 1:40

# determine the y-axis limits based on the range of values in all columns
y_min <- min(c(plot_metrics_pc$zmean, plot_metrics_sm$mean,
               plot_metrics_pc$zsd, plot_metrics_sm$sd,
               plot_metrics_pc$zmin, plot_metrics_sm$min,
               plot_metrics_pc$zmax, plot_metrics_sm$max))

y_max <- max(c(plot_metrics_pc$zmean, plot_metrics_sm$mean,
               plot_metrics_pc$zsd, plot_metrics_sm$sd,
               plot_metrics_pc$zmin, plot_metrics_sm$min,
               plot_metrics_pc$zmax, plot_metrics_sm$max))

# first plot: zmean
plot(tick_labels, plot_metrics_pc$zmean,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$mean,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)
legend('topleft', legend = c('point cloud', 'nDSM'),
       col = c('blue', 'red'), pch = 20, bty = 'n', x.intersp = 0.3)

# second plot: zsd
plot(tick_labels, plot_metrics_pc$zsd,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$sd,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# third plot: zmin
plot(tick_labels, plot_metrics_pc$zmin,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$min,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# fourth plot: zmax
plot(tick_labels, plot_metrics_pc$zmax,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$max,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

par(par_org)

#-----------------------------
# plots of percentiles
#-----------------------------

par_org <- par()
par(mfrow = c(3,5))

# generate the x-axis tick labels
tick_labels <- 1:40

# determine the y-axis limits based on the range of values in all columns
y_min <- min(c(plot_metrics_pc$zq1, plot_metrics_sm$`1%`,
               plot_metrics_pc$zq5, plot_metrics_sm$`5%`,
               plot_metrics_pc$zq10, plot_metrics_sm$`10%`,
               plot_metrics_pc$zq20, plot_metrics_sm$`20%`,
               plot_metrics_pc$zq25, plot_metrics_sm$`25%`,
               plot_metrics_pc$zq30, plot_metrics_sm$`30%`,
               plot_metrics_pc$zq40, plot_metrics_sm$`40%`,
               plot_metrics_pc$zq50, plot_metrics_sm$`50%`,
               plot_metrics_pc$zq60, plot_metrics_sm$`60%`,
               plot_metrics_pc$zq70, plot_metrics_sm$`70%`,
               plot_metrics_pc$zq75, plot_metrics_sm$`75%`,
               plot_metrics_pc$zq80, plot_metrics_sm$`80%`,
               plot_metrics_pc$zq90, plot_metrics_sm$`90%`,
               plot_metrics_pc$zq95, plot_metrics_sm$`95%`,
               plot_metrics_pc$zq99, plot_metrics_sm$`99%`))

y_max <- max(c(plot_metrics_pc$zq1, plot_metrics_sm$`1%`,
               plot_metrics_pc$zq5, plot_metrics_sm$`5%`,
               plot_metrics_pc$zq10, plot_metrics_sm$`10%`,
               plot_metrics_pc$zq20, plot_metrics_sm$`20%`,
               plot_metrics_pc$zq25, plot_metrics_sm$`25%`,
               plot_metrics_pc$zq30, plot_metrics_sm$`30%`,
               plot_metrics_pc$zq40, plot_metrics_sm$`40%`,
               plot_metrics_pc$zq50, plot_metrics_sm$`50%`,
               plot_metrics_pc$zq60, plot_metrics_sm$`60%`,
               plot_metrics_pc$zq70, plot_metrics_sm$`70%`,
               plot_metrics_pc$zq75, plot_metrics_sm$`75%`,
               plot_metrics_pc$zq80, plot_metrics_sm$`80%`,
               plot_metrics_pc$zq90, plot_metrics_sm$`90%`,
               plot_metrics_pc$zq95, plot_metrics_sm$`95%`,
               plot_metrics_pc$zq99, plot_metrics_sm$`99%`))

# first plot: zq1
plot(tick_labels, plot_metrics_pc$zq1,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$`1%`,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)
legend('topleft', legend = c('point cloud', 'nDSM'),
       col = c('blue', 'red'), pch = 20, bty = 'n', x.intersp = 0.3)

# second plot: zq5
plot(tick_labels, plot_metrics_pc$zq5,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$`5%`,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# third plot: zq10
plot(tick_labels, plot_metrics_pc$zq10,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$`10%`,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# fourth plot: zq20
plot(tick_labels, plot_metrics_pc$zq20,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$`20%`,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# fifth plot: zq25
plot(tick_labels, plot_metrics_pc$zq25,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$`25%`,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# sixth plot: zq30
plot(tick_labels, plot_metrics_pc$zq30,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$`30%`,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# seventh plot: zq40
plot(tick_labels, plot_metrics_pc$zq40,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$`40%`,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# eighth plot: zq50
plot(tick_labels, plot_metrics_pc$zq50,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$`50%`,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# nineth plot: zq60
plot(tick_labels, plot_metrics_pc$zq60,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$`60%`,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# tenth plot: zq70
plot(tick_labels, plot_metrics_pc$zq70,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$`70%`,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# eleventh plot: zq75
plot(tick_labels, plot_metrics_pc$zq75,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$`75%`,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# twelfth plot: zq80
plot(tick_labels, plot_metrics_pc$zq80,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$`80%`,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# thirteenth plot: zq90
plot(tick_labels, plot_metrics_pc$zq90,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$`90%`,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# fourteenth plot: zq95
plot(tick_labels, plot_metrics_pc$zq95,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$`95%`,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# fifteenth plot: zq99
plot(tick_labels, plot_metrics_pc$zq99,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$`99%`,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

par(par_org)

#------------------------------------
# plots of skewness, kurtosis and cv
#------------------------------------

par_org <- par()
par(mfrow = c(2,3))

# generate the x-axis tick labels
tick_labels <- 1:40

# determine the y-axis limits based on the range of values in all columns
y_min <- min(c(plot_metrics_pc$zskew, plot_metrics_sm$skewness,
               plot_metrics_pc$zkurt, plot_metrics_sm$kurtosis,
               plot_metrics_pc$zcv, plot_metrics_sm$cv,
               plot_metrics_pc$zskew_lmom, plot_metrics_sm$lmom_skew.t_3,
               plot_metrics_pc$zkurt_lmom, plot_metrics_sm$lmom_kurt.t_4,
               plot_metrics_pc$zcv_lmom, plot_metrics_sm$lmom_cv.l_2))

y_max <- max(c(plot_metrics_pc$zskew, plot_metrics_sm$skewness,
               plot_metrics_pc$zkurt, plot_metrics_sm$kurtosis,
               plot_metrics_pc$zcv, plot_metrics_sm$cv,
               plot_metrics_pc$zskew_lmom, plot_metrics_sm$lmom_skew.t_3,
               plot_metrics_pc$zkurt_lmom, plot_metrics_sm$lmom_kurt.t_4,
               plot_metrics_pc$zcv_lmom, plot_metrics_sm$lmom_cv.l_2))

# first plot: skewness
plot(tick_labels, plot_metrics_pc$zskew,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$skewness,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)
legend('topleft', legend = c('point cloud', 'nDSM'),
       col = c('blue', 'red'), pch = 20, bty = 'n', x.intersp = 0.3)

# second plot: kurtosis
plot(tick_labels, plot_metrics_pc$zkurt,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$kurtosis,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# third plot: cv
plot(tick_labels, plot_metrics_pc$zcv,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$cv,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# fourth plot: skewness lmoments
plot(tick_labels, plot_metrics_pc$zskew_lmom,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$lmom_skew.t_3,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# fifth plot: kurtosis lmoments
plot(tick_labels, plot_metrics_pc$zkurt_lmom,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$lmom_kurt.t_4,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

# sixth plot: cv lmoments
plot(tick_labels, plot_metrics_pc$zcv_lmom,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$lmom_cv.l_2,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

par(par_org)

#--------------
# plot of crr
#--------------

par_org <- par()

# generate the x-axis tick labels
tick_labels <- 1:40

# determine the y-axis limits based on the range of values in all columns
y_min <- min(c(plot_metrics_pc$zcrr, plot_metrics_sm$crr))
y_max <- max(c(plot_metrics_pc$zcrr, plot_metrics_sm$crr))

# plot: crr
plot(tick_labels, plot_metrics_pc$zcrr,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$crr,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)
legend('topleft', legend = c('point cloud', 'nDSM'),
       col = c('blue', 'red'), pch = 20, bty = 'n', x.intersp = 0.3)

par(par_org)

#------------------------------------
# plot of pzabove3 and above mean
#------------------------------------

par_org <- par()
par(mfrow = c(1,2))

# generate the x-axis tick labels
tick_labels <- 1:40

# determine the y-axis limits based on the range of values in all columns
y_min <- min(c(plot_metrics_pc$pzabove3, plot_metrics_sm$pabove3,
               plot_metrics_pc$pzabovezmean, plot_metrics_sm$pabovemean))
y_max <- max(c(plot_metrics_pc$pzabove3, plot_metrics_sm$pabove3,
               plot_metrics_pc$pzabovezmean, plot_metrics_sm$pabovemean))

# first plot: pzabove3
plot(tick_labels, plot_metrics_pc$pzabove3,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$pabove3,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)
legend('topleft', legend = c('point cloud', 'nDSM'),
       col = c('blue', 'red'), pch = 20, bty = 'n', x.intersp = 0.3)

# second plot: pzabovezmean
plot(tick_labels, plot_metrics_pc$pzabovezmean,
     col = 'blue', pch = 20, xaxt = 'n',
     ylim = c(y_min, y_max))
points(tick_labels, plot_metrics_sm$pabovemean,
       col = 'red', pch = 20)
axis(side = 1, at = tick_labels, labels = plot_metrics_pc$plot, cex.axis = 1)

par(par_org)


# 02 - forest metrics comparison
# 02.1: significance tests
#-------------------------------------

# convert plot_metrics_pc to data frame
# and remove unneeded columns
plot_metrics_pc <- as.data.frame(plot_metrics_pc)
plot_metrics_pc <- plot_metrics_pc[, -(c(1:5, ncol(plot_metrics_pc)))]
plot_metrics_sm <- plot_metrics_sm[, -(ncol(plot_metrics_sm))]

# perform t-tests for the respective metrics
# to determine possible significant differences
# between point cloud and nDSM metrics
ttest_results <- list()

column_names_pc <- names(plot_metrics_pc)
column_names_sm <- names(plot_metrics_sm)

for (i in 1:ncol(plot_metrics_pc)) {
  
  ttest_result <- stats::t.test(plot_metrics_pc[, i], plot_metrics_sm[, i], 
                                var.equal = T, alternative = 'two.sided')
  
  ttest_results[[paste(column_names_pc[i], column_names_sm[i], sep = '-')]] <- ttest_result
  
}

# print the t-test results
for (col_pair in names(ttest_results)) {
  
  cat('Column Pair:', col_pair, '\n')
  print(ttest_results[[col_pair]])
  cat('\n')
  
}







