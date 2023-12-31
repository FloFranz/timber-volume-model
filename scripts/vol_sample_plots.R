#----------------------------------------------------------------------------------
# Name:         vol_sample_plots.R
# Description:  Script calculates timber volume per ha for terrestrial sample plots.
#               Terrestrial data is first processed and then the single tree volume
#               is calculated. Based on this, the timber volume per sample point
#               is calculated.
# Author:       Florian Franz, Georgia Reeves
# Contact:      florian.franz@nw-fva.de, georgia.reeves@nw-fva.de
#----------------------------------------------------------------------------------


# source setup script
source('src/setup.R', local = TRUE)



# 01 - set file paths and read data
#-------------------------------------

# input path to terrestrial data
bi_path <- paste0(raw_data_dir, 'BI/')

# read BI data
bi_files <- list.files(bi_path)

bi_points <- read.table(paste0(bi_path, 'tblDatPh2_ZE_092023.txt'),
                        header = T, sep = ';')

bi_trees <- read.table(paste0(bi_path, 'tblDatPh2_Vorr_ZE_092023.txt'),
                       header = T, sep = ';')

# select specific forestry offices (e.g. Solling)
bi_points <- bi_points[bi_points$DatOrga_Key == '268-2022-002' | 
                         bi_points$DatOrga_Key == '254-2022-002',]

bi_trees <- bi_trees[bi_trees$DatOrga_Key == '268-2022-002' | 
                       bi_trees$DatOrga_Key == '254-2022-002',]

head(bi_points)
head(bi_trees)



# 02 - data preparation
#-------------------------------------

# source and apply function for data formatting
source('src/format_data.R', local = TRUE)

bi_points <- format_data(bi_points)
bi_trees <- format_data(bi_trees)

head(bi_points)
str(bi_points)
head(bi_trees)
str(bi_trees)

# remove outlier in coordinates (one hw = 500)
#plot(bi_points$rw, bi_points$hw ,ylim=c(501,6000000))
bi_points <- bi_points[!bi_points$hw == 500,]
#plot(bi_points$rw, bi_points$hw)

# delete deadwood and used trees
bi_trees <- bi_trees[!bi_trees$ba %in% seq(100,800,100),]

bi_trees <- bi_trees[bi_trees$'1' < 3 & bi_trees$'2' < 3,]

bi_trees <- bi_trees[bi_trees$art != 1 & bi_trees$art != 2 & bi_trees$bhd > 0,]

# remove unneeded columns
bi_trees <- bi_trees[,c(1:12)]

# assign tree species groups
bi_trees$bagr <- ifelse(bi_trees$ba > 0 & bi_trees$ba < 200, "EI",
                 ifelse(bi_trees$ba > 199 & bi_trees$ba < 300, "BU",
                 ifelse(bi_trees$ba > 299 & bi_trees$ba < 400, "ALH",	
                 ifelse(bi_trees$ba > 399 & bi_trees$ba < 500, "ALN",
                 ifelse(bi_trees$ba > 499 & bi_trees$ba < 600, "FI",
                 ifelse(bi_trees$ba > 599 & bi_trees$ba < 700, "DGL",
                 ifelse(bi_trees$ba > 699 & bi_trees$ba < 800, "KI",	"LAE")))))))

# BHD correction
# if not measured at 1.3 m (deviating measuring height), then correction to 1.3 m
bi_trees$ba1 <- bi_trees$ba

# red oak to oak, fir to spruce, hornbeam to beech
source('src/d_corr_func.R', local = TRUE)

bi_trees$ba <- input_d_korr(bi_trees$bagr)

# average BHD from 'Kreuzkluppung (bhdklup)', convert to cm
bi_trees$bhd <- ifelse(bi_trees$bhdklup > 0,
                       (0.5 * (bi_trees$bhd + bi_trees$bhdklup)) / 10,
                       bi_trees$bhd / 10)

# separate data:
# trees with diameter at deviating measurement height
# and without deviating measurement height
bi_trees_2 <- bi_trees[bi_trees$bhddiff > 0,]
bi_trees <- bi_trees[bi_trees$bhddiff == 0,]

# convert diameter to BHD in case of deviating measuring height
d <- d_korr(du = bi_trees_2$bhd, abwmh = bi_trees_2$bhddiff, ba = bi_trees_2$ba)
bi_trees_2$bhd <- d

# bind the two data frame together again (correctly back to original number)
bi_trees <- rbind(bi_trees, bi_trees_2)
rm(bi_trees_2, d)

# add original tree species again
bi_trees$ba <- bi_trees$ba1
bi_trees$ba1 <- NULL

head(bi_trees)

# number of stems per ha

# concentric sample circles:
#	r = 6 m all trees 
#	r = 13 m all trees with BHD >= 30 cm
# radius must be projected into the plane

# correct sample circle sizes with slope, calculate N_ha
# inclination from degrees in rad
bi_points$hang_rad <- (pi / 180) * bi_points$hang

bi_points_trees <- merge(bi_trees, bi_points[,c("key", "kspnr","hang_rad", "rw", "hw")],
                         by = c("key", "kspnr"))

# r_plane = r_slope * cos(slope_rad)
bi_points_trees$nha <- ifelse(bi_points_trees$bhd < 30, 10000 / (pi * 6**2 * cos(bi_points_trees$hang_rad)),
                              10000 / (pi * 13**2 * cos(bi_points_trees$hang_rad)))

# add heights 

# heights in m
bi_points_trees$hoehe <- bi_points_trees$hoehe / 10

# new ID consisting of key + sample point number
bi_points_trees$id2 <- paste(bi_points_trees$key, bi_points_trees$kspnr, sep = "_")

# data format for heights adding
source('src/ehk_func.R', local = TRUE)

dat <- input_ehk(id = bi_points_trees$id2, bnr = bi_points_trees$id,
                 bs = bi_points_trees$bestschicht, bhd = bi_points_trees$bhd,
                 hoe = bi_points_trees$hoehe, nha = bi_points_trees$nha,
                 bagr = bi_points_trees$bagr)

head(dat)

# assigning appropriate quantile percentages in order
# to properly remove outliers (unrealistic bhd height value pairs)
# in a statistically sound way by building a scam model with height and bhd
dat <- dat[dat$hoe > 0, ]
summary(dat)
m <- scam::scam(hoe ~ s(bhd, bs = 'mpi'),
                data = dat, 
                family = Gamma(link = 'log'))
nd <- data.frame('bhd' = floor(min(dat$bhd)):ceiling(max(dat$bhd)))
nd$hoe <- predict(m, newdata = nd, type = 'response')

p <- ggplot(data = dat, aes(x = bhd, y  = hoe)) + 
  geom_point(color = rgb(.5, .5, .5, alpha = .2)) + 
  geom_line(dat = nd, color = 1, size = 2)

tmp <- NULL
for (x in seq(10, 110, by = 10)) {
  nd2 <- data.frame('bhd' = x)
  nd2$hoe <- predict(m, newdata = nd2, type = 'response')
  v <- 1/m$sig2
  d <- stats::dgamma(1:60, shape = (nd2$hoe[1]^2)/v, scale = v/nd2$hoe[1])
  tmp <- rbind(tmp, 
               data.frame('bhd' = x - (9 * d / max(d)), 
                          'hoe' = 1:60, 
                          'x' = x))
}

p1 <- p + 
  geom_path(dat = tmp, aes(group = x, color = factor(x)), show.legend = F) +
  geom_vline(data = data.frame('bhd' = seq(10, 110, by = 10)), 
             aes(xintercept = bhd, color = factor(bhd)), show.legend = F, 
             linetype = 2)

v <- 1/m$sig2
dat$p <- stats::pgamma(q = dat$hoe, shape = (fitted(m)^2)/v, scale = v/fitted(m))
summary(dat$p)

dat$lab <- ''
ix_label <- sort(c(which(dat$p > .99), which(dat$p < .01)))
dat$lab[ix_label] <- paste0(round(dat$p[ix_label] * 100, 2), '%')

p2 <- ggplot(data = dat, aes(x = bhd,y  = hoe)) + 
  geom_point(color = rgb(.5, .5, .5, alpha = .2)) + 
  geom_line(dat = nd, color = 1, size = 2) + 
  geom_text_repel(aes(label = lab))

dat$lab <- round(dat$p*100, 2)
dat_extremes <- dat[dat$lab >= 99.98 | dat$lab <= 0.02,]
dat_without_extremes <- dat[dat$lab < 99.98 & dat$lab > 0.02,]

cowplot::plot_grid(p1, p2, ncol = 2)

# re-assigning dat to its original value
dat <- input_ehk(id = bi_points_trees$id2, bnr = bi_points_trees$id,
                 bs = bi_points_trees$bestschicht, bhd = bi_points_trees$bhd,
                 hoe = bi_points_trees$hoehe, nha = bi_points_trees$nha,
                 bagr = bi_points_trees$bagr)

dat2 <- dat[dat$hoe == 0,]

dat3 <- subset(dat_without_extremes, select = -c(p, lab))
dat <- rbind(dat2, dat3)
rm(dat_ohne_extremes)
rm(dat2)
rm(dat3)
rm(dat_extremes)

# Einheitshöhenkurve
dat2 <- ehk(dat)
plot(dat2$bhd, dat2$hoe_mod)

# merge modeled heights with original table
bi_points_trees <- merge(bi_points_trees, dat2[,c("id", "bnr", "hoe_mod")], 
                         by.x = c("id2","id"), by.y = c("id", "bnr"))

# remove unneeded data frames
rm(dat2, dat)



# 03 - calculate timber volume
# 03.1: single tree volume
#-------------------------------------

vor <- bi_points_trees

# recode tree species befor applying treegross
vor$ba1 <- ifelse(vor$ba == 561 | vor$ba == 526 , 511,
           ifelse(vor$ba == 21 , 211,
           ifelse(vor$ba == 716, 711, vor$ba)))

# add empty columns
vor$vol <- NA
vor$volC <- NA

# create empty table
cop <- vor[-(1:dim(vor)[1]),]

# calculation of volumes with tg_volume
# correction of negative volumes with GAM
# to avoid them
d = seq(7, 99, by = 2)
h = seq(1, 59,  by = 2)
data = expand.grid(d = d, h = h)

for(i in unique(vor$ba1)){
  
  print(i)
  
  vor2 <- vor[vor$ba1 == i,]
  vor2$vol <- anstaltspaket::tg_volumen(ba=i, bhd=vor2$bhd, h=vor2$hoe_mod, info = F)
  
  data$vol <- anstaltspaket::tg_volumen(ba=i, bhd=data$d, h=data$h, info = F)
  m <-mgcv::gam(vol ~ t2(d, h, k = 10), data = data, family = gaussian(link = 'log'))

  vor2$volC <- predict(m, newdata=data.frame(d=vor2$bhd, h=vor2$hoe_mod), type = 'response')
  
  cop <- rbind(cop, vor2)
  
}

cop$vol <- ifelse(cop$vol <= 0, cop$volC, cop$vol)

vor <- cop
rm(m, data, cop, i, vor2)
vor$ba1 <- NULL
vor$id2 <- NULL
vor$volC <- NULL 

bi_points_trees <- vor
summary(bi_points_trees$vol)
plot(bi_points_trees$bhd, bi_points_trees$vol)



# 03.2: timber volume per sample plot
#--------------------------------------

# group averages of vol and hoe_mod
bi_points_trees <- bi_points_trees %>%
  dplyr::group_by(key, kspnr) %>%
  dplyr::mutate(vol_ha = sum(vol * nha),
         hoe_mod_mean = mean(hoe_mod)) %>%
  dplyr::ungroup()

# extract unique volumes for all sample points
vol_stp <- unique(bi_points_trees[,c("key", "kspnr", "rw", "hw", "vol_ha", "hoe_mod_mean")])

vol_stp <- merge(bi_points[,c("key", "kspnr", "rw", "hw")], 
                 vol_stp, by=c("key", "kspnr", "rw", "hw"), 
                 all.x=T)

vol_stp[is.na(vol_stp)] <- 0

head(vol_stp)
summary(vol_stp)

# save data frame with the extracted volumes per sample points
if (!file.exists(paste0(processed_data_dir, 'vol_stp_092023.RDS'))) {
  
  saveRDS(vol_stp, file = paste0(processed_data_dir, 'vol_stp_092023.RDS'))
  
} else {
  
  print('File vol_stp_092023.RDS already exists.')
  
}

if (!file.exists(paste0(processed_data_dir, 'vol_stp_092023.txt'))) {
  
  write.table(vol_stp, file = paste0(processed_data_dir, 'vol_stp_092023.txt'), 
              sep = ';', row.names = F)
  
} else {
  
  print('File vol_stp_092023.txt already exists.')
  
}
