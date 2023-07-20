#----------------------------------------------------------------------------------
# Name:         vol_sample_plots.R
# Description:  script calculates timber volume per ha for terrestrial sample plots.
#               Terrestrial data is first processed and then the single tree volume
#               is calculated. Based on this, the timber volume per sample point
#               is calculated.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#----------------------------------------------------------------------------------


# source setup script
source('src/setup.R', local = TRUE)



# 01 - set file paths and read data
#-------------------------------------

# input path to terrestrial data
bi_path <- paste0(raw_data_dir, 'BI/')

# read BI data
bi_files <- list.files(bi_path)

bi_points <- read.table(paste0(bi_path, bi_files[9]),
                        header = T, sep = ';')

bi_trees <- read.table(paste0(bi_path, bi_files[1]),
                       header = T, sep = ';')

head(bi_points)
head(bi_trees)



# 02 - data preperation
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
# plot(bi_points$rw, bi_points$hw ,ylim=c(501,6000000))
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
# trees with diameter at deviating measurement height and without deviating measurement height
bi_trees_2 <- bi_trees[bi_trees$bhddiff > 0,]
bi_trees <- bi_trees[bi_trees$bhddiff == 0,]

# convert diameter to BHD in case of deviating measuring height
d <- d_korr(du = bi_trees_2$bhd, abwmh = bi_trees_2$bhddiff, ba = bi_trees_2$ba)
bi_trees_2$bhd <- d

# bind the two data frame together again
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
bi_points_trees <- merge(bi_trees, bi_points[,c("kspnr","hang_rad", "rw", "hw")], by = "kspnr")

# r_plane = r_slope * cos(slope_rad)
bi_points_trees$nha <- ifelse(bi_points_trees$bhd < 30, 10000 / (pi * 6**2 * cos(bi_points_trees$hang_rad)),
                              10000 / (pi * 13**2 * cos(bi_points_trees$hang_rad)))

head(bi_points_trees)

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

# bs == 0 (only 8 values) must me removed
dat <- dat[!dat$bs == 0,]

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

# add new empty column to bi_points_trees
bi_points_trees$vol <- NA

# recode tree species befor applying treegross
bi_points_trees$ba1 <- ifelse(bi_points_trees$ba == 561 | bi_points_trees$ba == 526 , 511,
                       ifelse(bi_points_trees$ba == 716, 711, bi_points_trees$ba))

# create empty table
cop <- bi_points_trees[-(1:dim(bi_points_trees)[1]),]


for(i in unique(bi_points_trees$ba1)){
  
  print(i)
  
  bi_points_trees2 <- bi_points_trees[bi_points_trees$ba1 == i,]
  
  bi_points_trees2$vol <- anstaltspaket::tg.volume(species = i, 
                                                   t.d = bi_points_trees2$bhd,
                                                   t.h = bi_points_trees2$hoe_mod, 
                                                   info = F)
  
  cop <- rbind(cop, bi_points_trees2)
  
}

# copy the table into the original
# delete the copy and unneeded columns
bi_points_trees <- cop
rm(cop, i, bi_points_trees2)
bi_points_trees$ba1=NULL
bi_points_trees$id2=NULL






















