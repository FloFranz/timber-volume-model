#-------------------------------------------------------------
# Name:         lm_timber_vol.R
# Description:  script creates a linear model to predict 
#               timber volume based on remote sensing data and
#               terrestrial sample plot measurements.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#-------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01 - set file paths
#-------------------------------------

# input path to DSMs and nDSM
dsm_path <- paste0(raw_data_dir, 'DSMs/')
ndsm_path <- paste0(raw_data_dir, 'nDSMs/')

# input path to terrestrial data
bi_path <- paste0(raw_data_dir, 'BI/')



# 02 - data reading
#-------------------------------------

# read nDSM
ndsm_files <- list.files(ndsm_path)

ndsm <- terra::rast(paste0(ndsm_path, ndsm_files))

# quick plot
terra::plot(ndsm)

# read BI data
bi_tables <- list.files(bi_path)

bi_vorr <- read.table(paste0(bi_path, bi_tables[1]),
                      header = T, sep = ';')

bi_plots <- read.table(paste0(bi_path, bi_tables[2]),
                       header = T, sep = ';')

head(bi_vorr)
head(bi_plots)



# 03 - data preperation
#-------------------------------------

# source and apply function for data formatting
source('src/format_data.R', local = TRUE)

bi_plots <- format_data(bi_plots)
bi_vorr <- format_data(bi_vorr)

head(bi_plots)
str(bi_plots)
head(bi_vorr)
str(bi_vorr)

# delete deadwood and used trees
bi_vorr <- bi_vorr[!bi_vorr$ba %in% seq(100,800,100),]

bi_vorr <- bi_vorr[bi_vorr$'1' < 3 & bi_vorr$'2' < 3,]

bi_vorr <- bi_vorr[bi_vorr$art !=1 & bi_vorr$art !=2 & bi_vorr$bhd > 0,]

# remove unneeded columns
bi_vorr <- bi_vorr[,c(1:12)]

# assign tree species groups
bi_vorr$bagr <- ifelse(bi_vorr$ba>0 & bi_vorr$ba<200, "EI",
                ifelse(bi_vorr$ba>199 & bi_vorr$ba<300, "BU",
                ifelse(bi_vorr$ba>299 & bi_vorr$ba<400, "ALH",	
                ifelse(bi_vorr$ba>399 & bi_vorr$ba<500, "ALN",
                ifelse(bi_vorr$ba>499 & bi_vorr$ba<600, "FI",
                ifelse(bi_vorr$ba>599 & bi_vorr$ba<700, "DGL",
                ifelse(bi_vorr$ba>699 & bi_vorr$ba<800, "KI",	"LAE")))))))

# BHD correction
# if not measured at 1.3 m (deviating measuring height), then correction to 1.3 m
bi_vorr$ba1 <- bi_vorr$ba

# red oak to oak, fir to spruce, hornbeam to beech
source('src/d_corr_func.R', local = TRUE)

bi_vorr$ba <- input_d_korr(bi_vorr$bagr)

# average BHD from 'Kreuzkluppung (bhdklup)', convert to cm
bi_vorr$bhd <- ifelse(bi_vorr$bhdklup > 0, (0.5 * (bi_vorr$bhd + bi_vorr$bhdklup))/10, bi_vorr$bhd/10)

# separate data:
# trees with diameter at deviating measurement height and without deviating measurement height
bi_vorr_2 <- bi_vorr[bi_vorr$bhddiff > 0,]
bi_vorr <- bi_vorr[bi_vorr$bhddiff == 0,]

# convert diameter to BHD in case of deviating measuring height
d <- d_korr(du = bi_vorr_2$bhd, abwmh = bi_vorr_2$bhddiff, ba = bi_vorr_2$ba)
bi_vorr_2$bhd=d

# bind the two data frame together again
bi_vorr <- rbind(bi_vorr, bi_vorr_2)
rm(bi_vorr_2, d)

# wieder originale Baumart
bi_vorr$ba <- bi_vorr$ba1
bi_vorr$ba1 <- NULL

head(bi_vorr)


