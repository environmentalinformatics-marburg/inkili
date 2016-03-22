#grnd = ground truth
#anm = animal

library(inkili)


inpath <- "/media/aziegler/Volume/data_div"
mod_date <- "16_03_17__50_eg_" #yy_mm_dd
###reading ground truth tables
#tec_info <- read.csv(paste0(inpath, "/", "tec_info.csv"), header=T, sep=",")
#anm_SR <- read.csv(paste0(inpath, "/", "anm_SR.csv"), header=T, sep=",")
beet_abbr <- read.csv(paste0(inpath, "/", "beet_abbr.csv"), header=T, sep=",")
beet_SR_fin <- read.csv(paste0(inpath, "/", "beet_SR_fin.csv"), header=T, sep=",")
insct_SR_fin <- read.csv(paste0(inpath, "/", "insct_SR_fin.csv"), header=T, sep=",")
tec_crdnt <- read.csv(paste0(inpath, "/", "tec_crdnt.csv"), header=T, sep=",")
###reading lidar table
ldr_stats <-read.csv(paste0(inpath, "/", mod_date, "ldr_stats", ".csv"), header=T, sep=",")

### reducing all tables to lidar cover
##creating vector with lidar plots
ldr_out <- ldr_names(plotID = tec_crdnt$plotID, crdnt_x = tec_crdnt$x_pnt, crdnt_y = tec_crdnt$y_pnt)
###reducing tables
beet_SR_rdc <- rdc_by_ldr(dataframe_plts = beet_SR_fin, v_plts = ldr_out)
insct_SR_rdc <- rdc_by_ldr(dataframe_plts = insct_SR_fin, v_plts = ldr_out)
tec_crdnt_rdc <- rdc_by_ldr(dataframe_plts = tec_crdnt, v_plts = ldr_out)
##############################################################################################################################

###crafting one Table with all information combined and sorted
#double-checking if rows match (by plot)
#grnd_anm_check <- cbind(tec_crdnt_rdc, insct_SR_rdc[,c(1:ncol(insct_SR_rdc))], beet_SR_rdc[,1:ncol(beet_SR_rdc)])
grnd_anm <- cbind(tec_crdnt_rdc, insct_SR_rdc[,c(6:ncol(insct_SR_rdc))], beet_SR_rdc[,6:ncol(beet_SR_rdc)])
## double-checking if rows match (by plot)
#grnd_ldr_check <- cbind (ldr_stats, grnd_anm)
grnd_ldr <- cbind (ldr_stats, grnd_anm)

write.csv(grnd_ldr, file = paste0("/media/aziegler/Volume/data_div/", mod_date, "grnd_ldr.csv"), row.names = F)

