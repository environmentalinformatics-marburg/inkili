setwd("/media/aziegler/Volume/data_div/") ###alz: wenn sich diese Zeile nicht ausführen lässt: Volume mounten

mod_date <- "16_02_09"
load(paste0("gpm_models_rf_", mod_date, ".rda"))


library(rasterVis)

tests_agg_sum <- read.csv(paste0(inpath, "/", "tests_agg_sum.csv"), header=T, sep=",")
tests_rdc <- tests_agg_sum[-which(tests_agg_sum$Group.1 %in% c("total_insct", "ttl_insct_rdc")),]
tests_rdc$Group.1 <- as.character(tests_rdc$Group.1)
tests_rdc$Group.1[tests_rdc$Group.1 == "rich_insct"] <- "assemblage richness"
tests_rdc$Group.1[tests_rdc$Group.1 == "Hym_ants"] <- "Ants"
tests_rdc$Group.1[tests_rdc$Group.1 == "Hym_excl_ants"] <- "Hymenoptera"
tests_rdc$Group.2 <- as.character(tests_rdc$Group.2)
tests_rdc$Group.2[tests_rdc$Group.2 == "cof"] <- "coffee"
tests_rdc$Group.2[tests_rdc$Group.2 == "forest_disturbed"] <- "disturbed forest"
tests_rdc$Group.2[tests_rdc$Group.2 == "gra"] <- "grassland"
tests_rdc$Group.2[tests_rdc$Group.2 == "hom"] <- "home garden"
tests_rdc$Group.2[tests_rdc$Group.2 == "mai"] <- "maize"
tests_rdc$Group.2[tests_rdc$Group.2 == "sav"] <- "savannah"

tests_rdc$Group.1 <- as.factor(tests_rdc$Group.1)
tests_rdc$Group.2 <- as.factor(tests_rdc$Group.2)
colnames(tests_rdc)[1] <- "arthro"
colnames(tests_rdc)[2] <- "landuse"
agglist <- aggregate(tests_rdc, by=list(tests_rdc$arthro), FUN=mean) 
agglist$arthro <- agglist$Group.1
agglist$landuse <- "average"
agglist <- agglist[,c(2:ncol(agglist))]
plot_tbl <- rbind(tests_rdc, agglist)

# #pred ~ resp
# plot_all <- lapply(seq(response), function(a){
#   plot(tests$testing_response[which(tests$model_response == response[a])] ~ 
#          tests$testing_predicted[which(tests$model_response == response[a])], main = response[a])
# })

######plotting "Levelplot of R2 of Species by landuse"###
species <- plot_tbl$arthro
species <- unique(species)

landuse <- plot_tbl$landuse
landuse <- unique(landuse)

lst <- lapply(as.character(species), function(i) {
  dat <- plot_tbl[grep(i, plot_tbl[, 1]), ]
  dat[, c("arthro", "landuse", "r_squared")]
})

lst_rsq <- lapply(lst, function(i) {
  i$r_squared
})

dat_rsq <- do.call("cbind", lst_rsq)
rst_rsq <- raster(dat_rsq, xmn = 0.5, xmx = 9.5, 
                  ymn = .5, ymx = 8.5)


clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))
levelplot(rst_rsq, scales = list(x = list(rot=45, at = 1:9, labels = as.character(species)), 
                                 y = list(at = c(8:1), labels = as.character(landuse))), ################sorum stimmt Plot, aber warum ist das hier umgedreht? 
          # levelplot(rst_rsq, scales = list(x = list(at = 1:11),y = list(at = 7:1)),
          margin = FALSE, 
          col.regions = clr(101), 
          at = seq(0, 0.60, 0.01))
####################################################################################
