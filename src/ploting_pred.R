setwd("/media/aziegler/Volume/data_div/") ###alz: wenn sich diese Zeile nicht ausführen lässt: Volume mounten

#mod_date <- "16_02_09"
#load(paste0("gpm_models_rf_", mod_date, ".rda"))
load("best_mods_16_03_17.rda")
models <- best_mods


library(rasterVis)


# #pred ~ resp
# plot_all <- lapply(seq(response), function(a){
#   plot(tests$testing_response[which(tests$model_response == response[a])] ~ 
#          tests$testing_predicted[which(tests$model_response == response[a])], main = response[a])
# })

######plotting "Levelplot of R2 of Species by landuse"###
species <- tests_agg_sum[, 1]
species <- unique(species)
species <- ("arthropod abundance", "Collembola", "Acari", "Araneae", "Heteroptera", "Orthoptera", "Coleoptera", "Ants*", "Hymenoptera")

landuse <- tests_agg_sum[, 2]
landuse <- unique(landuse)

lst <- lapply(as.character(species), function(i) {
  dat <- tests_agg_sum[grep(i, tests_agg_sum[, 1]), ]
  dat[, c("Group.1", "Group.2", "r_squared")]
})

lst_rsq <- lapply(lst, function(i) {
  i$r_squared
})

dat_rsq <- do.call("cbind", lst_rsq)
rst_rsq <- raster(dat_rsq, xmn = 0.5, xmx = 8.5, 
                  ymn = .5, ymx = 7.5)


clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))
levelplot(rst_rsq, scales = list(x = list(at = 1:8, labels = as.character(species)), 
                                 y = list(at = 7:1, labels = as.character(landuse))), ################sorum stimmt Plot, aber warum ist das hier umgedreht? 
# levelplot(rst_rsq, scales = list(x = list(at = 1:11),y = list(at = 7:1)),
          margin = FALSE, 
          col.regions = clr(101), 
          at = seq(0, 0.45, 0.01))
####################################################################################
