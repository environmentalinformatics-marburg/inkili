setwd("/media/aziegler/Volume/data_div/") ###alz: wenn sich diese Zeile nicht ausführen lässt: Volume mounten

#mod_date <- "16_02_09"
#load(paste0("gpm_models_rf_", mod_date, ".rda"))

load("best_mods_16_03_17.rda")
models <- best_mods

inpath <- "/media/aziegler/Volume/data_div/"

library(rasterVis)

tests_agg_sum <- read.csv(paste0(inpath, "/", "tests_agg_sum.csv"), header=T, sep=",")
#tests_rdc <- tests_agg_sum[-which(tests_agg_sum$Group.1 %in% c("total_insct", "ttl_insct_rdc", "rich_insct")),]
tests_rdc <- tests_agg_sum
tests_rdc$Group.1 <- as.character(tests_rdc$Group.1)
#tests_rdc$Group.1[tests_rdc$Group.1 == "rich_insct"] <- "assemblage richness"
tests_rdc$Group.1[tests_rdc$Group.1 == "Hym_ants"] <- "Ants*"
tests_rdc$Group.1[tests_rdc$Group.1 == "Hym_excl_ants"] <- "Hymenoptera*"
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

####creating average row
agg_list_spec <- aggregate(tests_rdc, by=list(tests_rdc$arthro), FUN = mean) 
agg_list_spec$arthro <- agg_list_spec$Group.1
agg_list_spec$landuse <- "average"
agg_list_spec <- agg_list_spec[,c(2:ncol(agg_list_spec))]
agg_df_spec <- rbind(agg_list_spec, tests_rdc)

agg_list_land <- aggregate(plot_tbl_spec, by = list(plot_tbl_spec$landuse), FUN = mean)
agg_list_land$landuse <- agg_list_land$Group.1
agg_list_land$arthro <- "average"
agg_list_land <- agg_list_land[,c(2:ncol(agg_list_land))]
agg_df_spec_land <- rbind(plot_tbl_spec, agg_list_land)

#plot_tbl_srt <- plot_tbl[with(agg_df_spec_land, order(agg_df_spec_land$landuse, decreasing = F)),]

plot_tbl_srt <- tests_rdc[with(tests_rdc, order(tests_rdc$landuse, tests_rdc$arthro, decreasing = F)),]

# #pred ~ resp
# plot_all <- lapply(seq(response), function(a){
#   plot(tests$testing_response[which(tests$model_response == response[a])] ~ 
#          tests$testing_predicted[which(tests$model_response == response[a])], main = response[a])
# })

######plotting "Levelplot of R2 of Species by landuse"###
species <- plot_tbl_srt$arthro
species <- unique(species)

landuse <- plot_tbl_srt$landuse
landuse <- unique(landuse)
# landuse_tst <- append(landuse[8],landuse[1:7])


###create big raster 
lst <- lapply(as.character(species), function(i) {
  dat <- plot_tbl_srt[grep(i, plot_tbl_srt[, 1]), ]
  dat[, c("arthro", "landuse", "r_squared")]
})

lst_rsq <- lapply(lst, function(i) {
  i$r_squared
})

dat_rsq <- do.call("cbind", lst_rsq)

rst_rsq <- raster(dat_rsq, xmn = 0.5, xmx = 8.5, 
                  ymn = .5, ymx = 7.5)

clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))
plt <- levelplot(rst_rsq, scales = list(x = list(rot=45, at = 1:8, labels = as.character(species)), 
                                        y = list(at = c(7:1), labels = as.character(landuse))), ################sorum stimmt Plot, aber warum ist das hier umgedreht? 
                 # levelplot(rst_rsq, scales = list(x = list(at = 1:11),y = list(at = 7:1)),
                 margin = FALSE, colorkey = FALSE,
                 col.regions = clr(101), 
                 at = seq(0, 0.30, 0.01))


###create landuse raster from agg_list_land
lst_land <- cbind(agg_list_land$arthro, agg_list_land$landuse, agg_list_land$r_squared)

lst_rsq_land <- as.list(agg_list_land$r_squared)

dat_rsq_land <- t(do.call("cbind", lst_rsq_land))

rst_rsq_land <- raster(dat_rsq_land, xmn = 0.5, xmx = 1.5, 
                  ymn = 0.5, ymx = 7.5)

clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))
plt_land <- spplot(rst_rsq_land, scales = list(draw = F), ################sorum stimmt Plot, aber warum ist das hier umgedreht? 
                      # levelplot(rst_rsq, scales = list(x = list(at = 1:11),y = list(at = 7:1)),
                      margin = FALSE, colorkey = F,
                      col.regions = clr(101), 
                      at = seq(0, 0.30, 0.01))

###create arthro raster from agg_list_spec
lst_land <- cbind(agg_list_spec$arthro, agg_list_spec$landuse, agg_list_spec$r_squared)

lst_rsq_spec <- as.list(agg_list_spec$r_squared)

dat_rsq_spec <- do.call("cbind", lst_rsq_spec)

rst_rsq_spec <- raster(dat_rsq_spec, xmn = 0.5, xmx = 8.5, 
                  ymn = 0.5, ymx = 1.5)

clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))

### apply customized theme

custom_theme <- old_theme <- trellis.par.get()

custom_theme <- list(
  layout.widths = list(
    left.padding = 0,
    key.ylab.padding = 0,
    ylab.axis.padding = 0,
    axis.key.padding = 0,
    right.padding = 0), 
  layout.heights = list(
    top.padding = 0, 
    bottom.padding = 0, 
    axis.xlab.padding = 0,
    xlab.key.padding = 0,
    key.axis.padding = 0, 
    main.key.padding = 0, 
    key.sub.padding = 0
  )
) 

trellis.par.set(custom_theme)

plt_spec <- spplot(rst_rsq_spec, scales = list(draw = F), ################sorum stimmt Plot, aber warum ist das hier umgedreht? 
                      # levelplot(rst_rsq, scales = list(x = list(at = 1:11),y = list(at = 7:1)),
                      margin = FALSE, colorkey = F,
                      col.regions = clr(101), 
                      at = seq(0, 0.30, 0.01))
####################################################################################

###
###

library(grid)

png("test.png", width = 14, height = 14, units = "cm", res = 300)
grid.newpage()

vp0 <- viewport(x = 0, y = 0, just = c("left", "bottom"), 
                width = .7, height = .7)
pushViewport(vp0)
print(plt, newpage = FALSE)

downViewport(trellis.vpname(name = "figure"))

## horizontal raster
trellis.par.set(custom_theme)
vp1 <- viewport(x = 0, y = .6, just = c("left", "bottom"), 
                width = 1, height = 1)
pushViewport(vp1)
print(plt_spec, newpage = FALSE)

## vertical raster
upViewport()

vp2 <- viewport(x = .59, y = 0, just = c("left", "bottom"), 
                width = 1, height = 1)
pushViewport(vp2)
print(plt_land, newpage = FALSE)

## colorkey
upViewport()

vp3 <- viewport(x = 1.25, y = 0, just = c("left", "bottom"), 
                width = .2, height = 1)
pushViewport(vp3)
draw.colorkey(key = list(col = clr(101), width = .75, height = .75,
                         at = seq(0, 0.30, 0.01),
                         space = "right"), draw = TRUE)

dev.off()

####################################################################################
