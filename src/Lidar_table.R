#Varnames
#tec= technical plot information
#rnd= round of messurement (abundance data)
#elev_insct= elevation from the insect abundance dataframe
#msr=messure
#z_dem_hmp = z coordinate of digital elevation model (hmp=? used in plot.shp)
#x_pnt/y_pnt= actual coordinates of the points in the shapefile (not messured) <<- these are to be used
#crdnt = Coordinates
#mrg = merge
#mid=middle
#pnts=points
#vars=variables
#hght=height
#ldr=lidar
#stats = statistics
#angl=angle (=scan angle of lidar)
#nr=number
#rtrn=return
#sngl=single
#dnst=density function
#mdn = median
#qntl= quantile
#cnt = count
#asl = above sea level

#load libraries
library(rgdal)
library(rPointDB)
library(rgl)
library(inkili)
library(raster)

#inpath <- ("E:/")
inpath <- ("/media/aziegler/Volume/data_div")
outpath <- ("/media/aziegler/Volume/data_div")
#setwd()
mod_date <- "16_03_17__100_"   # yy_mm_dd
ldr_radius <- 100
norm = "" #normalise = "origin,ground,extremes"
###############################################################################

#tec_info is created in Bodendaten_sort.R
tec_info <- read.csv(paste0(inpath, "/", "tec_info.csv"), header=T, sep=",")
#Plot shapefile (Aug 2015 from Thomas) contains more Plots than where covered by Lidar
plot_shp <- readOGR(dsn=paste0(inpath, "/PlotPoles_WGS1984_mod_20140807_final",
                               "_lidar_2/PlotPoles_WGS1984_mod_20140807_final",
                               "_lidar_2.shp"),
                    layer="PlotPoles_WGS1984_mod_20140807_final_lidar_2")
plot_df <- as.data.frame(plot_shp)

# DGM <- raster(paste0(inpath, "/DEM_UTM37S_WGS84_30m_Hemp.tif"))
# slp <- raster(paste0(inpath, "/DEM_UTM37S_WGS84_30m_Hemp_slp_qgis.tif"))
# asp <- raster(paste0(inpath, "/DEM_UTM37S_WGS84_30m_Hemp_asp_qgis.tif"))

#get middle Plotpoles
# plot_mid <- plot_shp[which(plot_df$PoleType %in% "AMP"),]
# plot_mid_df <- as.data.frame(plot_mid)
#
# ####slope&aspect are not calculated from lidar, but from DGM ###should be changed in the future
# slp_buff <- extract(slp, plot_mid, buffer = 100)
# slp_pnts <- lapply(seq(slp_buff), function(x) mean(slp_buff[[x]]))
# slp_df <- as.data.frame(t(as.data.frame(slp_pnts)))
#
#
# asp_buff <- extract(asp, plot_mid, buffer = 100)
# asp_pnts <- lapply(seq(asp_buff), function(x) mean(asp_buff[[x]]))
# asp_df <- as.data.frame(t(as.data.frame(asp_pnts)))
#
#
# plot_mid_slp_asp <- cbind(plot_mid_df, slp_df, asp_df)
#
# #merge: get middle plot pole coordinate for each entry in tec_info
# tec_crdnt_mrg <- merge(tec_info, plot_mid_slp_asp, by.x="plotID", by.y="PlotID")
# #cleanup
# colnames(tec_crdnt_mrg) <- (c("plotID", "plot_rnd", "date_coll_insct", "rnd",
#                           "elev_insct", "polename", "poletype", "x_msr","y_msr",
#                           "z_gps","z_dem_hmp","x_pnt","y_pnt", "slp", "asp"))
# tec_crdnt_mrg <- tec_crdnt_mrg[ , -which(names(tec_crdnt_mrg) %in% c("x_msr", "y_msr"))]
# tec_crdnt <- tec_crdnt_mrg[order(tec_crdnt_mrg$plot_rnd),]


#write.table(tec_crdnt, file = paste0(outpath, "/", "tec_crdnt.csv"),
#            row.names=F, sep = ",")
tec_crdnt <- read.csv(paste0(inpath, "/", "tec_crdnt.csv"), header=T, sep=",")

ldr_stats_norm <- ldr_query(plotID = tec_crdnt$plotID, crdnt_x = tec_crdnt$x_pnt,
                       crdnt_y = tec_crdnt$y_pnt, radius = ldr_radius, height = F, filter = NULL, dens = F, normalise = norm)


##height above sea level
ldr_hght_asl <- ldr_query(plotID = tec_crdnt$plotID, crdnt_x = tec_crdnt$x_pnt,
                           crdnt_y = tec_crdnt$y_pnt, radius = 1, height = T)

#average point density for each plot with ldr radius
ldr_pnt_dnst <- ldr_query(plotID = tec_crdnt$plotID, crdnt_x = tec_crdnt$x_pnt,
                          crdnt_y = tec_crdnt$y_pnt, radius = ldr_radius, height = F, dens = T)


#ldr_stats_all <- merge(ldr_stats_norm, ldr_hght_asl, by = c("plotID", "crdnt_x", "crdnt_y"))
ldr_stats_all <- cbind(ldr_stats_norm, ldr_hght_asl[which(colnames(ldr_hght_asl) == "hght_asl")], ldr_pnt_dnst[which(colnames(ldr_pnt_dnst)== "pnt_dnst")])

ldr_stats <- rdc_by_ldr(dataframe_plts = ldr_stats_all, dataframe_crdnt = tec_crdnt)
###write out table
write.table(ldr_stats, file=paste0(outpath, "/", mod_date, "ldr_stats.csv"),
            row.names=F, sep = ",")


# # # ###############testing request for single plot##################################
# #
# #
#testing ldr query for 1 coordinate
# ldr_radius_sngl = 1
# ldr_stats_sngl_ex <- ldr_query(plotID = tec_crdnt$plotID[1], crdnt_x = tec_crdnt$x_pnt[1],
#                            crdnt_y = tec_crdnt$y_pnt[1], radius = ldr_radius_sngl, height = F, normalise = norm, dens = F)


###db aufrufen (von Stephan Wöllauer)
#
# db_url <- "http://137.248.191.249:8081/pointdb"
# pointdb <- PointDB$new(db_url)
#
# #function to get lidar points to a provided x and y coordinate
#
# func_ldr <- function(utm_x, utm_y, r){
#   call <- paste0("pointdb$query_radius_rect(", "x=", utm_x, ",y=", utm_y,
#                  ",radius=", r, ")")
#   all_points <- eval(parse(text = call))
#   return(all_points)
# }
# r= 50
# sngl_pnts_all <- func_ldr(tec_crdnt[which(tec_crdnt$plot_rnd %in% "flm4_aug2011"),
#                                     'x_pnt'],
#                           tec_crdnt[which(tec_crdnt$plot_rnd %in% "flm4_aug2011"),
#                                     'y_pnt'], r)
# sngl_max_hght <- max(sngl_pnts_all$z)
# sngl_min_hght <- min(sngl_pnts_all$z)
# sngl_sd_hght <- sd(sngl_pnts_all$z)
# sngl_mdn_rtrn <- median(sngl_pnts_all$returns)
# sngl_max_angl <- max(abs(sngl_pnts_all$scanAngleRank))
# #Quantile berechnen #von Stefan
# data <- sngl_pnts_all$z
# qmin <- quantile(data, 0.01)
# qmax <- quantile(data, 0.99)
# data <- data[qmin<=data]
# data <- data[data<=qmax]
# # convert from values a.s.l. to "treeheight"
# #Problem: height above lowest point of lidarpoints
# #at slope Plots this might be a problem
# data <- data - qmin
# qntl <- quantile(data, probs=seq(0,1,0.25))
# #sngl_qntl_0 <- qntl[[1]]
# #sngl_qntl_25 <- qntl[[2]]
# #sngl_qntl_50 <- qntl[[3]]
# #sngl_qntl_75 <- qntl[[4]]
# #sngl_qntl_100 <- qntl[[5]]
# sngl_dnst <- density(sngl_pnts_all$z)
# plot3d(sngl_pnts_all$x, sngl_pnts_all$y, sngl_pnts_all$z)
#

