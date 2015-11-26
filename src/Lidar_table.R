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

#load libraries
library(rgdal)
library(rPointDB)
library(rgl)
library(inkili)
#inpath <- ("E:/")
inpath <- ("/media/aziegler/Volume/data_div")
outpath <- ("/media/aziegler/Volume/data_div")
#setwd()

###############################################################################

#tec_info is created in Bodendaten_sort.R
tec_info <- read.csv(paste0(inpath, "/", "tec_info.csv"), header=T, sep=",")
#Plot shapefile (Aug 2015 from Thomas) contains more Plots than where covered by Lidar
plot_shp <- readOGR(dsn=paste0(inpath, "/PlotPoles_WGS1984_mod_20140807_final",
                               "_lidar_2/PlotPoles_WGS1984_mod_20140807_final",
                               "_lidar_2.shp"),
                    layer="PlotPoles_WGS1984_mod_20140807_final_lidar_2")
plot_df <- as.data.frame(plot_shp)

#get middle Plotpoles
#plot_mid <- plot_df[which(plot_df$PoleType %in% "AMP"),] # alle middle poles! alle eindeutig
plot_mid <- plot_shp[which(plot_df$PoleType %in% "AMP"),]

#merge: get middle plot pole coordinate for each entry in tec_info
tec_crdnt_mrg <- merge(tec_info, plot_mid, by.x="plotID", by.y="PlotID")
#cleanup
colnames(tec_crdnt_mrg) <- (c("plotID", "plot_rnd", "date_coll_insct", "rnd",
                          "elev_insct", "polename", "poletype", "x_msr","y_msr",
                          "z_gps","z_dem_hmp","x_pnt","y_pnt"))
tec_crdnt_mrg <- tec_crdnt_mrg[ , -which(names(tec_crdnt_mrg) %in% c("x_msr", "y_msr"))]
tec_crdnt <- tec_crdnt_mrg[order(tec_crdnt_mrg$plot_rnd),]


write.table(tec_crdnt, file = paste0(outpath, "/", "tec_crdnt.csv"),
            row.names=F, sep = ",")


ldr_stats_all <- ldr_query(plotID = tec_crdnt$plotID, crdnt_x = tec_crdnt$x_pnt,
                       crdnt_y = tec_crdnt$y_pnt, radius = 40)

ldr_stats <- rdc_by_ldr(dataframe_plts = ldr_stats_all, dataframe_crdnt = tec_crdnt)

###write out table
write.table(ldr_stats, file=paste0(outpath, "/", "ldr_stats.csv"),
            row.names=F, sep = ",")


# ###############testing request for single plot##################################


###db aufrufen (von Stephan Wöllauer)

db_url <- "http://137.248.191.249:8081/pointdb"
pointdb <- PointDB$new(db_url)

#function to get lidar points to a provided x and y coordinate

func_ldr <- function(utm_x, utm_y, r){
  call <- paste0("pointdb$query_radius_rect(", "x=", utm_x, ",y=", utm_y,
                 ",radius=", r, ")")
  all_points <- eval(parse(text = call))
  return(all_points)
}
r= 40
sngl_pnts_all <- func_ldr(tec_crdnt[which(tec_crdnt$plot_rnd %in% "sav5_aug2011"),
                                    'x_pnt'],
                          tec_crdnt[which(tec_crdnt$plot_rnd %in% "sav5_aug2011"),
                                    'y_pnt'], r)
sngl_max_hght <- max(sngl_pnts_all$z)
sngl_min_hght <- min(sngl_pnts_all$z)
sngl_sd_hght <- sd(sngl_pnts_all$z)
sngl_mdn_rtrn <- median(sngl_pnts_all$returns)
sngl_max_angl <- max(abs(sngl_pnts_all$scanAngleRank))
#Quantile berechnen #von Stefan
data <- sngl_pnts_all$z
qmin <- quantile(data, 0.01)
qmax <- quantile(data, 0.99)
data <- data[qmin<=data]
data <- data[data<=qmax]
# convert from values a.s.l. to "treeheight"
#Problem: height above lowest point of lidarpoints
#at slope Plots this might be a problem
data <- data - qmin
qntl <- quantile(data, probs=seq(0,1,0.25))
#sngl_qntl_0 <- qntl[[1]]
#sngl_qntl_25 <- qntl[[2]]
#sngl_qntl_50 <- qntl[[3]]
#sngl_qntl_75 <- qntl[[4]]
#sngl_qntl_100 <- qntl[[5]]
sngl_dnst <- density(sngl_pnts_all$z)
plot3d(sngl_pnts_all$x, sngl_pnts_all$y, sngl_pnts_all$z)

####################testing####################################################
#
# df <- pointdb$query_radius_rect(x=322090, y=9635630, radius=200)
# #df <- pointdb$query_radius_rect(x=308285.0, y=9662407, radius=40)
#
# ######################################################################
#
#
#
# ###first try to do apply-loop as an l-apply, so that the density function and
# #the quantiles are kept properly in the list. Loop works, but how can I get the
# #list into a multidimensional data frame then?
# ldr_lapply <- lapply(seq(nrow(tec_crdnt)), function(z) {
#   ldr_pnts_all <- func_ldr(tec_crdnt[z, 'x_pnt'], tec_crdnt[z, 'y_pnt'], r)
#   ldr_max_hght <- max(ldr_pnts_all$z)
#   ldr_sd_hght <- sd(ldr_pnts_all$z)
#   ldr_mdn_rtrn <- median(ldr_pnts_all$returns) #median of total number of returns for each coordinate
#   ldr_max_angl <- max(abs(ldr_pnts_all$scanAngleRank))
#   # calculate quantiles # changed: from Stefan
#   data <- ldr_pnts_all$z
#   qmin <- quantile(data, 0.01)
#   qmax <- quantile(data, 0.99)
#   data <- data[qmin<=data]
#   data <- data[data<=qmax]
#   # convert from values a.s.l. to "treeheight"
#   #Problem: height above lowest point of lidarpoints
#   #at slope Plots this might be a problem
#   data <- data - qmin
#   qntl <- quantile(data, probs=seq(0,1,0.25))
#   ldr_dnst <- density(ldr_pnts_all$z)
#   #plot(ldr_dnst)
#   #print(z)
#   #print(max(ldr_pnts_all$z))
#   return(list(max_hght = ldr_max_hght, sd = ldr_sd_hght, mdn = ldr_mdn_rtrn,
#               max_angl = ldr_max_angl, qntl = qntl, dnst_fnct = ldr_dnst))
# })
# #
# var_keep <- c(1,2,3,4)
# ldr_var1 <- lapply(ldr_lapply, "[", var_keep)
# #x <- structure(ldr_var, class = "data.frame")
# #DF <- structure(L, row.names = c(NA, -n), class = "data.frame")
#
# library(plyr)
# library(reshape)
# t <- lapply(ldr, "[", var_keep)
# x <- melt(t, id.vars=c("max_hght", "sd"))
# y <- cast(x, x[,1]~L2)
#   #ddply(x, c("L2"))
#
# #ddply(iw.m, c("sex", "AGE"), function(x) as.data.frame(prop.table(table(x$value)))
