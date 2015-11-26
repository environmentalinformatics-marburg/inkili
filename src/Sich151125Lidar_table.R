#Varnames
#tec= technical plot information
#rnd= round of messurement (abundance data)
#elev_insct= elevation from the insect abundance dataframe
#msr=messure
#z_dem_hmp = z coordinate of digital elevation model (hmp=? used in plot.shp)
#x_pnt/y_pnt= actual coordinates of the points in the shapefile (not messured) <<- these are to be used
#crdnt = Coordinates
#mid=middle
#pnts=points
#vars=variables
#hght=height
#ldr=lidar
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
#inpath <- ("E:/")
inpath <- ("/media/aziegler/Volume")
outpath <- ("/media/aziegler/Volume/data_div")
setwd()

r= 40 ####change this to the suitable radius

###############################################################################

#tec_info is created in Bodendaten_sort.R
tec_info <- read.csv(paste0(inpath, "/", "Bodendaten/data/output/tec_info.csv"), header=T, sep=",")
#Plot shapefile (Aug 2015 from Thomas) contains more Plots than where covered by Lidar
plot_shp <- readOGR(dsn=paste0(inpath, "/", 
                               "data_div/PlotPoles_WGS1984_mod_20140807_final", 
                               "_lidar_2/PlotPoles_WGS1984_mod_20140807_final", 
                               "_lidar_2.shp"),  
                    layer="PlotPoles_WGS1984_mod_20140807_final_lidar_2")
plot_df <- as.data.frame(plot_shp)

#get middle Plotpoles
#plot_mid <- plot_df[which(plot_df$PoleType %in% "AMP"),] # alle middle poles! alle eindeutig
plot_mid <- plot_shp[which(plot_df$PoleType %in% "AMP"),]

#merge: get middle plot pole coordinate for each entry in tec_info
tec_crdnt <- merge(tec_info, plot_mid, by.x="plotID", by.y="PlotID")
#cleanup
colnames(tec_crdnt) <- (c("plotID", "plot_rnd", "date_coll", "rnd", 
                          "elev_insct", "polename", "poletype", "x_msr","y_msr",
                          "z_gps","z_dem_hmp","x_pnt","y_pnt"))
tec_crdnt <- tec_crdnt[ , -which(names(tec_crdnt) %in% c("x_msr", "y_msr"))]

###find out which plots are not included in the lidar data (one time detour for 
###the "plot_selection_lidar.R function)
###execute only once:
#plt_out <- subset(ldr_var, ldr_var$max_hght=="-Inf")
#plt_out <- as.data.frame(unique(plt_out$plotID))
#colnames(plt_out) <- "no_lidar_data"
#write.csv(plt_out, file="/media/aziegler/Volume/data_div/no_lidar_data")

###Script plt_out.r ausführen
source("/media/aziegler/Volume/scripts/plt_out.R")
tec_crdnt <- plt_out(tec_crdnt)

#getting lidar data involved
###db aufrufen (von Stefan)

db_url <- "http://137.248.191.249:8081/pointdb"
pointdb <- PointDB$new(db_url)

#function to get lidar points to a provided x and y coordinate
#additionally: calculate for example max of the points

func_ldr <- function(utm_x, utm_y, r){
  call <- paste0("pointdb$query_radius_rect(", "x=", utm_x, ",y=", utm_y, 
                 ",radius=", r, ")")
  all_points <- eval(parse(text = call))
  return(all_points)
}



ldr_sapply <- sapply(seq(nrow(tec_crdnt)), function(z) {
  ldr_pnts_all <- func_ldr(tec_crdnt[z, 'x_pnt'], tec_crdnt[z, 'y_pnt'], r)
  ldr_max_hght <- max(ldr_pnts_all$z)
  ldr_sd_hght <- sd(ldr_pnts_all$z)
  ldr_mdn_rtrn <- median(ldr_pnts_all$returns) #median of total number of 
                                               #returns for each coordinate
  ldr_max_angl <- max(abs(ldr_pnts_all$scanAngleRank))
  # calculate quantiles # changed: from Stefan
  data <- ldr_pnts_all$z
  qmin <- quantile(data, 0.01)
  qmax <- quantile(data, 0.99)
  data <- data[qmin<=data]
  data <- data[data<=qmax]
  # convert from values a.s.l. to "treeheight" 
  #Problem: height above lowest point of lidarpoints
  #at slope Plots this might be a problem
  data <- data - qmin 
  ldr_qntl <- quantile(data, probs=seq(0,1,0.25))
  ldr_qntl_0 <- ldr_qntl[[1]]
  ldr_qntl_25 <- ldr_qntl[[2]]
  ldr_qntl_50 <- ldr_qntl[[3]]
  ldr_qntl_75 <- ldr_qntl[[4]]
  ldr_qntl_100 <- ldr_qntl[[5]]
  #ldr_dnst <- density(ldr_pnts_all$z)
  #plot(ldr_dnst)
  #print(z)
  #print(max(ldr_pnts_all$z))
  return(list(max_hght = ldr_max_hght, sd = ldr_sd_hght, mdn = ldr_mdn_rtrn, 
              max_angl = ldr_max_angl, qntl_0 = ldr_qntl_0, 
              qntl_25 = ldr_qntl_25, qntl_50 = ldr_qntl_50,  
              qntl_75 = ldr_qntl_75, qntl_100 = ldr_qntl_100))
})


vars <- as.data.frame(t(ldr_sapply))
### unlist elements from sapply-loop
for (i in 1:ncol(vars)) {
  vars[, i] <- unlist(vars[, i])}
ldr_var <- cbind(tec_crdnt, vars)

###write out table
write.table(ldr_var, file=paste0(outpath, "/", "ldr_data_plots.csv"), row.names=F)


###############testing request for single plot##################################
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
#plot3d(sngl_pnts_all$x, sngl_pnts_all$y, sngl_pnts_all$z)

#############
###Script plt_out.r ausführen
source("/media/aziegler/Volume/scripts/plt_out.R")
ldr_var_fin <- plt_out(ldr_var)

###################

###used for finding out which plots are not included in the lidar data 
#(one time detour for the "plot_selection_lidar.R function)
##execute only once:
plt_out <- subset(ldr_var, ldr_var$max_hght=="-Inf")  
plt_out <- as.data.frame(unique(plt_out$plotID))
colnames(plt_out) <- "no_lidar_data"
write.csv(plt_out, file="/media/aziegler/Volume/data_div/no_lidar_data.csv", row.names=F)
##use that csv table to get names of not included plots for the plt_out function!!!


####################testing####################################################
for (i in 12:ncol(ldr_var)) {ldr_var[, i] <- unlist(ldr_var[, i])}
write.csv(ldr_var, file=paste0(outpath, "/", "ldr_data_plots.csv"))


zmax_all <- as.numeric(ldr_sapply[1,])


a <- ldr3((seq(nrow(tec_crdnt))), f)



df <- pointdb$query_radius_rect(x=322090, y=9635630, radius=200)
#df <- pointdb$query_radius_rect(x=308285.0, y=9662407, radius=40)

######################################################################




###first try to do apply-loop as an l-apply, so that the density function and 
#the quantiles are kept properly in the list. Loop works, but how can I get the 
#list into a multidimensional data frame then? Or how can I 
ldr_lapply <- lapply(seq(nrow(tec_crdnt)), function(z) {
  ldr_pnts_all <- func_ldr(tec_crdnt[z, 'x_pnt'], tec_crdnt[z, 'y_pnt'], r)
  ldr_max_hght <- max(ldr_pnts_all$z)
  ldr_sd_hght <- sd(ldr_pnts_all$z)
  ldr_mdn_rtrn <- median(ldr_pnts_all$returns) #median of total number of returns for each coordinate
  ldr_max_angl <- max(abs(ldr_pnts_all$scanAngleRank))
  # calculate quantiles # changed: from Stefan
  data <- ldr_pnts_all$z
  qmin <- quantile(data, 0.01)
  qmax <- quantile(data, 0.99)
  data <- data[qmin<=data]
  data <- data[data<=qmax]
  # convert from values a.s.l. to "treeheight" 
  #Problem: height above lowest point of lidarpoints
  #at slope Plots this might be a problem
  data <- data - qmin 
  qntl <- quantile(data, probs=seq(0,1,0.25))
  ldr_dnst <- density(ldr_pnts_all$z)
  #plot(ldr_dnst)
  #print(z)
  #print(max(ldr_pnts_all$z))
  return(list(max_hght = ldr_max_hght, sd = ldr_sd_hght, mdn = ldr_mdn_rtrn, 
              max_angl = ldr_max_angl, qntl = qntl, dnst_fnct = ldr_dnst))
})
# 
var_keep <- c(1,2,3,4)
ldr_var1 <- lapply(ldr_lapply, "[", var_keep)
#x <- structure(ldr_var, class = "data.frame")
#DF <- structure(L, row.names = c(NA, -n), class = "data.frame")

library(plyr)
library(reshape)
t <- lapply(ldr, "[", var_keep)
x <- melt(t, id.vars=c("max_hght", "sd"))
y <- cast(x, x[,1]~L2)
  #ddply(x, c("L2"))

#ddply(iw.m, c("sex", "AGE"), function(x) as.data.frame(prop.table(table(x$value)))