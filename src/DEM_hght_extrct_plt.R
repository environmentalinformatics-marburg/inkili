library(rgdal)
library(raster)

#DEM from Tim Nov 2015
DEM <- raster("/media/aziegler/Volume/data_div/DEM_UTM37S_WGS84_30m_Hemp.tif")
#Plot shapefile (Aug 2015 from Thomas) contains more Plots than where covered by Lidar
plot_shp <- readOGR(dsn = paste0("/media/aziegler/Volume/data_div/", 
                              "PlotPoles_WGS1984_mod_20140807_final_lidar_2/", 
                              "PlotPoles_WGS1984_mod_20140807_final_lidar_2.shp"),
                 layer = "PlotPoles_WGS1984_mod_20140807_final_lidar_2")
plot_df <- as.data.frame(plot_shp)

plot_hght <- as.data.frame(extract(DEM, plot_shp))
plt_hght_all <- cbind(plot_df$PlotID, plot_df$PoleType, plot_hght)
colnames(plt_hght_all) <- c("PlotID", "PlotType", "DEM_hght")
plt_hght <- plt_hght_all[which(plt_hght_all$PlotType %in% "AMP"),]

write.csv(plt_hght, file= "/media/aziegler/Volume/data_div/plt_hght.csv", row.names=F)
