

library(raster)
inpath <- "/media/aziegler/Volume/data_div/"



DGM <- raster(paste0(inpath, "DEM_UTM37S_WGS84_30m_Hemp.tif"))
slope <- raster(paste0(inpath, "DEM_UTM37S_WGS84_30m_Hemp_slp_qgis.tif"))
plots <- readOGR(paste0(inpath, "PlotPoles_WGS1984_mod_20140807_final_",
                              "lidar_2/PlotPoles_WGS1984_mod_20140807_final_lidar_2.shp"),
                 layer = "PlotPoles_WGS1984_mod_20140807_final_lidar_2")

plots_df <- as.data.frame(plots)
test_plot <- plots_df[]
