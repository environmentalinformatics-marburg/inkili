# Column with x-coordinates needs to be named "x_pnt"
# Column with y-coordinates needs to be named "y_pnt"
# Column with name of plots needs to be named "plotID"
# radius should be chosen small to make it work quicker

ldr_query <- function (plotID, crdnt_x, crdnt_y, radius = 1){
  
  #getting lidar data involved
  ###db aufrufen (von Stefan)
  
  db_url <- "http://137.248.191.249:8081/pointdb"
  pointdb <- PointDB$new(db_url)
  
  #function to get lidar points to a provided x and y coordinate
  
  func_ldr <- function(utm_x, utm_y, r){
    call <- paste0("pointdb$query_radius_rect(", "x=", utm_x, ",y=", utm_y, 
                   ",radius=", r, ")")
    all_points <- eval(parse(text = call))
    return(all_points)
  }
  
  ldr_sapply <- sapply(seq(length(crdnt_x)), function(i) {
    ldr_pnts_all <- func_ldr(crdnt_x[i], crdnt_y[i], radius)
    ldr_max <- max(ldr_pnts_all$z)
    return(ldr_max)
  })
  ldr_max_df <- as.data.frame(ldr_sapply)
  all_plts <- cbind(plotID, ldr_max_df)
  ldr_plts_all <- subset(all_plts, all_plts[,2]!="-Inf")
  ldr_plts <- as.character(unique(ldr_plts_all[,1])) 
  return(ldr_plts)
}
