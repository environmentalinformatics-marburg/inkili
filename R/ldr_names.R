#' Find out which plots are included in lidar coverage
#'
#' @description
#' This function produces a character vector with names of plots that are
#' covered by lidar data
#'
#' @param plotID Column with names of the plots
#' @param crdnt_x Column with x-coordinates (in UTM)
#' @param crdnt_y Column with y-coordinates (in UTM)
#' @param radius Search radius for Lidar points (should be chosen small to work quick)
#'
#' @author
#' Alice Ziegler
#'
#' @export ldr_names
#' @name ldr_names
#' @aliases ldr_names
#'

ldr_names <- function (plotID, crdnt_x, crdnt_y, radius = 1){

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
