#' Get statistical information about lidar data on a plot
#'
#' @description
#' This function produces a dataframe with statistical information about lidar data
#'
#' @param dataframe_crdnt Dataframe with plot-ID, x-coordinates and y-coordinates
#' in three columns named: "plotID", "x_pnt" and "y_pnt".
#'
#' @author
#' Alice Ziegler
#'
#' @export ldr_query
#' @name ldr_query
#' @aliases ldr_query
#'

###work on: statistical values as an option

ldr_query <- function(plotID, crdnt_x, crdnt_y, radius){

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
    ##check if order is right when cbinding
    #plts_name <- as.character(plotID[i])
    #calculate maximal height
    ldr_max_hght <- max(ldr_pnts_all$z)
    #calculate standard deviation
    ldr_sd_hght <- sd(ldr_pnts_all$z)
    #calculate median of total number of returns for each coordinate
    ldr_mdn_rtrn <- median(ldr_pnts_all$returns)
    #calculate maximum scan angle within plot
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
    return(list(plotID = plts_name, max_hght = ldr_max_hght, sd = ldr_sd_hght, mdn = ldr_mdn_rtrn,
                max_angl = ldr_max_angl, qntl_0 = ldr_qntl_0,
                qntl_25 = ldr_qntl_25, qntl_50 = ldr_qntl_50,
                qntl_75 = ldr_qntl_75, qntl_100 = ldr_qntl_100))
  })
  vars <- as.data.frame(t(ldr_sapply))
  ### unlist elements from sapply-loop
  for (i in 1:ncol(vars)) {
    vars[, i] <- unlist(vars[, i])}
  ldr_var <- cbind(plotID, crdnt_x, crdnt_y, vars)

  return(ldr_var)
}


