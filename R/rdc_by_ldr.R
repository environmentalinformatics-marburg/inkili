#' Reduces dataframe to entries that are included in lidar coverage only
#'
#' @description
#' This function reduces a dataframe, so that only entries are kept that are in
#' a vector (by names). If no list exists yet, a dataframe with names of plots
#' and coordinates can be passed so that the function "ldr_query" can create
#' such a vector.
#'
#' @param dataframe_plts Dataframe that should be reduced in the end.
#' @param dataframe_crdnt Dataframe with plot-ID, x-coordinates and y-coordinates
#' in three columns named: "plotID", "x_pnt" and "y_pnt". Default = NULL
#' @param v_plts Character vector with unique name entry for each plot that
#' should be kept during the reduction. Not necessary, if dataframe_crdnt is
#' provided.
#' @param ... Argument search radius for Lidar points (should be chosen small
#' to work quick) can be provided.
#'
#' @author
#' Alice Ziegler
#'
#' @export rdc_by_ldr
#' @name rdc_by_ldr
#' @aliases rdc_by_ldr
#'

###used abreviations in variables
#plts = plots
#ldr = ldr
#lst = list
#rdc = reduced

# Column with x-coordinates needs to be named "x_pnt"
# Column with y-coordinates needs to be named "y_pnt"
# Column with name of plots needs to be named "plotID"
rdc_by_ldr <- function(dataframe_plts,
                       dataframe_crdnt = NULL,
                       v_plts,
                       ...){        #...radius = 1
  if (!is.null(dataframe_crdnt)){
    v_plts <- ldr_query(dataframe_crdnt$plotID,
                           dataframe_crdnt$x_pnt,
                           dataframe_crdnt$y_pnt,
                           ...)     #schreibt hier radius = 1 automatisch rein
  }
  dataframe_plts_rdc <- dataframe_plts[which(dataframe_plts$plotID %in% v_plts),]
  return(dataframe_plts_rdc)
}

