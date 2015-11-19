# Column with name of plots needs to be named "plotID"
#plts = plots
#ldr = ldr
#lst = list
#rdc = reduced

# Column with x-coordinates needs to be named "x_pnt"
# Column with y-coordinates needs to be named "y_pnt"
# Column with name of plots needs to be named "plotID"
rdc_by_ldr <- function(dataframe_plts, 
                       dataframe_crdnt = NULL, 
                       lst_plts, 
                       ...){        #...radius = 1
  if (!is.null(dataframe_crdnt)){
    lst_plts <- ldr_query(dataframe_crdnt$plotID, 
                           dataframe_crdnt$x_pnt, 
                           dataframe_crdnt$y_pnt, 
                           ...)     #schreibt hier radius = 1 automatisch rein
  }
  dataframe_plts_rdc <- dataframe_plts[which(dataframe_plts$plotID %in% lst_plts),]
  return(dataframe_plts_rdc)
}

