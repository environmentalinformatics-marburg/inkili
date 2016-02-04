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

ldr_query <- function(plotID, crdnt_x, crdnt_y, radius, height = F){

  ###db aufrufen (von Stephan Wöllauer)

  db_url <- "http://137.248.191.249:8081/pointdb"
  pointdb <- PointDB$new(db_url)
  #normalise <- "origin,ground,extremes"

  #function to get lidar points to a provided x and y coordinate

  func_ldr <- function(utm_x, utm_y, r, normalise){
    all_points <- pointdb$query_radius_rect(x = utm_x, y = utm_y, radius = r, normalise=normalise)
    #all_points <- all_points[all_points$z>1,] #1m ist zu hoch
    return(all_points)
  }
  if (height ==F) {
    ldr_sapply <- sapply(seq(length(crdnt_x)), function(i) {
      ldr_pnts_all <- func_ldr(crdnt_x[i], crdnt_y[i], radius, normalise = "ground") #"origin,ground,extremes"
      qmin <- quantile(ldr_pnts_all$z, 0.005)
      qmax <- quantile(ldr_pnts_all$z, 0.995)
      ldr_pnts_all$z <- ldr_pnts_all$z[qmin<=ldr_pnts_all$z]
      ldr_pnts_all <- ldr_pnts_all[ldr_pnts_all$z<=qmax]
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


      # calculate quantiles # changed: from Stephan
      data <- ldr_pnts_all$z
      ###qmin <- quantile(data, 0.01)
      ###qmax <- quantile(data, 0.99)
      ###data <- data[qmin<=data]
      ###data <- data[data<=qmax]
      # convert from values a.s.l. to "treeheight"
      #Problem: height above lowest point of lidarpoints
      #at slope Plots this might be a problem
      ###data <- data - qmin
      ldr_qntl <- quantile(data, probs=seq(0,1,0.25))
      ldr_qntl_0 <- ldr_qntl[[1]]
      ldr_qntl_25 <- ldr_qntl[[2]]
      ldr_qntl_50 <- ldr_qntl[[3]]
      ldr_qntl_75 <- ldr_qntl[[4]]
      ldr_qntl_100 <- ldr_qntl[[5]]


      ##if statement: if no lidar points are available: density calculation
      ##produces error - here those cases are sortet out
      if (ldr_max_hght == "-Inf") {
        cffnt_intcpt <- NA
        cffnt_x <- NA
        cffnt_x2 <- NA
        cffnt_x3 <- NA
        #cffnt_x4 <- NA
        ldr_max_rtrn <- NA
        lst_sd_rtrn <- NA
        sd_per_rtrn_1 <- NA
        sd_per_rtrn_2 <- NA
        sd_per_rtrn_3 <- NA
        sd_per_rtrn_4 <- NA
        sd_per_rtrn_5 <- NA
        sd_per_rtrn_6 <- NA
        ldr_sd_last_rtrn <- NA
        ldr_sd_nmbr_rtrn <- NA

      } else {
        ### get 4 coefficients to describe density function
        ldr_dnst <- density(ldr_pnts_all$z)
        dnst <- data.frame(cbind(x=ldr_dnst$x, y=ldr_dnst$y))
        fnct_dnst <- glm(formula = y ~ x + I(x^2) + I(x^3) + I(x^4), data = dnst)
        smmry_dnst <- summary(fnct_dnst)
        cffnt_intcpt <- smmry_dnst$coefficients[1,1]
        cffnt_x <- smmry_dnst$coefficients[2,1]
        cffnt_x2 <- smmry_dnst$coefficients[3,1]
        cffnt_x3 <- smmry_dnst$coefficients[4,1]
        #cffnt_x4 <- smmry_dnst$coefficients[5,1] #rausgenommen für 16_02_02

        ###calculate sd of each "return set" (one return set consists of only first
        ###or seconde or... returns)
        # calculate maximum number of returns
        ldr_max_rtrn <- max(ldr_pnts_all$returns)
        # calculate for each return set
        #calculate standard deviation of all first, second, ... returns in one plot
        lst_sd_rtrn <- lapply((c(1:ldr_max_rtrn)), function(x) {
          points <- sd(ldr_pnts_all$z[which(ldr_pnts_all$returnNumber == x)])
        })
        lst_sd_rtrn <- do.call("cbind", lst_sd_rtrn)
        ldr_sd_rtrn_names <- lapply((c(1:ldr_max_rtrn)), function(x) {
          name <- paste0("ldr_sd_rtrn_", x)
        })
        colnames(lst_sd_rtrn) <- c(ldr_sd_rtrn_names)
        ####################################################################################to do: ganze liste außerhalb von lapply in dataframe integrieren
        ###Notlösung:
        sd_per_rtrn_1 <- lst_sd_rtrn[1]
        sd_per_rtrn_2 <- lst_sd_rtrn[2]
        sd_per_rtrn_3 <- lst_sd_rtrn[3]
        sd_per_rtrn_4 <- lst_sd_rtrn[4]
        sd_per_rtrn_5 <- lst_sd_rtrn[5]
        sd_per_rtrn_6 <- lst_sd_rtrn[6]
        #calculate standard deviation of last returns
        ldr_sd_last_rtrn <- sd(ldr_pnts_all$z[which(ldr_pnts_all$returnNumber ==
                                                      ldr_pnts_all$returns)])
        #calculate standard deviation of total number of returns
        ldr_sd_nmbr_rtrn <- sd(ldr_pnts_all$returns)
      }
      return(list(max_hght = ldr_max_hght, sd = ldr_sd_hght, mdn = ldr_mdn_rtrn,
                  max_angl = ldr_max_angl, max_rtrn = ldr_max_rtrn,
                  sd_lst_rtrn = ldr_sd_last_rtrn, sd_max_rtrn = ldr_sd_nmbr_rtrn,
                  qntl_0 = ldr_qntl_0, qntl_25 = ldr_qntl_25,
                  qntl_50 = ldr_qntl_50, qntl_75 = ldr_qntl_75,
                  qntl_100 = ldr_qntl_100, cffnt_intcpt = cffnt_intcpt,
                  cffnt_x = cffnt_x, cffnt_x2 = cffnt_x2, cffnt_x3 = cffnt_x3,
                  #cffnt_x4 = cffnt_x4,
                  sd_per_rtrn_1 = sd_per_rtrn_1, sd_per_rtrn_2 = sd_per_rtrn_2,
                  ldr_radius = radius))
    })
    vars <- as.data.frame(t(ldr_sapply))
    ################still to work on
    #   sd_per_rtrn_raw <- vars[, ncol(vars)]
    #   sd_per_rtrn_1 <- as.data.frame(sd_per_rtrn_raw)
    #   sd_per_rtrn_raw2 <- do.call("cbind", vars[, ncol(vars)])

    ### unlist elements from sapply-loop
    for (i in 1:ncol(vars)) {
      vars[, i] <- unlist(vars[, i])
    }
    ldr_var <- cbind(plotID, crdnt_x, crdnt_y, vars)

  } else {

    ldr_sapply_hght <- sapply(seq(length(crdnt_x)), function(i) {
      ldr_pnts_all <- func_ldr(crdnt_x[i], crdnt_y[i], radius, normalise = NULL)
      ##check if order is right when cbinding
      #plts_name <- as.character(plotID[i])
      #calculate maximal height
      ldr_hght_asl <- min(ldr_pnts_all$z)
      return(hght_asl = ldr_hght_asl)
    })
    vars <- as.data.frame(ldr_sapply_hght)
    ################still to work on
    #   sd_per_rtrn_raw <- vars[, ncol(vars)]
    #   sd_per_rtrn_1 <- as.data.frame(sd_per_rtrn_raw)
    #   sd_per_rtrn_raw2 <- do.call("cbind", vars[, ncol(vars)])

    ### unlist elements from sapply-loop
    ldr_var <- cbind(plotID, crdnt_x, crdnt_y, vars)
    colnames(ldr_var)[which(colnames(ldr_var) == "ldr_sapply_hght")] <- "hght_asl"
  }

  return(ldr_var)
}
