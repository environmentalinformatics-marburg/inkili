#Varnames
#abun = abundance of animals in general
#S=site
#R = Round(~Durchgang)
#crdnt = Coordinates
#mid=middle
#pnts=points
#vars=variables
#hght=height
#ldr=lidar
#nr=number
#rtrn=return
#sngl=single

#load libraries
library(rgdal)
library(rPointDB)
library(rgl)
#inpath <- ("E:/")
inpath <- ("/media/aziegler/Volume/")
#outpath <- ("")

r= 40 ####change this to the suitable radius

###############################################################################
#source("/media/aziegler/Volume/scripts/Bodendaten_sort.R")
abun_SR <- read.csv(paste0(inpath, "Bodendaten/data/output/abun_SR_fin.csv"), header=T, sep=",")
plot_shp <- readOGR(dsn=paste0(inpath, 
                               "data_div/PlotPoles_WGS1984_mod_20140807_final", 
                               "_lidar_2/PlotPoles_WGS1984_mod_20140807_final", 
                               "_lidar_2.shp"),  
                    layer="PlotPoles_WGS1984_mod_20140807_final_lidar_2")
plot_df <- as.data.frame(plot_shp)

#get middle Plotpoles
#plot_mid <- plot_df[which(plot_df$PoleType %in% "AMP"),] # alle middle poles! alle eindeutig
plot_mid <- plot_shp[which(plot_df$PoleType %in% "AMP"),]

#merge: get middle plot pole coordinate for each entry in abun_SR
abun_crdnt <- merge(abun_SR, plot_mid, by.x="plotID.x", by.y="PlotID")

#getting lidar data involved
db_url <- "http://137.248.191.249:8081/pointdb"
pointdb <- PointDB$new(db_url)



#function to get lidar points to a provided x and y coordinate
#additionally: calculate for example max of the points

func_ldr <- function(utm_x, utm_y, r){
  call <- paste0("pointdb$query_radius_rect(", "x=", utm_x, ",y=", utm_y, ",radius=", r, ")")
  all_points <- eval(parse(text = call))
  return(all_points)
}

                 #  [,c('coords.x1', 'coords.x2')]
ldr_vars <- sapply(seq(nrow(abun_crdnt)), function(z) {
  ldr_pnts_all <- func_ldr(abun_crdnt[z, 'coords.x1'], abun_crdnt[z, 'coords.x2'], r)
  ldr_max_hght <- max(ldr_pnts_all$z)
  ldr_sd_hght <- sd(ldr_pnts_all$z)
  print(z)
  print(max(ldr_pnts_all$z))
  return(cbind(ldr_max_hght, ldr_sd_hght))
  #ldr_max_rtrn <- max(ldr_pnts_all$returns) # @stefan: returns oder returnNumer als anzahl der Spalte?
})

vars <- as.data.frame(t(ldr_vars))
colnames(vars) <- c("max_hght", "sd_hght")
abun_var <- cbind(abun_crdnt, vars)
  



  





####################testing####################################################

###query läuft durch mit apply
test <- apply(abun_crdnt[,c('coords.x1', 'coords.x2')], 1, function(apl){
  #print(apl['coords.x1'])
  #print(apl['coords.x2'])
  ldr_pnts_all <- func_ldr(apl['coords.x1'], apl['coords.x2'], r)#, 
  ldr_max_hght <- max(ldr_pnts_all$z) 
  ldr_sd_hght <- sd(ldr_pnts_all$z)
  return(cbind(ldr_max_hght, ldr_sd_hght))
  #   #ldr_max_rtrn <- 
})
t <- as.data.frame(test)

# func_coord <- function(a, b) {
#   utm_x <- a
#   utm_y <- b
#   line <- data.frame(utm_x, utm_y)
#   return(line)
# }


#sapply(abun_crdnt[,c('coords.x1', 'coords.x2')], function(x)

blubber <- apply(abun_crdnt[,c('coords.x1', 'coords.x2')], 1, function(y) func_coord(y['coords.x1'], y['coords.x2']))

#open tabs
#http://stackoverflow.com/questions/15059076/r-how-to-call-apply-like-function-on-each-row-of-dataframe-with-multiple-argum
#http://stackoverflow.com/questions/6827299/r-apply-function-with-multiple-parameters
#http://www.r-bloggers.com/using-apply-sapply-lapply-in-r/
#http://www.ats.ucla.edu/stat/r/library/intro_function.htm



# testFunc <- function(a, b) a + b
# apply(dat[,c('x','z')], 1, function(y) testFunc(y['z'],y['x']))
# 

blub <- sapply(abun_crdnt$coords.x1, function(x) 

  #utm_x <- abun_crdnt$coords.x1[x], 
  #utm_y <- abun_crdnt$coords.x2[x],
  #r <- 40,
  #print("blub")
  abun_crdnt$coords.x1[x]
  #return(utm_x)
  )


utm_x=322090
utm_y=9635630
r <- 200

#craft executable line
call <- paste0("pointdb$query_radius_rect(", "x=", utm_x, ",y=", utm_y, ",radius=", r, ")")

#write df with function
all_points <- eval(parse(text = call))


#x=(abun_crdnt[1, (ncol(abun_crdnt)-1)])
test <- pointdb$query_radius_rect(x=322090, 
                                  y=local(expression(utm_y), envir = pointdb), 
                                  radius=local(expression(r), envir = pointdb))



# Y_crdnt <- sapply(abun_SR$plotID.x, function(x) 
#   plot_df$Y[plot_df$PlotID[x]])
df <- pointdb$query_radius_rect(x=322090, y=9635630, radius=200)
df <- pointdb$query_radius_rect(x=308285.0, y=9662407, radius=40)
