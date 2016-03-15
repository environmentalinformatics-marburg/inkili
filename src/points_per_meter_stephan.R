library(rPointDB)

pointdb <- PointDB$new("http://137.248.191.249:8081/pointdb")
#pointdb <- PointDB$new("http://192.168.191.183:8081/pointdb")

#pos <- c(327110, 9626290) #kili grassland with few trees
pos <-c(335390, 9642930) #kili forest

dx <- 50
dy <- 50

df <- pointdb$query_rect(x1=pos[1], y1=pos[2], x2=pos[1]+dx, y2=pos[2]+dy, filter="last_return=1") #only one return per pulse
#df <- pointdb$query_rect(x1=pos[1], y1=pos[2], x2=pos[1]+dx, y2=pos[2]+dy, filter="return=1") # should be roughly same point count
df <- pointdb$query_radius_rect( x = 335390, y =  9642930, radius = 25, filter = "last_return=1")
points_per_meter <- length(df[[1]]) / (dx*dy)
points_per_meter
