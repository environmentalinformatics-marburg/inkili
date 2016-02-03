library(rPointDB)
library(rgl)

?PointDB

pointdb <- PointDB$new("http://137.248.191.249:8081/pointdb")

normalise <- "origin,ground,extremes"
#normalise <- "origin,extremes"
normalise <- "extremes"
pos=c(313600, 9640980)#land

#pos=c(335400, 9642900)#forest

df <- pointdb$query_radius_rect(x=pos[1], y=pos[2], radius=100, columns="x,y,z", normalise=normalise)

df <- df[df$z>1,]
#plot3d(df$x, df$y, df$z, size=1, type="p", zlim=c(min(df$z),min(df$z)+200))
plot3d(df$x, df$y, df$z, size=1, type="p", zlim=c(0, 200))

plot(density(df$z))
#plot(hist(df$z))
