library(sp)
library(rgdal)
library(readr)
library(plot3D)
library(raster)
library(rgl)
library(plotly)
library(haven)


huayColordata <- read_sav("dataHuaycolor2.sav")
View(huayColordata)
#Function
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}
x<-huayColordata$longitude
y<-huayColordata$latitude
#LongLatToUTM(x,y,18)
datos<-LongLatToUTM(x,y,18)
x <- datos$X
y <- datos$Y
z <- huayColordata$altitudem

r<-cbind.data.frame(x,y,z)
str(r)

#create the extent and the raster
x<-huayColordata$longitude
y<-huayColordata$latitude
z <- huayColordata$altitudem
r3<-cbind.data.frame(x,y,z)
e2 <- extent(r3[,(1:2)])
r4 <- raster(e2, ncol=188, nrow=110, crs= "+proj=longlat +datum=WGS84")

#the data have about, 270000 cells. that is how the number of ncol and nrow specified. 
r_new2<- rasterize(r3[,1:2], r4, r3[,3], fun=max)
as.matrix(r3)->dem
dim(dem)
fig <- plot_ly(z = ~dem)
fig <- fig %>% add_surface()
fig



#create a custom pallate
rgb.pal <- colorRampPalette(c("dark blue","green","light green","green","dark green", "yellow","red"), space = "rgb", interpolate="linear",alpha=FALSE)

#png(paste("raster",".png",sep=""))
plot(r_new2, col=rgb.pal(100), xlab="Lon", ylab="Lat", main=paste("Horizontal"),
     legend.args=list(text=paste0("elevation"), side=4, font=1, line=3, cex=2))
dev.off()




#help('raster')

#using rgl
plot3d(x = x, y = y, z = z)

#create custom color for rgl plot
myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}
# z as the fourth variable 
col <- myColorRamp(c("blue", "green","yellow", "orange", "red"), z)
plot3d(x = x, y = y, z = z, col=col)
