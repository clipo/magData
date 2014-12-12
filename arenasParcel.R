library(sp)
library(classInt)
library(lattice)
library(raster)
library(rasterVis)
library(colorspace)
library(akima)
library(tcltk)
library(rpanel)
library(outliers)
library(akima)

install.packages("rgeos", repos="http://R-Forge.R-project.org")
install.packages('rgdal', type='source') 
# dont forget to "brew install gdal --HEAD"
# Download and install GDAL OS X install from kyngchaos
#  http://www.kyngchaos.com/software/frameworks)
# export PATH=”/Library/Frameworks/GDAL.framework/Programs:$PATH”

#2. Download and install proj4 from source
#- http://trac.osgeo.org/proj/wiki/WikiStart#Download
#- Download source code version proj-4.7.0.tar.gz
#> cd ~/Downloads/
#> tar -xzvf proj-4.7.0.tar.gz
#> cd proj-4.7.0
#> ./configure
#> make && make test
#> sudo make install
#[ should install to /usr/local/lib by default]
# download cran.r-project.org/src/contrib/rgdal_0.9-1.tar.gz
# cd ~/Downloads/
#sudo R CMD INSTALL –configure-args=’–with-proj-include=/usr/local/lib’ rgdal_0.9-1.tar.gz

setwd("/Volumes/Macintosh HD/Users/clipo/Dropbox/NAGPRA (2)/Arenas Parcel Project (Palm Springs)/GIS/")
data.path <- "FilteredMagData.csv" 

showPal <- function(pal, labs=pal, cex=0.6, ...){
  barplot(rep(1, length(pal)), col=pal,
          names.arg=labs, cex.names=cex,
          axes=FALSE, ...)
}
divPAL=brewer.pal(11,"RdYlBu")[11:1]

break2pal <- function(x, mx, pal){
  ## x = mx gives y = 1
  ## x = 0 gives y = 0.5
  y <- 1/2*(x/mx + 1)
  rgb(pal(y), maxColorValue=255)
}


trimData <- function(x) {
  limits<-quantile(x[,3],  c(0.2,0.8))
  #This finds the location of the outlier by finding that "True" value within the "outlier_tf" array. 
  data_new <- x[which(x[,3]>limits[1] & x[,3]<limits[2]),c(1,2,3)]
  #This creates a new dataset based on the old data, removing the one row that contains the outlier 
  data_new
}

# We read the whole file
mag <- read.table(data.path, header = TRUE, sep=",")
magGradiometryData <- mag[c(1,2,5)]
magTopSensorData <- mag[c(1,2,6)]
magBottomSensorData <- mag[c(1,2,7)]

mdensity <- density(magGradiometryData[,3])
plot(mdensity)
polygon(mdensity, col="red")

trimGradiometryData <- trimData(magGradiometryData)
trimTopData <- trimData(magTopSensorData)
trimBottomData <- trimData(magBottomSensorData)

tmdensity <- density(trimGradiometryData[,3])
plot(tmdensity)
polygon(tmdensity, col="red")

class_grad_e=classIntervals(trimGradiometryData[,3],30,style="equal")
class_grad_q=classIntervals(trimGradiometryData[,3],30,style="quantile")
class_grad_sd=classIntervals(trimGradiometryData[,3],30,style="sd")

class_top_grad_e=classIntervals(trimTopData[,3],30,style="equal")
class_top_grad_q=classIntervals(trimTopData[,3],30,style="quantile")
class_top_grad_sd=classIntervals(trimTopData[,3],30,style="sd")

class_bottom_grad_e=classIntervals(trimBottomData[,3],30,style="equal")
class_bottom_grad_q=classIntervals(trimBottomData[,3],30,style="quantile")
class_bottom_grad_sd=classIntervals(trimBottomData[,3],30,style="sd")

#class_hc=classIntervals(trimMagData[,3],30,style="hclust")
#class_f=classIntervals(trimMagData[,3],30,style="fisher")
pal1 <- c("wheat1", "red3")
plotclr=colorRampPalette(brewer.pal(7,"RdYlBu")[7:1] )(200)
plot(class_grad_sd, pal=pal1, main="sd")

myTheme=rasterTheme(region=sequential_hcl(10, power=2.2))
mapdata <- trimGradiometryData[-1,c(1,2,3)]
xmn=min(trimGradiometryData[,1]); xmx=max(trimGradiometryData[,1])
ymn=min(trimGradiometryData[,2]); ymx=max(trimGradiometryData[,2])

r <- raster(nrows=100, ncols=100, 
            xmn=xmn, xmx=xmx, 
            ymn=ymn, ymx=ymx )
grad_ras_raw <- rasterize(trimGradiometryData[,1:2], r, field = trimGradiometryData[,3])
top_ras_raw <- rasterize(trimTopData[,1:2], r, field = trimTopData[,3])
bottom_ras_raw <- rasterize(trimBottomData[,1:2], r, field = trimBottomData[,3])
# make a plot of the data

##########################
plot(grad_ras_raw, main="Gradiometry")
akima.li <- interp(trimGradiometryData[,1], trimGradiometryData[,2], trimGradiometryData[,3], duplicate = "median",
                   xo=seq(xmn,xmx, length=150),
                   yo=seq(ymn,ymx, length=150))
# plot data collection points
colnames(trimGradiometryData)<-c("East","North","nT")
#plot(trimGradiometryData[,2] ~ trimGradiometryData[,1], newdata = trimGradiometryData[,3], pch = 3, cex = 1,
#     xlab = "East", ylab = "North")
# plot interpolated raster over the top
image(akima.li, add=TRUE, col = terrain.colors(100, alpha = 1))

##########################
plot(top_ras_raw, main="Top Sensor")
akima.li <- interp(trimTopData[,1], trimTopData[,2], trimTopData[,3], duplicate = "median",
                   xo=seq(xmn,xmx, length=150),
                   yo=seq(ymn,ymx, length=150))
colnames(trimTopData)<-c("East","North","nT")
#plot(trimTopData[,2] ~ trimTopData[,1], data = trimTopData[,3], pch = 3, cex = 0.5,
#     xlab = "East", ylab = "North")
image(akima.li, add=TRUE,  col = terrain.colors(100, alpha = 1), useRaster=TRUE)

##########################
plot(bottom_ras_raw, main="Bottom Sensor")
akima.li <- interp(trimBottomData[,1], trimBottomData[,2], trimBottomData[,3], duplicate = "median",
                   xo=seq(xmn,xmx, length=150),
                   yo=seq(ymn,ymx, length=150))
colnames(trimBottomData)<-c("East","North","nT")
#plot(trimBottomData[,2] ~ trimBottomData[,1], data = trimBottomData[,3], pch = 3, cex = 0.5,
#     xlab = "East", ylab = "North")
image(akima.li, add=TRUE, col = terrain.colors(100, alpha = 1), useRaster=TRUE)
##########################


#levelplot(mag, par.settings=myTheme, contour=TRUE)
library(RColorBrewer)
plotclr=colorRampPalette(brewer.pal(7,"RdYlBu")[7:1] )(200)
colcode=findColours(class_grad_sd, plotclr)  
plot(grad_ras_raw,col=colcode, main="Gradiometry")

colcode=findColours(class_top_sd, plotclr)  
plot(top_ras_raw,col=colcode, main="Top Sensor")

colcode=findColours(class_grad_sd, plotclr)  
plot(bottom_ras_raw,col=colcode, main="Bottom Sensor")

plotclr <- rev(brewer.pal(11, "Spectral"))
divPAL <-brewer.pal(n=11,'PuOr')
colcode=findColours(class_grad_sd, divPAL)  
plot(grad_ras_raw,col=colcode)


# plot as interpolated raster






akima.li <- interp(trimBottomData[,1], trimBottomData[,2], trimBottomData[,3], duplicate = "median",
                   xo=seq(xmn,xmx, length=150),
                   yo=seq(ymn,ymx, length=150))
# plot data collection points
colnames(trimBottomData)<-c("East","North","nT")
plot(trimBottomData[,2] ~ trimBottomData[,1], data = trimBottomData[,3], pch = 3, cex = 0.5,
     xlab = "East", ylab = "North")
# plot interpolated raster over the top
image(akima.li, add=TRUE, main="Bottom Sensor", col = terrain.colors(100, alpha = 1), useRaster=TRUE, xlab="Easting", ylab="Northing")
# plot interpolated contour over the top
#contour(akima.li, add=TRUE)
# plot data collection points over the top
#points(data1[,1], data1[,2], pch = 3, cex = 0.25)



density.draw <- function(panel) {
  par(mfrow = c(2, 1))
  limits<-quantile(magData[,3],  c(panel$low,panel$high)) 
  selectdata <- magData[which(magData[,3]>limits[1] & magData[,3]<limits[2]),c(1,2,3)]
  ras_raw <- rasterize(selectdata[,1:2], r, field = selectdata[,3])
  # make a plot of the data
  plot(ras_raw)
  #par(mfrow=c(1,2))
  #plot(magData)
  #abline(v=limits[1])
  #abline(v=limits[2])
  tmdensity <- density(selectdata[,3])
  plot(tmdensity)
  polygon(tmdensity, col="red")
  #hist(selectdata[,3],breaks=50)
  panel
}

reset <- function(panel) {
  panel$low <- 0
  panel$high <-1 
  density.draw(panel)
}
panel<-rp.control("Minimum Value",low=0,high=1, size=c(600,450))
rp.slider(panel, low, 0,1, action=density.draw, resolution=0.05)
rp.slider(panel, high, 0,1, action=density.draw, resolution=0.05)
rp.button(panel, action = reset, title = "reset")

x11(width=4,height=4)
qq.draw <- function(panel) {
  z <- bc.fn(panel$y, panel$lambda)
  qqnorm(z, main = paste("lambda =",
                         round(panel$lambda, 2)))
  panel
}
panel <- rp.control(y = exp(rnorm(50)), lambda = 1)
rp.slider(panel, lambda, -2, 2, qq.draw,
          showvalue = TRUE)



divPAL <-brewer.pal(n=11,'Spectral')
divPAL[6]<-"#FFFFFF"
showPal(divPAL)
divTheme <- rasterTheme(region=divPAL)
levelplot(ras_raw, contour=TRUE, par.settings=divTheme)

grid <- expand.grid(x=mag[,1], y=mag[,2])

levelplot(mag[,3]~mag[,1]*mag[,2], grid, cuts = 50, scales=list(log="e"), xlab="",
          ylab="", main="Weird Function", sub="with log scales",
          colorkey = FALSE, region = TRUE)

