library(sp)
library(classInt)
library(lattice)
library(raster)
library(rasterVis)
library(colorspace)
library(akima)
library(tcltk)
library(rpanel)

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

setwd("/Volumes/Macintosh HD/Users/clipo/Dropbox/NAGPRA (2)/Reburial Project/MagData")
data.path <- "attachment.ashx.dat" 

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

library(outliers)
trimData <- function(x) {
  limits<-quantile(x[,3],  c(0.1,0.9))
  #This finds the location of the outlier by finding that "True" value within the "outlier_tf" array. 
  data_new <- x[which(x[,3]>limits[1] & x[,3]<limits[2]),c(1,2,3)]
  #This creates a new dataset based on the old data, removing the one row that contains the outlier 
  data_new
}

# We read the whole file
mag <- read.table(data.path, header = FALSE, sep="\t")
magData <- mag[c(1,2,6)]
mdensity <- density(magData[,3])
plot(mdensity)
polygon(mdensity, col="red")

trimMagData <- trimData(magData)
tmdensity <- density(trimMagData[,3])
plot(tmdensity)
polygon(tmdensity, col="red")

class_e=classIntervals(trimMagData[,3],30,style="equal")
class_q=classIntervals(trimMagData[,3],30,style="quantile")
class_sd=classIntervals(trimMagData[,3],30,style="sd")
class_km=classIntervals(trimMagData[,3],30,style="kmeans")
#class_hc=classIntervals(trimMagData[,3],30,style="hclust")
#class_f=classIntervals(trimMagData[,3],30,style="fisher")
pal1 <- c("wheat1", "red3")
plotclr=colorRampPalette(brewer.pal(7,"RdYlBu")[7:1] )(200)
plot(class_sd, pal=pal1, main="sd")

myTheme=rasterTheme(region=sequential_hcl(10, power=2.2))
mapdata <- trimMagData[-1,c(1,2,3)]
xmn=min(mapdata[,1]); xmx=max(mapdata[,1])
ymn=min(mapdata[,2]); ymx=max(mapdata[,2])

r <- raster(nrows=100, ncols=100, 
            xmn=xmn, xmx=xmx, 
            ymn=ymn, ymx=ymx )
ras_raw <- rasterize(mapdata[,1:2], r, field = mapdata[,3])

# make a plot of the data
plot(ras_raw)

#levelplot(mag, par.settings=myTheme, contour=TRUE)
library(RColorBrewer)
plotclr=colorRampPalette(brewer.pal(7,"RdYlBu")[7:1] )(200)
colcode=findColours(class_sd, plotclr)  
plot(ras_raw,col=colcode)

colcode=findColours(class_sd, plotclr)  
plot(ras_raw,col=colcode)

colcode=findColours(class_sd, plotclr)  
plot(ras_raw,col=colcode)
plotclr <- rev(brewer.pal(11, "Spectral"))
divPAL <-brewer.pal(n=11,'PuOr')
colcode=findColours(class_sd, divPAL)  
plot(ras_raw,col=colcode)



# plot as interpolated raster
library(akima)
akima.li <- interp(mapdata[,1], mapdata[,2], mapdata[,3], duplicate = "median",
                   xo=seq(xmn,xmx, length=150),
                   yo=seq(ymn,ymx, length=150))
# plot data collection points
colnames(mapdata)<-c("East","North","nT")
plot(mapdata[,2] ~ mapdata[,1], data = mapdata[,3], pch = 3, cex = 0.5,
     xlab = "East", ylab = "North")
# plot interpolated raster over the top
image(akima.li, add=TRUE, col = rainbow(100, alpha = 1))
# plot interpolated contour over the top
contour(akima.li, add=TRUE)
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

