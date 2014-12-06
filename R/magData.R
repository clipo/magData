library(sp)
library(classInt)
library(raster)
library(rasterVis)
library(colorspace)
library(akima)
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
  limits<-quantile(x,  c(0.1,0.9))
  #This finds the location of the outlier by finding that "True" value within the "outlier_tf" array. 
  data_new <- x[which(x>limits[1] & x<limits[2])]
  #This creates a new dataset based on the old data, removing the one row that contains the outlier 
  data_new
}

# We read the whole file
mag <- read.table(data.path, header = FALSE, sep="\t")
magData <- mag[,6]
mdensity <- density(magData)
plot(mdensity)
polygon(mdensity, col="red")

trimMagData <- trimData(magData)
tmdensity <- density(trimMagData)
plot(tmdensity)
polygon(tmdensity, col="red")

class_e=classIntervals(mag[,6],30,style="equal")
class_q=classIntervals(mag[,6],30,style="quantile")
class_sd=classIntervals(mag[,6],30,style="sd")
class_km=classIntervals(mag[,6],30,style="kmeans")
class_hc=classIntervals(mag[,6],30,style="hclust")
class_f=classIntervals(mag[,6],30,style="fisher")
pal1 <- c("wheat1", "red3")
plotclr=colorRampPalette(brewer.pal(7,"RdYlBu")[7:1] )(200)
plot(class_km, pal=pal1, main="fishers")

myTheme=rasterTheme(region=sequential_hcl(10, power=2.2))
data1 <- mag[-1,c(1,2,6)]
xmn=min(data1[,1]); xmx=max(data1[,1])
ymn=min(data1[,2]); ymx=max(data1[,2])

r <- raster(nrows=100, ncols=100, 
            xmn=xmn, xmx=xmx, 
            ymn=ymn, ymx=ymx )
ras_raw <- rasterize(data1[,1:2], r, field = trimMagData)

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
akima.li <- interp(data1[,1], data1[,2], data1[,3], duplicate = "median",
                   xo=seq(xmn,xmx, length=150),
                   yo=seq(ymn,ymx, length=150))
# plot data collection points
plot(data1[,2] ~ data1[,1], data = data1, pch = 3, cex = 0.5,
     xlab = "Easting", ylab = "Northing")
# plot interpolated raster over the top
image(akima.li, add=TRUE, col = rainbow(100, alpha = 1))
# plot interpolated contour over the top
contour(akima.li, add=TRUE)
# plot data collection points over the top
points(data1[,1], data1[,2], pch = 3, cex = 0.25)

divPAL <-brewer.pal(n=11,'Spectral')
divPAL[6]<-"#FFFFFF"
showPal(divPAL)
divTheme <- rasterTheme(region=divPAL)
levelplot(ras_raw, contour=TRUE, par.settings=divTheme)

grid <- expand.grid(x=mag[,1], y=mag[,2])

levelplot(mag[,3]~mag[,1]*mag[,2], grid, cuts = 50, scales=list(log="e"), xlab="",
          ylab="", main="Weird Function", sub="with log scales",
          colorkey = FALSE, region = TRUE)
