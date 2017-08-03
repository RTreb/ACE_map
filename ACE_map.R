# Circumpolar map centred on East Antarctica for ACE work

library(raadtools)
library(graticule)
library(raster)
library(rworldxtra)
data(countriesHigh)
# library(rworldmap)
# data(countriesHigh)
library(sp)
library(rgeos)
library(rgdal)
library(dplyr)

# projection - as a general starting point - we will define the extent later using the bathymetry layer as an example
prj<- "+proj=laea +lat_0=-90 +lon_0=75 +datum=WGS84 +ellps=WGS84 +no_defs +towgs84=0,0,0"

# bathy
bathy<- readtopo("etopo2")
# initial crop to just limit to sthrn hemisphere
crop_ext <- raster(xmn=-180, xmx=180, ymn=-90, ymx=0, crs = projection(bathy))
pbathy <- projectExtent(crop_ext, prj)
res(pbathy) <- 2e4
pbathy <- projectRaster(bathy, pbathy)

# this is where we can set up the extent
# un-comment
plot(pbathy)
# run locator and draw 2 corners for an extent (you get 2 clicks, to draw diagonally opposite corners)
ext <- drawExtent()
# happy with this? if not, reiterate the previous 2 lines (plot, then drawExtent)
plot(crop(pbathy, ext))
# if happy, lock it in (skip the next line, or update it to the new extent)
# here's one I prepared earlier
ext <- extent(-4778600, 5760700,  -2637660,6304900 )
# over-write old bathy with cropped version
pbathy <- crop(pbathy, ext)

# filled bathy
add_bathy <- function()  {
  brks <- c(-8000,-7000,-6000,-5000,-4000,-3000,-2000,-1000,-500,0)
  plotcols <- gray.colors(9, alpha=0.6)
  image(as(pbathy, "SpatialGridDataFrame"), col = plotcols, alpha=0.005,breaks = brks,add=TRUE,useRaster = TRUE) # plot.raster overtakes par() control
}

# bathy contours
bc <- rasterToContour(pbathy, levels = c(-500,-1000,-2000,-3000, -6000))

# cropped and full versions of world map
# cropped version is used to make a suitable extent for plotting, full is over-plotted so that the continents don't  show the "wedge" defined by the extent

w <-spTransform(countriesHigh, CRS(prj))
w <- crop(w,ext)

# fronts
library(orsifronts)
of <- spTransform(orsifronts, CRS(prj))
of <- crop(of, ext)

# # K-Axis stations
ks <- readRDS("k_axis_oceanog_summ.Rda")
coordinates(ks) <- c("longitude", "latidue")
projection(ks) <- "+proj=longlat +datum=WGS84"
ks <- spTransform(ks, CRS(prj))

# graticule
xx <- seq(-180,180,30); yy <- c(-90,-80, -70,-60, -50,-40,-30,-20)
grat <- graticule(lons = xx, lats = yy,  proj = prj)
grat <- crop(grat, ext)


# TODO:
# need to fix up graticule labels
# g1labs <- graticule_labels(lons=c(150, 120,90,60), ,xline=180, yline=-50, proj=projection(prj))
# 
# text(coordinates(g1labs[g1labs$islon, ]), lab=parse(text=g1labs$lab[g1labs$islon]), pos=3, cex=0.8, col="gray30")
# 
# g2labs <- graticule_labels(lats=c(-40, -50,-60,-70), , xline=60, yline=-50, proj=projection(prj))
# 
# text(coordinates(g2labs[!g2labs$islon, ]), lab=parse(text=g2labs$lab[!g2labs$islon]), pos=1, cex=0.8,col="gray30")



pdf("ACE_map.pdf", width=9, height=9)
op<- par(mar=rep(0,4), oma=rep(0.5,4))
#plot(ext, type="n", axes=F)
plot(w,col="darkgrey", border=F)
add_bathy()
plot(bc, col="dark grey", add=T) # bathy contours
# graticules
plot(grat, add=T, col="gray40", lty=2)

lines(of, col="blue")

# k-axis stations?
lines(ks$longitude, ks$latidue)
points(ks, pch=19, col="gray20", cex=0.7)

par(op)
dev.off()




### some other stuff you can add by moving above the plotting code and adding


# atlantis domain
atlantis24 <- readOGR("e_antarctica24.shp")
atlantis24 <- subset(atlantis24, boundary==0)
atlantis24 <- spTransform(atlantis24, CRS(prj))
atl24.col  <- rgb(147,112,219,100, maxColorValue = 255)
# plot with
# plot(atlantis24, add=T, border="orange", lwd=1)

atlantis99 <- readOGR("e_antarctica99.shp")
atlantis99 <- subset(atlantis99, boundary==0)
atlantis99 <- spTransform(atlantis99, CRS(prj))
# plot with
# plot(atlantis99, add=T, border="blue", lwd=1)

# other model domains
m1e <- graticule(c(60, 90), c(-70, -60), proj = "+proj=laea +lat_0=-90 +datum=WGS84")
m1b <- spTransform(m1e, CRS(prj)) 
m2e <- graticule(c(60, 90), c(-60, -50), proj = "+proj=laea +lat_0=-90 +datum=WGS84")
m2b <- spTransform(m2e, CRS(prj)) 
m1col<- rgb(55,126,184,80, maxColorValue = 255)
m2col <-rgb(77,175,74,80,maxColorValue = 255)


icejan <- readice("2016-01-15")
pij<- crop(projectRaster(icejan, pbathy), ext)
pij<- rasterToContour(pij, lev = 15)
# to plot
# plot(pij,add = TRUE, lty=1, col="#6BAED6", lwd=1.5)






