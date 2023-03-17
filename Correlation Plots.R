library(RColorBrewer)
library(sp)
library(sf)
library(lattice)
library(ncdf4)
library(extrafont)
library(gridExtra) # For creating multiple maps on one plot
library(agridat)
library(latticeExtra)
library(maps)
library(maptools)
library(sp)
library(raster)
library(rgdal)
setwd("C:/Users/inwayor/OneDrive - Indiana University/Past Semesters/5th semester/Climate Dynamics/Project/USAreal/RM")
load("C:/Users/inwayor/OneDrive - Indiana University/Past Semesters/5th semester/Climate Dynamics/Project/USAreal/RM/cor_ea_rm.RData")
load("C:/Users/inwayor/OneDrive - Indiana University/Past Semesters/5th semester/Climate Dynamics/Project/USAreal/RM/cor_es_rm.RData")
load("C:/Users/inwayor/OneDrive - Indiana University/Past Semesters/5th semester/Climate Dynamics/Project/USAreal/RM/cor_vpd_rm.RData")


dwpt <- nc_open("C:/Users/inwayor/OneDrive - Indiana University/Past Semesters/5th semester/Climate Dynamics/Project/USA_DT/DTUS.nc")
lon <- ncvar_get(dwpt,"longitude")
lat <- ncvar_get(dwpt,"latitude")
time <- ncvar_get(dwpt,"time")

dwpt <- ncvar_get(dwpt,'d2m')

yrs <- seq(1981+1/24, 2020+23/24, 1/12)

# create a variable
# create dimensions for lat, lon  and time

lonlatg <- expand.grid(lon=lon, lat=lat)
lonlatm <- as.matrix(lonlatg)



usa.map <- map("state", plot = FALSE, fill = TRUE, col = "transparent")

usa.sp <- map2SpatialPolygons(usa.map, IDs = usa.map$names, proj4string = CRS("+proj=longlat +datum=WGS84"))

writeOGR(usa.sp, ".", "usa_states", driver="ESRI Shapefile")


# load the shapefile
usa.states <- readOGR(".", "usa_states")
dev.new()

cutpts <- seq(-1,1, 0.25)
trellis.par.set(background = list(col="black"))


plt1 <- levelplot(cor_vpd_rm ~ lon * lat, 
                  par.settings=list(panel.background=list(col="black"),
                                    axis.line=list(lwd=3, col = 'white'), 
                                    strip.border=list(lwd=3)),
                  data=lonlatg, pretty=F,
                  at=cutpts,col.lab = 'white',
                  col.axis = 'white',
                  col.regions=rev((brewer.pal(9,"RdBu"))),
                  ylim = c(25.12993,49.38323 ),
                  xlim = c( -124.6813,-67.00742),
                  xlab = "", ylab = "",
                  main= list("Moving Correlation of VPD and SSRD ", cex =1.5, col = 'white'),
                  aspect=0.7, cex.main = 1.0,
                  scales=list(x=list(at=seq(-125,-67, 5), cex = 0.8, col = 'white'),
                              y=list(at=seq(25,49,5)), cex =0.8, col = 'white'),
                  colorkey=list(labels=list(cex=1.0, seq(-1,1, 0.25), col = 'white')))
plt1 + 
  latticeExtra::layer(sp.lines(usa.sp, col="black", lwd=1))


plt2 <- levelplot(cor_es_rm ~ lon * lat, 
                  par.settings=list(panel.background=list(col="black"),
                                    axis.line=list(lwd=3, col = 'white'), 
                                    strip.border=list(lwd=3)),
                  data=lonlatg, pretty=F,
                  at=cutpts,col.lab = 'white',
                  col.axis = 'white',
                  col.regions=rev((brewer.pal(9,"RdBu"))),
                  ylim = c(25.12993,49.38323 ),
                  xlim = c( -124.6813,-67.00742),
                  xlab = "", ylab = "",
                  main= list("Moving Correlation of es and SSRD ", cex =1.5, col = 'white'),
                  aspect=0.7, cex.main = 1.0,
                  scales=list(x=list(at=seq(-125,-67, 5), cex = 0.8, col = 'white'),
                              y=list(at=seq(25,49,5)), cex =0.8, col = 'white'),
                  colorkey=list(labels=list(cex=1.0, seq(-1,1, 0.25), col = 'white')))
plt2 + 
  latticeExtra::layer(sp.lines(usa.sp, col="black", lwd=1))


plt3 <- levelplot(cor_ea_rm ~ lon * lat, 
                  par.settings=list(panel.background=list(col="black"),
                                    axis.line=list(lwd=3, col = 'white'), 
                                    strip.border=list(lwd=3)),
                  data=lonlatg, pretty=F,
                  at=cutpts,col.lab = 'white',
                  col.axis = 'white',
                  col.regions=rev((brewer.pal(9,"RdBu"))),
                  ylim = c(25.12993,49.38323 ),
                  xlim = c( -124.6813,-67.00742),
                  xlab = "", ylab = "",
                  main= list("Moving Correlation of ea and SSRD ", cex =1.5, col = 'white'),
                  aspect=0.7, cex.main = 1.0,
                  scales=list(x=list(at=seq(-125,-67, 5), cex = 0.8, col = 'white'),
                              y=list(at=seq(25,49,5)), cex =0.8, col = 'white'),
                  colorkey=list(labels=list(cex=1.0, seq(-1,1, 0.25), col = 'white')))
plt3 + 
  latticeExtra::layer(sp.lines(usa.sp, col="black", lwd=1))





K<-  latticeExtra::layer(sp.lines(usa.sp, col="black", lwd=1)) 
dev.new()
grid.arrange(plt1+K, plt2+K, plt3+K, nrow = 1)


dev.new(width = 15, height = 3)

par(mfrow = c(1,1), bg = 'black')



# Check histogram

hist(cor_vpd_rm, main = " Moving correlation of VPD and SSRD",
     cex.lab = 1.5, cex.main = 2, cex.axis = 1.2,
     col.lab = 'white', col.main = 'white', col.axis = 'white',
     xlab = "correlation values", col = "red")
box(col = 'white')
hist(cor_es_rm, main = "Moving correlation of es and SSRD",
     cex.lab = 1.5,cex.main = 2,cex.axis = 1.2,
     col.lab = 'white', col.main = 'white', col.axis = 'white',
     xlab = "correlation values", col = "red")
box(col = 'white')
hist(cor_ea_rm, main = " Moving correlation of ea and SSRD",
     cex.lab = 1.5,cex.main = 2,cex.axis = 1.2,
     col.lab = 'white', col.main = 'white', col.axis = 'white',
     xlab = "correlation values", col = "red")
box(col = 'white')


