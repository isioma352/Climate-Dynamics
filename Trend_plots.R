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
setwd("C:/Users/inwayor/OneDrive - Indiana University/Past Semesters/5th semester/Climate Dynamics/Project/USAreal")
load("C:/Users/inwayor/OneDrive - Indiana University/Past Semesters/5th semester/Climate Dynamics/Project/USAreal/td_ea.RData")
load("C:/Users/inwayor/OneDrive - Indiana University/Past Semesters/5th semester/Climate Dynamics/Project/USAreal/td_es.RData")
load("C:/Users/inwayor/OneDrive - Indiana University/Past Semesters/5th semester/Climate Dynamics/Project/USAreal/td_vpd.RData")
load("C:/Users/inwayor/OneDrive - Indiana University/Past Semesters/5th semester/Climate Dynamics/Project/USAreal/td_sl.RData")


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

#writeOGR(usa.sp, ".", "usa_states", driver="ESRI Shapefile")


# load the shapefile
#usa.states <- readOGR(".", "usa_states")
dev.new()

trellis.par.set(background = list(col="black"))
cutpts <- seq(-0.6,1.2, 0.4)
plt1 <- levelplot(td_vpd*10 ~ lon * lat, 
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
                  main= list("VPD trend per decade", cex =1.5, col = 'white'),
                  aspect=0.7, cex.main = 1.0,
                  scales=list(x=list(at=seq(-125,-67, 5), cex = 0.8, col = 'white'),
                              y=list(at=seq(25,49,5)), cex =0.8, col = 'white'),
                  colorkey=list(labels=list(cex=1.0, seq(-0.6,1.2, 0.5), colr = 'white')))

plt1 + 
  latticeExtra::layer(sp.lines(usa.sp, col="black", lwd=1.5)) 

cutpts <- seq(-0.6,1.2, 0.4)
plt2 <- levelplot(td_es*10 ~ lon * lat, 
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
                  main= list("es trend per decade", cex =1.5, col = 'white'),
                  aspect=0.7, cex.main = 1.0,
                  scales=list(x=list(at=seq(-125,-67, 5), cex = 0.8, col = 'white'),
                              y=list(at=seq(25,49,5)), cex =0.8, col = 'white'),
                  colorkey=list(labels=list(cex=1.0, seq(-0.6,1.2, 0.4), col = 'white')))

plt2 + 
  latticeExtra::layer(sp.lines(usa.sp, col="black", lwd=1.5)) 

dev.new()
cutpts <- seq(-0.6,1.2, 0.4)
plt2 <- levelplot(td_ea*10 ~ lon * lat, 
                  par.settings=list(panel.background=list(col="black"),
                                    axis.line=list(lwd=3, col = 'white'), 
                                    strip.border=list(lwd=3)),
                  data=lonlatg, pretty=F,
                  at=cutpts,col.lab = 'white',
                  col.axis = 'white',
                  col.regions=rev((brewer.pal(9,"Blues"))),
                  ylim = c(25.12993,49.38323 ),
                  xlim = c( -124.6813,-67.00742),
                  xlab = "", ylab = "",
                  main= list("ea trend per decade", cex =1.5, col = 'white'),
                  aspect=0.7, cex.main = 1.0,
                  scales=list(x=list(at=seq(-125,-67, 5), cex = 0.8, col = 'white'),
                              y=list(at=seq(25,49,5)), cex =0.8, col = 'white'),
                  colorkey=list(labels=list(cex=1.0, seq(-0.6,1.2, 0.4), col = 'white')))

plt2 + 
  latticeExtra::layer(sp.lines(usa.sp, col="black", lwd=1.5)) 



K<-  latticeExtra::layer(sp.lines(usa.sp, col="black", lwd=1)) 
dev.new()
grid.arrange(plt1+K, plt3+K, plt2+K, nrow = 1)

