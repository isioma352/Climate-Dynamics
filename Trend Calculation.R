library(ncdf4)
setwd("/geode2/home/u040/inwayor/Carbonate/R/Climate_Dynamics/USA")
load("/geode2/home/u040/inwayor/Carbonate/R/Climate_Dynamics/USA/eaUS.RData")
load("/geode2/home/u040/inwayor/Carbonate/R/Climate_Dynamics/USA/esUS.RData")
load("/geode2/home/u040/inwayor/Carbonate/R/Climate_Dynamics/USA/solarUS.RData")
load("/geode2/home/u040/inwayor/Carbonate/R/Climate_Dynamics/USA/vpdUS.RData")


dwpt_data <- nc_open("/geode2/home/u040/inwayor/Carbonate/R/Climate_Dynamics/dwpt.nc")
yrs <- seq(1981+1/24, 2020+23/24, 1/12)

nlat <- 251
nlon <- 651
nt <- 480

# Do initialization for moving SD and trend 
td_es <- array(data = NA, dim=c(nlon,nlat))
td_ea <- array(data = NA, dim=c(nlon,nlat))
td_vpd<- array(data = NA, dim=c(nlon,nlat))
td_sl<- array(data = NA, dim=c(nlon,nlat))

#Create loop for moving SD and trend

for (i in 1:nlon) { 
  for (j in 1:nlat) {
    ea_miss <- sum(is.na(ea_mat[i,j,]))
    es_miss <- sum(is.na(es_mat[i,j,]))
    vpd_miss <- sum(is.na(vpd1_mat[i,j,]))
    sl_miss <- sum(is.na(solar1_mat[i,j,]))
    if  (ea_miss < 0.2*nt)  {
      if (es_miss < 0.2*nt) {
        if (vpd_miss < 0.2*nt) {
          if (sl_miss < 0.2*nt) {
      
      #Trend 1971 to 2020
      out_ea <- lsfit(yrs, ea_mat[i,j,])
      td_ea[i,j] <- (out_ea$coefficients[2])
      
      out_es <- lsfit(yrs, es_mat[i,j,])
      td_es[i,j] <- (out_es$coefficients[2])
      
      out_vpd <- lsfit(yrs, vpd1_mat[i,j,])
      td_vpd[i,j] <- (out_vpd$coefficients[2])
      
      out_sl <- lsfit(yrs, solar1_mat[i,j,])
      td_sl[i,j] <- (out_sl$coefficients[2])
      
          }
        }
      }
    }
  }
}


save(td_ea, file = "td_ea.RData")
save(td_es, file = "td_es.RData")
save(td_vpd, file = "td_vpd.RData")
save(td_sl, file = "td_sl.RData")

