library(ncdf4)
setwd("/geode2/home/u040/inwayor/Carbonate/R/Climate_Dynamics/USA")
load("/geode2/home/u040/inwayor/Carbonate/R/Climate_Dynamics/USA/eaUS.RData")
load("/geode2/home/u040/inwayor/Carbonate/R/Climate_Dynamics/USA/esUS.RData")
load("/geode2/home/u040/inwayor/Carbonate/R/Climate_Dynamics/USA/solarUS.RData")
load("/geode2/home/u040/inwayor/Carbonate/R/Climate_Dynamics/USA/vpdUS.RData")


dwpt_data <- nc_open("/geode2/home/u040/inwayor/Carbonate/R/Climate_Dynamics/dwpt.nc")

# lon <- ncvar_get(dwpt_data,"longitude")
# lat <- ncvar_get(dwpt_data,"latitude")
# time <- ncvar_get(dwpt_data,"time")
# time2 <- array(seq(1981+1/24, 2020+23/24, 1/12))


nlat <- 251
nlon <- 651
nt <- 480

cor_es_sl <- array(data = NA, dim=c(nlon,nlat))
cor_ea_sl <- array(data = NA, dim=c(nlon,nlat))
cor_vpd_sl<- array(data = NA, dim=c(nlon,nlat))


for (i in 1:nlon) { 
  for (j in 1:nlat) {
    es_dat <-  es_mat[i,j,]
    ea_dat <-  ea_mat[i,j,]
    vpd_dat <-   vpd1_mat[i,j,]
    solar_dat <-   solar1_mat[i,j,]
    es_miss <- sum(is.na(es_dat))  # find missing values
    ea_miss <- sum(is.na(ea_dat)) 
    vpd_miss <- sum(is.na(vpd_dat)) 
    solar_miss <- sum(is.na(solar_dat)) 
    if (es_miss< .3*nt) { # This could go in the method
      if (ea_miss < .3*nt) {
        if (vpd_miss < .3*nt){
          if (solar_miss < .3*nt){
            cor_es_sl[i,j] <- cor(es_dat, solar_dat, use = "complete.obs")
            cor_ea_sl[i,j] <- cor(ea_dat, solar_dat, use = "complete.obs")
            cor_vpd_sl[i,j] <- cor(vpd_dat, solar_dat, use = "complete.obs")
          }
        }
      }
    }
  }
}


save(cor_es_sl, file = "cor_es_sl.RData")
save(cor_ea_sl, file = "cor_ea_sl.RData")
save(cor_vpd_sl, file = "cor_vpd_sl.RData")



