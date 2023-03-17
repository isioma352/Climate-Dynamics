
library(ncdf4)
setwd("/geode2/home/u040/inwayor/Carbonate/R/Climate_Dynamics/USA")
dwpt_data<- nc_open("/geode2/home/u040/inwayor/Carbonate/R/Climate_Dynamics/dwptUS.nc")
tmp_data <- nc_open("/geode2/home/u040/inwayor/Carbonate/R/Climate_Dynamics/tmpUS.nc")
solar_data<- nc_open("/geode2/home/u040/inwayor/Carbonate/R/Climate_Dynamics/solarUS.nc")



lon <- ncvar_get(tmp_data,"longitude")
lat <- ncvar_get(tmp_data,"latitude")
time <- ncvar_get(tmp_data,"time")
time2 <- array(seq(1981+1/24, 2020+23/24, 1/12))

# create dimension

nlat <- dim(lat)
nlon <- dim(lon)
nt <- dim(time2)


# initialize 3D matrix
es_mat <- array(data = NA, dim=c(nlon,nlat,nt))
ea_mat <- array(data = NA, dim=c(nlon,nlat,nt))
vpd1_mat <- array(data = NA, dim=c(nlon,nlat,nt))
solar1_mat <- array(data = NA, dim=c(nlon,nlat,nt))

for (i in 1:nlon) { 
  for (j in 1:nlat) {
    tmp_vec <- ncvar_get(tmp_data, 't2m', start = c(i,j,1), count = c(1, 1, 480))
    dwpt_vec <- ncvar_get(dwpt_data, 'd2m', start = c(i,j,1), count = c(1, 1, 480))
    solar_vec <- ncvar_get(solar_data, 'ssrd', start = c(i,j,1), count = c(1, 1, 480))
    
    #convert data from kelvin to celsius
    tmp_c <- tmp_vec - 273.15
    dwpt_c <- dwpt_vec - 273.15
    
    #convert to mega joules 
    solar_MJ <- solar_vec / 1000000
    
    es_vec <- 6.112 * exp((17.67 * tmp_c) / (tmp_c + 243.5))
    ea_vec <- 6.112 * exp((17.67 * dwpt_c) / (dwpt_c + 243.5))
    vpd_vec <- es_vec - ea_vec
    vpd_vec[vpd_vec < 0] <- 0
    
    solar1_mat[i, j,] <- solar_MJ
    es_mat[i, j,] <- es_vec
    ea_mat[i, j,] <- ea_vec
    vpd1_mat[i, j,] <- vpd_vec
    
  }
}

save(es_mat, file = "esUS.RData")
save(ea_mat, file = "eaUS.RData")
save(vpd1_mat, file = "vpdUS.RData")
save(solar1_mat, file = "solarUS.RData")

