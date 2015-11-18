library("ncdf4")
setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/ERA_Data/")

tmax.nc <- nc_open("tmax_Jane_1998-2012.nc")
tmax <- ncvar_get(tmax.nc, "tmax")
tmax[tmax == "-32767"] <- NA

lon <- tmax.nc$dim$longitude$vals
lat <- tmax.nc$dim$latitude$vals
lat <- rev(lat)
time <- tmax.nc$dim$time$vals
time <- time - 1
time
time <- ymd(time, tz = "Europe/Amsterdam")

# test <- tmax[,,1]
# image(lon.t, lat.t, test)

o3.nc <- nc_open("surfO3.mda8.Jane.set.1998-2012.nc")
lon.o3.ind <- which(o3.nc$dim$lon$vals >= 2 & o3.nc$dim$lon$vals <= 8)
o3.benelux <- ncvar_get(o3.nc, "MDA8_SurfO3")[,lon.o3.ind,]
o3.benelux[o3.benelux == "-999"] <- NA

nc_close(o3.nc)

lat.o3 <- rev(o3.nc$dim$lat$vals)
lon.o3 <- o3.nc$dim$lon$val
lon.o3 <- lon.o3[lon.o3.ind]
time.o3 <- o3.nc$dim$time$vals
time.o3 <- ymd(time.o3, tz = "Europe/Amsterdam")

# ozone
o3.dim.names <- list(lat.o3, lon.o3, time.o3)
dimnames(o3.benelux) <- o3.dim.names
o3.df <- o3.benelux %>%
  melt(na.rm = TRUE)
colnames(o3.df) <- c("Lat", "Lon", "Time", "O3")
tbl_df(o3.df)

# temperature
tmax.dim.names <- list(lon, lat, time)
dimnames(tmax) <- tmax.dim.names
tmax.df <- tmax %>%
  melt(na.rm = TRUE)
colnames(tmax.df) <- c("Lon", "Lat", "Time", "T.Max")
tbl_df(tmax.df)
