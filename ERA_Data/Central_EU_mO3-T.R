library(ncdf4)
setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/ERA_Data/")

lon.max <- 11
lon.min <- 0
lat.max <- 54
lat.min <- 48

tmax.nc <- nc_open("tmax_EU_1998-2012.nc")
tmax.lon.ind <- which(tmax.nc$dim$longitude$vals >= lon.min & tmax.nc$dim$longitude$vals <= lon.max)
tmax.lat.ind <- which(tmax.nc$dim$latitude$vals >= lat.min & tmax.nc$dim$latitude$vals <= lat.max)
tmax <- ncvar_get(tmax.nc, "tmax")[tmax.lon.ind, tmax.lat.ind,]
tmax[tmax == "-32767"] <- NA
nc_close(tmax.nc)

lon <- tmax.nc$dim$longitude$vals[tmax.lon.ind]
lat <- tmax.nc$dim$latitude$vals[tmax.lat.ind]
lat <- rev(lat)
time <- tmax.nc$dim$time$vals - 0.75
time <- parse_date_time(time,"%Y%m%d")
time
test <- tmax[,,1]
filled.contour(lon, lat, test)

o3.nc <- nc_open("surfO3.mda8.eu.set.1998-2012.nc")
o3.lon <- o3.nc$dim$lon$vals
subLon <- function (x) {
  if (x > 300) {
    x  <- x - 360
  }
  return (x)
}
o3.lon <- sapply(o3.lon, subLon)
o3.lon.ind <- which(o3.lon >= lon.min & o3.lon <= lon.max)
o3.lat.ind <- which(o3.nc$dim$lat$vals >= lat.min & o3.nc$dim$lat$vals <= lat.max)
o3 <- ncvar_get(o3.nc, "MDA8_SurfO3")[o3.lon.ind, o3.lat.ind,]
o3[o3 == "-999"] <- NA
nc_close(o3.nc)

lat.o3 <- o3.nc$dim$lat$vals[o3.lat.ind]
lon.o3 <- o3.nc$dim$lon$val[o3.lon.ind]
lat.o3
time.o3 <- o3.nc$dim$time$vals
time.o3 <- parse_date_time(time.o3, "%Y%m%d")
time.o3
test <- o3[,,1]
filled.contour(lon.o3, lat.o3, test)

# ozone
o3.dim.names <- list(lon.o3, lat.o3, time.o3)
dimnames(o3) <- o3.dim.names
o3.df <- o3 %>%
  melt(na.rm = TRUE)
colnames(o3.df) <- c("Lon", "Lat", "Time", "O3.Max")
o3.df <- o3.df %>%
  group_by(Time) %>%
  summarise(O3.Max = mean(O3.Max))
tbl_df(o3.df)

# temperature
tmax.dim.names <- list(lon, lat, time)
dimnames(tmax) <- tmax.dim.names
tmax.df <- tmax %>%
  melt(na.rm = TRUE)
colnames(tmax.df) <- c("Lon", "Lat", "Time", "T.Max")
tmax.df <- tmax.df %>%
  group_by(Time) %>%
  summarise(T.Max = mean(T.Max))
tbl_df(tmax.df)

combined.df <- cbind(o3.df, tmax.df$T.Max)
colnames(combined.df) <- c("Time", "O3", "Temperature")
combined.df$Temperature <- round(combined.df$Temperature, digits = 0)
tbl_df(combined.df)

# upper.quantile <- quantile(combined.df$O3, 0.95) # get 95th percentile
# lower.quantile <- quantile(combined.df$O3, 0.05) # get 5th percentile
# 
# no.outliers <- combined.df %>%
#   filter(O3 < upper.quantile & O3 > lower.quantile)
# tbl_df(no.outliers)


# entire data set
whole.data <- combined.df %>%
  select(-Time) %>%
  group_by(Temperature) %>%
  summarise(Mean.O3 = mean(O3, na.rm = TRUE))

wd.plot <- ggplot(whole.data, aes(x = Temperature, y = Mean.O3)) + geom_point() + stat_smooth(method = lm)
wd.plot
wd.model = lm(Mean.O3 ~ Temperature, data = whole.data)
wd.Slope = sprintf("%.1f", abs(summary(wd.model)$coeff[2]))
wd.R2 = summary(wd.model)$r.squared
wd.Slope
wd.R2

# factored by year
no.outliers$Time <- as.POSIXct(no.outliers$Time, origin = origin)
yearly.data <- no.outliers %>%
  rowwise() %>%
  mutate(Year = year(Time)) %>%
  select(-Time) %>%
  group_by(Year, Temperature) %>%
  filter(O3 <= quantile(O3, 0.95) & O3 >= quantile(O3, 0.05)) %>%
  summarise(Mean.O3 = mean(O3))

y.plot <- ggplot(yearly.data, aes(x = Temperature, y = Mean.O3)) + geom_point() + stat_smooth(method = lm) + facet_wrap(~ Year)
y.plot
y.slopes = yearly.data %>%  
  group_by(Year) %>%
  do(model = lm(Mean.O3 ~ Temperature, data = .)) %>%
  mutate(Slope = sprintf("%.1f", abs(summary(model)$coeff[2])), R2 = summary(model)$r.squared) %>%
  select(-model) 
y.slopes

getSeason <- function (month) {
  if (month %in% c(12, 1, 2)) {
    season = "DJF"
  } else if (month %in% c(3, 4, 5)) {
    season = "MAM"
  } else if (month %in% c(6, 7, 8)) {
    season = "JJA"
  } else if (month %in% c(9, 10, 11)) {
    season = "SON"
  }
  return (season)
}

#factored by season
season <- no.outliers %>%
  rowwise() %>%
  mutate(Season = getSeason(month(Time))) %>%
  select(-Time) %>%
  group_by(Season, Temperature) %>%
  filter(O3 <= quantile(O3, 0.95) & O3 >= quantile(O3, 0.05)) %>%
  summarise(Mean.O3 = mean(O3))

s.plot <- ggplot(season, aes(x = Temperature, y = Mean.O3)) + geom_point() + facet_grid(~ Season) + stat_smooth(method = lm)
s.plot
s.slopes <- season %>%
  group_by(Season) %>%
  do(model = lm(Mean.O3 ~ Temperature, data = .)) %>%
  mutate(Slope = sprintf("%.1f", abs(summary(model)$coeff[2])), R2 = summary(model)$r.squared) %>%
  select(-model)
s.slopes

# factored by year and season
season.year <- no.outliers %>%
  rowwise() %>%
  mutate(Year = year(Time), Season = getSeason(month(Time))) %>%
  select(-Time) %>%
  group_by(Year, Season, Temperature) %>%
  filter(O3 <= quantile(O3, 0.95) & O3 >= quantile(O3, 0.05)) %>%
  summarise(Mean.O3 = mean(O3))

sy.plot <- ggplot(season.year, aes(x = Temperature, y = Mean.O3)) + geom_point() + facet_grid(Season ~ Year) + stat_smooth(method = lm)
sy.plot
sy.slopes <- season.year %>%
  group_by(Year, Season) %>%
  do(model = lm(Mean.O3 ~ Temperature, data = .)) %>%
  mutate(Slope = sprintf("%.1f", abs(summary(model)$coeff[2])), R2 = summary(model)$r.squared) %>%
  select(-model)
sy.slopes
