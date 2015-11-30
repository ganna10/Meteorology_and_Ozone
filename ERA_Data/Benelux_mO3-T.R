library("ncdf4")
setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/ERA_Data/")

tmax.nc <- nc_open("tmax_NW_EU_1998-2012.Benelux.nc")
tmax <- ncvar_get(tmax.nc, "tmax")
tmax[tmax == "-32767"] <- NA
nc_close(tmax.nc)

lon <- tmax.nc$dim$longitude$vals
lat <- tmax.nc$dim$latitude$vals
lat <- rev(lat)
time <- tmax.nc$dim$time$vals
time <- parse_date_time(time, "%Y%m%d", tz = "Europe/Amsterdam")
# test <- tmax[,,1]
# image(lon.t, lat.t, test)

o3.nc <- nc_open("surfO3.mda8.NW.EU.set.1998-2012.Benelux.nc")
o3.benelux <- ncvar_get(o3.nc, "MDA8_SurfO3")
o3.benelux[o3.benelux == "-999"] <- NA
nc_close(o3.nc)

lat.o3 <- rev(o3.nc$dim$lat$vals)
lon.o3 <- o3.nc$dim$lon$val
time.o3 <- o3.nc$dim$time$vals
time.o3 <- parse_date_time(time.o3, "%Y%m%d", tz = "Europe/Amsterdam")

# ozone
o3.dim.names <- list(lat.o3, lon.o3, time.o3)
dimnames(o3.benelux) <- o3.dim.names
o3.df <- o3.benelux %>%
  melt(na.rm = TRUE)
colnames(o3.df) <- c("Lat", "Lon", "Time", "O3.Max")
# o3.df <- o3.df %>%
#   group_by(Time) %>%
#   summarise(O3.Max = mean(O3.Max))
o3.df <- o3.df %>%
  arrange(Lon, Lat, Time)
tbl_df(o3.df)

# temperature
tmax.dim.names <- list(lon, lat, time)
dimnames(tmax) <- tmax.dim.names
tmax.df <- tmax %>%
  melt(na.rm = TRUE)
colnames(tmax.df) <- c("Lon", "Lat", "Time", "T.Max")
# tmax.df <- tmax.df %>%
#   group_by(Time) %>%
#   summarise(T.Max = mean(T.Max))
tmax.df <- tmax.df %>%
  arrange(Lon, Lat, Time)
tbl_df(tmax.df)

combined.df <- data.frame(O3 = o3.df$O3.Max, T.Max = tmax.df$T.Max)
tbl_df(combined.df)

upper.quantile <- quantile(combined.df$O3, 0.95, names = FALSE) # get 95th percentile
lower.quantile <- quantile(combined.df$O3, 0.05, names = FALSE) # get 5th percentile

no.outliers <- combined.df %>%
  filter(between(O3, lower.quantile, upper.quantile))
tbl_df(no.outliers)

# no.outliers$Temperature <- round(no.outliers$Temperature, digits = 0)

# entire data set
# whole.data <- no.outliers %>%
#   select(-Time) %>%
#   group_by(Temperature) %>%
#   summarise(Mean.O3 = mean(O3))

wd.plot <- ggplot(no.outliers, aes(x = T.Max, y = O3)) + geom_point() + stat_smooth(method = lm)
wd.plot
wd.model = lm(O3 ~ T.Max, data = no.outliers)
wd.Slope = sprintf("%.4f", abs(summary(wd.model)$coeff[2]))
wd.R2 = summary(wd.model)$r.squared
wd.Slope
wd.R2

# factored by year
no.outliers$Time <- as.POSIXct(no.outliers$Time, origin = origin, tz = "Europe/Amsterdam")
yearly.data <- no.outliers %>%
  rowwise() %>%
  mutate(Year = year(Time)) %>%
  select(-Time) %>%
  group_by(Year, Temperature) %>%
  summarise(Mean.O3 = mean(O3))

y.plot <- ggplot(yearly.data, aes(x = Temperature, y = Mean.O3)) + geom_point() + stat_smooth(method = lm) + facet_wrap(~ Year)
y.slopes = yearly.data %>%  
  group_by(Year) %>%
  do(model = lm(Mean.O3 ~ Temperature, data = .)) %>%
  mutate(Slope = sprintf("%.1f", abs(summary(model)$coeff[2])), R2 = summary(model)$r.squared) %>%
  select(-model) 

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
  summarise(Mean.O3 = mean(O3))

s.plot <- ggplot(season, aes(x = Temperature, y = Mean.O3)) + geom_point() + facet_grid(~ Season) + stat_smooth(method = lm)
s.slopes <- season %>%
  group_by(Season) %>%
  do(model = lm(Mean.O3 ~ Temperature, data = .)) %>%
  mutate(Slope = sprintf("%.1f", abs(summary(model)$coeff[2])), R2 = summary(model)$r.squared) %>%
  select(-model)

# factored by year and season
season.year <- no.outliers %>%
  rowwise() %>%
  mutate(Year = year(Time), Season = getSeason(month(Time))) %>%
  select(-Time) %>%
  group_by(Year, Season, Temperature) %>%
  summarise(Mean.O3 = mean(O3))

sy.plot <- ggplot(season.year, aes(x = Temperature, y = Mean.O3)) + geom_point() + facet_grid(Season ~ Year) + stat_smooth(method = lm)
sy.slopes <- season.year %>%
  group_by(Year, Season) %>%
  do(model = lm(Mean.O3 ~ Temperature, data = .)) %>%
  mutate(Slope = sprintf("%.1f", abs(summary(model)$coeff[2])), R2 = summary(model)$r.squared) %>%
  select(-model)
