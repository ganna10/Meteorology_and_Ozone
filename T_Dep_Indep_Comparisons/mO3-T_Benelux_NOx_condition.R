# slopes of linear increase of O3 with T at the calculated Benelux NOx emissions
# Version 0: Jane Coates 23/11/2015
library(ncdf4)

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//T_Dep_Indep_Comparisons")

##### Box models
runs <- c("Dependent", "Independent")
mechanisms <- c("CB05", "RADM2", "MOZART-4", "CRIv2", "MCMv3.2")
data.list <- lapply(runs, get_all_mixing_ratio_data)
data.df <- do.call("rbind", data.list)
tbl_df(data.df)

t.o3 <- data.df %>%
  mutate(Temperature.C = Temperature - 273) %>% 
  filter(NOx.Emissions == 147798000) %>%
  select(Mechanism, Temperature.C, O3, Run)

t.o3$Run <- factor(t.o3$Run, levels = c("Temperature Dependent\nIsoprene Emissions", "Temperature Independent\nIsoprene Emissions", "Low Isoprene Emissions", "High Isoprene Emissions"))


slopes <- t.o3  %>% 
  group_by(Mechanism, Run) %>%
  do(model = lm(O3 ~ Temperature.C, data = .)) %>%
  mutate(Slope = sprintf("%.1f", abs(summary(model)$coeff[2])), R2 = summary(model)$r.squared) %>%
  select(-model)  %>% 
  rowwise() %>%
  mutate(Run = str_replace(Run, "\nIsoprene Emissions", " "))
slopes
# write.table(slopes, file = "Regressions_statistics_Mean_O3_T_NOx.txt", quote = FALSE, row.names = FALSE, sep = ",")

#### ERA Interim data

lon.max <- 8
lon.min <- 2
lat.max <- 53
lat.min <- 48

tmax.nc <- nc_open("../ERA_Data/tmax_EU_1998-2012.nc")
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
# test <- tmax[,,1]
# filled.contour(lon, lat, test)

o3.nc <- nc_open("../ERA_Data/surfO3.mda8.eu.set.1998-2012.nc")
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
# test <- o3[,,1]
# filled.contour(lon.o3, lat.o3, test)

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
combined.df <- combined.df %>%
  mutate(Temperature.C = Temperature - 273)
tbl_df(combined.df)

# upper.quantile <- quantile(combined.df$O3, 0.95) # get 95th percentile
# lower.quantile <- quantile(combined.df$O3, 0.05) # get 5th percentile
# 
# no.outliers <- combined.df %>%
#   filter(O3 < upper.quantile & O3 > lower.quantile)
# tbl_df(no.outliers)


# entire data set
whole.data <- combined.df %>%
  select(-Time, -Temperature) %>%
  filter(Temperature.C >= 15 & Temperature.C <= 40) %>%
  group_by(Temperature.C) %>%
  summarise(O3 = mean(O3, na.rm = TRUE))

wd.plot <- ggplot(whole.data, aes(x = Temperature, y = O3)) + geom_point() + stat_smooth(method = lm)
wd.plot
wd.model = lm(Mean.O3 ~ Temperature, data = whole.data)
wd.Slope = sprintf("%.1f", abs(summary(wd.model)$coeff[2]))
wd.R2 = summary(wd.model)$r.squared
wd.Slope
wd.R2

### plot everything
p <- ggplot(t.o3, aes(x = Temperature.C, y = O3))
p <- p + geom_line(size = 1, aes(colour = Mechanism, linetype = Run))
p + geom_line(data = whole.data, colour = "black", size = 2)
