library(ncdf4)
setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/ERA_Data/")

subLon <- function (x) {
  if (x > 300) {
    x  <- x - 360
  }
  return (x)
}

get_subdata <- function (tmax.data, o3.data, lon.min, lon.max, lat.min, lat.max) {
  tmax.filter <- tmax.data %>%
    filter(between(Lon, lon.min, lon.max), between(Lat, lat.min, lat.max)) %>%
    arrange(Lon, Lat, Time) 
  
  o3.filter <- o3.data %>%
    filter(between(Lon, lon.min, lon.max), between(Lat, lat.min, lat.max)) %>%
    arrange(Lon, Lat, Time)
    
  combined.df <- data.frame(O3 = o3.filter$O3, Tmax = tmax.filter$T.Max, Time = o3.filter$Time)
#   upper.quantile <- quantile(combined.df$O3, 0.95, names = FALSE) # get 95th percentile
#   lower.quantile <- quantile(combined.df$O3, 0.05, names = FALSE) # get 5th percentile
#   
  combined.df$Time <- parse_date_time(combined.df$Time, "%Y%m%d")
  summer <- combined.df %>%
    mutate(Month = month(Time), Year = year(Time)) %>%
    filter(Year == 2007, Month %in% c(6, 7, 8), Tmax >= 288) %>%
    select(O3, Tmax)
  return(summer)
}

# get all temperature data
tmax.nc <- nc_open("tmax_EU_1998-2012.nc")
tmax <- ncvar_get(tmax.nc, "tmax")
tmax[tmax == "-32767"] <- NA
nc_close(tmax.nc)

lon <- tmax.nc$dim$longitude$vals
lat <- rev(tmax.nc$dim$latitude$vals)
time <- tmax.nc$dim$time$vals - 0.75
# time <- parse_date_time(time,"%Y%m%d")

# get all O3 data
o3.nc <- nc_open("surfO3.mda8.eu.set.1998-2012.nc")
o3 <- ncvar_get(o3.nc, "MDA8_SurfO3")
o3[o3 == "-999"] <- NA
nc_close(o3.nc)

o3.lon <- o3.nc$dim$lon$vals
o3.lon <- sapply(o3.lon, subLon)
o3.lat <- o3.nc$dim$lat$vals
time.o3 <- o3.nc$dim$time$vals
# time.o3 <- parse_date_time(time.o3, "%Y%m%d")

#convert tmax array to data frame
dimnames(tmax) <- list(lon, lat, time)
tmax.df <- tmax %>%
  melt(na.rm = TRUE, varnames = names(dimnames(tmax)))
names(tmax.df) <- c("Lon", "Lat", "Time", "T.Max")

#convert ozone array to data frame
dimnames(o3) <- list(o3.lat, o3.lon, time.o3)
o3.df <- o3 %>%
  melt(na.rm = TRUE, varnames = names(dimnames(o3)))
names(o3.df) <- c("Lat", "Lon", "Time", "O3")

#get data over specific regions
# NE.France = get_subdata(tmax.df, o3.df, lon.min = 4, lon.max = 5, lat.min = 47, lat.max = 49)
# NE.France$Area <- rep("NE France", length(NE.France$O3))

Germany = get_subdata(tmax.df, o3.df, lon.min = 9, lon.max = 14, lat.min = 50, lat.max = 53)
Germany$Area <- rep("Germany", length(Germany$O3))

# E.Germany = get_subdata(tmax.df, o3.df, lon.min = 13, lon.max = 14, lat.min = 50, lat.max = 53)
# E.Germany$Area <- rep("Eastern Germany", length(E.Germany$O3))

# Czech.Rep = get_subdata(tmax.df, o3.df, lon.min = 14, lon.max = 16, lat.min = 47, lat.max = 49)
# Czech.Rep$Area <- rep("Czech Republic", length(Czech.Rep$O3))
# 
# W.Austria = get_subdata(tmax.df, o3.df, lon.min = 14, lon.max = 16, lat.min = 47, lat.max = 48)
# W.Austria$Area <- rep("W Austria", length(W.Austria$O3))

Poland = get_subdata(tmax.df, o3.df, lon.min = 15, lon.max = 20, lat.min = 50, lat.max = 52)
Poland$Area <- rep("Poland", length(Poland$O3))

# C.Poland = get_subdata(tmax.df, o3.df, lon.min = 19, lon.max = 20, lat.min = 51, lat.max = 52)
# C.Poland$Area <- rep("C Poland", length(C.Poland$O3))

# Netherlands = get_subdata(tmax.df, o3.df, lon.min = 6, lon.max = 6, lat.min = 51, lat.max = 52)
# Netherlands$Area <- rep("Netherlands", length(Netherlands$O3))
# 
# N.Germany = get_subdata(tmax.df, o3.df, lon.min = 9, lon.max = 14, lat.min = 53, lat.max = 53)
# N.Germany$Area <- rep("N Germany", length(N.Germany$O3))

all.data <- rbind(Germany, Poland)
all.data <- all.data %>%
  mutate(Tmax.C = Tmax - 273) %>%
  select(-Tmax)

output_data <- function (df, area) {
  filtered <- df %>%
    filter(Area == area) %>%
    select(O3, Tmax.C)
  colnames(filtered) <- c("O3", "Temperature.C")
  filename <- paste0(area, "_O3-T_ERA_data_2007.csv")
  write.table(filtered, file = filename, sep = ",", quote = FALSE, row.names = FALSE)
}
lapply(levels(factor(all.data$Area)), output_data, df = all.data)

regression.data <- all.data %>%
  group_by(Area) %>%
  do(model = lm(O3 ~ Tmax.C, data = .)) %>%
  mutate(Slope = paste("m = ", sprintf("%.2f", abs(summary(model)$coeff[2]))), R2 = paste("R2 = ", sprintf("%.3f", summary(model)$r.squared))) %>%
  mutate(text = paste("m = ", sprintf("%.2f", abs(summary(model)$coeff[2])), " ppbv/C, R2 = ", sprintf("%.3f", summary(model)$r.squared))) %>%
  select(-model)
regression.data

wd.plot <- ggplot(all.data, aes(x = Tmax.C, y = O3))
wd.plot <- wd.plot + geom_point(alpha = 0.05)
wd.plot <- wd.plot + stat_smooth(method = "lm", formula = y ~ x, se = FALSE, size = 1, colour = "red")
wd.plot <- wd.plot + plot_theme()
wd.plot <- wd.plot + facet_wrap( ~ Area, scales = "free_x")
wd.plot <- wd.plot + geom_text(data = regression.data, aes(x = 20, y = 100, label = text, size = 10), show_guide = FALSE)
wd.plot <- wd.plot + xlab(expression(bold(paste("Maximum Daily Temperature (", degree, "C)")))) + ylab("Maximum 8-Hour daily mean O3 Mixing Ratio (ppv)")
wd.plot

CairoPDF(file = "Central_Europe_mO3-T", width = 10, height = 7)
print(wd.plot)
dev.off()
