setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//WRF-output//radm2//Germany/")
library(ncdf4)

get_data <- function(file) {
  date <- str_match(file, "2007-0\\d-\\d\\d")
  nc <- nc_open(file)
  temperature <- ncvar_get(nc, "T2")
  t.max <- max(temperature) - 273
  o3 <- ncvar_get(nc, "o3")
  o3 <- o3[,,1,] # surface layer data
  running.means <- stats::filter(c(o3), rep(1/8, 8), sides = 1)
  running.means[is.na(running.means)] <- 0
  max.8hr.o3 <- max(running.means)
  
  df <- data.frame(Date = date, Temperature.C = t.max, O3 = max.8hr.o3 * 1000, Mechanism = "RADM2", Type = "WRF-Chem")
  return(df)
}

my.files <- list.files()
data.list <- lapply(my.files, get_data)
data.df <- do.call("rbind", data.list)
tbl_df(data.df)
write.table(data.df, file = "~/Documents//Analysis//2015_Meteorology_and_Ozone//WRF-output//radm2/germany.csv", sep = ",", quote = FALSE, row.names = FALSE)
