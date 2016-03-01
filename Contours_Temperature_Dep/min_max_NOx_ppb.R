setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Contours_Temperature_Dep/")
mcm.data <- read.csv("out_Temperature_NOx_14112015.csv")
tbl_df(mcm.data)

NOx <- mcm.data %>%
  filter(Temperature == 293) %>%
  select(NOx, Temperature)
tbl_df(NOx)
min(NOx$NOx)
max(NOx$NOx)
