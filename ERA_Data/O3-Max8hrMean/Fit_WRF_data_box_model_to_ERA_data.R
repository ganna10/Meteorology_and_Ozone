# get fit values of ERA data to WRF-Chem and box model simulations
# Version 0: Jane Coates 28/12/2015

setwd("~/Documents/Analysis/2015_Meteorology_and_Ozone/ERA_Data/O3-Max8hrMean/")

get_era_slopes <- function (area) {
  era.file <- paste0(area, "_O3-T_ERA_data_2007.csv")
  era.data <- read.csv(era.file)
  era.linear.fit <- lm(O3 ~ Temperature.C, data = era.data)
  era.slope <- era.linear.fit$coefficients[2]
  return(era.slope)
}

get_wrf_slopes <- function (area) {
  wrf.mozart.file <- paste0(area, "_wrf_mozart.csv")
  wrf.mozart.data <- read.csv(wrf.mozart.file)
  wrf.radm2.file <- paste0(area, "_wrf_radm2.csv")
  wrf.radm2.data <- read.csv(wrf.radm2.file)
  wrf.data <- rbind(wrf.mozart.data, wrf.radm2.data)
  
  wrf.regression <- wrf.data %>%
    select(Temperature.C, O3, Mechanism) %>%
    group_by(Mechanism) %>%
    do(model = lm(O3 ~ Temperature.C, data = .)) %>%
    mutate(Slope = summary(model)$coeff[2]) %>%
    select(-model)
  return(wrf.regression)  
}

areas <- c("Germany", "Poland")

era.slopes <- lapply(areas, get_era_slopes)
era.slopes

wrf.slopes <- lapply(areas, get_wrf_slopes)
wrf.slopes

runs <- c("Dependent", "Independent")
mechanisms <- c("MCMv3.2", "CB05", "RADM2", "MOZART-4", "CRIv2")
data.list <- lapply(runs, get_all_mixing_ratio_data)
data.df <- do.call("rbind", data.list)
tbl_df(data.df)

boxmodel <- data.df %>%
  mutate(Temperature.C = Temperature - 273) %>% 
  rowwise()  %>% 
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
  select(Mechanism, Temperature.C, Max.O3.8hr.av, NOx.Condition, Run) %>%
  group_by(Mechanism, Run, NOx.Condition, Temperature.C) %>%
  summarise(O3 = mean(Max.O3.8hr.av))

boxmodel.slopes <- boxmodel %>%
  group_by(Mechanism, Run, NOx.Condition) %>%
  do(model = lm(O3 ~ Temperature.C, data = .)) %>%
  mutate(Slope = summary(model)$coeff[2]) %>%
  select(-model)
boxmodel.slopes$Mechanism <- factor(boxmodel.slopes$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2"))
boxmodel.slopes$NOx.Condition <- factor(boxmodel.slopes$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))

boxmodel.slopes %>%
  spread(NOx.Condition, Slope, drop = FALSE) %>%
  filter(Run =="Temperature Independent\nIsoprene Emissions")
