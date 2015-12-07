setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//ERA_Data//TNO_Data")

all.data <- read.table(file = "TNO_MACC_III_emissions_v1_1_2011.txt", sep = "\t", header = TRUE)

get_total_NO_emissions <- function (all.data, lon.min, lon.max, lat.min, lat.max) {
  data <- all.data %>%
    filter(between(Lat, lat.min, lat.max), between(Lon, lon.min, lon.max)) %>%
    select(Lon, Lat, ISO3, SourceType, NOX) %>%
    filter(NOX != 0) %>%
    mutate(NO.molecule.per.s = NOX * 1e6 * 6.022e23 / (30.01 * 365 * 24 * 60 * 60))
  
  distinct.grid.cells <- data %>%
    filter(SourceType == "A") %>%
    distinct(Lon, Lat)
  no.grid.cells <- length(distinct.grid.cells$Lat)
  area.cm2 <- no.grid.cells * 7 * 7 * (1000 * 100 * 1000 * 100)
  
  data.final <- data %>%
    mutate(NO.molecule.per.s.per.cm2 = NO.molecule.per.s / area.cm2)
  tbl_df(data.final)
  
  total.NO.emissions = sprintf("%.2e", sum(data.final$NO.molecule.per.s.per.cm2))
  return(total.NO.emissions)
}

NE.France = get_total_NO_emissions(all.data, lon.min = 4, lon.max = 5, lat.min = 47, lat.max = 49)

C.Germany = get_total_NO_emissions(all.data, lon.min = 9, lon.max = 12, lat.min = 50, lat.max = 51)

E.Germany = get_total_NO_emissions(all.data, lon.min = 13, lon.max = 14, lat.min = 50, lat.max = 53)

Czech.Rep = get_total_NO_emissions(all.data, lon.min = 14, lon.max = 16, lat.min = 47, lat.max = 49)

W.Austria = get_total_NO_emissions(all.data, lon.min = 14, lon.max = 16, lat.min = 47, lat.max = 48)

W.Poland = get_total_NO_emissions(all.data, lon.min = 15, lon.max = 17, lat.min = 50, lat.max = 52)

C.Poland = get_total_NO_emissions(all.data, lon.min = 19, lon.max = 20, lat.min = 51, lat.max = 52)

Netherlands = get_total_NO_emissions(all.data, lon.min = 5.9, lon.max = 6.0, lat.min = 51, lat.max = 52)

N.Germany = get_total_NO_emissions(all.data, lon.min = 9, lon.max = 14, lat.min = 52.9, lat.max = 53.0)
