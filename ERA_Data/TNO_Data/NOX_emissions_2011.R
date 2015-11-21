setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//ERA_Data//TNO_Data")

all.data <- read.table(file = "TNO_MACC_III_emissions_v1_1_2011.txt", sep = "\t", header = TRUE)
benelux <- all.data %>%
  filter(ISO3 == "BEL" | ISO3 == "NLD" | ISO3 == "LUX")

benelux.emissions <- benelux %>%
  select(Lon, Lat, ISO3, NMVOC, NOX)
tbl_df(benelux.emissions)

benelux.emissions %>%
  summarise(Sum.NMVOC.per.day = sum(NMVOC), Sum.NOx.per.day = sum(NOX)) %>%
  mutate(Sum.NMVOC.per.day = Sum.NMVOC.per.day + 7042 + 2198 + 1462) %>%
  mutate(NOx.VOC.Ratio = Sum.NOx.per.day / Sum.NMVOC.per.day)