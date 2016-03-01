# O3 vs T, allocated to the 3 NOx-regimes. Overlaid with ERA data and WRF output from different regions. Summer only values.
# Version 0: Jane Coates 24/12/2015
# Version 1: Jane Coates 11/2/2016 changing Run order

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//ERA_Data/O3-Max8hrMean/")

runs <- c("Dependent", "Independent")
mechanisms <- c("MCMv3.2", "CB05", "RADM2", "MOZART-4", "CRIv2")
data.list <- lapply(runs, get_all_mixing_ratio_data)
data.df <- do.call("rbind", data.list)
tbl_df(data.df)

t.o3 <- data.df %>%
  mutate(Temperature.C = Temperature - 273) %>% 
  rowwise()  %>% 
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
  select(Mechanism, Temperature.C, Max.O3.8hr.av, NOx.Condition, Run) %>%
  group_by(Mechanism, Run, NOx.Condition, Temperature.C) %>%
  summarise(O3 = mean(Max.O3.8hr.av))
t.o3$NOx.Condition <- factor(t.o3$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))
t.o3$Run <- factor(t.o3$Run, levels = c("Temperature Independent\nIsoprene Emissions", "Temperature Dependent\nIsoprene Emissions"))

# areas <- c("Central Germany", "Eastern Germany", "Poland")
areas <- c("Poland", "Germany")
plot.list <- lapply(areas, plot_dO3_dT_comparison_ERA, df = t.o3)
