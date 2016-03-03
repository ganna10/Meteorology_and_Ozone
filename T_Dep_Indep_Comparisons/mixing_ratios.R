setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//T_Dep_Indep_Comparisons")
runs <- c("Dependent", "Independent")
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2")
data.list <- lapply(runs, get_all_mixing_ratio_data)
data.df <- do.call("rbind", data.list)
tbl_df(data.df)

d <- data.df %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature.C = Temperature - 273) %>%
  select(Mechanism, Temperature.C, NOx.Condition, Run, O3, OH, HO2, HOx, HCHO, RO2NO2, RONO2, PAN) %>%
  group_by(Mechanism, Temperature.C, NOx.Condition, Run) %>%
  summarise(O3 = mean(O3), OH = mean(OH), HO2 = mean(HO2), HOx = mean(HOx), HCHO = mean(HCHO), RO2NO2 = mean(RO2NO2), RONO2 = mean(RONO2), PAN = mean(PAN)) 

gathered <- d %>%
  gather(Species, Mixing.Ratio, -Mechanism, -Temperature.C, -NOx.Condition, -Run)
d

ggplot(d, aes(x = Temperature.C, y = OH, colour = Mechanism)) + geom_line(size = 2) + facet_grid(Run ~ NOx.Condition) + plot_theme()
