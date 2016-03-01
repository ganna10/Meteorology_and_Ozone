# plotting peak OH vs T, over 3 NOx-Conditions. Each run and mechanism
# Version 0: Jane Coates 18/2/2016

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//T_Dep_Indep_Comparisons")
runs <- c("Dependent", "Independent")
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2")
data.list <- lapply(runs, get_all_mixing_ratio_data)
data.df <- do.call("rbind", data.list)
tbl_df(data.df)

t.oh <- data.df %>%
  mutate(Temperature.C = Temperature - 273) %>% 
  rowwise()  %>% 
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
  select(Mechanism, Temperature.C, OH, NOx.Condition, Run) %>%
  group_by(Mechanism, Run, NOx.Condition, Temperature.C) %>%
  summarise(OH = mean(OH))
t.oh$NOx.Condition <- factor(t.oh$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))
t.oh$Run <- factor(t.oh$Run, levels = c("Temperature Independent\nIsoprene Emissions", "Temperature Dependent\nIsoprene Emissions"))
t.oh$Mechanism <- factor(t.oh$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2"))

p <- ggplot(t.oh, aes(x = Temperature.C, y = OH, colour = Mechanism))
p <- p + geom_line(size = 2)
p <- p + facet_grid(NOx.Condition ~ Run)
p <- p + plot_theme()
p <- p + ylab("OH")
p <- p + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
p