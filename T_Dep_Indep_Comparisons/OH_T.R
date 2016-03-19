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

# m(OH-T)
t.oh  %>% 
  group_by(Mechanism, Run, NOx.Condition) %>%
  do(model = lm(OH ~ Temperature.C, data = .)) %>%
  mutate(Slope = sprintf("%.5f", abs(summary(model)$coeff[2])), R2 = summary(model)$r.squared) %>%
  select(-model)
  
# chemistry and emissions differences
t.oh %>%
  filter(Temperature.C == 20 | Temperature.C == 40) %>%
  rowwise() %>%
  mutate(Temperature.C = ifelse(Temperature.C == 20, "Twenty", "Fourty")) %>%
  spread(Temperature.C, OH, drop = FALSE) %>%
  mutate(Increase = Fourty - Twenty) %>%
  select(-Fourty, -Twenty) %>%
  mutate(Run = ifelse(Run == "Temperature Dependent\nIsoprene Emissions", "TD", "TI")) %>%
  spread(Run, Increase, drop = FALSE) %>%
  mutate(Emissions = TD - TI) %>%
  select(Mechanism, NOx.Condition, Chemistry = TI, Emissions, -TD)

my.colours <- c("MCMv3.2" = "#6c254f", "CRIv2" = "#ef6638", "MOZART-4" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")

p <- ggplot(t.oh, aes(x = Temperature.C, y = OH, colour = Mechanism))
p <- p + geom_line(size = 2)
p <- p + facet_grid(NOx.Condition ~ Run)
p <- p + plot_theme()
p <- p + ylab("OH (pptv)")
p <- p + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
p <- p + scale_colour_manual(values = my.colours)
p <- p + theme(axis.line.x = element_line(colour = "black"))
p <- p + theme(axis.line.y = element_line(colour = "black"))
p

CairoPDF(file = "OH_T.pdf")
print(p)
dev.off()
