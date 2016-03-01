# plotting peak O3 vs T, over 3 NOx-Conditions. Each run and mechanism
# Version 0: Jane Coates 7/12/2015

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//T_Dep_Indep_Comparisons")
runs <- c("Dependent", "Independent")
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2")
data.list <- lapply(runs, get_all_mixing_ratio_data)
data.df <- do.call("rbind", data.list)
tbl_df(data.df)

t.o3 <- data.df %>%
  mutate(Temperature.C = Temperature - 273) %>% 
  rowwise()  %>% 
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
  select(Mechanism, Temperature.C, O3, NOx.Condition, Run) %>%
  group_by(Mechanism, Run, NOx.Condition, Temperature.C) %>%
  summarise(O3 = mean(O3))
t.o3$NOx.Condition <- factor(t.o3$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))
t.o3$Run <- factor(t.o3$Run, levels = c("Temperature Independent\nIsoprene Emissions", "Temperature Dependent\nIsoprene Emissions"))
t.o3$Mechanism <- factor(t.o3$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2"))

p <- plot_dO3_dT(t.o3)
p <- p + ylab("O3 (ppbv)")
p <- p + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
p <- p + geom_vline(x = 20)
p

CairoPDF(file = "O3-T_correlation.pdf", width = 10, height = 7)
# print(p)
p1 = direct.label(p, list("last.bumpup", cex = 1.2))
p2 = ggplot_gtable(ggplot_build(p1))
p2$layout$clip[p2$layout$name == "panel"] = "off"
print(grid.draw(p2))
dev.off()
