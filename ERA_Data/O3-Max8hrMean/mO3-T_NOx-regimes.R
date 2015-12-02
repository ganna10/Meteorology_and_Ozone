# slopes of linear increase of O3 with T, allocated to the 3 NOx-regimes
# Version 0: Jane Coates 19/11/2015
# Version 1: Jane Coates 30/11/2015 using Maximum 8hr mean O3 values

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
t.o3$Run <- factor(t.o3$Run, levels = c("Temperature Dependent\nIsoprene Emissions", "Temperature Independent\nIsoprene Emissions", "Low Isoprene Emissions", "High Isoprene Emissions"))

p <- plot_dO3_dT(t.o3)

CairoPDF(file = "Model_O3-T_correlation.pdf", width = 10, height = 7)
p1 = direct.label(p, list("last.bumpup", cex = 0.7))
p2 = ggplot_gtable(ggplot_build(p1))
p2$layout$clip[p2$layout$name == "panel"] = "off"
print(grid.draw(p2))
dev.off()

slopes <- t.o3  %>% 
  group_by(Mechanism, Run, NOx.Condition) %>%
  do(model = lm(O3 ~ Temperature.C, data = .)) %>%
  mutate(Slope = sprintf("%.3f", abs(summary(model)$coeff[2])), R2 = summary(model)$r.squared) %>%
  select(-model)  %>% 
  rowwise() %>%
  mutate(Run = str_replace(Run, "\nIsoprene Emissions", ""))
slopes$Mechanism <- factor(slopes$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2"))

arranged.slopes <- slopes %>%
  select(-R2) %>%
  spread(Run, Slope, drop = FALSE) %>%
  arrange(NOx.Condition)
write.table(arranged.slopes, file = "Regressions_statistics_Mean_O3_T_NOx.txt", quote = FALSE, row.names = FALSE, sep = ",")
arranged.slopes
