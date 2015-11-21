# slopes of linear increase of O3 with T, allocated to the 3 NOx-regimes
# Version 0: Jane Coates 19/11/2015

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//T_Dep_Indep_Comparisons")

runs <- c("Dependent", "Independent", "Low", "High")
mechanisms <- c("CB05", "RADM2", "MOZART-4", "CRIv2")
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
t.o3$Run <- factor(t.o3$Run, levels = c("Temperature Dependent\nIsoprene Emissions", "Temperature Independent\nIsoprene Emissions", "Low Isoprene Emissions", "High Isoprene Emissions"))

plot_dO3_dT(t.o3)

slopes <- t.o3  %>% 
  group_by(Mechanism, Run, NOx.Condition) %>%
  do(model = lm(O3 ~ Temperature.C, data = .)) %>%
  mutate(Slope = sprintf("%.1f", abs(summary(model)$coeff[2])), R2 = summary(model)$r.squared) %>%
  select(-model)  %>% 
  rowwise() %>%
  mutate(Run = str_replace(Run, "\nIsoprene Emissions", " "))

# write.table(slopes, file = "Regressions_statistics_Mean_O3_T_NOx.txt", quote = FALSE, row.names = FALSE, sep = ",")
