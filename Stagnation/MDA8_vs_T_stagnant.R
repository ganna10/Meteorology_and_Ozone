# stagnation high-NOx O3 vs T

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Stagnation")
td.data <- read.csv("TD_data_18032016.csv")
tbl_df(td.data)

ti.data <- read.csv("TI_data_18032016.csv")
tbl_df(ti.data)

data <- rbind(ti.data, td.data)

mda.T <- data %>%
  select(Mechanism, NOx.Condition, Run, Temperature, MDA8) %>%
  mutate(Temperature.C = Temperature - 273) %>%
  group_by(Mechanism, Run, Temperature.C, NOx.Condition) %>%
  summarise(MDA8 = mean(MDA8))
mda.T$Type <- rep("No Mixing", length(mda.T$Mechanism))
mda.T
write.table(mda.T, file = "MDAO3_Temperature_No_Mixing.csv", sep = ",", row.names = FALSE, quote = FALSE)

p <- ggplot(mda.T, aes(x = Temperature.C, y = MDA8, colour = Mechanism))
p <- p + geom_line(size = 2)
p <- p + facet_grid(NOx.Condition ~ Run)
p <- p + plot_theme()
p

mda.T$Mechanism <- factor(mda.T$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2"))
mda.T$NOx.Condition <- factor(mda.T$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))
mda.T %>%  group_by(Mechanism, NOx.Condition, Run) %>% 
  do(model = lm(MDA8 ~ Temperature.C, data = .)) %>% 
  mutate(Slope = summary(model)$coeff[2], Intercept = summary(model)$coeff[1], R2 = summary(model)$r.squared) %>% 
  select(-model, -Intercept, -R2) %>%
  spread(NOx.Condition, Slope)
