# stagnation high-NOx O3 vs T

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Rate_Constants/")
#orig data
orig.data <- read.csv("../ERA_Data/O3-Max8hrMean/Temperature_Dependent_data.csv")
orig.data <- orig.data %>%
  filter(Mechanism == "MOZART-4") %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
  filter(NOx.Condition == "High-NOx")
orig.data$Type <- rep("Original", length(orig.data$Mechanism))
orig.data$Run <- rep("TD", length(orig.data$Mechanism))
tbl_df(orig.data)

#ro2no2 rate constants
ro2no2.data <- read.csv("RO2NO2/TD_data_20032016.csv")
ro2no2.data$Type <- rep("RO2NO2", length(ro2no2.data$Mechanism))
tbl_df(ro2no2.data)

# voc oxidation rate constants
voc.data <- read.csv("VOC/TD_data_20032016.csv")
voc.data$Type <- rep("VOC", length(voc.data$Mechanism))
tbl_df(voc.data)

data <- rbind(orig.data %>% select(Mechanism, Temperature, NOx.Condition, Run, MDA8 = Max.O3.8hr.av, Type), ro2no2.data %>% select(Mechanism, Temperature, NOx.Condition, Run, MDA8, Type), voc.data %>% select(Mechanism, Temperature, NOx.Condition, Run, MDA8, Type))
tbl_df(data)

mda.T <- data %>%
  mutate(Temperature.C = Temperature - 273) %>%
  group_by(Mechanism, Run, Temperature.C, NOx.Condition, Type) %>%
  summarise(MDA8 = mean(MDA8))
mda.T
# write.table(mda.T, file = "MDAO3_Temperature_No_Mixing.csv", sep = ",", row.names = FALSE, quote = FALSE)

p <- ggplot(mda.T, aes(x = Temperature.C, y = MDA8, colour = Type))
p <- p + geom_line(size = 2)
p <- p + facet_grid(NOx.Condition ~ Run)
p <- p + plot_theme()
p

# mda.T$Mechanism <- factor(mda.T$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2"))
# mda.T$NOx.Condition <- factor(mda.T$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))
mda.T %>%  group_by(Mechanism, NOx.Condition, Run, Type) %>% 
  do(model = lm(MDA8 ~ Temperature.C, data = .)) %>% 
  mutate(Slope = summary(model)$coeff[2], Intercept = summary(model)$coeff[1], R2 = summary(model)$r.squared) %>% 
  select(-model, -Intercept, -R2) %>%
  spread(NOx.Condition, Slope)
