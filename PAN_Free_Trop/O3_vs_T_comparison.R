# stagnation high-NOx O3 vs T

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//PAN_Free_Trop/")
# td.data <- read.csv("TD_data_18032016.csv")
# tbl_df(td.data)

ti.data <- read.csv("TI_data_19032016.csv")
ti.data$Type <- rep("PAN.Free", length(ti.data$Mechanism))
ti.data <- ti.data %>%
  select(Mechanism, Run , Temperature, NOx.Condition, Type, MDA8, PAN)
tbl_df(ti.data)

# data <- rbind(ti.data, td.data)

# orig.data
ti.orig <- read.csv("../T_Dep_Indep_Comparisons//Temperature_Independent_data.csv")
tbl_df(ti.orig)
ti.df <- ti.orig %>%
  filter(Mechanism == "RADM2") %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Run = "TI", Type = "Original") %>%
  filter(NOx.Condition == "High-NOx")
ti.df <- ti.df %>%
  select(Mechanism, Run , Temperature, NOx.Condition, Type, MDA8 = O3, PAN)

all <- rbind(ti.data, ti.df)
tbl_df(all)

final <- all %>%
  mutate(Temperature.C = Temperature - 273) %>%
  group_by(Mechanism, Run, Temperature.C, NOx.Condition, Type) %>%
  summarise(MDA8 = mean(MDA8), PAN = mean(PAN)) %>%
  gather(Species, Mixing.Ratio, -Mechanism, -Run, -Type, -Temperature.C, -NOx.Condition)

p <- ggplot(final, aes(x = Temperature.C, y = Mixing.Ratio, colour = Type))
p <- p + geom_line(size = 2)
p <- p + facet_wrap( ~ Species, scales = "free")
p <- p + plot_theme()
p

mda.T %>%  group_by(Mechanism, NOx.Condition, Run) %>% 
  do(model = lm(MDA8 ~ Temperature.C, data = .)) %>% 
  mutate(Slope = summary(model)$coeff[2], Intercept = summary(model)$coeff[1], R2 = summary(model)$r.squared) %>% 
  select(-model)
