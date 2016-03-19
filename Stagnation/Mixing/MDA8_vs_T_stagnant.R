# stagnation high-NOx O3 vs T

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Stagnation//Mixing")
data <- read.csv("TD_Temperature_NOx_17032016.csv")
tbl_df(data)

mda.T <- data %>%
  select(Mechanism, Run, Temperature, MDA8, H2O2, HNO3) %>%
  rowwise()  %>% 
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature.C = Temperature - 273) %>%
  group_by(Mechanism, Run, Temperature.C, NOx.Condition) %>%
  summarise(MDA8 = mean(MDA8))
mda.T

p <- ggplot(mda.T, aes(x = Temperature.C, y = MDA8, colour = Mechanism))
p <- p + geom_line(size = 2)
p <- p + facet_grid(NOx.Condition ~ Run)
p <- p + plot_theme()
p

mda.T %>%  group_by(Mechanism, NOx.Condition) %>% 
  do(model = lm(MDA8 ~ Temperature.C, data = .)) %>% 
  mutate(Slope = summary(model)$coeff[2], Intercept = summary(model)$coeff[1], R2 = summary(model)$r.squared) %>% 
  select(-model)
