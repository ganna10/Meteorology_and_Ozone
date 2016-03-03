# plotting peak OH vs T, over 3 NOx-Conditions. Each run and mechanism
# Version 0: Jane Coates 18/2/2016

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Contours_Temperature_Dep/")
td.data <- read.csv("out_Temperature_NOx_01032016.csv")
td.data$Run <- rep("TD", length(td.data$Mechanism))
tbl_df(td.data)

ti.data <- read.csv("../Contours_Temperature_Indep/out_Temperature_NOx_01032016.csv")
ti.data$Run <- rep("TI", length(ti.data$Mechanism))
tbl_df(ti.data)

data <- rbind(td.data, ti.data)

df <- data %>%
  mutate(Temperature.C = Temperature - 273) %>% 
  rowwise()  %>% 
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
  select(Mechanism, Temperature.C, HONO, NOx.Condition, Run) %>%
  group_by(Mechanism, Run, NOx.Condition, Temperature.C) %>%
  summarise(HONO = mean(HONO))
df$NOx.Condition <- factor(df$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))
# t.oh$Mechanism <- factor(t.oh$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2"))
tbl_df(df)

p <- ggplot(df, aes(x = Temperature.C, y = HONO, colour = Mechanism))
p <- p + geom_line(size = 2)
p <- p + facet_grid(Run ~ NOx.Condition)
p <- p + plot_theme()
p <- p + ylab("HONO")
p <- p + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
p
