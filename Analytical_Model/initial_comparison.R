# comparing model results to analytical model from Pusede:2014
# Version 0: Jane Coates 04/12/2015

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Analytical_Model")
date = "03122015"
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2")

# temperature independent data
ti.list <- lapply(mechanisms, get_analytical_model_data, Run.Label = "TI", Date = date)
ti.df <- do.call("rbind", ti.list)
tbl_df(ti.df)

# temperature dependent data
td.list <- lapply(mechanisms, get_analytical_model_data, Run.Label = "TD", Date = date)
td.df <- do.call("rbind", td.list)
tbl_df(td.df)

df <- rbind(ti.df, td.df)
data <- df %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature.C = Temperature - 273, NO.NOx = NO/(NO + NO2)) %>%
  select(-H2O2, -HNO3, -Temperature) %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition) %>%
  summarise(VOCR = mean(VOCR), PHOx = mean(PHOx), alpha = mean(alpha), NO.NOx = mean(NO.NOx), NO = mean(NO), NO2 = mean(NO2))

# VOCR Plot
p <- ggplot(data, aes(x = Temperature.C, y = VOCR, colour = Mechanism))
p <- p + geom_line(size = 1, aes(linetype = Run))
p <- p + facet_grid(~ NOx.Condition)
p <- p + plot_theme()
p

# PHOx Plot
p <- ggplot(data, aes(x = Temperature.C, y = PHOx, colour = Mechanism))
p <- p + geom_line(size = 1, aes(linetype = Run))
p <- p + facet_grid(~ NOx.Condition)
p <- p + plot_theme()
p

# NO/NOx Plot
p <- ggplot(data, aes(x = Temperature.C, y = NO.NOx, colour = Mechanism))
p <- p + geom_line(size = 1, aes(linetype = Run))
p <- p + facet_grid(~ NOx.Condition)
p <- p + plot_theme()
p

# alpha Plot
p <- ggplot(data, aes(x = Temperature.C, y = alpha, colour = Mechanism))
p <- p + geom_line(size = 1, aes(linetype = Run))
p <- p + facet_grid(~ NOx.Condition)
p <- p + plot_theme()
p

# NO Plot
p <- ggplot(data, aes(x = Temperature.C, y = NO, colour = Mechanism))
p <- p + geom_line(size = 1, aes(linetype = Run))
p <- p + facet_grid(~ NOx.Condition)
p <- p + plot_theme()
p

# NO2 Plot
p <- ggplot(data, aes(x = Temperature.C, y = NO2, colour = Mechanism))
p <- p + geom_line(size = 1, aes(linetype = Run))
p <- p + facet_grid(~ NOx.Condition)
p <- p + plot_theme()
p
