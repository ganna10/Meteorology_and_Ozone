# Ox/O3 ratio w. T
# Version 0: Jane Coates 16/3/2016

setwd("2015_Meteorology_and_Ozone/Budgets/")
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "RADM2", "CB05")

# Ox data
spc <- "Ox"
date <- "01032016"
#td data
ox.td.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TD", Date = date)
ox.td.df <- do.call("rbind", ox.td.list)
tbl_df(ox.td.df)

#ti data
ox.ti.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TI", Date = date)
ox.ti.df <- do.call("rbind", ox.ti.list)
tbl_df(ox.ti.df)

ox.df <- rbind(ox.ti.df, ox.td.df)
tbl_df(ox.df %>% arrange(Temperature))

ox.d <- ox.df %>%
  group_by(Mechanism, Temperature, Run, H2O2, HNO3) %>%
  summarise(Ox.Production = sum(Production.Rate), Ox.Consumption = sum(Consumption.Rate)) %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature.C = Temperature - 273) %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition) %>%
  summarise(Ox.Production = mean(Ox.Production), Ox.Consumption = mean(Ox.Consumption)) %>%
  arrange(Mechanism, Temperature.C, Run, NOx.Condition)
tbl_df(ox.d)

#O3 data
spc <- "O3"
date <- "16032016"
#td data
o3.td.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TD", Date = date)
o3.td.df <- do.call("rbind", o3.td.list)
tbl_df(o3.td.df)

#ti data
o3.ti.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TI", Date = date)
o3.ti.df <- do.call("rbind", o3.ti.list)
tbl_df(o3.ti.df)

o3.df <- rbind(o3.ti.df, o3.td.df)
tbl_df(o3.df %>% arrange(Temperature))

o3.d <- o3.df %>%
  group_by(Mechanism, Temperature, Run, H2O2, HNO3) %>%
  summarise(O3.Production = sum(Production.Rate), O3.Consumption = sum(Consumption.Rate)) %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature.C = Temperature - 273) %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition) %>%
  summarise(O3.Production = mean(O3.Production), O3.Consumption = mean(O3.Consumption)) %>%
  arrange(Mechanism, Temperature.C, Run, NOx.Condition)
tbl_df(o3.d)

all.data <- ox.d
all.data$O3.Production <- o3.d$O3.Production
all.data$O3.Consumption <- o3.d$O3.Consumption
all.data <- all.data %>%
  mutate(Ox.O3.Production = Ox.Production / O3.Production, Ox.O3.Consumption = Ox.Consumption / O3.Consumption) %>%
  select(Mechanism, Temperature.C, Run, NOx.Condition, Ox.O3.Production, Ox.O3.Consumption)

# gathered <- all.data %>%
#   gather(Item, Ratio, -Mechanism, -Temperature.C, -Run, -NOx.Condition)

net.ratio <- all.data %>%
  mutate(Ox.O3.Ratio = Ox.O3.Production - Ox.O3.Consumption)
net.ratio

p <- ggplot(net.ratio, aes(x = Temperature.C, y = Ox.O3.Ratio, colour =  Mechanism))
p <- p + geom_line(size = 2)
p <- p + facet_grid(NOx.Condition ~ Run)
p <- p + plot_theme()
p
