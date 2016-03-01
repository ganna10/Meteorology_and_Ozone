# normalised Ox Budgets with HOx production
# Version 0: Jane Coates 16/2/2016

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "O3"
date <- "10022016"
mechanisms <- c("CB05")

#temperature dependent o3 data
# td.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TD", Date = date)
# td.df <- do.call("rbind", td.list)
# tbl_df(td.df)

#temperature independent o3 data
ti.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TI", Date = date)
ti.df <- do.call("rbind", ti.list)
tbl_df(ti.df)

# HOx data
hox.spc <- "HOx"
hox.date <- "15022016"
hox.ti.list <- lapply(mechanisms, get_budget_data, Species = hox.spc, Run.Label = "TI", Date = hox.date)
hox.ti.df <- do.call("rbind", hox.ti.list)
tbl_df(hox.ti.df)

hox.norm <- hox.ti.df %>%
  rowwise() %>%
  mutate(Temperature.C = Temperature - 273, NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
  group_by(Mechanism, Temperature.C, NOx.Condition, Run) %>%
  summarise(Rate = sum(Rate))
tbl_df(hox.norm)

d <- ti.df %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature.C = Temperature - 273, Rate = Production.Rate + Consumption.Rate) %>%
  group_by(Mechanism, Temperature.C, Category, Run, NOx.Condition) %>%
  summarise(Rate = mean(Rate)) %>%
  rowwise() %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition, Category) %>%
  mutate(Rate = normalising_rates(Rate, Normalising.df = hox.norm, mechanism = Mechanism, temperature = Temperature.C, run = Run, NOx.condition = NOx.Condition)) 
tbl_df(d)

ti.plot <- ggplot(d, aes(x = Temperature.C, y = Rate, fill = Category, order = Category))
ti.plot <- ti.plot + geom_bar(data = subset(d, Rate > 0), stat = "identity", width = 1) 
ti.plot <- ti.plot + geom_bar(data = subset(d, Rate < 0), stat = "identity", width = 1) 
ti.plot <- ti.plot + facet_grid(Mechanism ~ NOx.Condition) 
ti.plot <- ti.plot + plot_theme()
ti.plot