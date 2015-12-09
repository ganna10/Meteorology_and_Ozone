# Ox budgets normalised by different factors
# Version 0: Jane Coates 9/12/2015

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Budgets")
o3.budget.date <- "03122015"
normalising.date <- "08122015"

mechanisms <- c("CRIv2", "MCMv3.2", "CB05", "RADM2", "MOZART-4")

o3.budget.list <- lapply(mechanisms, get_budget_data, Species = "O3", Run.Label = "TD", Date = o3.budget.date)
o3.budget.df <- do.call("rbind", o3.budget.list)
o3.budget.df <- tbl_df(o3.budget.df)

normalising.list <- lapply(mechanisms, get_budget_data, Species = "normalising", Run.Label = "TD", Date = normalising.date)
normalising.df <- do.call("rbind", normalising.list)
normalising.df <- tbl_df(normalising.df)

vocr.total <- normalising.df %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature.C = Temperature - 273) %>%
  select(Mechanism, Temperature.C, NOx.Condition, VOCR.total) %>%
  group_by(Mechanism, Temperature.C, NOx.Condition) %>%
  summarise(VOCR.total = mean(VOCR.total))

vocr.initial <- normalising.df %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature.C = Temperature - 273) %>%
  select(Mechanism, Temperature.C, NOx.Condition, VOCR.initial) %>%
  group_by(Mechanism, Temperature.C, NOx.Condition) %>%
  summarise(VOCR.initial = mean(VOCR.initial))

total.loss.initial <- normalising.df %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature.C = Temperature - 273) %>%
  select(Mechanism, Temperature.C, NOx.Condition, VOC.initial.losses) %>%
  group_by(Mechanism, Temperature.C, NOx.Condition) %>%
  summarise(VOC.initial.losses = mean(VOC.initial.losses))

ro2.no.reactivity <- normalising.df %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature.C = Temperature - 273) %>%
  select(Mechanism, Temperature.C, NOx.Condition, RO2.NO) %>%
  group_by(Mechanism, Temperature.C, NOx.Condition) %>%
  summarise(RO2.NO = mean(RO2.NO))

vocr.total.normalise <- function (value, mechanism, temperature.C, NOx.condition) {
  factor <- vocr.total  %>% 
    filter(Mechanism == mechanism, Temperature.C == temperature.C, NOx.Condition == NOx.condition)
  normalised <- value / factor$VOCR.total
  return(normalised)
}

vocr.initial.normalise <- function (value, mechanism, temperature.C, NOx.condition) {
  factor <- vocr.initial  %>% 
    filter(Mechanism == mechanism, Temperature.C == temperature.C, NOx.Condition == NOx.condition)
  normalised <- value / factor$VOCR.initial
  return(normalised)
}

ro2.no.r.normalise <- function (value, mechanism, temperature.C, NOx.condition) {
  factor <- ro2.no.reactivity  %>% 
    filter(Mechanism == mechanism, Temperature.C == temperature.C, NOx.Condition == NOx.condition)
  normalised <- value / factor$RO2.NO
  return(normalised)
}

initial.loss.esnormalise <- function (value, mechanism, temperature.C, NOx.condition) {
  factor <- total.loss.initial  %>% 
    filter(Mechanism == mechanism, Temperature.C == temperature.C, NOx.Condition == NOx.condition)
  normalised <- value / factor$VOC.initial.losses
  return(normalised)
}

o3.budget <- o3.budget.df %>%
  arrange(Temperature) %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature.C = Temperature - 273) %>%
  group_by(Mechanism, Temperature.C, NOx.Condition, Category) %>%
  summarise(Rate = mean(Rate)) %>%
  rowwise() %>%
  group_by(Mechanism, Temperature.C, NOx.Condition, Category) %>%
  mutate(Normalised = initial.loss.esnormalise(Rate, mechanism = Mechanism, temperature.C = Temperature.C, NOx.condition = NOx.Condition)) %>%
  select(-Rate) 
o3.budget$Category <- factor(o3.budget$Category, levels = c("HO2", "RO2", "ARO2", "RO2NO2", "Inorganic", "Other Organic"))
o3.budget$NOx.Condition <- factor(o3.budget$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))

my.colours = c("Inorganic" = "#b569b3", "Other Organic" = "#2b9eb3", "ARO2" = "#ef6638", "RO2" = "#0e5c28", "HO2" = "#f9c500", "RO2NO2" = "#6c254f")

p <- ggplot(o3.budget, aes(x = Temperature.C, y = Normalised, fill = Category, order = Category))
p <- p + geom_bar(stat = "identity")
p <- p + facet_grid(Mechanism ~ NOx.Condition)
p <- p + plot_theme()
p <- p + scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0))
p <- p + scale_fill_manual(values = my.colours)
p
