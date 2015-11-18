# analyse O3 Budgets, production and consumption separately, plotted as fraction of total Ox production
# Version 0: Jane Coates 09/11/2015
# Version 1: Jane Coates 18/11/2015 visualising by categories

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "O3"
date <- "10112015"
# mechanisms <- c("CB05", "RADM2", "MOZART-4", "CRIv2", "MCMv3.2")
mechanisms <- c("RADM2")

#temperature dependent
td.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TD", Date = date)
td.df <- do.call("rbind", td.list)
tbl_df(td.df)

#temperature independent
ti.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TI", Date = date)
ti.df <- do.call("rbind", ti.list)
tbl_df(ti.df)

df <- rbind(ti.df, td.df)
tbl_df(df)
d <- get_species_budget(dataframe = subset(df, Rate > 0), Reactants = TRUE, Absolute = FALSE)
tbl_df(d)

d <- d %>%
  mutate(Reaction = sapply(str_split(Reaction, " \\+ "), "[", 1)) %>%
  group_by(Mechanism, Run, Temperature.C, NOx.Condition, Reaction) %>%
  summarise(Fraction = toString(unique(Fraction))) %>%
  spread(Reaction, Fraction, drop = FALSE)

d[is.na(d)] <- 0

tbl_df(d)
t <- d %>%
  mutate(Sum = as.double(HO2) + as.double(CH3CO3) + as.double(CH3O2))

ggplot(t, aes(x = Temperature.C, y = Sum, linetype = Run)) + geom_line(size = 2) + facet_grid( ~ NOx.Condition)
  
