# analyse RO2NO2 Budgets, absolute rates
# Version 0: Jane Coates 09/11/2015

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "RO2NO2"
date <- "12112015"
mechanisms = c("CB05", "RADM2", "MOZART-4", "CRIv2", "MCMv3.2")

#temperature dependent
td.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TD", Date = date)
td.df <- do.call("rbind", td.list)
tbl_df(td.df)

#temperature independent
ti.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TI", Date = date)
ti.df <- do.call("rbind", ti.list)
tbl_df(ti.df)

df <- rbind(ti.df, td.df)
data <- get_species_budget(df, Reactants = TRUE, Absolute = TRUE)
tbl_df(data)
plot_species_budgets(data, Absolute = TRUE, Stacked = TRUE)