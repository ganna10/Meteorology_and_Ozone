# analyse Ox production Budgets, contributions of individual RO2 to total
# Version 0: Jane Coates 26/11/2015

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "O3"
date <- "10112015"
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4","CB05", "RADM2")

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

assigned <- d %>%
  mutate(Reaction = sapply(str_split(Reaction, " \\+ "), "[", 1))
tbl_df(assigned)

plot_species_budgets_stacked_area(subset(assigned, Run == "Temperature Independent\nIsoprene Emissions" & Mechanism == "RADM2" & Reaction != "HO2"), Absolute = FALSE)