# analyse CH3CO3 Budgets, absolute rates
# Version 0: Jane Coates 12/11/2015

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "CH3CHO"
date <- "11112015"
mechanisms = c("CB05", "RADM2", "MOZART-4", "CRIv2", "MCMv3.2")

# temperature dependent
td.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TD", Date = date)
td.df <- do.call("rbind", td.list)
tbl_df(td.df)

# temperature independent
ti.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TI", Date = date)
ti.df <- do.call("rbind", ti.list)
tbl_df(ti.df)

df <- rbind(ti.df, td.df)

# Absolute Budgets
data <- get_species_budget(df, Reactants = TRUE, Absolute = TRUE)
tbl_df(data)
plot_species_budgets(subset(data, Rate > 0), Absolute = TRUE, Stacked = FALSE)

# Contribution Budgets
prod.data <- get_species_budget(subset(df, Rate > 0), Reactants = TRUE, Absolute = FALSE)
tbl_df(prod.data)
plot_species_budgets(prod.data, Absolute = FALSE, Stacked = FALSE)

# net budget
net.df <- data %>%
  group_by(Mechanism, Run, Temperature.C, NOx.Condition) %>%
  summarise(Net = sum(Rate))
net.df

ggplot(net.df, aes(x = Temperature.C, y = Net, colour = Mechanism)) + geom_line(size = 2) + facet_grid(Run ~ NOx.Condition) + plot_theme()
