setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Stagnation//Budgets/")

# stagnant data
stag.data <- read.csv("Total_Ox_budget.csv")
stag.data$Type <- rep("Stagnant", length(stag.data$Mechanism))
tbl_df(stag.data)

# mixing data
mix.data <- read.csv("../../Budgets/Total_Ox_budget.csv")
mix.data$Type <- rep("Mixing", length(mix.data$Mechanism))
tbl_df(mix.data)

all.data <- rbind(stag.data, mix.data)
all.data <- tbl_df(all.data)
all.data

td.data <- all.data %>%
  filter(Run == "TD")
td.data

prod <- ggplot(td.data %>% filter(Mechanism == "MCMv3.2"), aes(x = Temperature.C, y = Rate, fill = Type))
prod <- prod + geom_bar(stat = "identity", position = "dodge")
prod <- prod + facet_grid(NOx.Condition ~ Category)
prod <- prod + plot_theme()
prod

ti.data <- all.data %>%
  filter(Run == "TI")
ti.data

prod <- ggplot(ti.data %>% filter(Mechanism == "RADM2"), aes(x = Temperature.C, y = Rate, fill = Type))
prod <- prod + geom_bar(stat = "identity", position = "dodge")
prod <- prod + facet_grid(NOx.Condition ~ Category)
prod <- prod + plot_theme()
prod
