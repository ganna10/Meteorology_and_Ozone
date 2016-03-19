setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Stagnation//Budgets/")

# stagnant data
stag.data <- read.csv("Total_Ox_budget.csv")
tbl_df(stag.data)

# OH + NO2 rate
date <- "19032016"
spc <- "OH_NO2"
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "RADM2", "CB05")

#temperature dependent o3 data
td.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TD", Date = date)
td.df <- do.call("rbind", td.list)
tbl_df(td.df)

#temperature independent o3 data
ti.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TI", Date = date)
ti.df <- do.call("rbind", ti.list)
tbl_df(ti.df)

df <- rbind(ti.df, td.df)
d <- df %>%
  mutate(Temperature.C = Temperature - 273) %>%
  select(Mechanism, Temperature.C, Run, NOx.Condition, OH.NO2.Rate = Rate)
d$Run[d$Run == "Temperature Dependent\nIsoprene Emissions"] <- "TD"
d$Run[d$Run == "Temperature Independent\nIsoprene Emissions"] <- "TI"
tbl_df(d)
tbl_df(stag.data)

normalising <- function (value, Normalising.df, mechanism, temperature, run, NOx.condition) {
  factor <- Normalising.df  %>% 
    filter(Mechanism == mechanism, Temperature.C == temperature, NOx.Condition == NOx.condition, Run == run)
  normalised <- value / factor$OH.NO2.Rate
  return(normalised)
}

prod.eff <- stag.data %>%
  filter(Rate > 0) %>%
  rowwise() %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition) %>%
  mutate(Ox.Prod.Efficiency = normalising(Rate, Normalising.df = d, mechanism = Mechanism, temperature = Temperature.C, run = Run, NOx.condition = NOx.Condition)) %>%
  select(-Rate)
prod.eff

td.data <- prod.eff %>%
  filter(Run == "TD")
td.p <- ggplot(td.data, aes(x = Temperature.C, y = Ox.Prod.Efficiency, fill = Category))
td.p <- td.p + geom_bar(stat = "identity")
td.p <- td.p + facet_grid(Mechanism ~ NOx.Condition)
td.p <- td.p + plot_theme()
td.p
