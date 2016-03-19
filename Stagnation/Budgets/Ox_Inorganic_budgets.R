# Inorganic Ox budgets by reactions

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Stagnation//Budgets")
spc <- "Ox_Inorganic"
date <- "18032016"
mechanisms <- c("MOZART-4")

#temperature dependent o3 data
td.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TD", Date = date)
td.df <- do.call("rbind", td.list)
tbl_df(td.df)

#temperature independent o3 data
ti.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TI", Date = date)
ti.df <- do.call("rbind", ti.list)
tbl_df(ti.df)

df <- rbind(ti.df, td.df)
tbl_df(df %>% arrange(Temperature))

# temperature dependent normalising data
norm.list.td <- lapply(mechanisms, get_budget_data, Species = "normalising", Run.Label = "TD", Date = "18032016")
norm.td.df <- do.call("rbind", norm.list.td)
tbl_df(norm.td.df)

# temperature independent normalising data
norm.list.ti <- lapply(mechanisms, get_budget_data, Species = "normalising", Run.Label = "TI", Date = "18032016")
norm.ti.df <- do.call("rbind", norm.list.ti)
tbl_df(norm.ti.df)

# normalising data
norm.df <- rbind(norm.td.df, norm.ti.df)
norm.data <- get_normalising_data_stagnation(norm.df)
tbl_df(norm.data)

d <- df %>%
  filter(Category != "Others") %>%
  mutate(Temperature.C = Temperature - 273, Net = Production.Rate + Consumption.Rate) %>%
  group_by(Mechanism, Temperature.C, Category, Run, NOx.Condition) %>%
  summarise(Rate = mean(Net)) %>%
  rowwise() %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition, Category) %>%
  mutate(Normalised = normalising_rates(Rate, Normalising.df = norm.data, mechanism = Mechanism, temperature = Temperature.C, run = Run, NOx.condition = NOx.Condition)) 
d$Run[d$Run == "Temperature Dependent\nIsoprene Emissions"] <- "TD"
d$Run[d$Run == "Temperature Independent\nIsoprene Emissions"] <- "TI"
d$NOx.Condition <- factor(d$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))
d$Type <- rep("Stagnation", length(d$Run))
d <- d %>%
  select(Mechanism, Temperature.C, Reaction = Category, Run, NOx.Condition, Normalised, Type)

d <- d %>%
  filter(Reaction != "Others") %>%
  ungroup()
# write.table(d, file = "Normalised_Ox_Inorganic_budget.csv", sep = ",", row.names = FALSE, quote = FALSE)

# mixing data
mix.data <- read.csv("Mixing_Inorganic_Data.csv")
mix.data$Type = rep("Mixing", length(mix.data$Run))
mix.data <- mix.data %>%
  ungroup()
tbl_df(mix.data)

all.data <- rbind(d, mix.data)

td.p <- ggplot(all.data %>% filter(Run == "TD"), aes(x = Temperature.C, y = Normalised, fill = Type))
td.p <- td.p + geom_bar(stat = "identity", position = "dodge")
td.p <- td.p + facet_grid(NOx.Condition ~ Reaction)
td.p <- td.p + plot_theme()
td.p


ti.p <- ggplot(all.data %>% filter(Run == "TI"), aes(x = Temperature.C, y = Normalised, fill = Type))
ti.p <- ti.p + geom_bar(stat = "identity", position = "dodge")
ti.p <- ti.p + facet_grid(NOx.Condition ~ Reaction)
ti.p <- ti.p + plot_theme()
ti.p
