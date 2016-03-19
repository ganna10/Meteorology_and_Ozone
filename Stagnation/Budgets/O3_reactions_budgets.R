# analyse individual reactions of O3 reactions to Ox budget
# Version 0: Jane Coates 13/2/2016

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Stagnation/Budgets/")
spc <- "O3_reactions"
date <- "18032016"

# mixing data
mix.data <- read.csv("../../Budgets/Mixing_O3_Reactions.csv")
mix.data$Type <- rep("Mixing", length(mix.data$Mechanism))
tbl_df(mix.data)

#temperature dependent o3 data
td.df <- get_budget_data(Mechanism = "MOZART-4", Species = spc, Run.Label = "TD", Date = date)
tbl_df(td.df)

#temperature independent o3 data
ti.df <- get_budget_data(Mechanism = "MOZART-4", Species = spc, Run.Label = "TI", Date = date)
tbl_df(ti.df)

df <- rbind(ti.df, td.df)
tbl_df(df %>% arrange(Temperature))

# temperature dependent normalising data
norm.td.df <- get_budget_data(Mechanism = "MOZART-4", Species = "normalising", Run.Label = "TD", Date = "18032016")
tbl_df(norm.td.df)

# temperature independent normalising data
norm.ti.df <- get_budget_data(Mechanism = "MOZART-4", Species = "normalising", Run.Label = "TI", Date = "18032016")
tbl_df(norm.ti.df)

# normalising data
norm.df <- rbind(norm.td.df, norm.ti.df)
norm.data <- get_normalising_data_stagnation(norm.df)
tbl_df(norm.data)

d <- df %>%
  mutate(Temperature.C = Temperature - 273) %>%
  group_by(Mechanism, Temperature.C, Reaction, Run, NOx.Condition) %>%
  summarise(Rate = mean(Rate)) %>%
  rowwise() %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition, Reaction) %>%
  mutate(Normalised = normalising_rates(Rate, Normalising.df = norm.data, mechanism = Mechanism, temperature = Temperature.C, run = Run, NOx.condition = NOx.Condition)) %>%
  select(-Rate)

d$Run[d$Run == "Temperature Dependent\nIsoprene Emissions"] <- "TD"
d$Run[d$Run == "Temperature Independent\nIsoprene Emissions"] <- "TI"
# tbl_df(d %>% filter(Run == "TD", NOx.Condition == "High-NOx", Temperature.C == 15, Reaction == "O3"))
d$NOx.Condition <- factor(d$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))
tbl_df(d)

write.table(d, file = "Stagnation_O3_Reactions.csv", sep = ",", row.names = FALSE, quote = FALSE)

d$Type <- rep("Stagnation", length(d$Mechanism))
tbl_df(d)

all.data <- rbind(d %>% ungroup(), mix.data %>% ungroup())

plot <- ggplot(all.data %>% filter(Run == "TD"), aes(x = Temperature.C, y = Normalised, fill = Type))
plot <- plot + geom_bar(stat = "identity", position = "dodge") 
plot <- plot + facet_grid(NOx.Condition ~ Reaction) 
plot <- plot + plot_theme()
# plot <- plot + scale_fill_manual(values = my.colours)
plot
