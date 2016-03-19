setwd("~/Documents/Analysis/2015_Meteorology_and_Ozone/Stagnation/Budgets/")
spc <- "HOx"
date <- "18032016"
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "RADM2", "CB05")

#td
td.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TD", Date = date)
td.df <- do.call("rbind", td.list)
tbl_df(td.df)

#ti
ti.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TI", Date = date)
ti.df <- do.call("rbind", ti.list)
tbl_df(ti.df)

df <- rbind(ti.df, td.df)
df$Run[df$Run == "Temperature Dependent\nIsoprene Emissions"] <- "TD"
df$Run[df$Run == "Temperature Independent\nIsoprene Emissions"] <- "TI"
tbl_df(df %>% arrange(Temperature))

stag.df <- df %>%
  mutate(Temperature.C = Temperature - 273) %>%
  group_by(Mechanism, Temperature.C, Category, Run, NOx.Condition) %>%
  summarise(Rate = sum(Rate))
stag.df$Type <- rep("Stagnation", length(stag.df$Mechanism))
tbl_df(stag.df %>% filter(Category == "RO2NO2"))

td <- stag.df %>%
  filter(Run == "TD")
ggplot(td, aes(x = Temperature.C, y = Rate, fill = Category)) + geom_bar(data = subset(td, Rate > 0), stat = "identity") + geom_bar(data = subset(td, Rate < 0), stat = "identity") + facet_grid(Mechanism ~ NOx.Condition) + plot_theme()

mix.data <- read.csv("../../Budgets/Mixing_HOx_budgets.csv")
mix.data$Type <- rep("Mixing", length(mix.data$Mechanism))
tbl_df(mix.data)

all.data <- rbind(stag.df %>% ungroup(), mix.data %>% ungroup())

p <- ggplot(all.data %>% filter(Run == "TI", Mechanism == "CB05"), aes(x = Temperature.C, y = Rate, fill = Type))
p <- p + geom_bar(stat = "identity", position = "dodge")
p <- p + facet_grid(NOx.Condition ~ Category)
p <- p + plot_theme()
p
 