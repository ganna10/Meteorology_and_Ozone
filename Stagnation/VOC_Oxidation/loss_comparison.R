setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Stagnation//VOC_Oxidation")

# stagnation loss
stag.data <- read.csv(file = "All_Loss_Assigned.csv")
stag.data$Type <- rep("Stagnation", length(stag.data$Mechanism))
tbl_df(stag.data)

# mixing loss
mix.data <- read.csv(file = "../../VOC_Oxidation/assigned_data.csv")
mix.data$Type <- rep("Mixing", length(mix.data$Mechanism))
mix.data <- mix.data %>%
  select(-Emission.Rate)
tbl_df(mix.data)

all.data <- rbind(stag.data %>% ungroup(), mix.data %>% ungroup())

p <- ggplot(all.data %>% filter(Run == "TD", Mechanism == "CB05"), aes(x = Temperature.C, y = Oxidation.Rate, fill = Type))
p <- p + geom_bar(stat = "identity", position = "dodge")
p <- p + facet_grid(NOx.Condition ~ Group)
p <- p + plot_theme()
p
  