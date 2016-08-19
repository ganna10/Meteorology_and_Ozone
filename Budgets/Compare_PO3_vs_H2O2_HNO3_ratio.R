setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Budgets/")
spc <- "O3"
date <- "16032016"

mcm.td.data <- get_budget_data(Mechanism = "MCMv3.2", Species = spc, Run.Label = "TD", Date = date)
tbl_df(mcm.td.data)

data <- mcm.td.data %>%
  filter(Temperature == 303) %>%
  mutate(H2O2.HNO3.ratio = H2O2/HNO3, Net.Prod = Production.Rate + Consumption.Rate) %>%
  select(-H2O2, -HNO3, -Production.Rate, -Consumption.Rate) %>%
  filter(Net.Prod > 0) %>%
  group_by(Mechanism, Temperature, Run, H2O2.HNO3.ratio) %>%
  summarise(Production = sum(Net.Prod)) 

all.data <- read.csv(file = "../Contours_Temperature_Dep/out_Temperature_NOx_14112015.csv")
all.data <- all.data %>% filter(Temperature == 303, Mechanism == "MCMv3.2")
all.NOx <- all.data$NOx.Emissions

data$NOx.Emissions <- all.NOx
data <- data %>%
  gather(Item, Value, -Mechanism, -Temperature, -Run, -NOx.Emissions)

p <- ggplot(data, aes(x = NOx.Emissions, y = Value))
p <- p + geom_line()
p <- p + facet_wrap(~ Item, scales = "free", ncol = 1)
p
