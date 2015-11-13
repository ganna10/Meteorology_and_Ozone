# analyse O3 Budgets
# Version 0: Jane Coates 09/11/2015
# Version 1: Jane Coates 13/11/2015 Using MetO3 package

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "O3"
date <- "10112015"
mechanisms <- c("CB05", "RADM2", "MCMv3.2", "CRIv2", "MOZART-4")

#temperature dependent
td.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TD", Date = date)
td.df <- do.call("rbind", td.list)
tbl_df(td.df)

#temperature independent
ti.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TI", Date = date)
ti.df <- do.call("rbind", ti.list)
tbl_df(ti.df)

df <- rbind(ti.df, td.df)
net.data <- get_net_budget_data(df)
tbl_df(net.data)


plot_net_budgets(net.data)
# 
# CairoPDF(file = "Net_Ox_Budgets.pdf", width = 10, height = 7)
# print(p)
# dev.off()

#diff from MCM
max.data <- net.data %>% select(Mechanism, Run, Net.Rate, NOx.Condition) %>% 
  group_by(Run, Mechanism, NOx.Condition) %>%
  summarise(Max.Rate = max(Net.Rate))
tbl_df(max.data)
max.data %>% spread(Mechanism, Max.Rate, drop = FALSE) %>%
  gather(Mechanism, Max.Rate, -NOx.Condition, -MCMv3.2, -Run) %>%
  mutate(Diff.from.MCM = (Max.Rate - MCMv3.2)/MCMv3.2)
