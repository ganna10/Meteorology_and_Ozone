# analyse CH3CHO Budgets
# Version 0: Jane Coates 12/11/2015
# Version 1: Jane Coates 13/11/2015 using MetO3 package

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "CH3CHO"
date <- "11112015"
mechanisms = c("CB05", "RADM2", "MOZART-4", "CRIv2", "MCMv3.2")

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
net.data$NOx.Condition <- factor(net.data$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))
tbl_df(net.data)
levels(factor(net.data$NOx.Condition))

p <- plot_net_budgets(net.data)

cri <- dl.move("CRIv2", vjust = 2.65)
mozart <- dl.move("MOZART-4", vjust = 1, hjust = 2.85)
CairoPDF(file = "net_CH3CHO_budgets.pdf", width = 10, height = 7)
print(direct.label(p, list(top.bumpup, mozart, cri)))
dev.off()

#diff from MCM
max.data <- net.data %>% select(Mechanism, Run, Net.Rate, NOx.Condition) %>% 
  group_by(Run, Mechanism, NOx.Condition) %>%
  summarise(Max.Rate = max(Net.Rate))
tbl_df(max.data)
max.data %>% spread(Mechanism, Max.Rate, drop = FALSE) %>%
  gather(Mechanism, Max.Rate, -NOx.Condition, -MCMv3.2, -Run) %>%
  mutate(Diff.from.MCM = (Max.Rate - MCMv3.2)/MCMv3.2)
