# analyse O3 Budgets
# Version 0: Jane Coates 09/11/2015
# Version 1: Jane Coates 13/11/2015 Using MetO3 package

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "O3"
date <- "10112015"
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2")

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
net.data$Run[net.data$Run == "Temperature Dependent\nIsoprene Emissions"] <- "TD"
net.data$Run[net.data$Run == "Temperature Independent\nIsoprene Emissions"] <- "TI"

p <- plot_net_budgets(net.data)

CairoPDF(file = "Net_Ox_Budgets.pdf", width = 10, height = 7)
print(p)
dev.off()

#diff from MCM
max.data <- net.data %>% select(Mechanism, Run, Net.Rate, NOx.Condition) %>% 
  group_by(Run, Mechanism, NOx.Condition) %>%
  summarise(Max.Rate = max(Net.Rate))
tbl_df(max.data)
diff.maxima <- max.data %>% spread(Mechanism, Max.Rate, drop = FALSE) %>%
  gather(Mechanism, Max.Rate, -NOx.Condition, -MCMv3.2, -Run) %>%
  mutate(Diff.from.MCM = (Max.Rate - MCMv3.2)/MCMv3.2)

#### same patterns as mixing ratios
ggplot(diff.maxima, aes(x = NOx.Condition, y = Diff.from.MCM, colour = Run)) + geom_point(size = 3) + facet_wrap(~ Mechanism, scales = "free") + scale_y_continuous(labels = percent)

# difference between net O3 production from 40 to 20 °C
diff.td.ti <- net.data %>%
  filter(Temperature.C == 20 | Temperature.C == 40) %>%
  rowwise() %>%
  mutate(Temperature.C = ifelse(Temperature.C == 20, "Reference", "Increased")) %>%
  spread(Temperature.C, Net.Rate, drop = FALSE) %>%
  mutate(Difference = (Increased - Reference)/Reference)

### same patterns as mixing ratios
ggplot(diff.td.ti, aes(x = Mechanism, y = Difference, colour = Run)) + geom_point(size = 3) + scale_y_continuous(labels = percent) + facet_wrap(~ NOx.Condition, scales = "free")
