# analyse individual reactions of O3 reactions to Ox budget
# Version 0: Jane Coates 13/2/2016

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "O3_reactions"
date <- "13022016"

#temperature dependent o3 data
td.df <- get_budget_data(Mechanism = "MOZART-4", Species = spc, Run.Label = "TD", Date = date)
tbl_df(td.df)

#temperature independent o3 data
ti.df <- get_budget_data(Mechanism = "MOZART-4", Species = spc, Run.Label = "TI", Date = date)
tbl_df(ti.df)

df <- rbind(ti.df, td.df)
tbl_df(df %>% arrange(Temperature))

# temperature dependent normalising data
norm.td.df <- get_budget_data(Mechanism = "MOZART-4", Species = "normalising", Run.Label = "TD", Date = "11022016")
tbl_df(norm.td.df)

# temperature independent normalising data
norm.ti.df <- get_budget_data(Mechanism = "MOZART-4", Species = "normalising", Run.Label = "TI", Date = "11022016")
tbl_df(norm.ti.df)

# normalising data
norm.df <- rbind(norm.td.df, norm.ti.df)
norm.data <- get_normalising_data(norm.df)
tbl_df(norm.data)

d <- df %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature.C = Temperature - 273) %>%
  group_by(Mechanism, Temperature.C, Reaction, Run, NOx.Condition) %>%
  summarise(Rate = mean(Rate)) %>%
  rowwise() %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition, Reaction) %>%
  mutate(Normalised = normalising_rates(Rate, Normalising.df = norm.data, mechanism = Mechanism, temperature = Temperature.C, run = Run, NOx.condition = NOx.Condition)) %>%
  select(-Rate)

d$Run[d$Run == "Temperature Dependent\nIsoprene Emissions"] <- "TD"
d$Run[d$Run == "Temperature Independent\nIsoprene Emissions"] <- "TI"
tbl_df(d %>% filter(Run == "TD", NOx.Condition == "High-NOx", Temperature.C == 15, Reaction == "O3"))
d$NOx.Condition <- factor(d$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))

my.colours <- c("HNO3 + hv" = "#000000", "HO2 + NO3" = "#9bb18d", "HONO + OH" = "#e7e85e", "N2O5" = "#6c254f", "NO2" = "#c65d6c", "NO2 + NO3" = "#0e5c28", "NO2 + O" = "#ba8b01", "NO2 + OH" = "#a67c52", "NO3 + hv" = "#0c3f78", "NO3 + OH" = "#b32448", "NO + NO" = "#dd7983", "NO + NO3" = "#898989", "O1D" = "#c9a415", "O3" = "#8c6238", "O3 + OH" = "#1c3e3d", "O + O3" = "#cc6329", "HO2 + O3" = "#f3aa7f", "NO2 + O3" = "#ae4901")

plot <- ggplot(d, aes(x = Temperature.C, y = Normalised, fill = Reaction, order = Reaction))
plot <- plot + geom_bar(data = subset(d, Normalised < 0), stat = "identity", width = 1) 
plot <- plot + geom_bar(data = subset(d, Normalised > 0), stat = "identity", width = 1) 
plot <- plot + facet_grid(Run ~ NOx.Condition) 
plot <- plot + plot_theme()
# plot <- plot + scale_fill_manual(values = my.colours)
plot
# plot <- plot + theme(legend.position = "top")
# plot <- plot + theme(legend.key.width = unit(2.5, "cm"))
# plot <- plot + theme(legend.key.height = unit(0.5, "cm"))
# plot <- plot + theme(legend.title = element_blank())
# plot <- plot + ggtitle("Temperature-Dependent Isoprene Emissions")
# plot <- plot + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
# plot <- plot + scale_y_continuous(limits = c(0, 7.9), breaks = seq(0, 7, 1), expand = c(0, 0))
# plot <- plot + theme(panel.margin = unit(4, "mm"))
# plot <- plot + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
# plot <- plot + ylab("Ox Production per Loss of Emitted VOC (molecules (Ox)/molecules (VOC))")
# plot <- plot + guides(fill = guide_legend(label.position = "top"))

# overall net effect
net <- d %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition) %>%
  summarise(Net = sum(Normalised))
tbl_df(net)

p <- ggplot(net, aes(x = Temperature.C, y = Net))
p <- p + geom_bar(data = subset(net, Net > 0), stat = "identity")
p <- p + geom_bar(data = subset(net, Net < 0), stat = "identity")
p <- p + facet_grid(Run ~ NOx.Condition)
p <- p + plot_theme()
p