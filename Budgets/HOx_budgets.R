# analyse HOx = OH + HO2 + RO2 Budgets
# Version 0: Jane Coates 15/2/2016

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "HOx"
date <- "23022016"
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2")

#temperature dependent HOxdata
td.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TD", Date = date)
td.df <- do.call("rbind", td.list)
tbl_df(td.df)

#temperature independent HOx data
ti.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TI", Date = date)
ti.df <- do.call("rbind", ti.list)
tbl_df(ti.df)

df <- rbind(ti.df, td.df)
tbl_df(df %>% arrange(Temperature))

# temperature dependent normalising data
norm.list.td <- lapply(mechanisms, get_budget_data, Species = "normalising", Run.Label = "TD", Date = "11022016")
norm.td.df <- do.call("rbind", norm.list.td)
tbl_df(norm.td.df)

# temperature independent normalising data
norm.list.ti <- lapply(mechanisms, get_budget_data, Species = "normalising", Run.Label = "TI", Date = "11022016")
norm.ti.df <- do.call("rbind", norm.list.ti)
tbl_df(norm.ti.df)

# normalising data
norm.df <- rbind(norm.td.df, norm.ti.df)
norm.data <- get_normalising_data(norm.df)
tbl_df(norm.data)

d <- df %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature.C = Temperature - 273) %>%
  group_by(Mechanism, Temperature.C, Category, Run, NOx.Condition) %>%
  summarise(Rate = mean(Rate))
%>%
  rowwise() %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition, Category) %>%
  mutate(Rate = normalising_rates(Rate, Normalising.df = norm.data, mechanism = Mechanism, temperature = Temperature.C, run = Run, NOx.condition = NOx.Condition)) 
d$Run[d$Run == "Temperature Dependent\nIsoprene Emissions"] <- "TD"
d$Run[d$Run == "Temperature Independent\nIsoprene Emissions"] <- "TI"
d$NOx.Condition <- factor(d$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))
d$Category <- factor(d$Category, levels = c("O1D", "Carbonyl Photolysis", "Inorganic Photolysis", "Other Photolysis", "VOC Oxidation", "RO2 + RO2", "Inorganic", "Other Organic", "Peroxide Formation", "RO2NO2"))
# write.table(d, file = "Mixing_HOx_budgets.csv", sep = ",", row.names = FALSE, quote = FALSE)

my.colours = c("Carbonyl Photolysis" = "#f9c500", "Other Photolysis" = "#2b9eb3", "Inorganic Photolysis" = "#0e5c28", "RO2 + RO2" = "#b569b3", "VOC Oxidation" = "#ef6638", "O1D" = "#6c254f")
tbl_df(d)
# temperature dependent plots
td <- d %>%
  filter(Run == "TD")
td
td.plot <- ggplot(td, aes(x = Temperature.C, y = Rate, fill = Category))
td.plot <- td.plot + geom_bar(data = subset(td, Rate < 0), stat = "identity", width = 1) 
td.plot <- td.plot + geom_bar(data = subset(td, Rate > 0), stat = "identity", width = 1) 
td.plot <- td.plot + facet_grid(Mechanism ~ NOx.Condition) 
td.plot <- td.plot + plot_theme()
# td.plot <- td.plot + scale_fill_manual(values = my.colours)
td.plot
td.plot <- td.plot + theme(legend.position = "top")
td.plot <- td.plot + theme(legend.key.width = unit(3.5, "cm"))
td.plot <- td.plot + theme(legend.key.height = unit(0.5, "cm"))
td.plot <- td.plot + theme(legend.title = element_blank())
td.plot <- td.plot + ggtitle("Temperature-Dependent Isoprene Emissions")
td.plot <- td.plot + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
# td.plot <- td.plot + scale_y_continuous(limits = c(0, 3e8), breaks = seq(0, 3e8, 1e8), expand = c(0, 0))
td.plot <- td.plot + theme(panel.margin = unit(4, "mm"))
td.plot <- td.plot + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
td.plot <- td.plot + ylab("HOx Production (molecules (HOx))")
td.plot <- td.plot + guides(fill = guide_legend(label.position = "top", nrow = 1))
td.plot

# CairoPDF(file = "TD_Ox_absolute_categorys.pdf", width = 10, height = 7)
# print(p)
# dev.off()

# net budget
# net.td <- td %>%
#   group_by(Mechanism, NOx.Condition, Temperature.C, Run) %>%
#   summarise(Net = sum(Rate))
# tbl_df(net.td)
# 
# net.plot.td <- ggplot(net.td, aes(x = Temperature.C, y = Net))
# net.plot.td <- net.plot.td + geom_bar(stat = "identity")
# net.plot.td <- net.plot.td + facet_grid(Mechanism ~ NOx.Condition)
# net.plot.td <- net.plot.td + plot_theme()
# net.plot.td

# contributions to total
# td.contributions <- td %>%
#   group_by(Mechanism, Run, NOx.Condition, Temperature.C) %>%
#   mutate(Sum = sum(Normalised), Fraction = Normalised/Sum) %>%
#   select(-Rate, -Sum, -Normalised)
# tbl_df(td.contributions)
# td.contributions$NOx.Condition <- factor(td.contributions$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))
# 
# p <- ggplot(td.contributions, aes(x = Temperature.C, y = Fraction, fill = Category, order = Category))
# p <- p + geom_area(position = "stack")
# p <- p + facet_grid(Mechanism ~ NOx.Condition) 
# p <- p + plot_theme()
# p <- p + theme(legend.position = "top")
# p <- p + theme(legend.key.width = unit(2, "cm"))
# p <- p + theme(legend.title = element_blank())
# p <- p + ggtitle("Temperature-Dependent Ox Budgets Fractional Contributions")
# p <- p + theme(panel.margin = unit(4, "mm"))
# p <- p + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
# p <- p + scale_y_continuous(labels = percent, expand = c(0, 0))
# p <- p + scale_fill_manual(values = my.colours)
# p

# CairoPDF(file = "TD_Ox_category_contributions.pdf", width = 10, height = 7)
# print(p)
# dev.off()

# temperature dependent plots
ti <- d %>%
  filter(Run == "TI")
tbl_df(ti)
ti.plot <- ggplot(ti, aes(x = Temperature.C, y = Rate, fill = Category))
ti.plot <- ti.plot + geom_bar(data = subset(ti, Rate > 0), stat = "identity", width = 1) 
ti.plot <- ti.plot + geom_bar(data = subset(ti, Rate < 0), stat = "identity", width = 1) 
ti.plot <- ti.plot + facet_grid(Mechanism ~ NOx.Condition) 
ti.plot <- ti.plot + plot_theme()
# ti.plot
ti.plot <- ti.plot + theme(legend.position = "top")
ti.plot <- ti.plot + theme(legend.key.width = unit(1, "cm"))
ti.plot <- ti.plot + theme(legend.title = element_blank())
ti.plot <- ti.plot + ggtitle("Temperature-Independent Isoprene Emissions")
ti.plot <- ti.plot + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
# ti.plot <- ti.plot + scale_y_continuous(limits = c(0, 3e8), breaks = seq(0, 3e8, 1e8), expand = c(0, 0))
# ti.plot <- ti.plot + scale_fill_manual(values = my.colours)
ti.plot <- ti.plot + theme(panel.margin = unit(4, "mm"))
ti.plot <- ti.plot + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
ti.plot <- ti.plot + ylab("HOx Production Rate (molecules cm-3)")
ti.plot

# grid.arrange td and ti plots together
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
my.legend <- g_legend(td.plot)

# grid.arrange(my.legend, 
#              arrangeGrob(td.plot + theme(legend.position = "none"), 
#                          ti.plot + theme(axis.title.y = element_blank()) + theme(legend.position = "none"), 
#                          nrow = 1), 
#              nrow = 2, 
#              heights = c(0.5, 10))

CairoPDF(file = "HOx_Budget_absolute_categories.pdf", width = 10, height = 7)
print(grid.arrange(my.legend, 
                   arrangeGrob(ti.plot + theme(legend.position = "none"), 
                               td.plot + theme(axis.title.y = element_blank()) + theme(legend.position = "none"), 
                               nrow = 1), 
                   nrow = 2, 
                   heights = c(0.38, 7)))
dev.off()

# CairoPDF(file = "TI_Ox_absolute_categorys.pdf", width = 10, height = 7)
# print(p)
# dev.off()

# contributions to total
contributions <- d %>%
  group_by(Mechanism, Run, NOx.Condition, Temperature.C) %>%
  mutate(Sum = sum(Normalised), Fraction = Normalised/Sum) %>%
  select(-Rate, -Sum, -Normalised)
tbl_df(ti.contributions)
ti.contributions$NOx.Condition <- factor(ti.contributions$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))

p <- ggplot(ti.contributions, aes(x = Temperature.C, y = Fraction, fill = Category, order = Category))
p <- p + geom_area(position = "stack")
p <- p + facet_grid(Mechanism ~ NOx.Condition) 
p <- p + plot_theme()
p <- p + theme(legend.position = "top")
p <- p + theme(legend.key.width = unit(2, "cm"))
p <- p + theme(legend.title = element_blank())
p <- p + ggtitle("Temperature-Independent Ox Budgets Fractional Contributions")
p <- p + theme(panel.margin = unit(4, "mm"))
p <- p + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
p <- p + scale_y_continuous(labels = percent, expand = c(0, 0))
p <- p + scale_fill_manual(values = my.colours)
p
# 
# CairoPDF(file = "TI_Ox_category_contributions.pdf", width = 10, height = 7)
# print(p)
# dev.off()

d$NOx.Condition <- factor(d$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))
d$Category <- factor(d$Category, levels = c("HO2", "RO2", "ARO2", "Other Organic", "RO2NO2", "Inorganic"))
# increase at 40°C from 20°C
df.increase <- d %>%
  filter(Temperature.C %in% c(20, 40)) %>%
  rowwise() %>%
  mutate(T.Cond = ifelse(Temperature.C == 20, "Reference", "Maximum")) %>%
  select(-Temperature.C) %>%
  spread(T.Cond, Normalised, drop = FALSE) %>%
  mutate(Difference = Maximum - Reference) %>%
  select(-Maximum, -Reference) %>%
  spread(Run, Difference, drop = FALSE) %>%
  select(-TI) %>%
  spread(NOx.Condition, TD, drop = FALSE)
print.data.frame(df.increase  %>% filter(Category %in% c("RO2NO2", "Inorganic")))

plot.increase <- df.increase %>%
  gather(NOx.Condition, Increase, -Mechanism, -Category)

# Difference of all radicals from RO2NO2
df.increase %>%
  filter(Category %in% c("ARO2", "RO2", "HO2", "RO2NO2")) %>%
  spread(Category, Increase) %>%
  mutate(Diff.from.RO2NO2 = RO2NO2 - ARO2 - RO2 - HO2) %>%
  select(-ARO2, -RO2, -HO2, -RO2NO2) %>%
  spread(Type, Diff.from.RO2NO2, drop = FALSE)

ggplot(plot.increase, aes(x = Mechanism, y = Increase)) + geom_bar(stat = "identity") + facet_grid(NOx.Condition ~  Category) + plot_theme()

df.increase %>%
  spread(Type, Increase, drop = FALSE)

# differences from MCMv3.2
mcm.diff <- d %>%
  spread(Mechanism, Normalised, drop = FALSE) %>%
  gather(Mechanism, Normalised, -Temperature.C, -Category, -Run, -NOx.Condition, -MCMv3.2) %>%
  mutate(Difference.from.MCM = MCMv3.2 - Normalised) %>%
  select(-MCMv3.2, -Normalised)
mcm.diff.td <- mcm.diff %>%
  filter(Run == "TD")
ggplot(mcm.diff.td, aes(x = Temperature.C, y = Difference.from.MCM, fill = Category, order = Category)) + geom_bar(data = subset(mcm.diff.td, Difference.from.MCM < 0), stat = "identity") + geom_bar(data = subset(mcm.diff, Difference.from.MCM > 0), stat = "identity") + facet_grid(Mechanism ~ NOx.Condition) + plot_theme() + scale_fill_manual(values = my.colours)
