# analyse O3 production Budgets, absolute increases and differences
# Version 0: Jane Coates 27/11/2015
# Version 1: Jane Coates 3/12/2015 using Ox = O3 + NO2 + O definition of budget and allocated production reactions to different categories
# Version 2: Jane Coates 9/12/2015 Normalising by total loss rate of VOCs
# Version 3: Jane Coates 10/2/2016 Looking at Net Ox budgets

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "O3"
date <- "01032016"
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2")

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
norm.list.td <- lapply(mechanisms, get_budget_data, Species = "normalising", Run.Label = "TD", Date = "18022016")
norm.td.df <- do.call("rbind", norm.list.td)
tbl_df(norm.td.df)

# temperature independent normalising data
norm.list.ti <- lapply(mechanisms, get_budget_data, Species = "normalising", Run.Label = "TI", Date = "18022016")
norm.ti.df <- do.call("rbind", norm.list.ti)
tbl_df(norm.ti.df)

# normalising data
norm.df <- rbind(norm.td.df, norm.ti.df)
norm.data <- get_normalising_data(norm.df)
tbl_df(norm.data)

d <- df %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature.C = Temperature - 273, Net = Production.Rate + Consumption.Rate) %>%
  group_by(Mechanism, Temperature.C, Category, Run, NOx.Condition) %>%
  summarise(Rate = mean(Net)) %>%
  rowwise() %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition, Category) %>%
  mutate(Rate = normalising_rates(Rate, Normalising.df = norm.data, mechanism = Mechanism, temperature = Temperature.C, run = Run, NOx.condition = NOx.Condition))
d$Run[d$Run == "Temperature Dependent\nIsoprene Emissions"] <- "TD"
d$Run[d$Run == "Temperature Independent\nIsoprene Emissions"] <- "TI"
d$NOx.Condition <- factor(d$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))
tbl_df(d)

# d <- d %>%
#   filter(Category != "Other Organic")
d$Category <- factor(d$Category, levels = c("HO2", "RO2", "ARO2", "Deposition", "Inorganic", "Other Organic", "RO2NO2"))

my.colours = c("Inorganic" = "#b569b3", "Other Organic" = "#2b9eb3", "RO2" = "#ef6638", "ARO2" = "#6c254f", "RO2NO2" = "#f9c500", "HO2" = "#0e5c28", "Deposition" = "#86b650")

# temperature dependent plots
td <- d %>%
  filter(Run == "TD")
net.td <- td %>%
  group_by(Mechanism, NOx.Condition, Temperature.C, Run) %>%
  summarise(Rate = sum(Rate))

td.plot <- ggplot(td, aes(x = Temperature.C, y = Rate))
td.plot <- td.plot + geom_bar(data = subset(td, Rate < 0), aes(fill = Category), stat = "identity", width = 1) 
td.plot <- td.plot + geom_bar(data = subset(td, Rate > 0), aes(fill = Category), stat = "identity", width = 1) 
td.plot <- td.plot + geom_line(data = net.td, colour = "white", size = 1)
td.plot <- td.plot + facet_grid(Mechanism ~ NOx.Condition) 
td.plot <- td.plot + plot_theme()
td.plot <- td.plot + scale_fill_manual(values = my.colours)
# td.plot
td.plot <- td.plot + theme(legend.position = "top")
td.plot <- td.plot + theme(legend.key.width = unit(2.5, "cm"))
td.plot <- td.plot + theme(legend.key.height = unit(0.5, "cm"))
td.plot <- td.plot + theme(legend.title = element_blank())
td.plot <- td.plot + ggtitle("Temperature-Dependent Isoprene Emissions")
td.plot <- td.plot + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
td.plot <- td.plot + scale_y_continuous(limits = c(-2.5, 3), breaks = seq(-2, 3, 1), expand = c(0, 0))
td.plot <- td.plot + scale_fill_manual(values = my.colours)
td.plot <- td.plot + theme(panel.margin = unit(4, "mm"))
td.plot <- td.plot + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
td.plot <- td.plot + ylab("Ox Budget per Loss of Emitted VOC (molecules (Ox)/molecules (VOC))")
td.plot <- td.plot + guides(fill = guide_legend(label.position = "top", nrow = 1))
td.plot

# CairoPDF(file = "TD_Ox_absolute_categorys.pdf", width = 10, height = 7)
# print(p)
# dev.off()

mechanism.colours = c("MCMv3.2" = "#6c254f", "CRIv2" = "#ef6638", "MOZART-4" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")
# net Ox budget - normalised
net.td <- td %>%
  group_by(Mechanism, NOx.Condition, Temperature.C, Run) %>%
  summarise(Rate = sum(Rate))
tbl_df(net.td)

net.plot.td <- ggplot(net.td, aes(x = Temperature.C, y = Rate, colour = Mechanism))
net.plot.td <- net.plot.td + geom_line(size = 1)
net.plot.td <- net.plot.td + facet_grid( ~ NOx.Condition)
net.plot.td <- net.plot.td + plot_theme()
net.plot.td <- net.plot.td + scale_colour_manual(values = mechanism.colours)
net.plot.td <- net.plot.td + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
net.plot.td <- net.plot.td + ylab("Ox Budget per Loss of Emitted VOC (molecules (Ox)/molecules (VOC))")
net.plot.td <- net.plot.td + theme(legend.position = "top")
net.plot.td <- net.plot.td + ggtitle("Temperature-Dependent Isoprene Emissions")
net.plot.td <- net.plot.td + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
net.plot.td <- net.plot.td + scale_y_continuous(limits = c(-0.8, 1.7), breaks = seq(0, 1.5, 0.5), expand = c(0, 0))
net.plot.td <- net.plot.td + theme(panel.margin = unit(4, "mm"))
net.plot.td

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
net.ti <- ti %>%
  group_by(Mechanism, NOx.Condition, Temperature.C, Run) %>%
  summarise(Rate = sum(Rate))
tbl_df(net.ti)

tbl_df(ti)
ti.plot <- ggplot(ti, aes(x = Temperature.C, y = Rate))
ti.plot <- ti.plot + geom_bar(data = subset(ti, Rate > 0), aes(fill = Category), stat = "identity", width = 1) 
ti.plot <- ti.plot + geom_bar(data = subset(ti, Rate < 0), aes(fill = Category), stat = "identity", width = 1) 
ti.plot <- ti.plot + geom_line(data = net.ti, colour = "white", size = 1)
ti.plot <- ti.plot + facet_grid(Mechanism ~ NOx.Condition) 
ti.plot <- ti.plot + scale_fill_manual(values = my.colours)
ti.plot <- ti.plot + plot_theme()
ti.plot <- ti.plot + theme(legend.position = "top")
ti.plot <- ti.plot + theme(legend.key.width = unit(1, "cm"))
ti.plot <- ti.plot + theme(legend.title = element_blank())
ti.plot <- ti.plot + ggtitle("Temperature-Independent Isoprene Emissions")
ti.plot <- ti.plot + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
ti.plot <- ti.plot + scale_y_continuous(limits = c(-2.5, 3), breaks = seq(-2, 3, 1), expand = c(0, 0))
ti.plot <- ti.plot + theme(panel.margin = unit(4, "mm"))
ti.plot <- ti.plot + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
ti.plot <- ti.plot + ylab("Ox Budget per Loss of Emitted VOC (molecules (Ox)/molecules (VOC))")
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

CairoPDF(file = "Ox_Budget_categories.pdf", width = 10, height = 7)
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

# net Ox budget - normalised

net.plot.ti <- ggplot(net.ti, aes(x = Temperature.C, y = Rate, colour = Mechanism))
net.plot.ti <- net.plot.ti + geom_line(size = 1)
net.plot.ti <- net.plot.ti + facet_grid( ~ NOx.Condition)
net.plot.ti <- net.plot.ti + plot_theme()
net.plot.ti <- net.plot.ti + scale_colour_manual(values = mechanism.colours)
net.plot.ti <- net.plot.ti + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
net.plot.ti <- net.plot.ti + ylab("Ox Budget per Loss of Emitted VOC (molecules (Ox)/molecules (VOC))")
net.plot.ti <- net.plot.ti + theme(legend.position = "top")
net.plot.ti <- net.plot.ti + ggtitle("Temperature-Independent Isoprene Emissions")
net.plot.ti <- net.plot.ti + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
net.plot.ti <- net.plot.ti + scale_y_continuous(limits = c(-0.8, 1.7), breaks = seq(0, 1.5, 0.5), expand = c(0, 0))
net.plot.ti <- net.plot.ti + theme(panel.margin = unit(4, "mm"))
net.plot.ti

net.legend <- g_legend(net.plot.td)

CairoPDF(file = "Net_Ox_Budget.pdf", width = 10, height = 7)
print(grid.arrange(net.legend, 
                   arrangeGrob(net.plot.ti + theme(legend.position = "none"), 
                               net.plot.td + theme(axis.title.y = element_blank()) + theme(legend.position = "none"), 
                               nrow = 1), 
                   nrow = 2, 
                   heights = c(0.38, 7)))
dev.off()

# differences TD and TI in increase from 20 to 40°C
filter.net.ti <- net.ti %>%
  filter(Temperature.C == 20.0 | Temperature.C == 40.0)
filter.net.ti$Temperature.C[filter.net.ti$Temperature.C == 20.0] <- "Twenty"
filter.net.ti$Temperature.C[filter.net.ti$Temperature.C == 40.0] <- "Fourty"

all.net.ti <- filter.net.ti %>%
  spread(Temperature.C, Net, drop = FALSE) %>%
  mutate(Increase = Fourty - Twenty) %>%
  select(Mechanism, NOx.Condition, Run, Increase)
all.net.ti

filter.net.td <- net.td %>%
  filter(Temperature.C == 20.0 | Temperature.C == 40.0)
filter.net.td$Temperature.C[filter.net.td$Temperature.C == 20.0] <- "Twenty"
filter.net.td$Temperature.C[filter.net.td$Temperature.C == 40.0] <- "Fourty"

all.net.td <- filter.net.td %>%
  spread(Temperature.C, Net, drop = FALSE) %>%
  mutate(Increase = Fourty - Twenty) %>%
  select(Mechanism, NOx.Condition, Run, Increase)
all.net.td

all.net <- rbind(all.net.ti, all.net.td)
all.net %>%
  spread(Run, Increase) 
%>%
  mutate(Emissions = TD - TI) %>%
  select(Mechanism, NOx.Condition, Chemistry = TI, Emissions) %>%
  gather(Source, Ox.Production, -Mechanism, -NOx.Condition) %>%
  spread(NOx.Condition, Ox.Production, drop = FALSE)

filtered <- d %>%
  filter(Temperature.C == 20.0 | Temperature.C == 40.0)
filtered$Temperature.C[filtered$Temperature.C == 20.0] <- "Twenty"
filtered$Temperature.C[filtered$Temperature.C == 40.0] <- "Fourty"

net <-filtered %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition) %>%
  summarise(Net = sum(Rate))

all <- net %>%
  spread(Temperature.C, Net, drop = FALSE) %>%
  mutate(Increase = Fourty - Twenty) %>%
  select(Mechanism, NOx.Condition, Run, Increase) %>%
  spread(Run, Increase)
all
