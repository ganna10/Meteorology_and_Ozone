# analyse Ox production Budgets, absolute increases and differences
# Version 0: Jane Coates 24/3/2016
# Version 1: Jane Coates 31/7/2016 correcting plotting error and including net line

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
data <- read.csv("Total_Ox_budget.csv")
tbl_df(data)
d <- data %>%
   filter(Category != "Deposition")

d$Mechanism <- factor(d$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2"))
d$Category <- factor(d$Category, levels = c("HO2", "RO2", "ARO2", "Inorganic", "Other Organic", "RO2NO2"))
d$NOx.Condition <- factor(d$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))

my.colours = c("Inorganic" = "#f9c500", "Other Organic" = "#86b650", "RO2" = "#0e5c28", "ARO2" = "#ef6638", "RO2NO2" = "#b569b3", "HO2" = "#6c254f", "Deposition" = "#2b9eb3")

# temperature dependent plots
td <- d %>%
  filter(Run == "TD")
net.td <- td %>%
  group_by(Mechanism, NOx.Condition, Temperature.C, Run) %>%
  summarise(Rate = sum(Rate))

td.plot <- ggplot(td, aes(x = Temperature.C, y = Rate))
td.plot <- td.plot + geom_bar(data = td %>% filter(Rate > 0) %>% arrange(Category), aes(fill = Category), stat = "identity", width = 1) 
td.plot <- td.plot + geom_bar(data = td %>% filter(Rate < 0) %>% arrange(Category), aes(fill = Category), stat = "identity", width = 1) 
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
td.plot <- td.plot + scale_y_continuous(limits = c(-5e8, 1.5e9), breaks = seq(-5e8, 1.5e9, 5e8), expand = c(0, 0))
td.plot <- td.plot + scale_fill_manual(values = my.colours)
td.plot <- td.plot + theme(panel.margin = unit(4, "mm"))
td.plot <- td.plot + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
td.plot <- td.plot + ylab("Ox Budget per Loss of Emitted VOC (molecules (Ox)/molecules (VOC))")
td.plot <- td.plot + guides(fill = guide_legend(label.position = "top", nrow = 1))
td.plot <- td.plot + theme(axis.line.y = element_line(colour = "black"))
td.plot <- td.plot + theme(axis.line.x = element_line(colour = "black"))
td.plot

# temperature independent plots
ti <- d %>%
  filter(Run == "TI")
net.ti <- ti %>%
  group_by(Mechanism, NOx.Condition, Temperature.C, Run) %>%
  summarise(Rate = sum(Rate))

ti.plot <- ggplot(ti, aes(x = Temperature.C, y = Rate))
ti.plot <- ti.plot + geom_bar(data = ti %>% filter(Rate > 0) %>% arrange(Category), aes(fill = Category), stat = "identity", width = 1) 
ti.plot <- ti.plot + geom_bar(data = ti %>% filter(Rate < 0) %>% arrange(Category), aes(fill = Category), stat = "identity", width = 1) 
ti.plot <- ti.plot + geom_line(data = net.ti, colour = "white", size = 1)
ti.plot <- ti.plot + facet_grid(Mechanism ~ NOx.Condition) 
ti.plot <- ti.plot + scale_fill_manual(values = my.colours)
ti.plot <- ti.plot + plot_theme()
ti.plot <- ti.plot + theme(legend.position = "top")
ti.plot <- ti.plot + theme(legend.key.width = unit(1, "cm"))
ti.plot <- ti.plot + theme(legend.title = element_blank())
ti.plot <- ti.plot + ggtitle("Temperature-Independent Isoprene Emissions")
ti.plot <- ti.plot + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
ti.plot <- ti.plot + scale_y_continuous(limits = c(-5e8, 1.5e9), breaks = seq(-5e8, 1.5e9, 5e8), expand = c(0, 0))
ti.plot <- ti.plot + theme(panel.margin = unit(4, "mm"))
ti.plot <- ti.plot + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
ti.plot <- ti.plot + ylab("Ox Budget (molecules (Ox) cm-3)")
ti.plot <- ti.plot + theme(axis.line.y = element_line(colour = "black"))
ti.plot <- ti.plot + theme(axis.line.x = element_line(colour = "black"))
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

CairoPDF(file = "Absolute_Ox_Budget_categories.pdf", width = 10, height = 7)
print(grid.arrange(my.legend, 
                   arrangeGrob(ti.plot + theme(legend.position = "none"), 
                               td.plot + theme(axis.title.y = element_blank()) + theme(legend.position = "none"), 
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

## contribution increases/decreases from 20 to 40
diff.df <- d %>%
  filter(Temperature.C == 20 | Temperature.C == 40) %>%
  rowwise() %>%
  mutate(Temperature.C = ifelse(Temperature.C == 20, "Twenty", "Fourty")) %>%
  spread(Temperature.C, Rate) %>%
  mutate(Diff = Fourty - Twenty) %>%
  select(-Fourty, -Twenty)

diff.df %>%
  filter(Category == "RO2") %>%
  spread(Run, Diff)

# total production
d %>%
  filter(Rate > 0) %>%
  filter(Temperature.C == 20 | Temperature.C == 40) %>%
  rowwise() %>%
  mutate(Temperature.C = ifelse(Temperature.C == 20, "Twenty", "Fourty")) %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition) %>%
  summarise(Rate = sum(Rate)) %>%
  spread(Temperature.C, Rate) %>%
  mutate(Diff = Fourty - Twenty) %>%
  select(-Fourty, -Twenty) %>%
  spread(Run, Diff)

# total consumption
d %>%
  filter(Rate < 0) %>%
  filter(Temperature.C == 20 | Temperature.C == 40) %>%
  rowwise() %>%
  mutate(Temperature.C = ifelse(Temperature.C == 20, "Twenty", "Fourty")) %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition) %>%
  summarise(Rate = sum(Rate)) %>%
  spread(Temperature.C, Rate) %>%
  mutate(Diff = Fourty - Twenty) %>%
  select(-Fourty, -Twenty) %>%
  spread(Run, Diff)
