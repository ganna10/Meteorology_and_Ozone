# analyse HOx production Budgets, absolute increases and differences
# Version 0: Jane Coates 8/12/2015

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "HOx"
date <- "07122015"
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
tbl_df(df)
d <- get_species_budget(df, Reactants = TRUE, Absolute = TRUE)
d$Run[d$Run == "Temperature Dependent\nIsoprene Emissions"] <- "TD"
d$Run[d$Run == "Temperature Independent\nIsoprene Emissions"] <- "TI"
tbl_df(d)
d$NOx.Condition <- factor(d$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))

assigned <- d %>%
  rowwise() %>%
  mutate(Reaction = ifelse(Reaction %in% c("CH3O", "IPECO", "HMVKBO", "HO2C5O", "ISOPBO", "ISOPDO", "BUTDBO", "C2H5O", "PEBO", "SC4H9O", "HOCH2CH2O"), "RO", Reaction)) %>%
  rowwise() %>%
  mutate(Reaction = ifelse(Reaction %in% c("ACRPAN", "C3PAN1", "C4PAN1", "C5PAN1", "C5PAN16", "C5PAN2", "C5PAN5", "CO2C3PAN", "GLYPAN", "MPAN", "PBN", "PHAN", "PPN", "TPAN", "PIPN"), "RO2NO2", Reaction)) %>%
  rowwise() %>%
  mutate(Reaction = ifelse(Reaction %in% c("CARB6 + hv", "CH3COCHO + hv", "MGLYOX + hv"), "Carbonyl Photolysis", Reaction)) %>%
  rowwise() %>%
  mutate(Reaction = ifelse(Reaction %in% c("O3 + OLE", "OLI + O3"), "Alkene Ozonolysis", Reaction)) %>%
  rowwise() %>%
  mutate(Reaction = ifelse(Reaction == "ROR", "Others", Reaction))

my.colours = c("Alkene Ozonolysis" = "#ae4901", "Carbonyl Photolysis" = "#e7e85e", "CH3O2 + NO" = "#b569b3", "CH3O2NO2" = "#be2448", "CXO3 + NO" = "#8ed6d2", "HC5 + OH" = "#86b650", "HCHO + hv" = "#f9c500", "HO2NO2" = "#0e5c27", "HONO + hv" = "#0352cb", "O1D" = "#6c254f", "Others" = "#898989", "PAN" = "#ef6638", "RO" = "#0c3f78", "RO2NO2" = "#2b9eb3")

# temperature dependent plots
td <- assigned %>%
  filter(Run == "TD")
td.plot <- ggplot(td, aes(x = Temperature.C, y = Rate, fill = Reaction, order = Reaction))
td.plot <- td.plot + geom_bar(stat = "identity") 
td.plot <- td.plot + facet_grid(Mechanism ~ NOx.Condition) 
td.plot <- td.plot + plot_theme()
td.plot <- td.plot + theme(legend.position = "right")
td.plot <- td.plot + theme(legend.key.width = unit(2, "cm"))
td.plot <- td.plot + theme(legend.title = element_blank())
td.plot <- td.plot + ggtitle("(a) Temperature-Dependent Isoprene Emissions")
td.plot <- td.plot + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
td.plot <- td.plot + scale_y_continuous(limits = c(0, 1e8), breaks = seq(0, 1e8, 2.5e7), expand = c(0, 0))
td.plot <- td.plot + theme(panel.margin = unit(4, "mm"))
td.plot <- td.plot + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
td.plot <- td.plot + ylab("Production Rate (molecules cm-3 s-1)")
td.plot <- td.plot + scale_fill_manual(values = my.colours)
td.plot <- td.plot + guides(fill = guide_legend(label.position = "top"))
td.plot
  
# CairoPDF(file = "TD_Ox_absolute_Reactions.pdf", width = 10, height = 7)
# print(p)
# dev.off()

# contributions to total
td.contributions <- td %>%
  group_by(Mechanism, Run, NOx.Condition, Temperature.C) %>%
  mutate(Sum = sum(Rate), Fraction = Rate/Sum) %>%
  select(-Rate, -Sum)
tbl_df(td.contributions)
td.contributions$NOx.Condition <- factor(td.contributions$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))

p <- ggplot(td.contributions, aes(x = Temperature.C, y = Fraction, fill = Reaction, order = Reaction))
p <- p + geom_area(position = "stack")
p <- p + facet_grid(Mechanism ~ NOx.Condition) 
p <- p + plot_theme()
p <- p + theme(legend.position = "top")
p <- p + theme(legend.key.width = unit(2, "cm"))
p <- p + theme(legend.title = element_blank())
p <- p + ggtitle("Temperature-Dependent Ox Budgets Fractional Contributions")
p <- p + theme(panel.margin = unit(4, "mm"))
p <- p + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
p <- p + scale_y_continuous(labels = percent, expand = c(0, 0))
p <- p + scale_fill_manual(values = my.colours)
p

CairoPDF(file = "TD_Ox_Reaction_contributions.pdf", width = 10, height = 7)
print(p)
dev.off()

# temperature independent plots
ti <- assigned %>%
  filter(Run == "TI")
ti.plot <- ggplot(ti, aes(x = Temperature.C, y = Rate, fill = Reaction, order = Reaction))
ti.plot <- ti.plot + geom_bar(stat = "identity") 
ti.plot <- ti.plot + facet_grid(Mechanism ~ NOx.Condition) 
ti.plot <- ti.plot + plot_theme()
ti.plot <- ti.plot + theme(legend.position = "right")
ti.plot <- ti.plot + theme(legend.key.width = unit(1, "cm"))
ti.plot <- ti.plot + theme(legend.title = element_blank())
ti.plot <- ti.plot + ggtitle("(b) Temperature-Independent Isoprene Emissions")
ti.plot <- ti.plot + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
ti.plot <- ti.plot + scale_y_continuous(limits = c(0, 1e8), breaks = seq(0, 1e8, 2.5e7), expand = c(0, 0))
ti.plot <- ti.plot + theme(panel.margin = unit(4, "mm"))
ti.plot <- ti.plot + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
ti.plot <- ti.plot + ylab("Production Rate (molecules cm-3 s-1)")
ti.plot <- ti.plot + scale_fill_manual(values = my.colours)
ti.plot

# grid.arrange td and ti plots together
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
my.legend <- g_legend(td.plot)

grid.arrange(my.legend, 
             arrangeGrob(td.plot + theme(legend.position = "none"), 
                         ti.plot + theme(axis.title.y = element_blank()) + theme(legend.position = "none"), 
                         nrow = 1), 
             nrow = 2, 
             heights = c(0.5, 10))

CairoPDF(file = "HOx_Budget_absolute_categories.pdf", width = 11, height = 7)
print(grid.arrange(arrangeGrob(td.plot + theme(legend.position = "none"), 
                               ti.plot + theme(axis.title.y = element_blank()) + theme(legend.position = "none"),                              
                               nrow = 1),
                   my.legend,
                   ncol = 2, 
                   widths = c(10, 1.5)))
dev.off()

# CairoPDF(file = "TI_Ox_absolute_Reactions.pdf", width = 10, height = 7)
# print(p)
# dev.off()

# contributions to total
ti.contributions <- ti %>%
  group_by(Mechanism, Run, NOx.Condition, Temperature.C) %>%
  mutate(Sum = sum(Rate), Fraction = Rate/Sum) %>%
  select(-Rate, -Sum)
tbl_df(ti.contributions)
ti.contributions$NOx.Condition <- factor(ti.contributions$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))

p <- ggplot(ti.contributions, aes(x = Temperature.C, y = Fraction, fill = Reaction, order = Reaction))
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

CairoPDF(file = "TI_Ox_Reaction_contributions.pdf", width = 10, height = 7)
print(p)
dev.off()

# increase at 40°C from 20°C
df.increase <- d %>%
  filter(Temperature.C %in% c(20, 40)) %>%
  rowwise() %>%
  mutate(T.Cond = ifelse(Temperature.C == 20, "Reference", "Maximum")) %>%
  select(-Temperature.C) %>%
  spread(T.Cond, Rate, drop = FALSE) %>%
  mutate(Difference = Maximum - Reference) %>%
  select(-Maximum, -Reference) %>%
  spread(Run, Difference, drop = FALSE) %>%
  mutate(Isoprene.Increase = TD - TI) %>%
  select(-TD, Chemistry.Increase = TI) %>%
  gather(Type, Increase, -Mechanism, -Reaction, -NOx.Condition)

# Difference of all radicals from RO2NO2
df.increase %>%
  filter(Reaction %in% c("ARO2", "RO2", "HO2", "RO2NO2")) %>%
  spread(Reaction, Increase) %>%
  mutate(Diff.from.RO2NO2 = RO2NO2 - ARO2 - RO2 - HO2) %>%
  select(-ARO2, -RO2, -HO2, -RO2NO2) %>%
  spread(Type, Diff.from.RO2NO2, drop = FALSE)

ggplot(df.increase, aes(x = Reaction, y = Increase, fill = Type)) + geom_bar(stat = "identity", position = "dodge") + facet_grid(NOx.Condition ~  Mechanism) + plot_theme()

# differences from MCMv3.2
mcm.diff <- d %>%
  spread(Mechanism, Rate, drop = FALSE) %>%
  gather(Mechanism, Rate, -Temperature.C, -Reaction, -Run, -NOx.Condition, -MCMv3.2) %>%
  mutate(Difference.from.MCM = MCMv3.2 - Rate) %>%
  select(-MCMv3.2, -Rate)
mcm.diff.td <- mcm.diff %>%
  filter(Run == "TD")
ggplot(mcm.diff.td, aes(x = Temperature.C, y = Difference.from.MCM, fill = Reaction)) + geom_bar(data = subset(mcm.diff.td, Difference.from.MCM < 0), stat = "identity") + geom_bar(data = subset(mcm.diff, Difference.from.MCM > 0), stat = "identity") + facet_grid(Mechanism ~ NOx.Condition) + plot_theme() + scale_fill_manual(values = my.colours)

# contributions at 40C
d %>%
  filter(Temperature.C == 40) %>%
  spread(Reaction, Rate, drop = FALSE) %>%
  mutate(Diff = (RO2NO2 - HO2)/RO2NO2*100) %>%
  select(Mechanism, Run, NOx.Condition, Diff) %>%
  spread(Run, Diff)
  

