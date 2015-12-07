# analyse RO2NO2 production Budgets, absolute increases and differences
# Version 0: Jane Coates 4/12/2015

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "RO2NO2"
date <- "04122015"
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2")

assign_RO2 <- function (x) {
  if (x == "CH3O2") {
    x = "CH3O2"
  } else if (x == "CH3CO3") {
    x = "CH3CO3"
  } else if (x == "HO2") {
    x = "HO2"
  } else if (x == "NO2") {
    print("error")
  } else {
    x = "Others"
  }
  return(x)
}

get_RO2_assigned <- function (mechanism, df) {
  filtered <- df %>%
    filter(Mechanism == mechanism)
  assigned <- filtered %>% 
    mutate(Reaction = sapply(str_split(Reaction, " \\+ "), "[", 1)) %>%
    rowwise() %>%
    mutate(Reaction = assign_RO2(Reaction))
  return(assigned)
}

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
#d$Reaction <- factor(d$Reaction, levels = c("Inorganic", "Other Organic", "ARO2", "RO2", "HO2", "RO2NO2"))
d$NOx.Condition <- factor(d$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))

production <- d %>%
  filter(Rate > 0)

assigned.list <- lapply(mechanisms, get_RO2_assigned, df = production)
assigned.df <- do.call("rbind", assigned.list)
assigned <- assigned.df %>%
  group_by(Mechanism, Run, Temperature.C, NOx.Condition, Reaction) %>%
  summarise(Rate = sum(Rate)) %>% # sum contributions of same reactants but different products
  spread(Reaction, Rate, drop = FALSE)

assigned[is.na(assigned)] <- 0
tbl_df(assigned)
final <- assigned %>%
  gather(Category, Rate, -Mechanism, -Temperature.C, -Run, -NOx.Condition)

my.colours = c("CH3CO3" = "#6c254f", "CH3O2" = "#ef6638", "HO2" = "#2b9eb3", "Others" = "#0e5c28")

# temperature dependent plots
td <- final %>%
  filter(Run == "TD")
td.plot <- ggplot(td, aes(x = Temperature.C, y = Rate, fill = Category, order = Category))
td.plot <- td.plot + geom_bar(stat = "identity") 
td.plot <- td.plot + facet_grid(Mechanism ~ NOx.Condition) 
td.plot <- td.plot + plot_theme()
td.plot <- td.plot + theme(legend.position = "top")
td.plot <- td.plot + theme(legend.key.width = unit(1, "cm"))
td.plot <- td.plot + theme(legend.title = element_blank())
td.plot <- td.plot + ggtitle("(a) Temperature-Dependent Isoprene Emissions")
td.plot <- td.plot + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
td.plot <- td.plot + scale_y_continuous(limits = c(0, 1.5e9), breaks = seq(0, 1.5e9, 5e8), expand = c(0, 0))
td.plot <- td.plot + scale_fill_manual(values = my.colours)
td.plot <- td.plot + theme(panel.margin = unit(4, "mm"))
td.plot <- td.plot + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
td.plot <- td.plot + ylab("Production Rate (molecules cm-3)")
td.plot

# CairoPDF(file = "TD_Ox_absolute_categorys.pdf", width = 10, height = 7)
# print(p)
# dev.off()

# temperature dependent plots
ti <- final %>%
  filter(Run == "TI")
ti.plot <- ggplot(ti, aes(x = Temperature.C, y = Rate, fill = Category, order = Category))
ti.plot <- ti.plot + geom_bar(stat = "identity") 
ti.plot <- ti.plot + facet_grid(Mechanism ~ NOx.Condition) 
ti.plot <- ti.plot + plot_theme()
ti.plot <- ti.plot + theme(legend.position = "top")
ti.plot <- ti.plot + theme(legend.key.width = unit(1, "cm"))
ti.plot <- ti.plot + theme(legend.title = element_blank())
ti.plot <- ti.plot + ggtitle("(b) Temperature-Independent Isoprene Emissions")
ti.plot <- ti.plot + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
ti.plot <- ti.plot + scale_y_continuous(limits = c(0, 1.5e9), breaks = seq(0, 1.5e9, 5e8), expand = c(0, 0))
ti.plot <- ti.plot + scale_fill_manual(values = my.colours)
ti.plot <- ti.plot + theme(panel.margin = unit(4, "mm"))
ti.plot <- ti.plot + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
ti.plot <- ti.plot + ylab("Production Rate (molecules cm-3)")
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

CairoPDF(file = "RO2NO2_Budget_absolute_categories.pdf", width = 10, height = 7)
print(grid.arrange(my.legend, 
                   arrangeGrob(td.plot + theme(legend.position = "none"), 
                               ti.plot + theme(axis.title.y = element_blank()) + theme(legend.position = "none"), 
                               nrow = 1), 
                   nrow = 2, 
                   heights = c(0.6, 10)))
dev.off()

# increase at 40°C from 20°C
df.increase <- final %>%
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
  gather(Type, Increase, -Mechanism, -Category, -NOx.Condition)

ggplot(df.increase, aes(x = Mechanism, y = Increase, fill = Type)) + geom_bar(stat = "identity", position = "dodge") + facet_grid(NOx.Condition ~  Category) + plot_theme()

# differences from MCMv3.2
mcm.diff <- final %>%
  spread(Mechanism, Rate, drop = FALSE) %>%
  gather(Mechanism, Rate, -Temperature.C, -Category, -Run, -NOx.Condition, -MCMv3.2) %>%
  mutate(Difference.from.MCM = MCMv3.2 - Rate) %>%
  select(-MCMv3.2, -Rate)
mcm.diff.td <- mcm.diff %>%
  filter(Run == "TD")
ggplot(mcm.diff.td, aes(x = Temperature.C, y = Difference.from.MCM, fill = Category)) + geom_bar(data = subset(mcm.diff.td, Difference.from.MCM < 0), stat = "identity") + geom_bar(data = subset(mcm.diff.td, Difference.from.MCM > 0), stat = "identity") + facet_grid(Mechanism ~ NOx.Condition) + plot_theme() + scale_fill_manual(values = my.colours)


# contributions to total
# td.contributions <- td %>%
#   group_by(Mechanism, Run, NOx.Condition, Temperature.C) %>%
#   mutate(Sum = sum(Rate), Fraction = Rate/Sum) %>%
#   select(-Rate, -Sum)
# tbl_df(td.contributions)
# td.contributions$NOx.Condition <- factor(td.contributions$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))
# 
# p <- ggplot(td.contributions, aes(x = Temperature.C, y = Fraction, fill = Reaction, order = Reaction))
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
# 
# CairoPDF(file = "TD_Ox_category_contributions.pdf", width = 10, height = 7)
# print(p)
# dev.off()

# CairoPDF(file = "TI_Ox_absolute_categorys.pdf", width = 10, height = 7)
# print(p)
# dev.off()

# contributions to total
# ti.contributions <- ti %>%
#   group_by(Mechanism, Run, NOx.Condition, Temperature.C) %>%
#   mutate(Sum = sum(Rate), Fraction = Rate/Sum) %>%
#   select(-Rate, -Sum)
# tbl_df(ti.contributions)
# ti.contributions$NOx.Condition <- factor(ti.contributions$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))
# 
# p <- ggplot(ti.contributions, aes(x = Temperature.C, y = Fraction, fill = Reaction, order = Reaction))
# p <- p + geom_area(position = "stack")
# p <- p + facet_grid(Mechanism ~ NOx.Condition) 
# p <- p + plot_theme()
# p <- p + theme(legend.position = "top")
# p <- p + theme(legend.key.width = unit(2, "cm"))
# p <- p + theme(legend.title = element_blank())
# p <- p + ggtitle("Temperature-Independent Ox Budgets Fractional Contributions")
# p <- p + theme(panel.margin = unit(4, "mm"))
# p <- p + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
# p <- p + scale_y_continuous(labels = percent, expand = c(0, 0))
# p <- p + scale_fill_manual(values = my.colours)
# p
# 
# CairoPDF(file = "TI_Ox_category_contributions.pdf", width = 10, height = 7)
# print(p)
# dev.off()
