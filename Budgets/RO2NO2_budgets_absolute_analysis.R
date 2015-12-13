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

# temperature dependent normalising data
norm.list.td <- lapply(mechanisms, get_budget_data, Species = "normalising", Run.Label = "TD", Date = "08122015")
norm.td.df <- do.call("rbind", norm.list.td)
tbl_df(norm.td.df)

# temperature independent normalising data
norm.list.ti <- lapply(mechanisms, get_budget_data, Species = "normalising", Run.Label = "TI", Date = "08122015")
norm.ti.df <- do.call("rbind", norm.list.ti)
tbl_df(norm.ti.df)

# normalising data
norm.df <- rbind(norm.td.df, norm.ti.df)
norm.data <- get_normalising_data(norm.df)
tbl_df(norm.data)

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

assigned$Run[assigned$Run == "TD"]  <- "Temperature Dependent\nIsoprene Emissions"
assigned$Run[assigned$Run == "TI"]  <- "Temperature Independent\nIsoprene Emissions"

final <- assigned %>%
  gather(Category, Rate, -Mechanism, -Temperature.C, -Run, -NOx.Condition) %>%
  group_by(Mechanism, Run, Temperature.C, NOx.Condition, Category) %>%
  mutate(Normalised = normalising_rates(Rate, Normalising.df = norm.data, mechanism = Mechanism, temperature = Temperature.C, run = Run, NOx.condition = NOx.Condition))

plot <- final %>%
  select(-Rate)
plot$Category <- factor(plot$Category, levels = c("HO2", "CH3CO3", "Others", "CH3O2"))

my.colours = c("CH3CO3" = "#6c254f", "CH3O2" = "#ef6638", "HO2" = "#2b9eb3", "Others" = "#0e5c28")

# temperature dependent plots
td <- plot %>%
  filter(Run == "Temperature Dependent\nIsoprene Emissions")
td.plot <- ggplot(td, aes(x = Temperature.C, y = Normalised,  fill = Category, order = Category))
td.plot <- td.plot + geom_bar(stat = "identity") 
td.plot <- td.plot + facet_grid(Mechanism ~ NOx.Condition) 
td.plot <- td.plot + plot_theme()
td.plot <- td.plot + theme(legend.position = "top")
td.plot <- td.plot + theme(legend.key.width = unit(2.5, "cm"))
td.plot <- td.plot + theme(legend.key.height = unit(0.5, "cm"))
td.plot <- td.plot + theme(legend.title = element_blank())
td.plot <- td.plot + ggtitle("(a) Temperature-Dependent Isoprene Emissions")
td.plot <- td.plot + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
td.plot <- td.plot + scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1.0), expand = c(0, 0))
td.plot <- td.plot + scale_fill_manual(values = my.colours)
td.plot <- td.plot + theme(panel.margin = unit(4, "mm"))
td.plot <- td.plot + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
td.plot <- td.plot + ylab("Ox Production per Loss of Emitted VOC (molecules (Ox)/molecules (VOC))")
td.plot <- td.plot + guides(fill = guide_legend(label.position = "top"))
td.plot

# CairoPDF(file = "TD_Ox_absolute_categorys.pdf", width = 10, height = 7)
# print(p)
# dev.off()

# temperature dependent plots
ti <- plot %>%
  filter(Run == "Temperature Independent\nIsoprene Emissions")
ti.plot <- ggplot(ti, aes(x = Temperature.C, y = Normalised, fill = Category, order = Category))
ti.plot <- ti.plot + geom_bar(stat = "identity") 
ti.plot <- ti.plot + facet_grid(Mechanism ~ NOx.Condition) 
ti.plot <- ti.plot + plot_theme()
ti.plot <- ti.plot + theme(legend.position = "top")
ti.plot <- ti.plot + theme(legend.key.width = unit(1, "cm"))
ti.plot <- ti.plot + theme(legend.title = element_blank())
ti.plot <- ti.plot + ggtitle("(b) Temperature-Independent Isoprene Emissions")
ti.plot <- ti.plot + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
ti.plot <- ti.plot + scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1), expand = c(0, 0))
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
                   heights = c(0.38, 7)))
dev.off()
plot$Run[plot$Run == "Temperature Dependent\nIsoprene Emissions"] <- "TD"
# increase at 40°C from 20°C
df.increase <- plot %>%
  filter(Temperature.C %in% c(20, 40), Run == "TD") %>%
  rowwise() %>%
  mutate(T.Cond = ifelse(Temperature.C == 20, "Reference", "Maximum")) %>%
  select(-Temperature.C) %>%
  spread(T.Cond, Normalised, drop = FALSE) %>%
  mutate(Difference = Maximum - Reference) %>%
  select(-Maximum, -Reference) %>%
  spread(Run, Difference, drop = FALSE) %>%
  spread(NOx.Condition, TD, drop = FALSE)

df.increase %>%
  gather(NOx.Condition, Increase, -Mechanism, -Category) %>%
  spread(Category, Increase) %>%
  mutate(Rest = HO2 + CH3CO3) %>%
  select(-HO2, -CH3CO3, -Others, -CH3O2) %>%
  spread(NOx.Condition, Rest, drop = FALSE)

ggplot(df.increase, aes(x = Mechanism, y = Difference)) + geom_bar(stat = "identity") + facet_grid(NOx.Condition ~  Category) + plot_theme()

# differences from MCMv3.2
mcm.diff <- plot %>%
  spread(Mechanism, Normalised, drop = FALSE) %>%
  gather(Mechanism, Normalised, -Temperature.C, -Category, -Run, -NOx.Condition, -MCMv3.2) %>%
  mutate(Difference.from.MCM = MCMv3.2 - Normalised) %>%
  select(-MCMv3.2, -Normalised)
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
