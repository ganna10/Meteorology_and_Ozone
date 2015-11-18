# analyse O3 Budgets, production and consumption separately, plotted as fraction of total Ox production
# Version 0: Jane Coates 09/11/2015
# Version 1: Jane Coates 18/11/2015 visualising by categories

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "O3"
date <- "10112015"
mechanisms <- c("CB05", "CRIv2", "MCMv3.2", "MOZART-4", "RADM2")

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
d <- get_species_budget(dataframe = subset(df, Rate > 0), Reactants = TRUE, Absolute = FALSE)
tbl_df(d)

d <- d %>%
  mutate(Reaction = sapply(str_split(Reaction, " \\+ "), "[", 1)) %>%
  group_by(Mechanism, Run, Temperature.C, NOx.Condition, Reaction) %>%
  summarise(Fraction = sum(Fraction)) %>%   # sum contributions of same reactants but different products
  spread(Reaction, Fraction, drop = FALSE)

d[is.na(d)] <- 0
tbl_df(d)

d <- d %>%
  gather(Reaction, Fraction, -Mechanism, -Run, -Temperature.C, -NOx.Condition, -HO2, -CH3O2, -CH3CO3) %>%
  group_by(Mechanism, Run, Temperature.C, NOx.Condition, HO2, CH3CO3, CH3O2) %>%
  summarise(RO2 = sum(as.double(Fraction))) %>%
  gather(Reaction, Fraction, -Run, -Mechanism, -Temperature.C, - NOx.Condition)

d$NOx.Condition <- factor(d$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))

my.colours = c("HO2" = "#6c254f", "CH3O2" = "#2b9eb3", "CH3CO3" = "#ef6638", "RO2" = "#0e5c28")

p <- plot_species_budgets(d, Absolute = FALSE, Stacked = FALSE)
p <- p + scale_colour_manual(values = my.colours)
p <- p + theme(legend.position = "top") + theme(panel.margin.y = unit(0.1, "mm"))
p <- direct.label(p, list("last.bumpup", cex = 0.6))
p1 = ggplot_gtable(ggplot_build(p))
p1$layout$clip[p1$layout$name == "panel"] = "off"

CairoPDF(file = "Ox_budgets_Contributions.pdf", width = 10, height = 10)
print(grid.draw(p1))
dev.off()
  