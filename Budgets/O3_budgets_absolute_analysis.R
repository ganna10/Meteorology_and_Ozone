# analyse O3 production Budgets, absolute increases and differences
# Version 0: Jane Coates 27/11/2015
# Version 1: Jane Coates 27/11/2015 using O3 production budgets normalised by NMVOC emissions

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "O3"
date <- "27112015"
mechanisms <- c("CB05")

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
d <- get_species_budget(dataframe = subset(df, Rate > 0), Reactants = TRUE, Absolute = TRUE)
d$Run[d$Run == "Temperature Dependent\nIsoprene Emissions"] <- "TD"
d$Run[d$Run == "Temperature Independent\nIsoprene Emissions"] <- "TI"
tbl_df(d)

# total production
my.colours <- c("MCMv3.2" = "#6c254f", "CRIv2" = "#ef6638", "MOZART-4" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")
total.prod <- d %>%
  group_by(Mechanism, Run, Temperature.C, NOx.Condition) %>%
  summarise(Total = sum(Rate))
ggplot(total.prod, aes(x = Temperature.C, y = Total, colour = Mechanism, linetype = Run)) + geom_line(size = 2) + facet_wrap( ~ NOx.Condition) + plot_theme() + scale_colour_manual(values = my.colours)


# difference between T = 40°C and T = 15°C
difference <- final %>%
  filter(Temperature.C == 15 | Temperature.C == 40) %>%
  rowwise() %>%
  mutate(Temperature.C = ifelse(Temperature.C == 15, "Low", "High")) %>%
  spread(Temperature.C, Fraction, drop = FALSE) %>%
  mutate(Difference = (High - Low)/Low) %>%
  select(-High, -Low)

p <- ggplot(difference, aes(x = NOx.Condition, y = Difference, colour = Mechanism))
p <- p + geom_point(size = 3)
p <- p + facet_grid(Reaction ~ Run)
p <- p + plot_theme()
p <- p + scale_colour_manual(values = my.colours)
p <- p + theme(legend.title = element_blank(), legend.position = "top")
p
# CairoPDF(file = "Differences_RO2_by_NOx.pdf", width = 10, height = 7)
# print(p)
# dev.off()