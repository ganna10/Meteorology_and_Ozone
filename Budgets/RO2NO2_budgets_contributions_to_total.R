# analyse RO2NO2 Budgets, production and consumption separately, plotted as fraction of total Ox production
# Version 0: Jane Coates 09/11/2015
# Version 1: Jane Coates 13/12/2015 using MetO3 package

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "RO2NO2"
date <- "12112015"
mechanisms = c("CB05", "RADM2", "MOZART-4", "CRIv2", "MCMv3.2")

#temperature dependent
td.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TD", Date = date)
td.df <- do.call("rbind", td.list)
tbl_df(td.df)

#temperature independent
ti.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TI", Date = date)
ti.df <- do.call("rbind", ti.list)
tbl_df(ti.df)

df <- rbind(ti.df, td.df)
data <- get_species_budget(df, Reactants = TRUE, Absolute = FALSE)
tbl_df(data)
plot_species_budgets(data, Absolute = FALSE, Stacked = FALSE)

#need to fine tune the direct labelling
# p + geom_dl(aes(label = Reactants), method = list(defaultpf.ggplot("line",,,), cex = 0.7))
# p1 = direct.label(p, list("far.from.others.borders", cex = 0.8))
# p1
# p2 = ggplot_gtable(ggplot_build(p1))
# p2$layout$clip[p2$layout$name == "panel"] = "off"
# grid.draw(p2)