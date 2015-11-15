# analyse O3 Budgets, production and consumption separately, plotted as fraction of total Ox production
# Version 0: Jane Coates 09/11/2015

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "O3"
date <- "10112015"
mechanisms = c("CB05", "RADM2", "MOZART-4", "CRIv2", "MCMv3.2")
mechanisms = c("CB05")

#temperature dependent
td.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TD", Date = date)
td.df <- do.call("rbind", td.list)
tbl_df(td.df)

#temperature independent
ti.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TI", Date = date)
ti.df <- do.call("rbind", ti.list)
tbl_df(ti.df)

df <- rbind(ti.df, td.df)
d <- get_species_budget(subset(df, Rate > 0), Reactants = TRUE, Absolute = TRUE)
tbl_df(d)



data <- get_species_budget(full, Reactants = TRUE, Absolute = FALSE)
tbl_df(data)
plot_species_budgets(full, Absolute = TRUE, Stacked = FALSE)
# CairoPDF(file = "Contributions_Ox_budgets.pdf", width = 7, height = 10)
# print(p)
# dev.off()

#compare HO2 + NO contribution to Ox budget
# df = rbind(ti.prod.df, td.prod.df)
# df = df %>% as.data.frame() %>% filter(Reactants == "HO2 + NO")
# tbl_df(df)
# p = ggplot(df, aes(x = Temperature.C, y = Fraction, colour = Mechanism))
# p = p + geom_line(size = 2)
# p = p + facet_grid(NOx.Condition ~ Run)
# p 

#direct.label(p, list("last.points", cex = 0.6))
#direct.label(p, "last.qp")

##need to fine tune the direct labelling
#p + geom_dl(aes(label = Reactants), method = list(defaultpf.ggplot("line",,,), cex = 0.7))
#p1 = direct.label(p, list("far.from.others.borders", cex = 0.8))
#p1
#p2 = ggplot_gtable(ggplot_build(p1))
#p2$layout$clip[p2$layout$name == "panel"] = "off"
#grid.draw(p2)

# Using Others category
# td.df <- td.df %>% select(-Net.Reaction.Rate, -NOx.Mixing.Ratio)
# prod.td <- td.df %>% filter(Rate > 0)
# tbl_df(prod.td)
# cutoff <- 1e8
# sign <- prod.td %>% filter(Rate > cutoff)
# tbl_df(sign)
# others <- prod.td %>% filter(Rate <= cutoff) %>%
#   group_by(Mechanism, H2O2, HNO3, Run, NOx.Emissions, Temperature) %>%
#   summarise(Rate = sum(Rate)) 
# tbl_df(others)
# others$Reaction <- rep("Others", length(others$Run))
# others <- others[c("Mechanism", "Temperature", "H2O2", "HNO3", "NOx.Emissions", "Reaction", "Rate", "Run")]
# full <- rbind(sign, others)
# tbl_df(full)
# levels(factor(full$Reaction))
# data <- get_species_budget(full, Reactants = FALSE, Absolute = TRUE)
# plot_species_budgets(data, Absolute = TRUE, Stacked = FALSE)