# analyse O3 Budgets, production and consumption separately, plotted as fraction of total Ox production
# Version 0: Jane Coates 09/11/2015
# Version 1: Jane Coates 18/11/2015 visualising by categories

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "O3"
date <- "10112015"
# mechanisms <- c("CB05", "RADM2", "MOZART-4", "CRIv2", "MCMv3.2")
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
d <- get_species_budget(dataframe = subset(df, Rate > 0), Reactants = TRUE, Absolute = FALSE)
tbl_df(d)

t <- d %>% filter(Mechanism == "MOZART-4")
levels(factor(t$Reaction))

my.colours <- c("HO2 + NO" = "#898989",
                "CH3CO3 + NO" = "#000000",
                "CH3O2 + NO" = "red",
                "CXO3 + NO" = "purple",
                "XO2 + NO" = "blue",
                "C2H5O2 + NO" = "orange",
                "HOCH2CO3 + NO" = "red",
                "RI16O2 + NO" = "green",
                "RN13O2 + NO" = "turquoise",
                "RN15AO2 + NO" = "lightblue",
                "RN16O2 + NO" = "darkblue",
                "RU10O2 + NO" = "maroon",
                "RU11O2 + NO" = "indianred",
                "RU14O2 + NO" = "black",
                "BUTDBO2 + NO" = "darkred",
                "ISOPBO2 + NO" = "yellow",
                "ISOPDO2 + NO" = "brown",
                "NO + SC4H9O2" = "orangered",
                "ALKO2 + NO" = "blue",
                "ENEO2 + NO" = "orange",
                "ISOPO2 + NO" = "green",
                "MACRO2 + NO" = "lightgreen",
                "HC3P + NO" = "yellow",
                "HC5P + NO" = "purple",
                "OLIP + NO" = "orange",
                "OLTP + NO" = "red",
                "Production Others" = "#E89C2C")


cb05.plot <- plot_species_budgets(d %>% filter(Mechanism == "CB05"), Absolute = FALSE, Stacked = TRUE)
# cri.plot <- plot_species_budgets(d %>% filter(Mechanism == "CRIv2"), Absolute = FALSE, Stacked = FALSE)
mcm.plot <- plot_species_budgets(d %>% filter(Mechanism == "MCMv3.2"), Absolute = FALSE, Stacked = TRUE)
# mozart.plot <- plot_species_budgets(d %>% filter(Mechanism == "MOZART-4"), Absolute = FALSE, Stacked = FALSE)
radm2.plot <- plot_species_budgets(d %>% filter(Mechanism == "RADM2"), Absolute = FALSE, Stacked = TRUE)

cb05.plot <- cb05.plot + theme(axis.title.x = element_blank()) + theme(legend.position = "top") + scale_colour_manual(values = my.colours)
# cri.plot <- cri.plot + theme(axis.title.x = element_blank()) + theme(legend.position = "none") + theme(strip.text.x = element_blank()) + scale_colour_manual(values = my.colours)
mcm.plot <- mcm.plot + theme(axis.title.x = element_blank()) + theme(legend.position = "none") + theme(strip.text.x = element_blank()) + scale_colour_manual(values = my.colours)
# mozart.plot <- mozart.plot + theme(axis.title.x = element_blank()) + theme(legend.position = "none") + theme(strip.text.x = element_blank()) + scale_colour_manual(values = my.colours)
radm2.plot <- radm2.plot + theme(legend.position = "none") + theme(strip.text.x = element_blank()) + scale_colour_manual(values = my.colours)

cb05.plot <- direct.label(cb05.plot, list("last.points", cex = 0.75))
# cri.plot <- direct.label(cri.plot, list("last.points", cex = 0.75))
mcm.plot <- direct.label(mcm.plot, list("last.points", cex = 0.75))
# mozart.plot <- direct.label(mozart.plot, list("last.points", cex = 0.75))
radm2.plot <- direct.label(radm2.plot, list("last.points", cex = 0.75))

grid.arrange(cb05.plot, mcm.plot, radm2.plot, ncol = 1)

grid.arrange(cb05.plot, cri.plot, mcm.plot, mozart.plot, radm2.plot, ncol = 1)

p <- p + scale_x_continuous(limits = c(12, 43), breaks = seq(15, 40, 5))
cxo3 <- dl.move("CXO3 + NO", vjust = -1, hjust = 1)
p + geom_dl(aes(y = Fraction, label = Reaction), list("last.points", dl.trans(x = x - 0.5, y = y + 1), cex = 0.7, cxo3))

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