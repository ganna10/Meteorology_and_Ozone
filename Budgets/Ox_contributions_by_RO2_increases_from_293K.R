# Determine differences from 293 K of CH3O2, HO2 and CH3CO3 budgets 
# Version 0: Jane Coates 26/11/2015

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "O3"
date <- "10112015"
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4","CB05", "RADM2")

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
d$Run[d$Run == "Temperature Dependent\nIsoprene Emissions"] <- "TD"
d$Run[d$Run == "Temperature Independent\nIsoprene Emissions"] <- "TI"

assigned <- d %>%
  mutate(Reaction = sapply(str_split(Reaction, " \\+ "), "[", 1)) %>%
  filter(Reaction %in% c("HO2", "CH3O2", "CH3CO3")) %>%
  spread(Run, Fraction, drop = FALSE) %>%  
  mutate(Difference = (TD - TI)/TI)
assigned[is.na(assigned)] <- 0
tbl_df(assigned)

my.colours <- c("MCMv3.2" = "#6c254f", "CRIv2" = "#ef6638", "MOZART-4" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")
p <- ggplot(assigned, aes(x = Temperature.C, y = Difference, colour = Mechanism)) + geom_point(size = 2) + facet_grid(Reaction ~ NOx.Condition) + scale_y_continuous(labels = percent) + scale_colour_manual(values = my.colours)
p
CairoPDF(file = "Relative_Difference_Budgets_TI_TD.pdf")
print(p)
dev.off()
