# Plot contours,  facet run ~ mechanism
# Version 0: Jane Coates 13/11/2015

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//T_Dep_Indep_Comparisons")
runs = c("Dependent", "Independent")
mechanisms = c("CB05", "RADM2")
spc = "O3"

list = lapply(runs, read_mixing_ratio_data, spc = spc, mechanisms = mechanisms)
df = do.call("rbind", list)
p = plot_contours(df, spc)
direct.label(p)
