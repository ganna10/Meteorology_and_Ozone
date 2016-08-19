# Plot contours,  facet run ~ mechanism
# Version 0: Jane Coates 13/11/2015
# Version 1: Jane Coates 21/11/2015 including low and high isoprene emissions to runs
# Version 2: Jane Coates 10/2/2016 changing order of facets
# Version 3: Jane Coates 3/5/2016 Looking at RONO2. Not Working

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//T_Dep_Indep_Comparisons")
runs <- c("Dependent", "Independent")
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2")
spc <- "OH"

list <- lapply(runs, read_mixing_ratio_data, spc = spc, mechanisms = mechanisms)
df <- do.call("rbind", list)
df$Mechanism <- factor(df$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2"))
df$Run <- factor(df$Run, levels = c("Temperature Independent \nIsoprene Emissions", "Temperature Dependent \nIsoprene Emissions"))
tbl_df(df)

p <- plot_contours(df, spc)
p <- p + facet_grid(Run ~ Mechanism)
p <- p + scale_colour_gradient(low = "#000000", high = "#898989")
p <- p + theme(panel.margin.y = unit(3, "mm"))
p

CairoPDF(file = "RONO2_comparison.pdf", width = 11, height = 7)
p1 = direct.label(p, list("top.pieces", cex = 0.7))
p2 = ggplot_gtable(ggplot_build(p1))
p2$layout$clip[p2$layout$name == "panel"] = "off"
print(grid.draw(p2))
dev.off()

