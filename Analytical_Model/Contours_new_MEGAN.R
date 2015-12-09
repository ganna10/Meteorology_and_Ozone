# Plot contours,  facet run ~ mechanism
# Version 0: Jane Coates 08/12/2015

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Analytical_Model/")
# mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2")
mechanisms <- c("CB05")
spc <- "O3"

data.list <- lapply(mechanisms, get_analytical_model_data, Run.Label = "Modified_MEGAN", Date = date)
df <- do.call("rbind", data.list)
tbl_df(df)

data <- df %>% 
  mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)), O3 = O3 * 1e9)
colnum <- match(spc, names(data))
fld <- with(data, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = data[[colnum]]))
df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", spc)
df$Temperature <- fld$x[df$x]
df$NOx.Emissions <- fld$y[df$y] 
df$Mechanism <- rep("CB05", length(df$NOx.Emissions))

p <- ggplot(df, aes(x = Temperature, y = NOx.Emissions, colour = ..level..))
p <- p + stat_contour(aes_string(z = spc), binwidth = 5) 
p <- p + facet_grid( ~ Mechanism)
p <- p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("NOx Emissions (molecules cm-3 s-1)")
# p <- p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels)
# p <- p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels)
p <- p + plot_theme()
p <- p + scale_colour_gradient(low = "#000000", high = "#898989")
p <- p + theme(panel.margin.y = unit(3, "mm"))
direct.label(p)

CairoPDF(file = "O3_contours.pdf")
p1 = direct.label(p, list("top.pieces", cex = 0.7))
p2 = ggplot_gtable(ggplot_build(p1))
p2$layout$clip[p2$layout$name == "panel"] = "off"
print(grid.draw(p2))
dev.off()
