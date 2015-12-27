#' Plot rate of change of O3 with temperature over the different NOx Conditions, overlaid with ERA summer values for particular region
#' Plots of the mean O3 mixing ratio in each Mechanism, facetted by NOx.Condition and Run
#' @param df Data frame with simulated data
#' @param area Subset of ERA grid for comparison
#' @return The plot
#' @export

plot_dO3_dT_comparison_ERA <- function (df, area) {
  my.colours <- c("MCMv3.2" = "#6c254f", "CRIv2" = "#ef6638", "MOZART-4" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")
  
  data.file <- paste0(area, "_O3-T_ERA_data_2007.csv")
  era.data <- read.csv(file = data.file)
  wrf.mozart.data <- read.csv(file = paste0(area, "_wrf_mozart.csv"))
  wrf.radm2.data <- read.csv(file = paste0(area, "_wrf_radm2.csv"))
  plot.title <- paste(area, "2007 ERA Data and WRF-Chem Output")
  
  p <- ggplot(df, aes(x = Temperature.C, y = O3))
  p <- p + facet_grid(NOx.Condition ~ Run)
  p <- p + geom_point(data = era.data, , alpha = 0.08)
  p <- p + geom_point(data = wrf.mozart.data, shape = 0, colour = "#7570b3")
  p <- p + geom_point(data = wrf.radm2.data, shape = 2, colour = "#d95f02")
  p <- p + geom_line(size = 1, aes(colour = Mechanism))
  p <- p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("Maximum O3 8-Hr Mean (ppv)")
  p <- p + plot_theme()
  p <- p + ggtitle(plot.title)
  p <- p + theme(legend.position = "top", legend.title = element_blank())
  p <- p + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
  p <- p + theme(panel.margin = unit(5, "mm"))
  p <- p + scale_colour_manual(values = my.colours)
  
  filename <- paste0("Model_O3-T_vs_", area, "_ERA_and_WRF_2007.pdf")
  CairoPDF(file = filename, width = 10, height = 7)
#   p1 = direct.label(p, list("last.bumpup", cex = 0.7))
#   p2 = ggplot_gtable(ggplot_build(p1))
#   p2$layout$clip[p2$layout$name == "panel"] = "off"
#   print(grid.draw(p2))
  print(p)
  dev.off()
}