#' Plot rate of change of O3 with temperature over the different NOx Conditions
#' Plots of the mean O3 mixing ratio in each Mechanism, facetted by NOx.Condition, linetypes are different runs
#' @param df Data frame with plotting data
#' @return The plot
#' @export

plot_dO3_dT <- function (df) {
  my.colours <- c("MCMv3.2" = "#6c254f", "CRIv2" = "#ef6638", "MOZART-4" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")
  p <- ggplot(df, aes(x = Temperature.C, y = O3, colour = Mechanism))
  p <- p + geom_line(size = 1)
  p <- p + facet_grid(NOx.Condition ~ Run)
  p <- p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("O3 Mixing Ratio (ppv)")
  p <- p + plot_theme()
  p <- p + theme(legend.position = "top", legend.title = element_blank())
  p <- p + scale_x_continuous(limits = c(15, 45), breaks = seq(15, 40, 5), expand = c(0, 0))
  p <- p + theme(panel.margin = unit(5, "mm"))
  p <- p + scale_colour_manual(values = my.colours)
  return (p)
}