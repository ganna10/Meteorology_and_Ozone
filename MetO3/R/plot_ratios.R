#' Plot ratios of mixing ratios 2 species
#' Plots of the ratios of the mixing ratios of 2 species in each Mechanism, facetted by NOx.Condition, linetypes are different runs
#' @param df Data frame with plotting data
#' @return The plot
#' @export

plot_ratios <- function (df) {
  my.colours = c("MCMv3.2" = "#6c254f", "CRIv2" = "#ef6638", "MOZART-4" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")
  p <- ggplot(df, aes(x = Temperature.C, y = Ratio, colour = Mechanism, linetype = Run))
  p <- p + geom_line(size = 2)
  p <- p + facet_wrap( ~ NOx.Condition)
  p <- p + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
  p <- p + plot_theme()
  p <- p + scale_colour_manual(values = my.colours)
  return (p)
}