#' Plot net Budgets 
#' Net Budget Plots of each Mechanism, facetted by NOx.Condition, linetypes are different runs
#' @param df Data frame with plotting data
#' @return The plot
#' @export

plot_net_budgets <- function (df) {
  my.colours = c("MCMv3.2" = "#6c254f", "CRIv2" = "#ef6638", "MOZART-4" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")
  p <- ggplot(net.data, aes(x = Temperature.C, y = Net.Rate, colour = Mechanism, linetype = Run))
  p <- p + geom_line()
  p <- p + facet_wrap( ~ NOx.Condition)
  p <- p + plot_theme()
  p <- p + scale_colour_manual(values = my.colours)
  return (p)
}