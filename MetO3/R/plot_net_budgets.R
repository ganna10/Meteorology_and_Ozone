#' Plot net Budgets 
#' Net Budget Plots of each Mechanism, facetted by NOx.Condition, linetypes are different runs
#' @param df Data frame with plotting data
#' @return The plot
#' @export

plot_net_budgets <- function (df) {
  my.colours = c("MCMv3.2" = "#6c254f", "CRIv2" = "#ef6638", "MOZART-4" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")
  p <- ggplot(net.data, aes(x = Temperature.C, y = Net.Rate, colour = Mechanism, linetype = Run))
  p <- p + geom_line(size = 1)
  p <- p + facet_wrap( ~ NOx.Condition)
  p <- p + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
  p <- p + ylab("Net Production Rate (molecules cm-3)")
  p <- p + plot_theme()
  p <- p + scale_colour_manual(values = my.colours)
  p <- p + guides(linetype = guide_legend(keywidth = 2, keyheight = 1, title = NULL))
  p <- p + theme(legend.position = "top")
  p <- p + scale_x_continuous(limits = c(13.5, 43), breaks = seq(15, 40, 5))
  return (p)
}