#' Plots of Budgets of Species, stacked area
#' 
#' Plotting of budgets of the species. Switches for Absolute or Fractional data 
#' @param df The data frame with plotting data
#' @param Absolute TRUE => Plot absolute Rates, FALSE => Plot Fractional contributions
#' @return Final plot
#' @export

plot_species_budgets_stacked_area <- function (df, Absolute) {
  p <- ggplot(df, aes(x = Temperature.C, colour = Reaction))
    
  p <- p + facet_grid(Mechanism ~ NOx.Condition)
  p <- p + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
  p <- p + plot_theme()
  p <- p + theme(axis.title.y = element_blank())
  p <- p + theme(panel.margin = unit(5, "mm"))
  p <- p + ggtitle(levels(factor(df$Run)))
  p <- p + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
    
  if (Absolute == TRUE) {
    p <- p + geom_area(aes(y = Rate), position = "stack")
  } else if (Absolute == FALSE) {
    p <- p + geom_line(aes(y = Fraction), , ymax = 1, size = 1)
    p <- p + scale_y_continuous(labels = percent, expand = c(0, 0))
  }  
  return(p)
}