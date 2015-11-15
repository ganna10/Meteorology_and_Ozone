#' Plots of Budgets of Species
#' 
#' Plotting of budgets of the species. Switches for Absolute or Fractional data and Stacked or non-Stacked lines
#' @param df The data frame with plotting data
#' @param Absolute TRUE => Plot absolute Rates, FALSE => Plot Fractional contributions
#' @param Stacked TRUE => Lines are stacked, FALSE => Lines are not-stacked
#' @return Final plot
#' @export

plot_species_budgets <- function (df, Absolute, Stacked) {
  p <- ggplot(df, aes(x = Temperature.C, colour = Reaction, linetype = Run))
  p <- p + facet_grid(Mechanism ~ NOx.Condition)
  p <- p + scale_x_continuous(expand = c(0, 0))
  p <- p + scale_y_continuous(expand = c(0, 0))
  p <- p + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
  p <- p + plot_theme()
  
  if (Absolute == TRUE & Stacked == FALSE) {
    p <- p + geom_line(data = subset(df, Rate < 0), aes(y = Rate), size = 2)
    p <- p + geom_line(data = subset(df, Rate > 0), aes(y = Rate), size = 2)    
  } else if (Absolute == TRUE & Stacked == TRUE) {
    p <- p + geom_line(data = subset(df, Rate < 0), aes(y = Rate), position = "stack", size = 2)
    p <- p + geom_line(data = subset(df, Rate > 0), aes(y = Rate), position = "stack", size = 2)
  } else if (Absolute == FALSE & Stacked == FALSE) {
    p <- p + geom_line(aes(y = Fraction), ymax = 1, size = 2)
    p <- p + scale_y_continuous(labels = percent, expand = c(0, 0), size = 2)
  } else if (Absolute == FALSE & Stacked == TRUE) {
    p <- p + geom_line(aes(y = Fraction), position = "stack", ymax = 1, size = 2)
    p <- p + scale_y_continuous(labels = percent, expand = c(0, 0), size = 2)
  }  
  return(p)
}