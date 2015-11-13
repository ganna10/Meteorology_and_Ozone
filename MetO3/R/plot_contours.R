#' Contour plots
#' 
#' Plots the contours of spc with Temperature and NOx Emissions
#' @param dataframe Data frame containing the contour data for plotting
#' @param spc The species for which to get contour data
#' @return plot of interpolated mixing ratio data at each Temperature and NOx level
#' @export

plot_contours <- function (dataframe, spc) {
  orig = read.csv(file = "Temperature_Dependent_data.csv") %>% 
    filter(Mechanism == "RADM2") %>% 
    mutate(Temperature.C = Temperature - 273)
  temperature.break.points = seq(0, 1, 0.2)
  temperature.labels = get_contour_axis_label(temperature.break.points, orig$Temperature.C, digits = 2) 
  NOx.Emissions.break.points = seq(0, 1, 0.2)
  NOx.Emissions.labels = get_contour_axis_label(NOx.Emissions.break.points, orig$NOx.Emissions, digits = 2)
  NOx.Emissions.labels = lapply(NOx.Emissions.labels, function (i) sprintf("%0.1e", i))
  
  colnum = match(spc, names(dataframe))
  p = ggplot(dataframe, aes(x = Temperature, y = NOx.Emissions, colour = ..level..))
  p = p + stat_contour(aes_string(z = spc)) 
  p = p + facet_grid(Mechanism ~ Run)
  p = p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("NOx Emissions (molecules cm-3 s-1)")
  p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels)
  p = p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels)
  p = p + theme_tufte()
  p = p + theme(axis.title = element_text(face = "bold"))
  p = p + theme(strip.text = element_text(face = "bold"))
  p = p + theme(strip.text.y = element_text(angle = 0))
  return(p)
}