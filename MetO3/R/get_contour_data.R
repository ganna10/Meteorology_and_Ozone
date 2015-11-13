#' Interpolate data of a species over each Temperature and NOx level for the species
#' 
#' Interpolates the data frame of mixing ratio of input species over the range of NOx and Temperatures for a mechanism.
#' @param mechanism Chemical mechanism for which the interpolated data will be calculated
#' @param species The species for which contours will be analysed
#' @param dataframe The data frame included the raw data
#' @return The data frame of mixing ratio data interpolated at each Temperature and NOx level

get_contour_data <- function (mechanism, species, dataframe) {
  data = dataframe %>% filter(Mechanism == mechanism)
  data = data %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))
  colnum = match(species, names(data))
  fld = with(data, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = data[[colnum]]))
  df = melt(fld$z, na.rm = TRUE)
  names(df) = c("x", "y", species)
  df$Temperature = fld$x[df$x]
  df$NOx.Emissions = fld$y[df$y] 
  df$Mechanism = rep(mechanism, length(df$NOx))
  return (df)
}