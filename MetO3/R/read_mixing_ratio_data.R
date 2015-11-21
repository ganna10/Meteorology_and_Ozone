#' Read in mixing ratio data of a run type, output interpolated data for contour plot
#' 
#' Reads in csv data from NOx and T variations from a Run and adds the Run column with information about the Run label. Uses get_contour_data to interpolate the data for a specific species.
#' @param run.label The Run form which to extract data
#' @param spc The species for which to get contour data
#' @param mechanisms The mechanisms for which to get the data
#' @return data frame of interpolated mixing ratio data at each Temperature and NOx level
#' @export

read_mixing_ratio_data <- function (run.label, spc, mechanisms) {
  if (run.label %in% c("Low", "High")) {
    filename <- paste0(run.label, "_Isoprene_emissions.csv")
  } else {
    filename <- paste0("Temperature_", run.label, "_data.csv")
  }
  d <- read.csv(filename)
  data <- lapply(mechanisms, get_contour_data, species = spc, dataframe = d)
  df <- do.call("rbind", data)
  if (run.label == "Low") {
    df$Run <- rep("Low Isoprene Emissions", length(df$Temperature))
  } else if (run.label == "High") {
    df$Run <- rep("High Isoprene Emissions", length(df$Temperature))
  } else {
    df$Run <- rep(paste("Temperature", run.label, "\nIsoprene Emissions"), length(df$Temperature)) 
  }
  return(df)
}