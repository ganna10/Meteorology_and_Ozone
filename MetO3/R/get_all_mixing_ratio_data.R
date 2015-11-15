#' get raw mixing ratio data
#' 
#' Get the raw data of the mixing ratios of all from the Mechanisms from a Run, that was processed at a Date
#' @param Run.Label Run from which to get the data (Indepedent or Dependent)
#' @return Data frame containing raw budget data for further analysis
#' @export

get_all_mixing_ratio_data <- function (Run.Label) {
  filename <- paste0("Temperature_", Run.Label, "_data.csv")
  data <- read.csv(filename)
  if (Run.Label == "Independent") {
    data$Run <- rep("Temperature Independent\nIsoprene Emissions", length(data$Mechanism))
  } else if (Run.Label == "Dependent") {
    data$Run <- rep("Temperature Dependent\nIsoprene Emissions", length(data$Mechanism))
  }
  return(data)
}