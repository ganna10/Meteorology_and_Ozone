#' get raw data from analytical model variables
#' 
#' Get the raw data of the analytical model of Pusede:2014 for Mechanisms from a Run, that was processed at a Date
#' @param Mechanism The mechanism to get the data frame
#' @param Species The species whose budgets are being analysed
#' @param Run.Label Run from which to get the data (TD/TI)
#' @param Date The data processing date (ddmmyyyy)
#' @return Data frame containing raw data for further analysis
#' @export

get_analytical_model_data <- function (Mechanism, Run.Label, Date) {
  filename <- paste0(Run.Label, "/", Mechanism, "_analytical_model_", Date, ".txt")
  data <- read.csv(filename)
  if (Run.Label == "TI") {
    data$Run <- rep("Temperature Independent\nIsoprene Emissions", length(data$Mechanism))
  } else if (Run.Label == "TD") {
    data$Run <- rep("Temperature Dependent\nIsoprene Emissions", length(data$Mechanism))
  }
  return(data)
}