#' get raw budget data
#' 
#' Get the raw data of the budgets of a Species in a Mechanisms from a Run, that was processed at a Date
#' @param Mechanism The mechanism to get the data frame
#' @param Species The species whose budgets are being analysed
#' @param Run.Label Run from which to get the data (TD/, I)
#' @param Date The data processing date (ddmmyyyy)
#' @return Data frame containing raw budget data for further analysis
#' @export

get_budget_data <- function (Mechanism, Species, Run.Label, Date) {
  filename <- paste0(Run.Label, "_", Species, "/", Mechanism, "_", Species, "_budget_", Date, ".txt")
  data <- read.csv(filename)
  if (Run.Label == "TI") {
    data$Run <- rep("Temperature Independent\nIsoprene Emissions", length(data$Mechanism))
  } else if (Run.Label == "TD") {
    data$Run <- rep("Temperature Dependent\nIsoprene Emissions", length(data$Mechanism))
  }
  return(data)
}