#' Normalise production rates by total loss rate of emitted VOC
#' 
#' Divide the rate of a reaction ny total loss rate of all emitted VOC
#' @param Rate The reaction rate that needs to be normalised
#' @param Normalising.df The df containing the values of the total loss rates of emitted VOC, grouped by Mechanism, Temperature, Run, NO.Condition
#' @param mechanism The mechanism to get the data frame
#' @param temperature The temperatures of the study
#' @param run The Run type (TD or TI)
#' @param NOx.condition The NOx.Condition (Low-NOx, Maximal-O3 or High-NOx)
#' @return normalise The normalised rate
#' @export

normalising_rates <- function (value, Normalising.df, mechanism, temperature, run, NOx.condition) {
  factor <- Normalising.df  %>% 
    filter(Mechanism == mechanism, Temperature.C == temperature, NOx.Condition == NOx.condition, Run == run)
  normalised <- value / factor$VOC.initial.losses
  return(normalised)
}