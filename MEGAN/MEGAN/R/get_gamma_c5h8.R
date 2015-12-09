#' gamma value for isoprene emissions
#' 
#' gamma_c5h8 = C_ce * LAI * gamma_p * gamma_t * gamma_a * gamma_sm * gamma_co2
#' return value of final gamma for isoprene emissions
#' @param LAI Leaf Area Index
#' @param gamma_P gamma due to radiation
#' @param gamma_T gamma due to temperature
#' @param gamma_A gamma due to age
#' @param gamma_SM gamma due to soil moisture
#' @param gamma_co2 gamma due to CO2
#' @return gamma_c5h8 value
#' @export

get_gamma_c5h8 <- function (LAI, gamma_P, gamma_T, gamma_A, gamma_SM, gamma_CO2) {
  # declare constants
  C_ce <- 0.3
  
  gamma_c5h8 = C_ce * LAI * gamma_P * gamma_T * gamma_A * gamma_SM * gamma_CO2
  return (gamma_c5h8)
}