#' isoprene emissions 
#' 
#' script to bring together all the functions to output isoprene emissions in molecules per cm2 per s
#' @param Temperature (K)
#' @param LDF Light Dependent Fraction
#' @param EF Emission Factor
#' @param LAI Leaf Area Index
#' @return isoprene emissions in molecules per cm2 per s
#' @export

C5H8_emissions <- function (Temperature, LDF, EF, LAI) {
  gamma_CO2 <- get_gamma_co2()
  gamma_SM <- get_gamma_sm()
  gamma_A <- get_gamma_A()
  gamma_T <- get_gamma_T(Temperature, LDF)
  gamma_P <- get_gamma_P(LDF)
  gamma_c5h8 <- get_gamma_c5h8(LAI, gamma_P, gamma_T, gamma_A, gamma_SM, gamma_CO2)
  
  c5h8_emissions_ugm2h <- get_c5h8_emissions_ug_m2_h(gamma_c5h8, EF)
  c5h8_emissions_moleculescm2s <- get_c5h8_emissions_molecules_cm2_s(c5h8_emissions_ugm2h)
  return (c5h8_emissions_moleculescm2s)
}