#' isoprene emissions in ug m-2 h-1
#' 
#' C5H8 emissions = gamma_c5h8 * sum(EFs) * Fractional grid coverage
#' return value of isoprene emissions in ug per m2 per h
#' @param gamma_c5h8 gamma for isoprene emissions
#' @param EF emission factor
#' @return isoprene emissions in ug per m2 per h
#' @export

get_c5h8_emissions_ug_m2_h <- function (gamma_c5h8, EF) {
  # declare constants
  F_grid <- 1
  
  c5h8_emissions_ugm2h = gamma_c5h8 * EF * F_grid
  return (c5h8_emissions_ugm2h)
}