#' isoprene emissions in molecules cm-2 s-1
#' 
#' convert C5H8 emissions in ug per m2 per h to molecules cm-2 s-1
#' return value of isoprene emissions in molecules per cm2 per s
#' @param c5h8_emissions_ugm2h isoprene emissions in ug m-2 h-1
#' @return isoprene emissions in molecules per cm2 per s
#' @export

get_c5h8_emissions_molecules_cm2_s <- function (c5h8_emissions_ugm2h) {
  # declare constants
  N_a <- 6.02e23
  m_weight <- 68.117 # g/molecule of isoprene
  
  c5h8_emissions_moleculescm2s <- c5h8_emissions_ugm2h * N_a * 1e-6 / (m_weight * 60 * 60 * 1e6)
  return (c5h8_emissions_moleculescm2s)
}