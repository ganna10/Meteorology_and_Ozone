#' gamma_P value
#' 
#' gamma_P = (1 - LDF) + LDF*gamma_LDF
#' gamma_LDF = C_p(alpha * PPFD / (1 + alpha^2*PPFD^2)^(1/2))
#' alpha = 0.004 - 0.0005ln(P_240)
#' C_p = 0.0468*exp(0.0005(P_24 - Ps))*P_240^0.6
#' return value of gamma due to PAR (Photosythetically Active Radiation)
#' @param LDF light dependent factor of BVOC
#' @return gamma_P value
#' @export

get_gamma_P <- function (LDF) {
  # declare constants
  PPFD <- 1233.319209
  P_24 <- PPFD
  P_240 <- PPFD
  P_sun <- 200
  P_shade <- 50
  
  alpha <- 0.004 - (0.0005 * log(P_240))
  C_p_sun <- 0.0468 * exp(0.0005 * (P_240 - P_sun)) * P_240^0.6
  C_p_shade <- 0.0468 * exp(0.0005 * (P_240 - P_shade)) * P_240^0.6
  
  gamma_p_sun <- C_p_sun * (alpha * PPFD / sqrt(1 + (alpha^2 * PPFD^2)))
  gamma_p_shade <- C_p_shade * (alpha * PPFD / sqrt(1 + (alpha^2 * PPFD^2)))
  gamma_P = gamma_p_sun + gamma_p_shade
  return (LDF * gamma_P)
}