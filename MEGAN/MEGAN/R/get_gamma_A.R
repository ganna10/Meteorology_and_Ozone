#' gamma_A value
#' gamma_A = Fnew*Anew + Fgro*Agro + Fmat*Amat + Fsen*Asen
#' Output value of gamma_A
#' @return gamma_A value
#' @export

get_gamma_A <- function () {
  Fnew <- 0
  Anew <- 0.05
  
  Fgro <- 0.1
  Agro <- 0.6
  
  Fmat <- 0.8
  Amat <- 1
    
  Fsen <- 0.1
  Asen <- 0.9
    
  gamma_A <- (Fnew * Anew) + (Fgro * Agro) + (Fmat * Amat) + (Fsen * Asen)
  return (gamma_A)
}