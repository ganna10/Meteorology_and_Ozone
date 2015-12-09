#' gamma_T value. gamma_t,LIF = 0 for isoprene
#' 
#' gamma_T = (1 - LDF)gamma_t,LIF + LDF*gamma_t,LDF
#' gamma_t,LDF = Eopt*C_T2*exp(C_T1*x) / (C_T2 - C_T1(1 - exp(C_T2*x)))
#' Eopt = C_eo*exp(0.05(T_24 - T_s))*exp(0.05(T_240 - T_s))
#' x = (1/T_opt - 1/T)/0.00831
#' T_opt = 313 + (0.6(T_240 - T_s))
#' return value of gamma due to temperature
#' @param Temperature (K)
#' @param LDF light dependent factor of BVOC
#' @return gamma_T value
#' @export

get_gamma_T <- function (Temperature, LDF) {
  # declare constants
  C_T1 <- 95
  C_T2 <- 230
  C_eo <- 2
  T_s <- 297
  T_24 <- Temperature
  T_240 <- Temperature
  
  T_opt <- 313 + (0.6 * (T_240 - T_s))
  x <- (1/T_opt - 1/Temperature) / 0.00831
  Eopt <- C_eo * exp(0.05 * (T_24 - T_s)) * exp(0.05 * (T_240 - T_s))
  
  gamma_T = Eopt * C_T2 * exp(C_T1 * x) / (C_T2 - (C_T1 * (1 - exp(C_T2 * x))))
  return (LDF * gamma_T)
}