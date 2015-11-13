#' Get NOx-Condition based on H2O2/HNO3 ratio Sillman:1995
#' 
#' NOx-Condition is determined from H2O2/HNO3 ratio based on values from Sillman:1995.
#' @param x Ratio of H2O2 to HNO3 mixing ratios
#' @return The NOx-Condition associated with ratio

get_NOx_condition <- function (x) {
  if (x > 0.5) {
    condition <- "Low-NOx"
  } else if (x < 0.3) {
    condition <- "High-NOx"
  } else {
    condition <- "Maximal-O3"
  }
  return (condition)
}