# plot temperature dependence of reaction rate constants
# Version 0: Jane Coates 13/2/2016

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Testing")
temps <- seq(15, 40, 0.5)

ho2_no <- function (temperature) {
  A <- 3.25e-12
  return(A * exp(270 / temperature))
}

ch3o2_no <- function (temperature) {
  A <- 2.3e-12
  return(A * exp(360 / temperature))
}

ch3co3_no <- function (temperature) {
  A <- 7.5e-12
  return(A * exp(290 / temperature))
}

ho2.no.list <- lapply(temps, ho2_no)
ch3o2.no.list <- lapply(temps, ch3o2_no)
ch3co3.no.list <- lapply(temps, ch3co3_no)
df <- data.frame(HO2.NO = unlist(ho2.no.list), CH3O2.NO = unlist(ch3o2.no.list), CH3CO3.NO = unlist(ch3co3.no.list), Temperature = temps)
tbl_df(df)

gathered <- df %>%
  gather(Reaction, K, -Temperature)

p <- ggplot(gathered, aes(x = Temperature, y = K, colour = Reaction))
p <- p + geom_line()
p
