# rate constants
setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//VOC_Oxidation")
temperatures <- seq(288, 313, 0.5)

arr <- function (temperature, A, E) {
  k <- A * exp(E / temperature)
}

arr.temp <- function (temperature, A, E) {
  k <- A * temperature * exp(E / temperature)
}

troe <- function (temperature, A0, B0, AI) {
  k0 <- A0 * (temperature / 300)**B0
  ki <- AI
  g <- (1 + (log10(k0 * 2.5e19 / ki))**2)**(-1)
  d <- 1 + (k0 * 2.5e19)/ki
  k <- k0 * 2.5e19 / (d * 0.6)**g
}

moz.troe <- function (temperature, A0, B0, AI) {
  k0 <- A0 * (temperature / 300)**(B0)
  ki <- AI
  a <- k0 * 2.5e19 / ki
  b <- log10(a)
  m <- k0 * 2.5e19 / (1 + a)
  e <- (1 + b**2)**(-1)
  k <- m * 0.6**e
}

# CB VOCs
par <- rep(8e-13, length(temperatures))
ole <- rep(3.2e-11, length(temperatures))
tol <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1.8e-12, E = 355))
xyl <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1.7e-11, E = 116))
form <- rep(9e-12, length(temperatures))
ald2 <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.6e-12, E = 270))
aldx <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.1e-12, E = 405))
meoh <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 7.3e-12, E = -620))
etoh <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 6.9e-12, E = 230))
facd <- rep(4e-13, length(temperatures))
aacd <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 4e-13, E = 200))
# eth <- do.call("rbind.data.frame", lapply(temperatures, troe, A0 = 1e-28, B0 = -0.8, AI = 8.8e-12))
etha <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 8.7e-12, E = -1070))
iole <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1e-11, E = 550))
isop <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 2.54e-11, E = 407.6))
terp <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1.5e-11, E = 449))

cb.df <- data.frame(temperatures, par, ole, tol, xyl, form, ald2, aldx, meoh, etoh, facd, aacd, etha, iole, isop, terp)
colnames(cb.df) <- c("Temperature", "PAR", "OLE", "TOL", "XYL", "HCHO", "CH3CHO", "ALDX", "MEOH", "ETOH", "FACD", "AACD", "C2H6", "IOLE", "ISOP", "TERP")
tbl_df(cb.df)

cb.gathered <- cb.df %>% 
  gather(VOC, kOH, -Temperature)
cb.gathered$Mechanism <- rep("CB05", length(cb.gathered$VOC))
tbl_df(cb.gathered)

# radm2
eth <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1.37e-17, E = -444))
hc3 <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1.59e-11, E = -540))
hc5 <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1.73e-11, E = -380))
hc8 <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 3.64e-11, E = -380))
ol2 <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 2.15e-12, E = 411))
olt <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.32e-12, E = 504))
oli <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1.07e-11, E = 549))
iso <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 2.55e-11, E = 409))
tol <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 2.1e-12, E = 322))
xyl <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1.89e-11, E = 116))
csl <- rep(4e-11, length(temperatures))
hcho <- rep(9e-12, length(temperatures))
ald <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 6.87e-12, E = 256))
ket <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1.2e-12, E = -745))
mgly <- rep(1.7e-11, length(temperatures))
radm.df <- data.frame(temperatures, eth, hc3, hc5, hc8, ol2, olt, oli, iso, tol, xyl, csl, hcho, ald, ket, mgly)
colnames(radm.df) <- c("Temperature", "C2H6", "HC3", "HC5", "HC8", "C2H4", "OLT", "OLI", "ISOP", "TOL", "XYL", "CSL", "HCHO", "CH3CHO", "KET", "MGLY")
tbl_df(radm.df)
radm.gathered <- radm.df %>%
  gather(VOC, kOH, -Temperature)
radm.gathered$Mechanism <- rep("RADM2", length(temperatures))

# MOZART-4
c2h6 <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 8.7e-12, E = -1070))
c3h8 <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1e-11, E = -660))
bigalk <- rep(2.5e-12, length(temperatures))
c2h4 <- do.call("rbind.data.frame", lapply(temperatures, moz.troe, A0 = 1e-28, B0 = -0.8, AI = 8.8e-12))
c3h6 <- do.call("rbind.data.frame", lapply(temperatures, moz.troe, A0 = 8e-27, B0 = -3.5, AI = 3e-11))
bigene <- rep(5.4e-11, length(temperatures))
toluene <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1.7e-12, E = 352))
ch2o <- rep(9e-12, length(temperatures))
ch3cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.6e-12, E = 270))
macr <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1.86e-11, E = 175))
ch3coho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 8.4e-13, E = 830))
isop <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 2.4e-11, E = 410))
hcooh <- rep(4.5e-12, length(temperatures))
ch3cooh <- rep(7e-13, length(temperatures))
ch3oh <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 7.3e-12, E = -620))
c2h5oh <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 6.9e-12, E = -230))
ch3coch3 <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 3.82e-11, E = -2000))
mek <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 2.3e-12, E = -170)) # +1.33e-13
c10h16 <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1.2e-11, E = 444))

moz.df <- data.frame(temperatures, c2h6, c3h8, bigalk, c2h4, c3h6, bigene, toluene, ch2o, ch3cho, macr, ch3coho, isop, hcooh, ch3cooh, ch3oh, c2h5oh, ch3coch3, mek, c10h16)
colnames(moz.df) <- c("Temperature", "C2H6", "C3H8", "BIGALK", "C2H4", "C3H6", "BIGENE", "TOL", "HCHO", "CH3CHO", "MACR", "MGLY", "ISOP", "FACD", "AACD", "MEOH", "ETOH", "KET", "MEK", "TERP")
moz.gather <- moz.df %>%
  gather(VOC, kOH, -Temperature)
moz.gather$Mechanism <- rep("MOZART-4", length(moz.gather$VOC))
tbl_df(moz.gather)

# mcm
c5h8 <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 2.7e-11, E = 390))

mcm.df <- data.frame(temperatures, c5h8)
colnames(mcm.df) <- c("Temperature", "ISOP")
mcm.gathered <- mcm.df %>%
  gather(VOC, kOH, -Temperature)
mcm.gathered$Mechanism = rep("MCMv3.2", length(mcm.gathered$VOC))
tbl_df(mcm.gathered)

# cri
c5h8 <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 2.54e-11, E = 410))

cri.df <- data.frame(temperatures, c5h8)
colnames(cri.df) <- c("Temperature", "ISOP")
cri.gathered <- cri.df %>%
  gather(VOC, kOH, -Temperature)
cri.gathered$Mechanism = rep("CRIv2", length(cri.gathered$VOC))
tbl_df(cri.gathered)

# all
gathered <- rbind(cb.gathered, radm.gathered, moz.gather, mcm.gathered, cri.gathered)

p <- ggplot(subset(gathered, VOC == "ISOP"), aes(x = Temperature, y = kOH, colour = Mechanism))
p <- p + geom_line(size = 2)
p <- p + plot_theme()
# p <- p + facet_wrap(~ Mechanism)
p
