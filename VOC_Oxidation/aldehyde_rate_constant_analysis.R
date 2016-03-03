# rate constants
setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//VOC_Oxidation")
temperatures <- seq(288, 313, 0.5)

arr <- function (temperature, A, E) {
  k <- A * exp(E / temperature)
}

arr.temp <- function (temperature, A, E) {
  k <- A * temperature * exp(E / temperature)
}

# hcho
mcm.hcho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.4e-12, E = 135))
cri.hcho <- do.call("rbind.data.frame", lapply(temperatures, arr.temp, A = 1.2e-14, E = 287))
hcho.df <- data.frame(MCM = mcm.hcho, CRI = cri.hcho, MOZART = rep(9e-12, length(temperatures)), RADM2 = rep(9e-12, length(temperatures)), CB05 = rep(9e-12, length(temperatures)), VOC = rep("HCHO", length(temperatures)), Temperature = temperatures)
colnames(hcho.df) <- c("MCM", "CRI", "MOZART", "RADM2", "CB05", "VOC", "Temperature")
hcho.gathered <- hcho.df %>%
  gather(Type, k, -Temperature, -VOC)

# ch3cho
mcm.ch3cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 4.7e-12, E = 345))
cri.ch3cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.55e-12, E = 311))
moz.ch3cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.6e-12, E = 270))
rad.ch3cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 6.87e-12, E = 256))
ch3cho.df <- data.frame(MCM = mcm.ch3cho, CRI = cri.ch3cho, MOZART = moz.ch3cho, RADM2 = rad.ch3cho, CB05 = moz.ch3cho, VOC = rep("CH3CHO", length(temperatures)), Temperature = temperatures)
colnames(ch3cho.df) <- c("MCM", "CRI", "MOZART", "RADM2", "CB05", "VOC", "Temperature")
ch3cho.gathered <- ch3cho.df %>%
  gather(Type, k, -Temperature, -VOC)

# c2h5cho
mcm.c2h5cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 4.9e-12, E = 405))
moz.c2h5cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.6e-12, E = 270))
cb5.c2h5cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.1e-12, E = 405))
rad.c2h5cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 6.87e-12, E = 256))
c2h5cho.df <- data.frame(MCM = mcm.c2h5cho, CRI = rep(1.96e-11, length(temperatures)), MOZART = moz.c2h5cho, RADM2 = rad.c2h5cho, CB05 = cb5.c2h5cho + 8e-13, VOC = rep("C2H5CHO", length(temperatures)), Temperature = temperatures)
colnames(c2h5cho.df) <- c("MCM", "CRI", "MOZART", "RADM2", "CB05", "VOC", "Temperature")
c2h5cho.gathered <- c2h5cho.df %>%
  gather(Type, k, -Temperature, -VOC)

# c3h7cho
mcm.c3h7cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 6e-12, E = 410))
cri.c3h7cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.26e-12, E = 446))
moz.c3h7cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.6e-12, E = 270))
cb5.c3h7cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.1e-12, E = 405))
rad.c3h7cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 6.87e-12, E = 256))
c3h7cho.df <- data.frame(MCM = mcm.c3h7cho, CRI = cri.c3h7cho, MOZART = moz.c3h7cho, RADM2 = rad.c3h7cho, CB05 = cb5.c3h7cho + (2*8e-13), VOC = rep("C3H7CHO", length(temperatures)), Temperature = temperatures)
colnames(c3h7cho.df) <- c("MCM", "CRI", "MOZART", "RADM2", "CB05", "VOC", "Temperature")
c3h7cho.gathered <- c3h7cho.df %>%
  gather(Type, k, -Temperature, -VOC)

# iprcho
mcm.iprcho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 6.8e-12, E = 410))
cri.iprcho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 6.61e-12, E = 411))
moz.iprcho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.6e-12, E = 270))
cb5.iprcho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.1e-12, E = 405))
rad.iprcho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 6.87e-12, E = 256))
iprcho.df <- data.frame(MCM = mcm.iprcho, CRI = cri.iprcho, MOZART = moz.iprcho, RADM2 = rad.iprcho, CB05 = cb5.iprcho + (2*8e-13), VOC = rep("IPRCHO", length(temperatures)), Temperature = temperatures)
colnames(iprcho.df) <- c("MCM", "CRI", "MOZART", "RADM2", "CB05", "VOC", "Temperature")
iprcho.gathered <- iprcho.df %>%
  gather(Type, k, -Temperature, -VOC)

# c4h9cho
mcm.c4h9cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 6.34e-12, E = 448))
cri.c4h9cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 6.34e-12, E = 488))
moz.c4h9cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.6e-12, E = 270))
cb5.c4h9cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.1e-12, E = 405))
rad.c4h9cho <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 6.87e-12, E = 256))
c4h9cho.df <- data.frame(MCM = mcm.c4h9cho, CRI = cri.c4h9cho, MOZART = moz.c4h9cho, RADM2 = rad.c4h9cho, CB05 = cb5.c4h9cho + (3*8e-13), VOC = rep("C4H9CHO", length(temperatures)), Temperature = temperatures)
colnames(c4h9cho.df) <- c("MCM", "CRI", "MOZART", "RADM2", "CB05", "VOC", "Temperature")
c4h9cho.gathered <- c4h9cho.df %>%
  gather(Type, k, -Temperature, -VOC)

# acr
moz.acr <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1.86e-11, E = 75))
cb5.acr <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.1e-12, E = 405))
rad.acr <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 6.87e-12, E = 256))
acr.df <- data.frame(MCM = rep(2e-11, length(temperatures)), CRI = rep(2.5e-11, length(temperatures)), MOZART = moz.acr, RADM2 = rad.acr, CB05 = cb5.acr + (3.2e-11), VOC = rep("ACR", length(temperatures)), Temperature = temperatures)
colnames(acr.df) <- c("MCM", "CRI", "MOZART", "RADM2", "CB05", "VOC", "Temperature")
acr.gathered <- acr.df %>%
  gather(Type, k, -Temperature, -VOC)

# macr
mcm.macr <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 8e-12, E = 380))
moz.macr <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1.86e-11, E = 75))
cb5.macr <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.1e-12, E = 405))
rad.macr <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 6.87e-12, E = 256))
macr.df <- data.frame(MCM = mcm.macr, CRI = rep(2.5e-11, length(temperatures)), MOZART = moz.macr, RADM2 = rad.macr, CB05 = cb5.macr + 8.1e-13 + 3.2e-11, VOC = rep("MACR", length(temperatures)), Temperature = temperatures)
colnames(macr.df) <- c("MCM", "CRI", "MOZART", "RADM2", "CB05", "VOC", "Temperature")
macr.gathered <- macr.df %>%
  gather(Type, k, -Temperature, -VOC)

# c4aldb
moz.c4aldb <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1.86e-11, E = 75))
cb5.c4aldb <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.1e-12, E = 405))
rad.c4aldb <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 6.87e-12, E = 256))
c4aldb.df <- data.frame(MCM = rep(3.4e-11, length(temperatures)), CRI = rep(2.5e-11, length(temperatures)), MOZART = moz.c4aldb, RADM2 = rad.c4aldb, CB05 = cb5.c4aldb + 3.2e-11, VOC = rep("C4ALDB", length(temperatures)), Temperature = temperatures)
colnames(c4aldb.df) <- c("MCM", "CRI", "MOZART", "RADM2", "CB05", "VOC", "Temperature")
c4aldb.gathered <- c4aldb.df %>%
  gather(Type, k, -Temperature, -VOC)

# mglyox
mcm.mglyox <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 1.9e-12, E = 575))
moz.mglyox <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 8.4e-13, E = 830))
cb5.mglyox <- do.call("rbind.data.frame", lapply(temperatures, arr, A = 5.1e-12, E = 405))
mglyox.df <- data.frame(MCM = mcm.mglyox, CRI = rep(1.72e-11, length(temperatures)), MOZART = moz.mglyox, RADM2 = rep(1.7e-11, length(temperatures)), CB05 = cb5.mglyox + 9e-12, VOC = rep("MGLYOX", length(temperatures)), Temperature = temperatures)
colnames(mglyox.df) <- c("MCM", "CRI", "MOZART", "RADM2", "CB05", "VOC", "Temperature")
mglyox.gathered <- mglyox.df %>%
  gather(Type, k, -Temperature, -VOC)

my.colours <- c("MCM" = "#6c254f", "CRI" = "#ef6638", "MOZART" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")

df <- rbind(hcho.gathered, ch3cho.gathered, c2h5cho.gathered, c3h7cho.gathered, iprcho.gathered, c4h9cho.gathered, acr.gathered, macr.gathered, c4aldb.gathered, mglyox.gathered)
ggplot(df, aes(x = Temperature, y = k, colour = Type)) + geom_line(size = 2) + plot_theme() + facet_wrap( ~ VOC, scales = "free") + scale_colour_manual(values = my.colours) + theme(legend.title = element_blank())
