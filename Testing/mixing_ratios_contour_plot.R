library(reshape2)
library(ggplot2)
library(dplyr)
library(akima)
library(directlabels)
library(Cairo)

d = read.table(file = "out_29082015.csv", header = TRUE, sep  = ",")
d = tbl_df(d)

mozart = d %>% filter(Mechanism == "MOZART") %>% select(O3, NOx, VOC)
mozart = mozart %>% filter(NOx <= 50)

mcm = d %>% filter(Mechanism == "MCM") %>% select(O3, NOx, VOC)
mcm = mcm %>% filter(NOx <= 50)

mozart.fld = with(mozart, interp(x = VOC, y = NOx, z = O3))
mozart.df = melt(mozart.fld$z, na.rm = TRUE)
names(mozart.df) = c("x", "y", "O3")
mozart.df$VOC = mozart.fld$x[mozart.df$x]
mozart.df$NOx = mozart.fld$y[mozart.df$y]
mozart.df$Mechanism = rep("MOZART-4", length(mozart.df$O3))

mcm.fld = with(mcm, interp(x = VOC, y = NOx, z = O3))
mcm.df = melt(mcm.fld$z, na.rm = TRUE)
names(mcm.df) = c("x", "y", "O3")
mcm.df$VOC = mcm.fld$x[mcm.df$x]
mcm.df$NOx = mcm.fld$y[mcm.df$y]
mcm.df$Mechanism = rep("MCMv3.2", length(mcm.df$O3))

head(mcm.df)

df = rbind(mcm.df, mozart.df)

p = ggplot(df, aes(x = VOC, y = NOx, z = O3)) + stat_contour(aes(colour = ..level..), bins = 7) + facet_wrap(~ Mechanism)
CairoPDF(file = "plot_mixing_ratio,pdf")
print(p)
dev.off()
