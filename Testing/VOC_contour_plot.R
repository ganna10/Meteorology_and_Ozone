library(reshape2)
library(ggplot2)
library(dplyr)
library(akima)
library(directlabels)
library(Cairo)

d = read.table(file = "out_29082015.csv", header = TRUE, sep  = ",")
d = tbl_df(d)

mozart = d %>% filter(Mechanism == "MOZART") %>% select(-Mechanism, -VOC)
mozart = mozart %>% filter(NOx <= 50) %>% filter(VOCR <= 250)

fld = with(mozart, interp(x = VOCR, y = NOx, z = O3))
df = melt(fld$z, na.rm = TRUE)
names(df) = c("x", "y", "O3")
df$VOCR = fld$x[df$x]
df$NOx = fld$y[df$y]
head(df)
p = ggplot(df, aes(x = VOCR, y = NOx, z = O3)) + stat_contour(aes(colour = ..level..), bins = 7)
CairoPDF(file = "plot_VOCR.pdf")
print(p)
dev.off()
