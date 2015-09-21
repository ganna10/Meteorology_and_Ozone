library(reshape2)
library(ggplot2)
library(dplyr)
library(akima)
library(directlabels)
library(Cairo)
library(ggthemes)

d = read.table(file = "out_VOC_06092015.csv", header = TRUE, sep  = ",")
d = tbl_df(d)

mozart.emissions = d %>% filter(Mechanism == "MOZART") %>% select(O3, NOx.Emissions, VOC.Emissions)
mozart.emissions = mozart.emissions %>% filter(NOx.Emissions <= 7.5e9)

mcm.emissions = d %>% filter(Mechanism == "MCM") %>% select(O3, NOx.Emissions, VOC.Emissions)
mcm.emissions = mcm.emissions %>% filter(NOx.Emissions <= 7.5e9)

mozart.fld = with(mozart.emissions, interp(x = VOC.Emissions, y = NOx.Emissions, z = O3))
mozart.df = melt(mozart.fld$z, na.rm = TRUE)
names(mozart.df) = c("x", "y", "O3")
mozart.df$VOC.Emissions = mozart.fld$x[mozart.df$x]
mozart.df$NOx.Emissions = mozart.fld$y[mozart.df$y]
mozart.df$Mechanism = rep("MOZART-4", length(mozart.df$O3))

mcm.fld = with(mcm.emissions, interp(x = VOC.Emissions, y = NOx.Emissions, z = O3))
mcm.df = melt(mcm.fld$z, na.rm = TRUE)
names(mcm.df) = c("x", "y", "O3")
mcm.df$VOC.Emissions = mcm.fld$x[mcm.df$x]
mcm.df$NOx.Emissions = mcm.fld$y[mcm.df$y]
mcm.df$Mechanism = rep("MCMv3.2", length(mcm.df$O3))

head(mcm.df)

df = rbind(mcm.df, mozart.df)

p = ggplot(df, aes(x = VOC.Emissions, y = NOx.Emissions, z = O3))
#p = p + geom_point(aes(colour = O3)) 
p = p + stat_contour(aes(colour = ..level..)) 
p = p + facet_wrap(~ Mechanism) 
p = p + theme_tufte() 
p = p + theme(axis.line = element_line(colour = "black")) 
p = p + theme(strip.text = element_text(face = "bold")) 
p = p + xlab("VOC emissions (molecules(VOC) cm-3 s-1)") 
p = p + ylab("NOx emissions (molecules(NOx) cm-3 s-1)") 
p = p + theme(axis.title = element_text(face = "bold")) 
p = p + scale_colour_continuous(name = "O3 (ppbv)")

CairoPDF(file = "plot_emissions.pdf", width = 10, height = 7)
print(direct.label(p))
dev.off()
