library(reshape2)
library(ggplot2)
library(dplyr)
library(akima)
#library(directlabels)
library(Cairo)
library(ggthemes)

d = read.table(file = "out_Temperature_06092015.csv", header = TRUE, sep  = ",")
d = tbl_df(d)

mozart.data = d %>% filter(Mechanism == "MOZART") %>% select(O3, NOx.Emissions, Temperature)
mozart.data = mozart.data %>% filter(NOx.Emissions <= 7.5e9)
mozart.data = mozart.data %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))

mcm.data = d %>% filter(Mechanism == "MCM") %>% select(O3, NOx.Emissions, Temperature)
mcm.data = mcm.data %>% filter(NOx.Emissions <= 7.5e9) %>% arrange(Temperature)
mcm.data = mcm.data %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))

mozart.fld = with(mozart.data, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = O3))
mozart.df = melt(mozart.fld$z, na.rm = TRUE)
names(mozart.df) = c("x", "y", "O3")
mozart.df$Temperature = mozart.fld$x[mozart.df$x]
mozart.df$NOx.Emissions = mozart.fld$y[mozart.df$y]
mozart.df$Mechanism = rep("MOZART-4", length(mozart.df$O3))

mcm.fld = with(mcm.data, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = O3, duplicate = "strip"))
mcm.df = melt(mcm.fld$z, na.rm = TRUE)
names(mcm.df) = c("x", "y", "O3")
mcm.df$Temperature = mcm.fld$x[mcm.df$x]
mcm.df$NOx.Emissions = mcm.fld$y[mcm.df$y]
mcm.df$Mechanism = rep("MCMv3.2", length(mcm.df$O3))

df = rbind(mcm.df, mozart.df)

get.labels = function (break.points, orig.data, digits) {
    labels = lapply(break.points,
                    function (i) round ((i * (max(orig.data) - min(orig.data))) + min(orig.data), digits )
            )
    labels
}
temperature.break.points = seq(0, 1, 0.2)
temperature.labels = get.labels(temperature.break.points, mozart.data$Temperature, digits = 2)
NOx.Emissions.break.points = seq(0, 1, 0.2)
NOx.Emissions.labels = get.labels(NOx.Emissions.break.points, mozart.data$NOx.Emissions, digits = 2)
NOx.Emissions.labels = lapply(NOx.Emissions.labels, function (i) sprintf("%0.2e", i))

p = ggplot(df, aes(x = Temperature, y = NOx.Emissions, z = O3))
p = p + stat_contour(aes(colour = ..level..)) 
p = p + facet_wrap(~ Mechanism) 
p = p + theme_tufte() 
p = p + theme(axis.line = element_line(colour = "black")) 
p = p + theme(strip.text = element_text(face = "bold")) 
p = p + xlab("Temperature (K)") 
p = p + ylab("NOx emissions (molecules(NOx) cm-3 s-1)") 
p = p + theme(axis.title = element_text(face = "bold")) 
p = p + scale_colour_continuous(name = "O3 (ppbv)")
p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels)
p = p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels)

CairoPDF(file = "plot_temperature.pdf", width = 10, height = 7)
print(p)
#print(direct.label(p))
dev.off()
