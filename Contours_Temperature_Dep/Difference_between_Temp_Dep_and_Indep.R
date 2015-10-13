# Plots of differences in O3 from NOx and Temperature for temperature dependent and independent. Temp Dep - Temp Indep. 
# Version 0: Jane Coates 28/9/2015

library(reshape2)
library(methods)
library(grid)
library(quadprog)
library(proto)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(akima)
library(directlabels)
library(Cairo)
library(ggthemes)
library(scales)

temp.dep = read.csv(file = "out_Temperature_NOx_11102015.csv")
temp.dep = tbl_df(temp.dep)
mcm.temp.dep.data = temp.dep %>% filter(Mechanism == "MCM") %>% select(NOx.Emissions, Temperature, O3)

temp.indep = read.csv(file = "../Contours_Temperature_Indep/out_Temperature_NOx_11102015.csv")
temp.indep = tbl_df(temp.indep)
mcm.temp.indep.data = temp.indep %>% filter(Mechanism == "MCM") %>% select(NOx.Emissions, Temperature, O3)

get.labels = function (break.points, orig.data, digits) {
    labels = lapply(break.points,
                    function (i) round ((i * (max(orig.data) - min(orig.data))) + min(orig.data), digits )
            )
    return (labels)
}

data = mcm.temp.dep.data %>% mutate(O3.Diff = mcm.temp.dep.data$O3 - mcm.temp.indep.data$O3) %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))

fld = with(data, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = O3.Diff))
df = melt(fld$z, na.rm = TRUE)
names(df) = c("x", "y", "O3.Difference")
df$Temperature = fld$x[df$x]
df$NOx.Emissions = fld$y[df$y]

p = ggplot(df, aes(x = Temperature, y = NOx.Emissions, z = O3.Difference))
p = p + stat_contour(aes(colour = ..level..)) 
p = p + theme_tufte() 
p = p + ggtitle("Difference in O3 mixing ratios (ppbv) between Temp Dep and Temp Indep emissions in MCMv3.2")
p = p + theme(plot.title = element_text(face = "bold"))
p = p + theme(axis.line = element_line(colour = "black")) 
p = p + theme(strip.text = element_text(face = "bold")) 
p = p + theme(panel.margin = unit("5", "mm"))
p = p + xlab("Temperature (K)") 
p = p + ylab("NOx emissions (molecules(NOx) cm-3 s-1)") 
p = p + theme(axis.title = element_text(face = "bold")) 
p = p + scale_colour_continuous(name = "O3 (ppbv)")

filename = paste0("O3_differences_temperature_NOx_temp_dep_indep.pdf")
CairoPDF(file = filename, width = 10, height = 7)
print(direct.label(p, list("top.pieces", cex = 0.7)))
dev.off()
