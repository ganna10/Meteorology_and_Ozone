# Plots of differences in O3 from NOx and Temperature for each mechanism from MCM. Date of file (ddmmyyyy) are the input args
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

args = commandArgs(trailingOnly = TRUE)

filename = paste0("out_Temperature_NOx_", args[[1]], ".csv")
d = read.csv(file = filename)
d = tbl_df(d)

mcm.data = d %>% filter(Mechanism == "MCM") %>% select(NOx.Emissions, Temperature, O3)
d = d %>% filter(Mechanism != "MCM") %>% select(Mechanism, NOx.Emissions, Temperature, O3)

get.labels = function (break.points, orig.data, digits) {
    labels = lapply(break.points,
                    function (i) round ((i * (max(orig.data) - min(orig.data))) + min(orig.data), digits )
            )
    return (labels)
}

get.data = function (mechanism, dataframe, mcm.data) {
    data = dataframe %>% filter(Mechanism == mechanism)
    data = data %>% mutate(O3.Diff = mcm.data$O3 - data$O3) %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))

    fld = with(data, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = O3.Diff))
    df = melt(fld$z, na.rm = TRUE)
    names(df) = c("x", "y", "O3.Difference")
    df$Temperature = fld$x[df$x]
    df$NOx.Emissions = fld$y[df$y]
    
    if (mechanism == "MOZART") {
        df$Mechanism = rep("MOZART-4", length(df$O3))
    } else if (mechanism == "CRI") {
        df$Mechanism = rep("CRIv2", length(df$O3))
    } else {
        df$Mechanism = rep(mechanism, length(df$O3))
    }
    return (df)
}

mechanisms = c("MOZART", "CRI", "RADM2", "CB05")
mechanism.data = lapply(mechanisms, get.data, dataframe = d, mcm.data = mcm.data) #returns list of dataframes

df = do.call("rbind", mechanism.data) #combining into 1 data frame
#df = tbl_df(df)
#df

mozart.data = d %>% filter(Mechanism == "MOZART") #to get labels
temperature.break.points = seq(0, 1, 0.2)
temperature.labels = get.labels(temperature.break.points, mozart.data$Temperature, digits = 2) 
NOx.Emissions.break.points = seq(0, 1, 0.2)
NOx.Emissions.labels = get.labels(NOx.Emissions.break.points, mozart.data$NOx.Emissions, digits = 2)
NOx.Emissions.labels = lapply(NOx.Emissions.labels, function (i) sprintf("%0.2e", i))

p = ggplot(df, aes(x = Temperature, y = NOx.Emissions, z = O3.Difference))
p = p + stat_contour(aes(colour = ..level..)) 
p = p + facet_wrap(~ Mechanism, scales = "free_x") 
p = p + theme_tufte() 
p = p + ggtitle("Difference in O3 mixing ratios (ppbv) from MCMv3.2")
p = p + theme(plot.title = element_text(face = "bold"))
p = p + theme(axis.line = element_line(colour = "black")) 
p = p + theme(strip.text = element_text(face = "bold")) 
p = p + theme(panel.margin = unit("5", "mm"))
p = p + xlab("Temperature (K)") 
p = p + ylab("NOx emissions (molecules(NOx) cm-3 s-1)") 
p = p + theme(axis.title = element_text(face = "bold")) 
p = p + scale_colour_continuous(name = "O3 (ppbv)")
p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels)
p = p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels)

filename = paste0("plot_O3_differences_temperature_NOx.pdf")
CairoPDF(file = filename, width = 10, height = 7)
print(direct.label(p, list("top.pieces", cex = 0.7)))
dev.off()
