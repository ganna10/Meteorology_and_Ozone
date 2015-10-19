# Plots of NOx and Temperature facetted by variable and mechanism. date of file (ddmmyyyy) is the input arg
# Version 0: Jane Coates 29/9/2015

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
library(tidyr)

args = commandArgs(trailingOnly = TRUE)

filename = paste0("out_Temperature_NOx_", args[[1]], ".csv")
d = read.table(file = filename, header = TRUE, sep  = ",")
d = tbl_df(d)

species = c("O3", "H2O2", "HNO3", "OH", "HO2", "RONO2", "RO2NO2", "VOCR")

get.labels = function (break.points, orig.data, digits) {
    labels = lapply(break.points,
                    function (i) round ((i * (max(orig.data) - min(orig.data))) + min(orig.data), digits )
            )
    return (labels)
}

get.data = function (mechanism, spc, dataframe) {
    data = dataframe %>% filter(Mechanism == mechanism) 
    #data = data %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx = (NOx - min(NOx))/(max(NOx) - min(NOx)))

    colnum = match(spc, names(data))
    fld = with(data, interp(x = Temperature, y = NOx, z = data[[colnum]], duplicate = "strip"))
    #fld = with(data, interp(x = Scaled.Temperature, y = Scaled.NOx, z = data[[colnum]], duplicate = "strip"))
    df = melt(fld$z, na.rm = TRUE)
    names(df) = c("x", "y", spc)
    df$Temperature = fld$x[df$x]
    df$NOx = fld$y[df$y]
        
    if (mechanism == "MOZART") {
        df$Mechanism = rep("MOZART-4", length(df$Temperature))
    } else if (mechanism == "MCM") {
        df$Mechanism = rep("MCMv3.2", length(df$Temperature))
    } else if (mechanism == "CRI") {
        df$Mechanism = rep("CRIv2", length(df$Temperature))
    } else {
        df$Mechanism = rep(mechanism, length(df$Temperature))
    }
    df = df %>% select(-x, -y) %>% gather(Species, Mixing.Ratio, -Mechanism, -Temperature, -NOx)
    return (df)
}

get.plot = function (spc, data) { 
    columns = c("Mechanism", spc, "NOx", "Temperature")
    column.numbers = match(columns, names(data))
    data = data %>% select(column.numbers)
    
    mechanisms = c("CB05")
    #mechanisms = c("MCM", "MOZART", "CRI", "RADM2", "CB05")
    mechanism.data = lapply(mechanisms, get.data, spc = spc, dataframe = data) #returns list of dataframes
    
    df = do.call("rbind", mechanism.data) #combining into 1 data frame 
}
df = lapply(species, get.plot, data = d)
plot.df = do.call("rbind", df)

mozart.data = d %>% filter(Mechanism == "CB05") #to get labels
temperature.break.points = seq(0, 1, 0.2)
temperature.labels = get.labels(temperature.break.points, mozart.data$Temperature, digits = 2) 
NOx.break.points = seq(0, 1, 0.2)
NOx.labels = get.labels(NOx.break.points, mozart.data$NOx, digits = 2)
NOx.labels = lapply(NOx.labels, function (i) sprintf("%0.2e", i))

p = ggplot(plot.df, aes(x = Temperature, y = NOx, z = Mixing.Ratio))
p = p + stat_contour(aes(colour = ..level..)) 
p = p + facet_grid(Mechanism ~ Species, scales = "free_x") 
p = p + theme_tufte() 
#p = p + ggtitle(title)
#p = p + theme(plot.title = element_text(face = "bold"))
p = p + theme(axis.line = element_line(colour = "black")) 
p = p + theme(strip.text = element_text(face = "bold")) 
p = p + theme(panel.margin = unit("5", "mm"))
p = p + xlab("Temperature (K)") 
p = p + ylab("NOx Mixing Ratio (ppbv)") 
p = p + theme(axis.title = element_text(face = "bold")) 
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))
#p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels)
#p = p + scale_y_continuous(breaks = NOx.break.points, labels = NOx.labels)
#p = p + geom_vline(xintercept = 0.92)
#p = p + geom_hline(yintercept = 0.1997)

filename = paste0("facet_plot_temperature_NOx.pdf")
CairoPDF(file = filename, width = 15, height = 10)
print(direct.label(p, list("top.pieces", cex = 0.7)))
dev.off()
