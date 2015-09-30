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

species = c("O3", "H2O2", "HNO3", "OH", "HO2", "RONO2", "RO2NO2")

get.labels = function (break.points, orig.data, digits) {
    labels = lapply(break.points,
                    function (i) round ((i * (max(orig.data) - min(orig.data))) + min(orig.data), digits )
            )
    return (labels)
}

get.data = function (mechanism, spc, dataframe) {
    data = dataframe %>% filter(Mechanism == mechanism)
    data = data %>% filter(NOx.Emissions <= 7.5e9)
    data = data %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))

    colnum = match(spc, names(data))
    fld = with(data, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = data[[colnum]], duplicate = "strip"))
    df = melt(fld$z, na.rm = TRUE)
    names(df) = c("x", "y", spc)
    df$Temperature = fld$x[df$x]
    df$NOx.Emissions = fld$y[df$y]
        
    if (mechanism == "MOZART") {
        df$Mechanism = rep("MOZART-4", length(df$Temperature))
    } else if (mechanism == "MCM") {
        df$Mechanism = rep("MCMv3.2", length(df$Temperature))
    } else if (mechanism == "CRI") {
        df$Mechanism = rep("CRIv2", length(df$Temperature))
    } else {
        df$Mechanism = rep(mechanism, length(df$Temperature))
    }
    df = df %>% select(-x, -y) %>% gather(Species, Mixing.Ratio, -Mechanism, -Temperature, -NOx.Emissions)
    return (df)
}

get.plot = function (spc, data) { 
    columns = c("Mechanism", spc, "NOx.Emissions", "Temperature")
    column.numbers = match(columns, names(data))
    data = data %>% select(column.numbers)
    
    mechanisms = c("MCM", "MOZART", "CRI", "RADM2", "CB05")
    mechanism.data = lapply(mechanisms, get.data, spc = spc, dataframe = data) #returns list of dataframes
    
    df = do.call("rbind", mechanism.data) #combining into 1 data frame 
}
df = lapply(species, get.plot, data = d)
plot.df = do.call("rbind", df)

mozart.data = d %>% filter(Mechanism == "MOZART") #to get labels
temperature.break.points = seq(0, 1, 0.2)
temperature.labels = get.labels(temperature.break.points, mozart.data$Temperature, digits = 2) 
NOx.Emissions.break.points = seq(0, 1, 0.2)
NOx.Emissions.labels = get.labels(NOx.Emissions.break.points, mozart.data$NOx.Emissions, digits = 2)
NOx.Emissions.labels = lapply(NOx.Emissions.labels, function (i) sprintf("%0.2e", i))

p = ggplot(plot.df, aes(x = Temperature, y = NOx.Emissions, z = Mixing.Ratio))
p = p + stat_contour(aes(colour = ..level..)) 
p = p + facet_grid(Mechanism ~ Species, scales = "free_x") 
p = p + theme_tufte() 
#p = p + ggtitle(title)
#p = p + theme(plot.title = element_text(face = "bold"))
p = p + theme(axis.line = element_line(colour = "black")) 
p = p + theme(strip.text = element_text(face = "bold")) 
p = p + theme(panel.margin = unit("5", "mm"))
p = p + xlab("Temperature (K)") 
p = p + ylab("NOx emissions (molecules(NOx) cm-3 s-1)") 
p = p + theme(axis.title = element_text(face = "bold")) 
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))
p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels)
p = p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels)
#p = p + geom_vline(xintercept = 0.92)
#p = p + geom_hline(yintercept = 0.1997)

filename = paste0("facet_plot_temperature_NOx.pdf")
CairoPDF(file = filename, width = 15, height = 10)
print(direct.label(p, list("top.pieces", cex = 0.7)))
dev.off()
