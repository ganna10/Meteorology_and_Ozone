# Plots of NOx and Temperature vs variable for each mechanism. date of file (ddmmyyyy) and variable are the input args
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
d = read.table(file = filename, header = TRUE, sep  = ",")
d = tbl_df(d)

species = args[[2]]

get.labels = function (break.points, orig.data, digits) {
    labels = lapply(break.points,
                    function (i) round ((i * (max(orig.data) - min(orig.data))) + min(orig.data), digits )
            )
    return (labels)
}

get.data = function (mechanism, spc, dataframe) {
    data = dataframe %>% filter(Mechanism == mechanism)
    #data = data %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx = (NOx - min(NOx))/(max(NOx) - min(NOx)))
    if (spc == "HOx") {
        data = data %>% mutate(HOx = OH + HO2) %>% select(-OH, -HO2)
    }

    colnum = match(spc, names(data))
    fld = with(data, interp(x = Temperature, y = NOx, z = data[[colnum]]))
    #fld = with(data, interp(x = Scaled.Temperature, y = Scaled.NOx, z = data[[colnum]], duplicate = "strip"))
    df = melt(fld$z, na.rm = TRUE)
    names(df) = c("x", "y", "O3")
    df$Temperature = fld$x[df$x]
    df$NOx = fld$y[df$y]
    
    if (mechanism == "MOZART") {
        df$Mechanism = rep("MOZART-4", length(df$O3))
    } else if (mechanism == "MCM") {
        df$Mechanism = rep("MCMv3.2", length(df$O3))
    } else if (mechanism == "CRI") {
        df$Mechanism = rep("CRIv2", length(df$O3))
    } else {
        df$Mechanism = rep(mechanism, length(df$O3))
    }
    return (df)
}

get.plot = function (spc, data) { 
    if (spc == "HOx") {
        columns = c("Mechanism", "OH", "HO2", "NOx", "Temperature")
    } else {
        columns = c("Mechanism", spc, "NOx", "Temperature")
    }
    column.numbers = match(columns, names(data))
    data = data %>% select(column.numbers)
    
    mechanisms = c("CB05")
#    #mechanisms = c("MCM", "MOZART", "CRI", "RADM2", "CB05")
    mechanism.data = lapply(mechanisms, get.data, spc = spc, dataframe = data) #returns list of dataframes
    
    df = do.call("rbind", mechanism.data) #combining into 1 data frame
    max(df$NOx)
    
#    mcm.data = data %>% filter(Mechanism == "CB05") #to get labels
#    temperature.break.points = seq(0, 1, 0.2)
#    temperature.labels = get.labels(temperature.break.points, mcm.data$Temperature, digits = 2) 
#    NOx.break.points = seq(0, 1, 0.2)
#    NOx.labels = get.labels(NOx.break.points, mcm.data$NOx, digits = 2)
#    NOx.labels = lapply(NOx.labels, function (i) sprintf("%0.2e", i))

    if (spc == "O3" | spc == "HNO3" | spc == "HCHO" | spc == "H2O2" | spc == "RO2NO2" | spc == "RONO2") {
        title = paste(spc, "Mixing Ratio (ppbv) Contour Plot")
    } else if (spc == "HO2" | spc == "OH" | spc == "HOx") {
        title = paste(spc, "Mixing Ratio (pptv) Contour Plot")
    } else {
        title = "No title as yet"
    }
    
    p = ggplot(df, aes(x = Temperature, y = NOx, z = O3))
    p = p + stat_contour(aes(colour = ..level..)) 
    p = p + facet_wrap(~ Mechanism, scales = "free_x") 
    p = p + theme_tufte() 
    p = p + ggtitle(title)
    p = p + theme(plot.title = element_text(face = "bold"))
    p = p + theme(axis.line = element_line(colour = "black")) 
    p = p + theme(strip.text = element_text(face = "bold")) 
    p = p + theme(panel.margin = unit("5", "mm"))
    p = p + xlab("Temperature (K)") 
    p = p + ylab("NOx mixing ratio (ppbv)") 
    p = p + theme(axis.title = element_text(face = "bold")) 
    p = p + scale_colour_continuous(name = "O3 (ppbv)")
#    p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels)
#    p = p + scale_y_continuous(breaks = NOx.break.points, labels = NOx.labels)

    filename = paste0("plot_temperature_NOx_", spc, ".pdf")
    CairoPDF(file = filename, width = 10, height = 7)
    print(direct.label(p, list("top.pieces", cex = 0.7)))
    dev.off()
}
lapply(species, get.plot, data = d)
