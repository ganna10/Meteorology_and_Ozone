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
d = read.csv(file = filename)
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
    data = data %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))

    colnum = match(spc, names(data))
    #fld = with(data, interp(x = Temperature, y = NOx.Emissions, z = data[[colnum]]))
    fld = with(data, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = data[[colnum]], duplicate = "strip"))
    df = melt(fld$z, na.rm = TRUE)
    names(df) = c("x", "y", "O3")
    df$Temperature = fld$x[df$x]
    df$NOx.Emissions = fld$y[df$y]
    
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
    columns = c("Mechanism", spc, "NOx.Emissions", "Temperature")
    column.numbers = match(columns, names(data))
    data = data %>% select(column.numbers)
    
    mechanisms = c("CB05", "MOZART-4", "CRIv2", "RADM2")
#    #mechanisms = c("MCM", "MOZART", "CRI", "RADM2", "CB05")
    mechanism.data = lapply(mechanisms, get.data, spc = spc, dataframe = data) #returns list of dataframes
    
    df = do.call("rbind", mechanism.data) #combining into 1 data frame
    
#    mcm.data = data %>% filter(Mechanism == "CB05") #to get labels
#    temperature.break.points = seq(0, 1, 0.2)
#    temperature.labels = get.labels(temperature.break.points, mcm.data$Temperature, digits = 2) 
#    NOx.Emissions.break.points = seq(0, 1, 0.2)
#    NOx.Emissions.labels = get.labels(NOx.Emissions.break.points, mcm.data$NOx.Emissions, digits = 2)
#    NOx.Emissions.labels = lapply(NOx.Emissions.labels, function (i) sprintf("%0.2e", i))

    if (spc == "O3" | spc == "HNO3" | spc == "HCHO" | spc == "H2O2" | spc == "RO2NO2" | spc == "RONO2") {
        title = paste(spc, "Mixing Ratio (ppbv) Contour Plot")
    } else if (spc == "HO2" | spc == "OH" | spc == "HOx") {
        title = paste(spc, "Mixing Ratio (pptv) Contour Plot")
    } else {
        title = "No title as yet"
    }
    
    p = ggplot(df, aes(x = Temperature, y = NOx.Emissions, z = O3))
    p = p + stat_contour(aes(colour = ..level..)) 
    p = p + facet_wrap(~ Mechanism, scales = "free_x") 
    p = p + theme_tufte() 
    p = p + ggtitle(title)
    p = p + theme(plot.title = element_text(face = "bold"))
    p = p + theme(axis.line = element_line(colour = "black")) 
    p = p + theme(strip.text = element_text(face = "bold")) 
    p = p + theme(panel.margin = unit("5", "mm"))
    p = p + xlab("Temperature (K)") 
    p = p + ylab("NOx.Emissions mixing ratio (ppbv)") 
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
