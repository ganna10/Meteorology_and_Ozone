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

d = read.table(file = "out_Temperature_23092015.csv", header = TRUE, sep  = ",")
d = tbl_df(d)

species = c("O3", "HNO3", "HO2", "OH", "H2O2", "HCHO", "HOx")

get.labels = function (break.points, orig.data, digits) {
    labels = lapply(break.points,
                    function (i) round ((i * (max(orig.data) - min(orig.data))) + min(orig.data), digits )
            )
    labels
}

get.plot = function (spc, data) { 
    if (spc == "HOx") {
        columns = c("OH", "HO2", "NOx.Emissions", "Temperature")
    } else {
        columns = c(spc, "NOx.Emissions", "Temperature")
    }
    column.numbers = match(columns, names(data))
    
    mozart.data = d %>% filter(Mechanism == "MOZART") %>% select(column.numbers)
    mozart.data = mozart.data %>% filter(NOx.Emissions <= 7.5e9)
    mozart.data = mozart.data %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))
    if (spc == "HOx") {
        mozart.data = mozart.data %>% mutate(HOx = OH + HO2) %>% select(-OH, -HO2)
    }
    
    #mcm.data = d %>% filter(Mechanism == "MCM") %>% select(O3, NOx.Emissions, Temperature)
    #mcm.data = mcm.data %>% filter(NOx.Emissions <= 7.5e9) %>% arrange(Temperature)
    #mcm.data = mcm.data %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))

    mozart.colnum = match(spc, names(mozart.data))
    mozart.fld = with(mozart.data, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = mozart.data[[mozart.colnum]]))
    mozart.df = melt(mozart.fld$z, na.rm = TRUE)
    names(mozart.df) = c("x", "y", "O3")
    mozart.df$Temperature = mozart.fld$x[mozart.df$x]
    mozart.df$NOx.Emissions = mozart.fld$y[mozart.df$y]
    mozart.df$Mechanism = rep("MOZART-4", length(mozart.df$O3))

    #mcm.fld = with(mcm.data, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = O3, duplicate = "strip"))
    #mcm.df = melt(mcm.fld$z, na.rm = TRUE)
    #names(mcm.df) = c("x", "y", "O3")
    #mcm.df$Temperature = mcm.fld$x[mcm.df$x]
    #mcm.df$NOx.Emissions = mcm.fld$y[mcm.df$y]
    #mcm.df$Mechanism = rep("MCMv3.2", length(mcm.df$O3))
    
    #df = rbind(mcm.df, mozart.df)
    df = mozart.df
    
    temperature.break.points = seq(0, 1, 0.2)
    temperature.labels = get.labels(temperature.break.points, mozart.data$Temperature, digits = 2)
    NOx.Emissions.break.points = seq(0, 1, 0.2)
    NOx.Emissions.labels = get.labels(NOx.Emissions.break.points, mozart.data$NOx.Emissions, digits = 2)
    NOx.Emissions.labels = lapply(NOx.Emissions.labels, function (i) sprintf("%0.2e", i))

    if (spc == "O3" | spc == "HNO3" | spc == "HCHO" | spc == "H2O2") {
        title = paste(spc, "Mixing Ratio (ppbv) Contour Plot")
    } else if (spc == "HO2" | spc == "OH" | spc == "HOx") {
        title = paste(spc, "Mixing Ratio (pptv) Contour Plot")
    } else {
        title = "No title as yet"
    }
    
    p = ggplot(df, aes(x = Temperature, y = NOx.Emissions, z = O3))
    p = p + stat_contour(aes(colour = ..level..)) 
    p = p + facet_wrap(~ Mechanism) 
    p = p + theme_tufte() 
    p = p + ggtitle(title)
    p = p + theme(plot.title = element_text(face = "bold"))
    p = p + theme(axis.line = element_line(colour = "black")) 
    p = p + theme(strip.text = element_text(face = "bold")) 
    p = p + xlab("Temperature (K)") 
    p = p + ylab("NOx emissions (molecules(NOx) cm-3 s-1)") 
    p = p + theme(axis.title = element_text(face = "bold")) 
    p = p + scale_colour_continuous(name = "O3 (ppbv)")
    p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels)
    p = p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels)

    filename = paste0("plot_temperature_", spc, ".pdf")
    CairoPDF(file = filename, width = 10, height = 7)
    print(direct.label(p, "last.points"))
    dev.off()
}
lapply(species, get.plot, data = d)
