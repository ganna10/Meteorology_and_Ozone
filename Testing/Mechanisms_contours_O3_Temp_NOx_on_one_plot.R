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

d = read.csv(file = "out_Temperature_NOx_06102015.csv")
d = tbl_df(d)

species = c("O3")

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

p = ggplot(plot.df, aes(x = Temperature, y = NOx.Emissions, z = Mixing.Ratio, linetype = factor(Mechanism)))
p = p + stat_contour(aes(colour = ..level..))

CairoPDF(file = "Temperature_NOx_contours_mechanisms_linetype.pdf")
print(p)
dev.off()
