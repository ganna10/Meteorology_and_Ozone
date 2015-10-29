# Compare Contours of species (argument) for each mechanism between temperature dep and indep runs
# Version 0: Jane Coates 23/10/2015

args = commandArgs(trailingOnly = TRUE)
spc = args[[1]]

runs = c("Dependent", "Independent")
mechanisms = c("CB05", "RADM2", "MOZART-4")

mechanism_data_frame = function (mechanism, dataframe) {
    data = dataframe %>% filter(Mechanism == mechanism)
    data = data %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))
    colnum = match(spc, names(data))
    fld = with(data, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = data[[colnum]]))
    df = melt(fld$z, na.rm = TRUE)
    names(df) = c("x", "y", spc)
    df$Temperature = fld$x[df$x]
    df$NOx.Emissions = fld$y[df$y] 
    df$Mechanism = rep(mechanism, length(df$NOx))
    return (df)
}

get_data = function (run) {
    filename = paste0("Temperature_", run, "_data.csv")
    d = read.csv(filename)
    data = lapply(mechanisms, mechanism_data_frame, dataframe = d)
    df = do.call("rbind", data)
    df$Run = rep(paste("Temperature", run, "\nIsoprene Emissions"), length(df$NOx)) 
    return(df)
}

get.labels = function (break.points, orig.data, digits) {
    labels = lapply(break.points,
                    function (i) round ((i * (max(orig.data) - min(orig.data))) + min(orig.data), digits )
            )
    return (labels)
}

run.data = lapply(runs, get_data)

df = do.call("rbind", run.data) #combining into 1 data frame
colnum = match(spc, names(df)) 

data = read.csv("Temperature_Dependent_data.csv")
cb05.data = data %>% filter(Mechanism == "CB05") %>% mutate(Temperature.C = Temperature - 273)
temperature.break.points = seq(0, 1, 0.2)
temperature.labels = get.labels(temperature.break.points, cb05.data$Temperature.C, digits = 2) 
NOx.Emissions.break.points = seq(0, 1, 0.2)
NOx.Emissions.labels = get.labels(NOx.Emissions.break.points, cb05.data$NOx.Emissions, digits = 2)
NOx.Emissions.labels = lapply(NOx.Emissions.labels, function (i) sprintf("%0.1e", i))

p = ggplot(df, aes(x = Temperature, y = NOx.Emissions, z = df[[colnum]]))
p = p + facet_grid(Mechanism ~ Run)
p = p + stat_contour(aes(colour = ..level..), binwidth = 5)
p = p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("NOx Emissions (molecules cm-3 s-1)")
p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels)
p = p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels)
p = p + theme_tufte()
p = p + theme(axis.line = element_line())
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))
p = p + theme(panel.margin = unit(5, "mm"))
p = p + scale_colour_gradient(low = "#252525", high = "#bdbdbd")

filename = paste0(spc, "_comparison.pdf")
CairoPDF(file = filename, width = 7, height = 10)
print(direct.label(p))
dev.off()
