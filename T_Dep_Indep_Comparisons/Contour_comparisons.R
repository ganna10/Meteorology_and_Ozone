# Compare Contours of species (argument) for each mechanism between temperature dep and indep runs
# Version 0: Jane Coates 23/10/2015

args = commandArgs(trailingOnly = TRUE)
spc = args[[1]]

runs = c("Dependent", "Independent")
mechanisms = c("CB05")

mechanism_data_frame = function (mechanism, dataframe) {
    data = dataframe %>% filter(Mechanism == mechanism)
    colnum = match(spc, names(data))
    fld = with(data, interp(x = Temperature, y = NOx, z = data[[colnum]]))
    df = melt(fld$z, na.rm = TRUE)
    names(df) = c("x", "y", spc)
    df$Temperature = fld$x[df$x]
    df$NOx = fld$y[df$y] 
    df$Mechanism = rep(mechanism, length(df$NOx))
    return (df)
}

get_data = function (run) {
    filename = paste0("Temperature_", run, "_data.csv")
    d = read.csv(filename)
    data = lapply(mechanisms, mechanism_data_frame, dataframe = d)
    df = do.call("rbind", data)
    df$Run = rep(paste("Temperature", run), length(df$NOx)) 
    return(df)
}
run.data = lapply(runs, get_data)

df = do.call("rbind", run.data) #combining into 1 data frame
colnum = match(spc, names(df))

p = ggplot(df, aes(x = Temperature, y = NOx, z = df[[colnum]]))
p = p + facet_grid(Mechanism ~ Run)
p = p + stat_contour(aes(colour = ..level..))
p = p + xlab("Temperature (K)") + ylab("NOx Mixing Ratio (ppbv)")
p = p + scale_y_continuous(limits = c(0, 9.6))
p = p + theme_tufte()
p = p + theme(axis.line = element_line())
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))
p = p + theme(panel.margin = unit(5, "mm"))

filename = paste0(spc, "_comparison.pdf")
CairoPDF(file = filename, width = 10, height = 7)
print(direct.label(p))
dev.off()
