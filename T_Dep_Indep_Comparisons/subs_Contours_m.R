# compare O3 mixing ratio contours of temperature dependent and independent runs, with m_O3-T as tiles at each NOx level
# Jane Coates : Version 0 07/11/2015

runs = c("Dependent", "Independent")
mechanisms = c("CRIv2")

get.labels = function (break.points, orig.data, digits) {
    labels = lapply(break.points,
                    function (i) round ((i * (max(orig.data) - min(orig.data))) + min(orig.data), digits )
            )
    return (labels)
}

contour_mechanism_data_frame = function (mechanism, dataframe) {
    data = dataframe %>% filter(Mechanism == mechanism)
    data = data %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))
    fld = with(data, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = O3))
    df = melt(fld$z, na.rm = TRUE)
    names(df) = c("x", "y", "O3")
    df$Temperature = fld$x[df$x]
    df$NOx.Emissions = fld$y[df$y] 
    df$Mechanism = rep(mechanism, length(df$NOx))
    return (df)
}

get_contour_data = function (run) {
    filename = paste0("Temperature_", run, "_data.csv")
    d = read.csv(filename)
    data = lapply(mechanisms, contour_mechanism_data_frame, dataframe = d)
    df = do.call("rbind", data)
    df$Run = rep(paste("Temperature", run, "\nIsoprene Emissions"), length(df$NOx)) 
    return(df)
}

get_slope_data = function (run) {
    filename = paste0("Temperature_", run, "_data.csv")
    d = read.csv(filename)
    slopes = d %>%  mutate(Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions))) %>%  
                    select(-NOx.Emissions) %>% 
                    group_by(Mechanism, Scaled.NOx.Emissions) %>%
                    do(model = lm(O3 ~ Temperature, data = .)) %>%
                    mutate(Slope = sprintf("%.1f", abs(summary(model)$coeff[2])), R2 = summary(model)$r.squared) %>%
                    select(-model) %>%
                    filter(Mechanism == "CRIv2")
    slopes$Run = rep(paste("Temperature", run, "\nIsoprene Emissions"), length(slopes$Mechanism)) 
    slopes = slopes[rep(seq_len(nrow(slopes)), each = length(df$Temperature)),] %>% cbind(df$Temperature)
    names(slopes) = c("Mechanism", "NOx.Emissions", "Slope", "R2", "Run", "Temperature")
    return(slopes)
}

contour.data = lapply(runs, get_contour_data)
df = do.call("rbind", contour.data) #combining into 1 data frame

slopes.data = lapply(runs, get_slope_data)
slopes = do.call("rbind", slopes.data)

#labels
data = read.csv("Temperature_Dependent_data.csv")
cb05.data = data %>% filter(Mechanism == "CB05") %>% mutate(Temperature.C = Temperature - 273)
temperature.break.points = seq(0, 1, 0.2)
temperature.labels = get.labels(temperature.break.points, cb05.data$Temperature.C, digits = 2) 
NOx.Emissions.break.points = seq(0, 1, 0.2)
NOx.Emissions.labels = get.labels(NOx.Emissions.break.points, cb05.data$NOx.Emissions, digits = 2)
NOx.Emissions.labels = lapply(NOx.Emissions.labels, function (i) sprintf("%0.1e", i))

colour.count = length(unique(slopes$Slope))
get.palette = colorRampPalette(brewer.pal(9, "Greys"))

p = ggplot(df, aes(x = Temperature, y = NOx.Emissions))
p = p + geom_tile(data  = slopes, aes(fill = factor(Slope)))
p = p + stat_contour(aes(z = O3, colour = ..level..), binwidth = 5) 
p = p + facet_grid(Mechanism ~ Run)
p = p + scale_colour_gradient(low = "#7d3f96", high = "#7d3f96")
p = p + scale_fill_manual(values = get.palette(colour.count), name = expression(bold(paste(m[O3-T]))))
#p = p + scale_fill_grey(limits = rev(levels(factor(slopes$Slope))), name = expression(bold(paste(m[O3-T]))))
p = p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("NOx Emissions (molecules cm-3 s-1)")
p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels, expand = c(0, 0.03))
p = p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels, expand = c(0, 0.01))
p = p + theme_tufte()
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))

CairoPDF(file = "O3_contours_with_m.pdf", width = 10, height = 10)
print((p))
#print(direct.label(p))
dev.off()
