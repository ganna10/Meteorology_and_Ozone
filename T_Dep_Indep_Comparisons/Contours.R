# Plot contours,  facet run ~ mechanism
# Version 0: Jane Coates 12/11/2015

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//T_Dep_Indep_Comparisons")

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
  names(df) = c("x", "y", "HOx")
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

runs = c("Dependent", "Independent")
mechanisms = c("CB05", "RADM2", "MCMv3.2", "MOZART-4", "CRIv2")

#Contours
contour.data = lapply(runs, get_contour_data)
df = do.call("rbind", contour.data) #combining into 1 data frame

#labels
d = read.csv(file = "Temperature_Dependent_data.csv") %>% filter(Mechanism == "CB05") %>% mutate(Temperature.C = Temperature - 273)
temperature.break.points = seq(0, 1, 0.2)
temperature.labels = get.labels(temperature.break.points, d$Temperature.C, digits = 2) 
NOx.Emissions.break.points = seq(0, 1, 0.2)
NOx.Emissions.labels = get.labels(NOx.Emissions.break.points, d$NOx.Emissions, digits = 2)
NOx.Emissions.labels = lapply(NOx.Emissions.labels, function (i) sprintf("%0.1e", i))

p = ggplot(df, aes(y = NOx.Emissions))
p = p + stat_contour(aes(x = Temperature, z = HOx, colour = ..level..)) 
p = p + facet_grid(Mechanism ~ Run)
#p = p + scale_colour_gradient(low = "#cc6329", high = "#ef6638")
p = p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("NOx Emissions (molecules cm-3 s-1)")
p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels)
p = p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels)
p = p + theme_tufte()
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))

direct.label(p)
#CairoPDF(file = "O3_contours_with_m.pdf", width = 7, height = 10)
#print(direct.label(p, list('top.pieces', cex = 0.6)))
#dev.off()
