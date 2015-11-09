# Contours of O3 mixing ratios of each mechanism for td and ti runs
# Version 0: Jane Coates 08/11/2015

#setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//T_Dep_Indep_Comparisons")
mechanisms = c("CRIv2", "RADM2", "MOZART-4", "CB05", "MCMv3.2")
#mechanisms = c("CRIv2") 

mechanism_data_frame = function (mechanism, dataframe) {
  d = dataframe %>% filter(Mechanism == mechanism) %>%
    mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))
  fld = with(d, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = O3))
  df = melt(fld$z, na.rm = TRUE)
  names(df) = c("x", "y", "O3")
  df$Temperature = fld$x[df$x]
  df$NOx.Emissions = fld$y[df$y] 
  df$Mechanism = rep(mechanism, length(df$NOx))
  return (df)  
}

get.labels = function (break.points, orig.data, digits) {
    labels = lapply(break.points,
                    function (i) round ((i * (max(orig.data) - min(orig.data))) + min(orig.data), digits )
            )
    return (labels)
}

td = read.csv(file = "Temperature_Dependent_data.csv")
ti = read.csv(file = "Temperature_Independent_data.csv")
td = td %>% select(Mechanism, NOx.Emissions, Temperature, O3) %>% arrange(Temperature)
ti = ti %>% select(Mechanism, NOx.Emissions, Temperature, O3) %>% arrange(Temperature)

td.data = lapply(mechanisms, mechanism_data_frame, dataframe = td)
td.df = do.call("rbind", td.data) #combining into 1 data frame
td.df$Run = rep("Temperature Dependent", length(td.df$Mechanism))

ti.data = lapply(mechanisms, mechanism_data_frame, dataframe = ti)
ti.df = do.call("rbind", ti.data) #combining into 1 data frame
ti.df$Run = rep("Temperature Independent", length(ti.df$Mechanism))

df = rbind(ti.df, td.df)

#labels
data.labels = read.csv("Temperature_Dependent_data.csv")
cb05.data = data.labels %>% filter(Mechanism == "CB05") %>% mutate(Temperature.C = Temperature - 273)
temperature.break.points = seq(0, 1, 0.2)
temperature.labels = get.labels(temperature.break.points, cb05.data$Temperature.C, digits = 2) 
NOx.Emissions.break.points = seq(0, 1, 0.2)
NOx.Emissions.labels = get.labels(NOx.Emissions.break.points, cb05.data$NOx.Emissions, digits = 2)
NOx.Emissions.labels = lapply(NOx.Emissions.labels, function (i) sprintf("%0.1e", i))

p = ggplot(df, aes(x = Temperature, y = NOx.Emissions)) 
p = p + stat_contour(aes(colour = ..level.., z = O3), binwidth = 5)
p = p + facet_grid(Mechanism ~ Run)
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

CairoPDF(file = "O3_comparison.pdf", width = 7, height = 10)
direct.label(p)
print(direct.label(p))
dev.off()
