# calculate differences between td and ti, O3 mixing ratios
# Version 0: Jane Coates 08/11/2015

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//T_Dep_Indep_Comparisons")
mechanisms = c("CRIv2", "RADM2", "MOZART-4", "CB05", "MCMv3.2")
runs = c("Dependent", "Independent")

mechanism_data_frame = function (mechanism, td.df, ti.df) {
  mech.td = td %>% filter(Mechanism == mechanism)
  mech.ti = ti %>% filter(Mechanism == mechanism)
  diff = td %>% filter(Mechanism == mechanism) %>% select(NOx.Emissions, Temperature, O3)
  diff$O3.Diff = mech.td$O3 - mech.ti$O3
  
  diff = diff %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))
  fld = with(diff, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = O3.Diff))
  df = melt(fld$z, na.rm = TRUE)
  names(df) = c("x", "y", "O3.Diff")
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

data = lapply(mechanisms, mechanism_data_frame, td.df = td, ti.df = ti)
df = do.call("rbind", data) #combining into 1 data frame
df$O3.Diff = sprintf("%.0f", abs(df$O3.Diff))
df$O3.Diff = factor(df$O3.Diff, levels = seq(0, 16, 1))

df %>% group_by(Mechanism) %>%
  summarise(Max.O3.Diff = max(O3.Diff))

data = read.csv("Temperature_Dependent_data.csv")
cb05.data = data %>% filter(Mechanism == "CB05") %>% mutate(Temperature.C = Temperature - 273)
temperature.break.points = seq(0, 1, 0.2)
temperature.labels = get.labels(temperature.break.points, cb05.data$Temperature.C, digits = 2) 
NOx.Emissions.break.points = seq(0, 1, 0.2)
NOx.Emissions.labels = get.labels(NOx.Emissions.break.points, cb05.data$NOx.Emissions, digits = 2)
NOx.Emissions.labels = lapply(NOx.Emissions.labels, function (i) sprintf("%0.1e", i))

colour.count = length(unique(df$O3.Diff))
get.palette = colorRampPalette(brewer.pal(9, "Oranges"))

p = ggplot(df, aes(x = Temperature, y = NOx.Emissions))
p = p + geom_tile(aes(fill = factor(O3.Diff)))
#p = p + stat_contour(aes(colour = ..level.., z = O3.Diff))
p = p + facet_wrap(~ Mechanism, scales = "free_x")
p = p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("NOx Emissions (molecules cm-3 s-1)")
p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels, expand = c(0, 0))
p = p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels, expand = c(0, 0))
p = p + theme_tufte()
p = p + theme(axis.line = element_line())
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))
p = p + theme(panel.margin = unit(5, "mm"))
p = p + scale_fill_manual(values = get.palette(colour.count), name = "O3 (ppb)")

# CairoPDF(file = "Difference_O3_TD-TI.pdf")
# print(p)
# dev.off()
