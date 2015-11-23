# calculate differences between td and ti, O3 mixing ratios
# Version 0: Jane Coates 08/11/2015
# Version 1: Jane Coates 21/11/2015 differences of low and high isoprene emissions, re-factoring to use MetO3 package

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//T_Dep_Indep_Comparisons")
mechanisms = c("CRIv2", "RADM2", "MOZART-4", "CB05")

mechanism_data_frame = function (mechanism, td.df, df) {
  mech.td = td.df %>% filter(Mechanism == mechanism)
  mech.ti = df %>% filter(Mechanism == mechanism)
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

td = read.csv(file = "Temperature_Dependent_data.csv")
ti = read.csv(file = "Temperature_Independent_data.csv")
low = read.csv(file = "Low_Isoprene_emissions.csv")
high = read.csv(file = "High_Isoprene_emissions.csv")
td = td %>% select(Mechanism, NOx.Emissions, Temperature, O3) %>% arrange(Temperature)
ti = ti %>% select(Mechanism, NOx.Emissions, Temperature, O3) %>% arrange(Temperature)
low = low %>% select(Mechanism, NOx.Emissions, Temperature, O3) %>% arrange(Temperature)
high = high %>% select(Mechanism, NOx.Emissions, Temperature, O3) %>% arrange(Temperature)

ti.data = lapply(mechanisms, mechanism_data_frame, td.df = td, df = ti)
ti.df = do.call("rbind", ti.data) #combining into 1 data frame
ti.df$O3.Diff = sprintf("%.0f", abs(ti.df$O3.Diff))
ti.df$O3.Diff = factor(ti.df$O3.Diff, levels = seq(0, 17, 1))
ti.df$Run = rep("T = 293 K", length(ti.df$O3.Diff))

low.data = lapply(mechanisms, mechanism_data_frame, td.df = td, df = low)
low.df = do.call("rbind", low.data) #combining into 1 data frame
low.df$O3.Diff = sprintf("%.0f", abs(low.df$O3.Diff))
low.df$O3.Diff = factor(low.df$O3.Diff, levels = seq(0, 17, 1))
low.df$Run = rep("T = 288 K", length(low.df$O3.Diff))

high.data = lapply(mechanisms, mechanism_data_frame, td.df = td, df = high)
high.df = do.call("rbind", high.data) #combining into 1 data frame
high.df$O3.Diff = sprintf("%.0f", abs(high.df$O3.Diff))
high.df$O3.Diff = factor(high.df$O3.Diff, levels = seq(0, 17, 1))
high.df$Run = rep("T = 313 K", length(high.df$O3.Diff))

df = rbind(ti.df, low.df, high.df)

df %>% group_by(Mechanism, Run) %>%
  summarise(Max.O3.Diff = max(as.numeric(O3.Diff)))

colour.count = length(unique(df$O3.Diff))
get.palette = colorRampPalette(brewer.pal(9, "Oranges"))

p = ggplot(df, aes(x = Temperature, y = NOx.Emissions))
p = p + geom_tile(aes(fill = factor(O3.Diff)))
p = p + facet_grid(Mechanism ~ Run)
p = p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("NOx Emissions (molecules cm-3 s-1)")
# p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels, expand = c(0, 0))
# p = p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels, expand = c(0, 0))
p = p + theme_tufte()
p = p + theme(axis.line = element_line())
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))
p = p + theme(panel.margin = unit(5, "mm"))
p = p + scale_fill_manual(values = get.palette(colour.count), name = "O3 (ppb)")
p
CairoPDF(file = "Difference_O3_TD-TI.pdf", width = 10, height = 12)
print(p)
dev.off()
