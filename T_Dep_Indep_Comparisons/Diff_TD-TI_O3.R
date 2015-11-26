# calculate differences between td and ti, O3 mixing ratios
# Version 0: Jane Coates 08/11/2015
# Version 1: Jane Coates 21/11/2015 differences of low and high isoprene emissions, re-factoring to use MetO3 package
# Version 2: Jane Coates 26/11/2015 Computing relative difference from TI

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//T_Dep_Indep_Comparisons")
mechanisms = c("MCMv3.2", "CRIv2", "RADM2", "MOZART-4", "CB05")

mechanism_data_frame = function (mechanism, td.df, df) {
  mech.td = td.df %>% filter(Mechanism == mechanism)
  mech.ti = df %>% filter(Mechanism == mechanism)
  diff = td %>% filter(Mechanism == mechanism) %>% select(NOx.Emissions, Temperature, O3)
  diff$O3.Diff = (mech.td$O3 - mech.ti$O3)/mech.ti$O3 * 100
  
  diff = diff %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))
  fld = with(diff, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = O3.Diff))
  df = melt(fld$z, na.rm = TRUE)
  names(df) = c("x", "y", "O3.Diff")
  df$Temperature = fld$x[df$x]
  df$NOx.Emissions = fld$y[df$y] 
  df$Mechanism = rep(mechanism, length(df$NOx))
  return (df)
}

contours_data_frame = function (mechanism, df) {
  data = df %>% 
    filter(Mechanism == mechanism) %>%
    select(NOx.Emissions, Temperature, Diff)
    
  diff = data %>% 
    mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))
  fld = with(diff, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = Diff))
  df = melt(fld$z, na.rm = TRUE)
  names(df) = c("x", "y", "Diff")
  df$Temperature = fld$x[df$x]
  df$NOx.Emissions = fld$y[df$y] 
  df$Mechanism = rep(mechanism, length(df$NOx))
  return (df)
}

td = read.csv(file = "Temperature_Dependent_data.csv")
ti = read.csv(file = "Temperature_Independent_data.csv")
td = td %>% select(Mechanism, NOx.Emissions, Temperature, O3) %>% arrange(Temperature)
ti = ti %>% select(Mechanism, NOx.Emissions, Temperature, O3) %>% arrange(Temperature)

data = lapply(mechanisms, mechanism_data_frame, td.df = td, df = ti)
df = do.call("rbind", data) #combining into 1 data frame

df %>%
  group_by(Mechanism) %>%
  summarise(Max = max(O3.Diff))

df$O3.Diff = sprintf("%.0f", df$O3.Diff)
df$O3.Diff = factor(df$O3.Diff, levels = seq(-1, 29, 1))

colour.count = length(unique(df$O3.Diff))
get.palette = colorRampPalette(brewer.pal(9, "Oranges"))

p = ggplot(df, aes(x = Temperature, y = NOx.Emissions))
p = p + geom_tile(aes(fill = factor(O3.Diff)))
p = p + facet_grid(Mechanism ~ .)
p = p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("NOx Emissions (molecules cm-3 s-1)")
# p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels, expand = c(0, 0))
# p = p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels, expand = c(0, 0))
p = p + theme_tufte()
p = p + theme(axis.line = element_line())
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))
p = p + theme(panel.margin = unit(5, "mm"))
p = p + scale_fill_manual(values = get.palette(colour.count), name = "%Increase")
p
CairoPDF(file = "Difference_O3_TD-TI.pdf", width = 10, height = 12)
print(p)
dev.off()

### difference from MCM - TD
td <- tbl_df(td)
td.diff <- td %>%
  spread(Mechanism, O3, drop = FALSE) %>%
  gather(Mechanism, O3, -NOx.Emissions, -Temperature, -MCMv3.2) %>%
  mutate(Diff = (O3 - MCMv3.2)/MCMv3.2*100)

mechanisms = levels(factor(td.diff$Mechanism))
td.list <- lapply(mechanisms, contours_data_frame, df = td.diff)
td.df <- do.call("rbind", td.list)
td.df$Run <- rep("TD", length(td.df$Mechanism))

### difference from MCM - TI
ti <- tbl_df(ti)
ti.diff <- ti %>%
  spread(Mechanism, O3, drop = FALSE) %>%
  gather(Mechanism, O3, -NOx.Emissions, -Temperature, -MCMv3.2) %>%
  mutate(Diff = (O3 - MCMv3.2)/MCMv3.2*100)

ti.list <- lapply(mechanisms, contours_data_frame, df = ti.diff)
ti.df <- do.call("rbind", ti.list)
ti.df$Run <- rep("TI", length(ti.df$Mechanism))

all <- rbind(td.df, ti.df)
all %>%
  group_by(Mechanism) %>%
  summarise(Max = max(Diff), Min = min(Diff))

all$Diff <- sprintf("%.0f", all$Diff)
all$Diff = factor(all$Diff, levels = seq(-8, 13, 1))

colour.count = length(unique(all$Diff))
get.palette = colorRampPalette(brewer.pal(9, "Oranges"))

p = ggplot(all, aes(x = Temperature, y = NOx.Emissions))
p = p + geom_tile(aes(fill = factor(Diff)))
p = p + facet_grid(Mechanism ~ .)
p = p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("NOx Emissions (molecules cm-3 s-1)")
# p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels, expand = c(0, 0))
# p = p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels, expand = c(0, 0))
p = p + theme_tufte()
p = p + theme(axis.line = element_line())
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))
p = p + theme(panel.margin = unit(5, "mm"))
p = p + scale_fill_manual(values = get.palette(colour.count), name = "%Increase")
p
