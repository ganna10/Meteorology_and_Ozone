# differences in ozone from MCM
# Version 0: Jane Coates 31/7/2016

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//T_Dep_Indep_Comparisons")
runs <- c("Dependent", "Independent")
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2")
spc <- "O3"

list <- lapply(runs, get_all_mixing_ratio_data)
df <- do.call("rbind", list)
df <- tbl_df(df)
df$Run[df$Run == "Temperature Dependent\nIsoprene Emissions"] <- "TD"
df$Run[df$Run == "Temperature Independent\nIsoprene Emissions"] <- "TI"

difference <- df %>%
  arrange(Temperature) %>%
  select(Mechanism, Temperature, Run, NOx.Emissions, O3) %>%
  spread(Mechanism, O3) %>%
  gather(Mechanism, O3, -NOx.Emissions, -Temperature, -Run, -MCMv3.2) %>%
  mutate(Diff = (O3 - MCMv3.2) / MCMv3.2 * 100) %>%
  select(-O3, -MCMv3.2)
tbl_df(difference)


get_axis_label <- function (break.points, orig.data, digits) {
  labels <- lapply(break.points, function (i) round ((i * (max(orig.data) - min(orig.data))) + min(orig.data), digits ))
  return (labels)
}
temperature.break.points = seq(0, 1, 0.2)
temperature.labels <- get_axis_label(temperature.break.points, difference$Temperature, digits = 2) 
temperature.labels <- lapply(temperature.labels, function (x) x - 273)

NOx.Emissions.break.points <- seq(0, 1, 0.3333)
NOx.Emissions.labels <- get_axis_label(NOx.Emissions.break.points, difference$NOx.Emissions, digits = 2)
NOx.Emissions.labels <- lapply(NOx.Emissions.labels, function (i) sprintf("%0.1e", i))

mechanism_data_frame_td = function (mechanism, df) {
  mech = df %>% 
    filter(Mechanism == mechanism, Run == "TD")
   
  diff = mech %>% 
    mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))
  
  fld = with(diff, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = Diff, duplicate = "mean"))
  df = melt(fld$z, na.rm = TRUE)
  names(df) = c("x", "y", "Diff")
  df$Temperature = fld$x[df$x]
  df$NOx.Emissions = fld$y[df$y] 
  df$Mechanism = rep(mechanism, length(df$NOx))
  df$Run = rep("TD", length(df$NOx))
  return (df)
}

mechanism_data_frame_ti = function (mechanism, df) {
  mech = df %>% 
    filter(Mechanism == mechanism, Run == "TI")
  
  diff = mech %>% 
    mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))
  
  fld = with(diff, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = Diff, duplicate = "mean"))
  df = melt(fld$z, na.rm = TRUE)
  names(df) = c("x", "y", "Diff")
  df$Temperature = fld$x[df$x]
  df$NOx.Emissions = fld$y[df$y] 
  df$Mechanism = rep(mechanism, length(df$NOx))
  df$Run = rep("TI", length(df$NOx))
  return (df)
}


diff.mechs <- c("CRIv2", "MOZART-4", "RADM2", "CB05")
data.list.td <- lapply(diff.mechs, mechanism_data_frame_td, df = difference)
df.td <- do.call("rbind", data.list.td)
data.list.ti <- lapply(diff.mechs, mechanism_data_frame_ti, df = difference)
df.ti <- do.call("rbind", data.list.ti)

# mcm.data <- df %>%
#   filter(Mechanism == "CRIv2")
# mcm.data$Diff = rep(0, length(mcm.data$Mechanism))
# mcm.data$Mechanism = rep("MCMv3.2", length(mcm.data$Mechanism))
# df <- rbind(df, mcm.data)

df <- rbind(df.td, df.ti)
df$Diff = sprintf("%.0f", df$Diff)
df$Diff[df$Diff == "-0"] <- 0
df$Diff = factor(df$Diff, levels = seq(-8,13, 1))
df$Mechanism <- factor(df$Mechanism, levels = c("CRIv2", "MOZART-4", "CB05", "RADM2"))
df$Run <- factor(df$Run, levels = c("TI", "TD"))
tbl_df(df)
library("plyr")
df <- df %>%
  mutate(Run = revalue(Run, c("TD" = "Temperature Dependent Isoprene Emissions", "TI" = "Temperature Independent Isoprene Emissions")))

p <- ggplot(df, aes(x = Temperature, y = NOx.Emissions))
p <- p + geom_tile(aes(fill = as.numeric(as.character(factor(Diff)))))
p <- p + scale_fill_distiller(palette = "Spectral", name = "%Difference\nfrom MCMv3.2")
p <- p + facet_grid(Mechanism ~ Run)
p <- p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("NOx Emissions (molecules cm-3)")
p <- p + plot_theme()
p <- p + theme(panel.margin.y = unit(3, "mm"))
p <- p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels, expand = c(0, 0.01))
p <- p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels, expand = c(0, 0))
p <- p + theme(legend.title = element_text(face = "bold"))
p <- p + theme(legend.position = "top")
p

CairoPDF(file = "Percent_diff_from_MCM.pdf", width = 7, height = 10)
print(p)
dev.off()
