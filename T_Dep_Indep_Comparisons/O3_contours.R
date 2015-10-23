dep = read.csv("Temperature_Dependent_data.csv")
dep = tbl_df(dep)
dep
indep = read.csv("Temperature_Independent_data.csv")
indep = tbl_df(indep)
indep = indep %>% filter(Mechanism == "CB05")
indep
dep$Run = rep("Temperature Dependent", length(dep$Mechanism))
indep$Run = rep("Temperature Independent", length(indep$Mechanism))
dep
o3.dep = dep %>% select(Mechanism, Run, NOx.Emissions, Temperature, O3)
o3.indep = indep %>% select(Mechanism, Run, NOx.Emissions, Temperature, O3)
o3.dep = %>% o3.dep %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))
o3.dep = o3.dep %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))
o3.indep = o3.indep %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))
fld = with(o3.dep, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = O3))
ind.fld = with(o3.indep, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = O3))
dep.df = melt(fld$z, na.rm = TRUE)
library(reshape2)
dep.df = melt(fld$z, na.rm = TRUE)
indep.df = melt(ind.fld$z, na.rm = TRUE)
names(dep.df) = c("x", "y", "O3")
names(indep.df) = c("x", "y", "O3")
dep.df$Temperature = fld$x[df$x]
dep.df$Temperature = fld$x[dep.df$x]
indep.df$Temperature = ind.fld$x[indep.df$x]
dep.df$NOx.Emissions = fld$y[dep.df$y]
indep.df$NOx.Emissions = ind.fld$y[indep.df$y]
df = rbind(dep.df, indep.df)
df
indep.df$Mechanism = rep("CB05", length(indep.df$O3))
dep.df$Mechanism = rep("CB05", length(dep.df$O3))
dep.df$Run = rep("Temperature Dependent", length(dep.df$O3))
indep.df$Run = rep("Temperature Independent", length(indep.df$O3))
df = rbind(dep.df, indep.df)
head(df)
p = ggplot(df, aes(x = Temperature, y = NOx.Emissions, z = O3))
p = facet_grid(Mechanism ~ Run)
p = ggplot(df, aes(x = Temperature, y = NOx.Emissions, z = O3))
p = p + ggplot(df, aes(x = Temperature, y = NOx.Emissions, z = O3))
p = ggplot(df, aes(x = Temperature, y = NOx.Emissions, z = O3))
p = p + facet_grid(Mechanism ~ Run)
p = p + stat_contour(colour = aes(..level..))
direct.label(p)
p = p + stat_contour(aes(colour = aes(..level..)))
direct.label(p)
p = p + stat_contour(aes(colour = ..level..))
direct.label(p)
p = p + stat_contour(aes(colour = ..level..))
p
p = ggplot(df, aes(x = Temperature, y = NOx.Emissions, z = O3))
p = p + facet_grid(Mechanism ~ Run)
p = p + stat_contour(aes(colour = ..level..))
p
direct.label(p)
savehistory(file = "O3_contours.R")
