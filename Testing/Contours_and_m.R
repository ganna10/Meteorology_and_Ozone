setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Testing")

get.labels = function (break.points, orig.data, digits) {
    labels = lapply(break.points,
                    function (i) round ((i * (max(orig.data) - min(orig.data))) + min(orig.data), digits )
            )
    return (labels)
}

#temp independent
ind_data = read.csv("../Contours_Temperature_Indep/out_Temperature_NOx_07112015.csv")
ind_d = ind_data %>% select(Mechanism, Temperature, NOx.Emissions, O3)
ind_d = ind_d %>%   filter(Mechanism == "CB05") %>% 
            mutate(Temperature.C = Temperature - 273, Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))

ind_fld = with(ind_d, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = O3))
ind_df = melt(ind_fld$z, na.rm = TRUE)
names(ind_df) = c("x", "y", "O3")
ind_df$Temperature = ind_fld$x[ind_df$x]
ind_df$NOx.Emissions = ind_fld$y[ind_df$y]
ind_df$Run = rep("Temperature Independent", length(ind_df$O3))
ind_df$Mechanism = rep("CB05", length(ind_df$O3))

temperature.break.points = seq(0, 1, 0.2)
temperature.labels = get.labels(temperature.break.points, ind_d$Temperature.C, digits = 2) 
NOx.Emissions.break.points = seq(0, 1, 0.2)
NOx.Emissions.labels = get.labels(NOx.Emissions.break.points, ind_d$NOx.Emissions, digits = 2)
NOx.Emissions.labels = lapply(NOx.Emissions.labels, function (i) sprintf("%0.1e", i))

ind_slopes = ind_d %>%  group_by(Mechanism, Scaled.NOx.Emissions) %>%
                do(model = lm(O3 ~ Temperature, data = .)) %>%
                mutate(Slope = sprintf("%.1f", abs(summary(model)$coeff[2])), R2 = summary(model)$r.squared) %>%
                select(-model)

ind_test = ind_slopes[rep(seq_len(nrow(ind_slopes)), each = length(ind_df$Temperature)),] %>% cbind(ind_df$Temperature)
names(ind_test) = c("Mechanism", "NOx.Emissions", "Slope", "R2", "Temperature")
ind_test$Run = rep("Temperature Independent", length(ind_test$R2))

#temp dependent
dep_data = read.csv("../Contours_Temperature_Dep/out_Temperature_NOx_07112015.csv")
dep_d = dep_data %>% select(Mechanism, Temperature, NOx.Emissions, O3)
dep_d = dep_d %>%   filter(Mechanism == "CB05") %>% 
            mutate(Temperature.C = Temperature - 273, Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))

dep_fld = with(dep_d, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = O3))
dep_df = melt(dep_fld$z, na.rm = TRUE)
names(dep_df) = c("x", "y", "O3")
dep_df$Temperature = dep_fld$x[dep_df$x]
dep_df$NOx.Emissions = dep_fld$y[dep_df$y]
dep_df$Run = rep("Temperature Dependent", length(dep_df$O3))
dep_df$Mechanism = rep("CB05", length(dep_df$O3))

dep_slopes = dep_d %>%  group_by(Mechanism, Scaled.NOx.Emissions) %>%
                do(model = lm(O3 ~ Temperature, data = .)) %>%
                mutate(Slope = sprintf("%.1f", abs(summary(model)$coeff[2])), R2 = summary(model)$r.squared) %>%
                select(-model)

dep_test = dep_slopes[rep(seq_len(nrow(dep_slopes)), each = length(dep_df$Temperature)),] %>% cbind(dep_df$Temperature)
names(dep_test) = c("Mechanism", "NOx.Emissions", "Slope", "R2", "Temperature")
dep_test$Run = rep("Temperature Dependent", length(dep_test$R2))

df = rbind(ind_df, dep_df)
test = rbind(ind_test, dep_test)
test$Slope = as.numeric(test$Slope)

p = ggplot(df, aes(x = Temperature, y = NOx.Emissions))
p = p + geom_tile(data  = test, aes(fill = factor(Slope)))
p = p + stat_contour(aes(z = O3, colour = ..level..)) 
p = p + facet_grid(Mechanism ~ Run)
p = p + scale_colour_gradient(low = "#7d3f96", high = "#000000")
p = p + scale_fill_grey(limits = rev(levels(factor(test$Slope))), name = expression(bold(paste(m[O3-T]))))
p = p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("NOx Emissions (molecules cm-3 s-1)")
p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels)
p = p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels, expand = c(0, 0))
p = p + theme_tufte()
p = p + theme(panel.margin = unit(0.2, "mm"))
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))

direct.label(p)
#CairoPDF(file = "O3_contours_with_m.pdf", width = 10, height = 7)
#print(direct.label(p))
#dev.off()
