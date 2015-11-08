get.labels = function (break.points, orig.data, digits) {
    labels = lapply(break.points,
                    function (i) round ((i * (max(orig.data) - min(orig.data))) + min(orig.data), digits )
            )
    return (labels)
}

#temp independent
ind_data = read.csv("../Contours_Temperature_Indep/out_Temperature_NOx_22102015.csv")
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
NOx.Emissions.labels = lapply(NOx.Emissions.labels, function (i) sprintf("%0.2e", i))

ind_slopes = ind_d %>%  group_by(Mechanism, Scaled.NOx.Emissions) %>%
                do(model = lm(O3 ~ Temperature, data = .)) %>%
                mutate(Slope = sprintf("%.1f", abs(summary(model)$coeff[2])), R2 = summary(model)$r.squared) %>%
                select(-model)

ind_test = ind_slopes[rep(seq_len(nrow(ind_slopes)), each = length(ind_df$Temperature)),] %>% cbind(ind_df$Temperature)
names(ind_test) = c("Mechanism", "NOx.Emissions", "Slope", "R2", "Temperature")
ind_test$Run = rep("Temperature Independent", length(ind_test$R2))

#temp dependent
dep_data = read.csv("../Contours_Temperature_Dep/out_Temperature_NOx_23102015.csv")
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

#my.colours = c( "0.0" = "#f0f0f0", 
#                "0.1" = "#e8e8e8", 
#                "0.2" = "#d8d8d8", 
#                "0.3" = "#c8c8c8", 
#                "0.4" = "#b8b8b8", 
#                "0.5" = "#a8a8a8", 
#                "0.6" = "#989898", 
#                "0.7" = "#909090", 
#                "0.8" = "#888888", 
#                "0.9" = "#787878", 
#                "1.0" = "#686868", 
#                "1.1" = "#585858",
#                "1.2" = "#484848",
#                "1.3" = "#303030",
#                "1.4" = "#181818",
#                "1.5" = "#000000")
my.colours = c( "0.0" = "#000000",
                "0.1" = "#080808",
                "0.2" = "#0f0f0f",
                "0.3" = "#171717",
                "0.4" = "#1f1f1f",
                "0.5" = "#262626",
                "0.6" = "#2e2e2e",
                "0.7" = "#363636",
                "0.8" = "#3d3d3d",
                "0.9" = "#454545",
                "1.0" = "#4d4d4d",
                "1.1" = "#545454",
                "1.2" = "#5c5c5c",
                "1.3" = "#636363",
                "1.4" = "#6b6b6b",
                "1.5" = "#737373")

p = ggplot(df, aes(x = Temperature, y = NOx.Emissions))
p = p + geom_tile(data  = test, aes(fill = factor(Slope)))
p = p + stat_contour(aes(z = O3, colour = ..level..)) 
p = p + facet_grid(Mechanism ~ Run)
p = p + scale_colour_gradient(low = "#7d3f96", high = "#7d3f96")
p = p + scale_fill_grey(limits = rev(levels(factor(test$Slope))), name = expression(bold(paste(m[O3-T]))))
#p = p + scale_fill_manual(values = my.colours, limits = rev(levels(factor(test$Slope))), name = expression(bold(paste(m[O3-T]))))
p = p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("NOx Emissions (molecules cm-3 s-1)")
p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels)
p = p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels, expand = c(0, 0))
p = p + theme_tufte()
p = p + theme(panel.margin = unit(0.2, "mm"))
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))

CairoPDF(file = "O3_contours_with_m.pdf", width = 10, height = 7)
print(direct.label(p))
dev.off()
