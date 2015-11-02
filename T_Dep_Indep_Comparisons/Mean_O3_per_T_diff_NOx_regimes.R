# Plot mean O3 at each Temperature for different NOx conditions, determined on H2O2/HNO3 ratio. Each mechanism and run
# Version 0: Jane Coates 27/10/2015

runs = c("Dependent", "Independent")
mechanisms = c("CB05", "RADM2", "MOZART-4")

get_NOx_condition = function (x) {
    if (x > 0.5) {
        condition = "Low-NOx"
    } else if (x < 0.3) {
        condition = "High-NOx"
    } else {
        condition = "Maximal-O3"
    }
    return (condition)
}

mechanism_data_frame = function (mechanism, dataframe) {
    data = dataframe %>%    mutate(H2O2.HNO3.Ratio = H2O2/HNO3, Temperature.C = Temperature - 273) %>% 
                            select(Mechanism, NOx.Emissions, Temperature.C, O3, H2O2.HNO3.Ratio) %>%
                            rowwise() %>% 
                            mutate(NOx.Condition = get_NOx_condition(H2O2.HNO3.Ratio))
    summarised.data = data %>%  group_by(Mechanism, Temperature.C, NOx.Condition) %>%
                                summarise(Mean.O3 = max(O3))
    summarised.data$NOx.Condition = factor(summarised.data$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))
    return(summarised.data)
}

get_data = function (run) {
    filename = paste0("Temperature_", run, "_data.csv")
    d = read.csv(filename)
    data = lapply(mechanisms, mechanism_data_frame, dataframe = d)
    df = do.call("rbind", data)
    df$Run = rep(paste("Temperature", run, "\nIsoprene Emissions"), length(df$NOx.Condition)) 
    df$Print = rep(paste("Temperature", run), length(df$NOx.Condition)) 
    return(df)
}
list.data = lapply(runs, get_data)
data = do.call("rbind", list.data)

my.colours = c("MCMv3.2" = "#000000", "CB05" = "#0e5c28", "RADM2" = "#e6ab02", "MOZART-4" = "#6c254f", "CRIv2" = "#ef6638")

regression.data = data %>%  select(-Run) %>% 
                            group_by(Mechanism, NOx.Condition, Print) %>% 
                            do(model = lm(Mean.O3 ~ Temperature.C, data = .)) %>% 
                            mutate(Slope = summary(model)$coeff[2], Intercept = summary(model)$coeff[1], R2 = summary(model)$r.squared) %>% 
                            select(-model)
write.table(regression.data, file = "Regressions_statistics_Mean_O3_T_NOx.txt", quote = FALSE, row.name = FALSE, sep = ",")
regression.data$NOx.Condition = factor(regression.data$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))

reg.p = ggplot(regression.data, aes(x = NOx.Condition, y = Slope, colour = Mechanism, group = Mechanism))
reg.p = reg.p + geom_point() + geom_line()
reg.p = reg.p + facet_grid(. ~ Print)
reg.p = reg.p + ylab(expression(bold(paste("Slope of Mean O3 vs Temperature (ppbv / ", degree, "C)"))))
reg.p = reg.p + theme_tufte()
reg.p = reg.p + theme(axis.line = element_line(colour = "black"))
reg.p = reg.p + theme(axis.title = element_text(face = "bold"))
reg.p = reg.p + theme(strip.text = element_text(face = "bold"))
reg.p = reg.p + theme(axis.title.x = element_blank())
reg.p = reg.p + theme(legend.title = element_blank())
reg.p = reg.p + theme(legend.position = "top")
reg.p = reg.p + scale_colour_manual(values = my.colours)

CairoPDF(file = "Slope_O3_T_NOx.pdf", width = 10, height = 7)
print(reg.p)
dev.off()

p = ggplot(data, aes(x = Temperature.C, y = Mean.O3, colour = Mechanism))
p = p + geom_point()
p = p + geom_line()
p = p + ylab("Mean O3 (ppbv)") + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
p = p + stat_smooth(method = lm, se = FALSE)
p = p + facet_grid(NOx.Condition ~ Run)
p = p + theme_tufte()
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(legend.title = element_blank())
p = p + theme(legend.position = "top")
p = p + theme(strip.text.y = element_text(angle = 0))
p = p + scale_colour_manual(values = my.colours)

CairoPDF(file = "Mean_O3_T_NOx_conditions.pdf", width = 7, height = 10)
print(p)
dev.off()
