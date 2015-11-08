data = read.csv("../Contours_Temperature_Indep/out_Temperature_NOx_22102015.csv")

d = data %>% select(Mechanism, Temperature, NOx.Emissions, O3)
data = d %>% group_by(Mechanism, Temperature, cut(NOx.Emissions, 15)) %>% summarise(Mean.NOx = mean(NOx.Emissions), Mean.O3 = mean(O3))

my.colours = c("MCMv3.2" = "#000000", "CB05" = "#0e5c28", "RADM2" = "#f9c500", "MOZART-4" = "#6c254f", "CRIv2" = "#ef6638")

plot_lines = function () {
    list(   geom_point(),
            theme_tufte(),
            theme(strip.text = element_text(face = "bold")),
            theme(axis.title = element_text(face = "bold")),
            theme(axis.line = element_line(colour = "black")),
            theme(legend.title = element_blank()),
            theme(legend.position = "top"),
            scale_colour_manual(values = my.colours)
    )
}

p = ggplot(data, aes(x = Temperature, y = Mean.O3, colour = Mechanism))
p = p + facet_wrap(~ Mean.NOx, scales = "free_y", ncol = 5)
p = p + plot_lines()

CairoPDF(file = "Mean_O3_NOx_each_T.pdf", width = 10, height = 7)
print(p)
dev.off()

regression.data = data %>% group_by(Mechanism, Mean.NOx) %>% do(model = lm(Mean.O3 ~ Temperature, data = .)) %>% mutate(Slope = summary(model)$coeff[2], Intercept = summary(model)$coeff[1], R2 = summary(model)$r.squared) %>% select(-model)

p = ggplot(regression.data, aes(x = Mean.NOx, y = Slope, colour = Mechanism)) 
p = p + geom_line() 
p = p + plot_lines() 

CairoPDF(file = "Slopes_Mean_O3_NOx_each_T.pdf", width = 10, height = 7)
print(p)
dev.off()
