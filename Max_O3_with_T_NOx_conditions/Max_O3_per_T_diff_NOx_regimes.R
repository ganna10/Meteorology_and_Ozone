# Plot max O3 at each Temperature for different NOx conditions, determined on H2O2/HNO3 ratio
# Version 0: Jane Coates 22/10/2015

get_NOx_condition = function (x) {
    if (x > 0.5) {
        condition = "Low-NOx"
    } else if (x < 0.05) {
        condition = "High-NOx"
    } else {
        condition = "Maximal-O3"
    }
    return (condition)
}

d = read.csv("Temperature_Independent_Data.csv")
d = tbl_df(d)
d = d %>% mutate(H2O2.HNO3.Ratio = H2O2/HNO3, Temperature.C = Temperature - 273)
o3.data = d %>% select(Mechanism, NOx.Emissions, Temperature.C, O3, H2O2.HNO3.Ratio)
o3.data = o3.data %>% rowwise() %>% mutate(NOx.Condition = get_NOx_condition(H2O2.HNO3.Ratio))
#print.data.frame(o3.data %>% filter(NOx.Condition == "High-NOx") %>% group_by(Mechanism, Temperature.C) %>% summarise(Max.NOx.Emissions = min(NOx.Emissions)))
o3.data = o3.data %>% group_by(Mechanism, Temperature.C, NOx.Condition) %>% summarise(Max.O3 = max(O3))
o3.data$NOx.Condition = factor(o3.data$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))


my.colours = c("MCMv3.2" = "#000000", "CB05" = "#0e5c28", "RADM2" = "#f9c500", "MOZART-4" = "#6c254f", "CRIv2" = "#ef6638")

regression.data = o3.data %>% group_by(Mechanism, NOx.Condition) %>% do(model = lm(Max.O3 ~ Temperature.C, data = .)) %>% mutate(Slope = summary(model)$coeff[2], Intercept = summary(model)$coeff[1], R2 = summary(model)$r.squared) %>% select(-model)
write.table(regression.data, file = "Regressions_statistics_Max_O3_T_NOx.txt", quote = FALSE, row.name = FALSE, sep = ",")
regression.data$NOx.Condition = factor(regression.data$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))

reg.p = ggplot(regression.data, aes(x = NOx.Condition, y = Slope, colour = Mechanism, group = Mechanism))
reg.p = reg.p + geom_point() + geom_line()
reg.p = reg.p + ylab(expression(bold(paste("Slope of Max O3 vs Temperature (ppbv / ", degree, "C)"))))
reg.p = reg.p + theme_tufte()
reg.p = reg.p + theme(axis.line = element_line(colour = "black"))
reg.p = reg.p + theme(axis.title = element_text(face = "bold"))
reg.p = reg.p + theme(axis.title.x = element_blank())
reg.p = reg.p + theme(legend.title = element_blank())
reg.p = reg.p + theme(legend.position = "top")
reg.p = reg.p + scale_colour_manual(values = my.colours)

CairoPDF(file = "Slope_O3_T_NOx.pdf", width = 10, height = 7)
print(reg.p)
dev.off()

p = ggplot(o3.data, aes(x = Temperature.C, y = Max.O3, colour = Mechanism))
p = p + geom_point()
p = p + geom_line()
p = p + ylab("Maximum O3 (ppbv)") + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
p = p + stat_smooth(method = lm, se = FALSE)
p = p + facet_wrap( ~ NOx.Condition)
p = p + theme_tufte()
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(legend.title = element_blank())
p = p + theme(legend.position = "top")
p = p + scale_colour_manual(values = my.colours)

CairoPDF(file = "Max_O3_T_NOx_conditions.pdf", width = 10, height = 7)
print(p)
dev.off()
