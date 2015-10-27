# Plot differences in mean O3 at each Temperature for different NOx conditions, determined on H2O2/HNO3 ratio
# Version 0: Jane Coates 22/10/2015

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

d = read.csv("Temperature_Independent_Data.csv")
d = d %>%   mutate(h2o2.hno3.ratio  = H2O2 / HNO3, Temperature.C = Temperature - 273) %>% 
            select(Mechanism, h2o2.hno3.ratio, Temperature.C, O3) %>% 
            rowwise() %>% 
            mutate(NOx.Condition = get_NOx_condition(h2o2.hno3.ratio)) %>% 
            group_by(Mechanism, Temperature.C, NOx.Condition) %>% 
            summarise(Mean.O3 = mean(O3))
d$NOx.Condition = factor(d$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))

mcm.data = d %>% filter(Mechanism == "MCMv3.2")
data = d %>% filter(Mechanism != "MCMv3.2")

Abs.O3.Diff = mcm.data$Mean.O3 - data$Mean.O3 
Rel.O3.Diff = Abs.O3.Diff / data$Mean.O3
df = data.frame(Abs.O3.Diff, Rel.O3.Diff, Mechanism = data$Mechanism, Temperature.C = data$Temperature.C, NOx.Condition = data$NOx.Condition)

my.colours = c("MCMv3.2" = "#000000", "CB05" = "#0e5c28", "RADM2" = "#f9c500", "MOZART-4" = "#6c254f", "CRIv2" = "#ef6638")

p = ggplot(df, aes(x = Temperature.C, y = Abs.O3.Diff, colour = Mechanism))
p = p + geom_point()
p = p + geom_line()
p = p + ggtitle("Absolute Difference from MCMv3.2 Mean O3")
p = p + ylab("O3 (ppbv)") + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
p = p + facet_wrap( ~ NOx.Condition)
p = p + theme_tufte()
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(legend.title = element_blank())
p = p + theme(legend.position = "top")
p = p + scale_colour_manual(values = my.colours)

CairoPDF(file = "Absolute_Differences_Mean_O3_T_NOx_conditions.pdf", width = 10, height = 7)
print(p)
dev.off()

p = ggplot(df, aes(x = Temperature.C, y = Rel.O3.Diff, colour = Mechanism))
p = p + geom_point()
p = p + geom_line()
p = p + ggtitle("Relative Difference from MCMv3.2 Mean O3")
p = p + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
p = p + facet_wrap( ~ NOx.Condition)
p = p + scale_y_continuous(labels = percent)
p = p + theme_tufte()
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(axis.title.y = element_blank())
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(legend.title = element_blank())
p = p + theme(legend.position = "top")
p = p + scale_colour_manual(values = my.colours)

CairoPDF(file = "Relative_Differences_Mean_O3_T_NOx_conditions.pdf", width = 10, height = 7)
print(p)
dev.off()
