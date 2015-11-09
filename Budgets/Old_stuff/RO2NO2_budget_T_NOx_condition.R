get_NOx_condition = function (x) {
    if (x < 1002076928) {
        condition = "Low-NOx"
    } else if (x > 2013518336) {
        condition = "High-NOx" 
    } else {
        condition = "Maximal-O3"
    }
    return(condition)
}

d = read.csv("RADM2_RO2NO2_budget_22102015.txt")
d = d %>% mutate(Temperature.C = Temperature - 273)

net.budget = d %>%  select(Temperature.C, Total.NOx.Emissions, Reaction, Rate) %>% 
                    filter(Reaction == "HO2 + NO2 = HO2NO2" | Reaction == "ACO3 + NO2 = PAN") %>% 
                    rowwise() %>% 
                    mutate(NOx.Condition = get_NOx_condition(Total.NOx.Emissions)) %>% 
                    spread(Reaction, Rate) 

names(net.budget) = c("Temperature", "Total.NOx.Emissions", "NOx.Condition", "PAN", "HO2NO2")
net.budget = net.budget %>%     mutate(Ratio = PAN / HO2NO2) %>%
                                group_by(NOx.Condition, Temperature) %>% 
                                summarise(Mean.Ratio = mean(Ratio))

net.budget$NOx.Condition = factor(net.budget$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))

p = ggplot(net.budget, aes(x = Temperature, y = Mean.Ratio))
p = p + facet_grid( ~ NOx.Condition)
p = p + geom_point()
p = p + ylab("Reaction Rate (molecules cm-3)") + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
p = p + scale_x_continuous(expand = c(0, 0))
p = p + scale_y_continuous(expand = c(0, 0))
p = p + theme_tufte()
p = p + theme(panel.margin = unit(5, "mm"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(legend.title = element_blank())
p = p + theme(axis.title = element_text(face = "bold"))

CairoPDF(file = "RO2NO2_Budgets_NOx_Condition_T.pdf", width = 10, height = 7)
print(p)
dev.off()
