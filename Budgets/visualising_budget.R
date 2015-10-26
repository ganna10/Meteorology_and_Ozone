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

net.budget = d %>%  select(Mechanism, Temperature, Total.NOx.Emissions, Net.Reaction.Rate) %>% 
                    distinct(Mechanism, Temperature, Total.NOx.Emissions, Net.Reaction.Rate) %>% 
                    rowwise() %>% 
                    mutate(NOx.Condition = get_NOx_condition(Total.NOx.Emissions)) %>% 
                    group_by(Mechanism, NOx.Condition, Temperature) %>% 
                    summarise(Mean.Rate = mean(Net.Reaction.Rate), Median.Rate = median(Net.Reaction.Rate))

#to check whether mean or median
stats = net.budget %>% gather(Statistic, Value, -Mechanism, -NOx.Condition, -Temperature)

p = ggplot(stats, aes(x = Temperature, y = Value, colour = Statistic))
p = p + facet_grid(~ NOx.Condition)
p = p + geom_point()

CairoPDF(file = "net_budgets_stat_check.pdf")
print(p)
dev.off()
