td = read.csv("TD/CB05_O3_budget_05112015.txt")
ti = read.csv(file = "TI/CB05_O3_budget_05112015.txt")

td = td %>% select(Mechanism, Temperature, Total.NOx.Emissions, Net.Reaction.Rate)
test = ti %>% filter(Total.NOx.Emissions <= 1870862)

td = td %>% distinct(Mechanism, Temperature, Total.NOx.Emissions, Net.Reaction.Rate)
test1 = td %>% filter(Total.NOx.Emissions <= 1870862)

bins = td %>% group_by(Mechanism, Temperature, cut(Total.NOx.Emissions, 15)) %>% summarise(NOx.Emission.Bins = mean(Total.NOx.Emissions), Mean.Net.Rate = mean(Net.Reaction.Rate)) 
bins = bins %>% select(Mechanism, Temperature, NOx.Emission.Bins, Mean.Net.Rate)

p = ggplot(bins, aes(x = Temperature, y = Mean.Net.Rate, colour = Mechanism))
p = p + geom_point() + facet_wrap( ~ NOx.Emission.Bins)
