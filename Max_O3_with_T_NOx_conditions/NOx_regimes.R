d = read.csv("Temperature_Independent_Data.csv")
d = tbl_df(d)
d = d %>% mutate(H2O2.HNO3.Ratio = H2O2/HNO3)
o3.data = d %>% select(Mechanism, NOx.Emissions, Temperature, O3, H2O2.HNO3.Ratio)

nox.sens = o3.data %>% filter(H2O2.HNO3.Ratio > 0.3)
voc.sens = o3.data %>% filter(H2O2.HNO3.Ratio < 0.05)
nox.voc.sens = o3.data %>% filter(H2O2.HNO3.Ratio < 0.3 & H2O2.HNO3.Ratio > 0.05)

print.data.frame(voc.sens %>% group_by(Mechanism, Temperature) %>% summarise(Max.O3 = max(O3)))
print.data.frame(nox.sens %>% group_by(Mechanism, Temperature) %>% summarise(Max.O3 = max(O3)))
print.data.frame(nox.voc.sens %>% group_by(Mechanism, Temperature) %>% summarise(Max.O3 = max(O3)))
print.data.frame(o3.data %>% group_by(Mechanism, Temperature) %>% summarise(Max.O3 = max(O3)))

max.O3.ratios = o3.data %>% group_by(Mechanism, Temperature) %>% slice(which.max(O3)) 
p = ggplot(max.O3.ratios, aes(x = Temperature, y = H2O2.HNO3.Ratio, colour = Mechanism))
p = p + geom_point() + stat_smooth(method = lm, se = FALSE, aes(colour = Mechanism))
