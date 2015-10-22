# Plot ratios of O3 to different reactive nitrogen groups 
# Version 0: Jane Coates 22/10/2015

d = read.csv("Temperature_NOx_data.csv")
d = tbl_df(d)

my.colours = c("MCMv3.2" = "#000000", "CB05" = "#0e5c28", "RADM2" = "#f9c500", "MOZART-4" = "#6c254f", "CRIv2" = "#ef6638")

plot.lines = function () {
    list(   geom_point(),
            facet_wrap( ~ Temperature, ncol = 9),
            xlab("NOx Emissions (molecules cm-3 s-1)"),
            theme_tufte(),
            theme(axis.line = element_line(colour = "black")),
            theme(axis.title = element_text(face = "bold")),
            theme(strip.text = element_text(face = "bold")),
            theme(legend.title = element_blank()),
            theme(legend.position = "top"),
            scale_colour_manual(values = my.colours)
    )
}

#PAN
pan.o3 = d %>% mutate(O3.PAN.Ratio = O3/PAN) %>% select(Mechanism, NOx.Emissions, Temperature, O3.PAN.Ratio)
ratio.pan.o3 = pan.o3 %>% group_by(Mechanism, Temperature, NOx.Emissions) %>% summarise(Max.Ratio = max(O3.PAN.Ratio))

p = ggplot(ratio.pan.o3, aes(x = NOx.Emissions, y = Max.Ratio, colour = Mechanism))
p = p + plot.lines()
p = p + ylab("O3 / PAN (ppbv/ppbv)")

CairoPDF(file = "O3_PAN_ratios.pdf", width = 10, height = 7)
print(p)
dev.off()

#RONO2
rono2.data = d %>% mutate(O3.RONO2.Ratio = O3 / RONO2) %>% select(Mechanism, NOx.Emissions, Temperature, O3.RONO2.Ratio)
ratio.rono2 = rono2.data %>% group_by(Mechanism, Temperature, NOx.Emissions) %>% summarise(Max.Ratio = max(O3.RONO2.Ratio))

p1 = ggplot(ratio.rono2, aes(x = NOx.Emissions, y = Max.Ratio, colour = Mechanism)) 
p1 = p1 + plot.lines()
p = p + ylab("O3 / RONO2 (ppbv/ppbv)")

CairoPDF(file = "O3_RONO2_ratios.pdf", width = 10, height = 7)
print(p1)
dev.off()

#RO2NO2
ro2no2.data = d %>% mutate(O3.RO2NO2.Ratio = O3 / RO2NO2) %>% select(Mechanism, NOx.Emissions, Temperature, O3.RO2NO2.Ratio)
ratio.ro2no2 = ro2no2.data %>% group_by(Mechanism, Temperature, NOx.Emissions) %>% summarise(Max.Ratio = max(O3.RO2NO2.Ratio))
p2 = ggplot(ratio.ro2no2, aes(x = NOx.Emissions, y = Max.Ratio, colour = Mechanism)) 
p2 = p2 + plot.lines()
p = p + ylab("O3 / RO2NO2 (ppbv/ppbv)")

CairoPDF(file = "O3_RO2NO2_ratios.pdf", width = 10, height = 7)
print(p2)
dev.off()

#NOy
NOy.data = d %>% mutate(NOy = NOx + HNO3 + RO2NO2 + RONO2, O3.NOy.Ratio = O3 / NOy) %>% select(Mechanism, NOx.Emissions, Temperature, O3.NOy.Ratio)
ratio.NOy = NOy.data %>% group_by(Mechanism, Temperature, NOx.Emissions) %>% summarise(Max.Ratio = max(O3.NOy.Ratio))
p3 = ggplot(ratio.NOy, aes(x = NOx.Emissions, y = Max.Ratio, colour = Mechanism)) 
p3 = p3 + plot.lines()
p = p + ylab("O3 / NOy (ppbv/ppbv)")

CairoPDF(file = "O3_NOy_ratios.pdf", width = 10, height = 7)
print(p3)
dev.off()
