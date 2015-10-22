d = read.csv("Temperature_Independent_Data.csv")
d = tbl_df(d)
d = d %>% mutate(NOy = NOx + HNO3 + RONO2 + RO2NO2)

h2o2.hno3 = d %>% select(Mechanism, NOx.Emissions, Temperature, H2O2, HNO3) %>% mutate(H2O2.HNO3.Ratio = H2O2 / HNO3)
o3.noz = d %>% select(Mechanism, NOx.Emissions, Temperature, O3, NOx, NOy) %>% mutate(O3.NOz.Ratio = O3 / (NOy - NOx))

my.colours = c("MCMv3.2" = "#000000", "CB05" = "#0e5c28", "RADM2" = "#f9c500", "MOZART-4" = "#6c254f", "CRIv2" = "#ef6638")

plot.lines = function () {
    list(   geom_line(),
            facet_wrap( ~ Temperature, scales = "free_x", ncol = 9),
            xlab("NOx Emissions (molecules cm-3 s-1)"),
            theme_tufte(),
            theme(axis.title = element_text(face = "bold")),
            theme(strip.text = element_text(face = "bold")),
            theme(axis.line = element_line(colour = "black")),
            theme(legend.position = "top"),
            theme(legend.title = element_blank()),
            scale_colour_manual(values = my.colours)
    )
}

#print.data.frame(h2o2.hno3 %>% filter(Mechanism == "CRIv2") %>% select(NOx.Emissions, H2O2.HNO3.Ratio))

p = ggplot(h2o2.hno3, aes(x = NOx.Emissions, y = H2O2.HNO3.Ratio, colour = Mechanism))
p = p + ylab("H2O2/HNO3 in ppbv/ppbv")
p = p + scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, 25))
p = p + geom_hline(y = 5)
p = p + geom_hline(y = 20)
p = p + plot.lines()

CairoPDF(file = "H2O2_HNO3_ratios_facet_T.pdf", width = 12, height = 9)
print(p)
dev.off()

p1 = ggplot(o3.noz, aes(x = NOx.Emissions, y = O3.NOz.Ratio, colour = Mechanism))
p1 = p1 + ylab("O3/(NOy - NOx) in ppbv/ppbv")
p1 = p1 + scale_y_continuous(limits = c(0, 2500), breaks = seq(0, 2500, 250))
p1 = p1 + plot.lines()

CairoPDF(file = "O3_NOz_ratios_facet_T.pdf", width = 12, height = 9)
print(p1)
dev.off()
