# Plot ratios of O3 to different reactive nitrogen groups 
# Version 0: Jane Coates 30/10/2015

runs = c("Dependent", "Independent")
mechanisms = c("CB05", "RADM2", "MOZART-4", "CRIv2", "MCMv3.2")

get_data = function (run) {
    filename = paste0("Temperature_", run, "_data.csv")
    d = read.csv(filename)
    d$Run = rep(paste("Temperature", run, "\nIsoprene Emissions"), length(d$Temperature)) 
    return(d)
}
list.data = lapply(runs, get_data)
data = do.call("rbind", list.data)

my.colours = c("MCMv3.2" = "#000000", "CB05" = "#0e5c28", "RADM2" = "#f9c500", "MOZART-4" = "#6c254f", "CRIv2" = "#ef6638")

plot.lines = function () {
    list(   geom_point(),
            xlab(expression(bold(paste("Temperature (", degree, "C)")))),
            facet_wrap( ~ Run),
            theme_tufte(),
            theme(axis.line = element_line(colour = "black")),
            theme(axis.title = element_text(face = "bold")),
            theme(strip.text = element_text(face = "bold")),
            theme(legend.title = element_blank()),
            theme(legend.position = "top"),
            scale_colour_manual(values = my.colours)
    )
}

#RONO2
rono2.data = data %>%   mutate(O3.RONO2.Ratio = O3 / RONO2) %>% 
                        select(Mechanism, Temperature, Run, O3.RONO2.Ratio)
ratio.rono2 = rono2.data %>%    group_by(Mechanism, Run, Temperature) %>% 
                                summarise(Mean.Ratio = max(O3.RONO2.Ratio)) %>% 
                                mutate(Temperature.C = Temperature - 273)

p1 = ggplot(ratio.rono2, aes(x = Temperature.C, y = Mean.Ratio, colour = Mechanism)) 
p1 = p1 + plot.lines()
p1 = p1 + ylab("O3 / RONO2 (ppbv/ppbv)")

CairoPDF(file = "O3_RONO2_ratios.pdf", width = 10, height = 7)
print(p1)
dev.off()

#RO2NO2
ro2no2.data = data %>%  mutate(O3.RO2NO2.Ratio = O3 / RO2NO2) %>% 
                        select(Mechanism, Temperature, Run, O3.RO2NO2.Ratio)
ratio.ro2no2 = ro2no2.data %>%  group_by(Mechanism, Run, Temperature) %>% 
                                summarise(Mean.Ratio = max(O3.RO2NO2.Ratio)) %>% 
                                mutate(Temperature.C = Temperature - 273)

p2 = ggplot(ratio.ro2no2, aes(x = Temperature.C, y = Mean.Ratio, colour = Mechanism)) 
p2 = p2 + plot.lines()
p2 = p2 + ylab("O3 / RO2NO2 (ppbv/ppbv)")

CairoPDF(file = "O3_RO2NO2_ratios.pdf", width = 10, height = 7)
print(p2)
dev.off()
