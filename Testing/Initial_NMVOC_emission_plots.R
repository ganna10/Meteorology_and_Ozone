Temps = seq(15, 40, 0.5)
d = read.csv("Mapped_emissions.csv")
megan = read.csv("MEGAN_emissions_T.csv")

add.Ts = function (T, df) { df = df %>% mutate(T1 = rep(T, length(df$Species))) }

temp.indep = d %>% filter(Run == "T-Independent")
list.temp.indep = lapply(Temps, add.Ts, df = temp.indep)
temps.indep.df = do.call("rbind", list.temp.indep)
names(temps.indep.df) = c("Run", "Species", "Emissions", "Temperature")
indep.fractions = temps.indep.df %>% group_by(Run, Temperature) %>% mutate(Fraction.Emissions = Emissions / sum(Emissions))

temp.dep = d %>% filter(Run == "T-Dependent")
list.temp.dep = lapply(Temps, add.Ts, df = temp.dep)
temps.dep.df = do.call("rbind", list.temp.dep)
names(temps.dep.df) = c("Run", "Species", "Emissions", "Temperature")
temps.dep.df = rbind(temps.dep.df, megan)
dep.fractions = temps.dep.df %>% group_by(Run, Temperature) %>% mutate(Fraction.Emissions = Emissions / sum(Emissions))

fractions = rbind(indep.fractions, dep.fractions)
absolute = rbind(temps.indep.df, temps.dep.df)

plot.lines = function () {
    list(   facet_wrap( ~ Temperature, ncol = 3),
            geom_bar(stat = "identity", width = 0.97),
            scale_x_discrete(expand = c(0, 0)),
            theme_tufte(),
            theme(panel.margin.x = unit(5, "mm")),
            theme(strip.text = element_text(face = "bold")),
            theme(axis.title = element_text(face = "bold")),
            theme(axis.title.x = element_blank()),
            theme(axis.ticks = element_blank()),
            theme(legend.title = element_blank())
            #theme(legend.position = "top"),
            #guides(fill = guide_legend(label.position = "bottom", label.theme = element_text(angle = 45)))
    )
}

p = ggplot(fractions, aes(x = Run, y = Fraction.Emissions, fill = Species)) 
p = p + scale_y_continuous(labels = percent, expand = c(0, 0))
p = p + plot.lines()

CairoPDF(file = "NMVOC_emissions_fractions.pdf", width = 11, height = 15)
print(p)
dev.off()

p = ggplot(absolute, aes(x = Run, y = Emissions, fill = Species)) 
p = p + scale_y_continuous(expand = c(0, 0))
p = p + plot.lines()
p = p + geom_hline(yintercept = seq(5e11, 2.5e12, 5e11), colour = "white")

CairoPDF(file = "NMVOC_emissions_absolute.pdf", width = 10, height = 17)
print(p)
dev.off()
