# plot isoprene emissions with T
# Version 0:Jane Coates 23/10/2015

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//MEGAN")
d = read.csv("MEGAN_c5h8_emissions_with_T.csv")
d = d %>% mutate(Temperature.C = Temperature - 273)

p = ggplot(d, aes(x = Temperature.C, y = C5H8.Emissions))
p = p + geom_point()
p = p + geom_line()
p = p + ylab("Isoprene Emissions (molecules cm-2 s-1)") + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
p = p + theme_tufte()
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(axis.title = element_text(face = "bold"))
p = p + scale_x_continuous(expand = c(0, 0.2))
p = p + scale_y_continuous(expand = c(0, 1e10))
p

CairoPDF(file = "isoprene_emissions.pdf", width = 5, height = 4)
print(p)
dev.off()
