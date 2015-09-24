library(reshape2)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(akima)
library(directlabels)
library(Cairo)
library(ggthemes)

d = read.csv(file = "out_Temperature_23092015.csv")
d = tbl_df(d)

mozart.data = d %>% filter(Mechanism == "MOZART") %>% select(O3, NOx.Emissions, Temperature)
moz.lm = lm(formula = I(log(O3) - log(NOx.Emissions)) ~ I(log(Temperature) - log(NOx.Emissions)), data = mozart.data)
summary(moz.lm)

f <- function( x, y ) {
    exp( coef( moz.lm )[ 1 ] ) * ( x ^ coef( moz.lm )[ 2 ] ) * ( y ^ ( 1 - coef( moz.lm )[ 2 ] ) )
}

y.interval = 1e9
x.interval = 100
x = seq( from = floor( min(mozart.data$Temperature) / x.interval ) * x.interval, to = ceiling( max(mozart.data$Temperature) / x.interval ) * x.interval, by = x.interval)
y = seq( from = floor(min(mozart.data$NOx.Emissions) / y.interval) * y.interval, to = ceiling(max(mozart.data$NOx.Emissions) / y.interval) * y.interval, by = y.interval)

O3.calc = f(x = mozart.data$Temperature, y = mozart.data$NOx.Emissions)
model = mozart.data %>% select(-O3) %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions))) %>% select(-Temperature, -NOx.Emissions)
model$O3 = O3.calc

mozart.fld = with(model, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = O3))
mozart.df = melt(mozart.fld$z, na.rm = TRUE)
names(mozart.df) = c("x", "y", "O3")
mozart.df$Temperature = mozart.fld$x[mozart.df$x]
mozart.df$NOx.Emissions = mozart.fld$y[mozart.df$y]
mozart.df$Mechanism = rep("MOZART-4", length(mozart.df$O3))

get.labels = function (break.points, orig.data, digits) {
    labels = lapply(break.points,
                    function (i) round ((i * (max(orig.data) - min(orig.data))) + min(orig.data), digits )
            )
    labels
}
temperature.break.points = seq(0, 1, 0.2)
temperature.labels = get.labels(temperature.break.points, mozart.data$Temperature, digits = 2)
NOx.Emissions.break.points = seq(0, 1, 0.2)
NOx.Emissions.labels = get.labels(NOx.Emissions.break.points, mozart.data$NOx.Emissions, digits = 2)
NOx.Emissions.labels = lapply(NOx.Emissions.labels, function (i) sprintf("%0.2e", i))

p = ggplot(mozart.df, aes(x = Temperature, y = NOx.Emissions, z = O3))
p = p + stat_contour(aes(colour = ..level..))
p = p + facet_wrap(~ Mechanism) 
p = p + theme_tufte() 
p = p + theme(axis.line = element_line(colour = "black")) 
p = p + theme(strip.text = element_text(face = "bold")) 
p = p + xlab("Temperature (K)") 
p = p + ylab("NOx emissions (molecules(NOx) cm-3 s-1)") 
p = p + theme(axis.title = element_text(face = "bold")) 
p = p + scale_colour_continuous(name = "O3 (ppbv)")
p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels)
p = p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels)

CairoPDF(file = "temperature_production_function.pdf")
print(direct.label(p, "last.points"))
dev.off()
