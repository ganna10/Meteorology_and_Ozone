library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(Cairo)
library(ggthemes)

args = commandArgs(trailingOnly = TRUE) #date ddmmyyyy

mechanisms = c("CB05", "CRIv2", "MCMv3.2", "MOZART-4", "RADM2")

get.data = function(mechanism) {
    filename = paste0(mechanism, "_RO2NO2_budget_", args[[1]], ".txt")
    d = read.csv(filename)
    d$Max.NOx.Mixing.Ratio = d$Max.NOx.Mixing.Ratio * 1e9
    d = d %>% select(Mechanism, Total.NOx.Emissions, Temperature, Net.Reaction.Rate)
    return(d)
}
data.list = lapply(mechanisms, get.data)
data.df = as.data.frame(do.call("rbind", data.list))
data.df$Temperature = as.numeric(as.character(data.df$Temperature))
data.df = data.df %>% mutate(Temperature.C = Temperature - 273)
data.df$Mechanism = factor(data.df$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "RADM2", "CB05"))

my.colours = c("MCMv3.2" = "#000000", "CB05" = "#0e5c28", "RADM2" = "#f9c500", "MOZART-4" = "#6c254f", "CRIv2" = "#ef6638")

p = ggplot(data.df, aes(x = Total.NOx.Emissions, y = Net.Reaction.Rate, colour = Mechanism))
p = p + geom_point()
p = p + facet_wrap(~ Temperature.C, scales = "free")
p = p + xlab("Total NOx Emissions (molecules cm-3)") + ylab("Net RO2NO2 Budget (molecules cm-3)")
p = p + ggtitle("RO2NO2 Cumulative Net Budget with Total NOx emissions at each Temperature")
p = p + theme_tufte()
p = p + theme(legend.position = "top")
p = p + theme(plot.title = element_text(face = "bold"))
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(legend.title = element_blank())
p = p + scale_y_continuous(limits = c(0, 6e7))
p = p + scale_colour_manual(values = my.colours)

CairoPDF(file = "RO2NO2_net_budget_NOx_facet_Temp.pdf", width = 20, height = 14)
print(p)
dev.off()
