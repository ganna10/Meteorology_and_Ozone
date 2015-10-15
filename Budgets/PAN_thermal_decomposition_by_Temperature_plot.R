library(ggplot2)
library(dplyr, warn.conflicts=FALSE)
library(Cairo)
library(ggthemes)

d = read.csv(file = "PAN_thermal_decomposition_rate_constant_data.csv")
mechanisms = c("CB05", "CRIv2", "MCMv3.2", "MOZART-4", "RADM2")

get.data = function (mechanism, data) {
    df = data %>% filter(Mechanism == mechanism) %>% select(Mechanism, Temperature, Rate.Constant) %>% distinct(Temperature, Rate.Constant)
    return(df)
}
data.list = lapply(mechanisms, get.data, data = d)
data.df = as.data.frame(do.call("rbind", data.list))
data.df$Mechanism = factor(data.df$Mechanism, levels = c("MOZART-4", "MCMv3.2", "CRIv2", "RADM2", "CB05"))

cri = data.df %>% filter(Mechanism == "CRIv2")
mcm = data.df %>% filter(Mechanism == "MCMv3.2")
cri$Rate.Constant - mcm$Rate.Constant

my.colours = c("MCMv3.2" = "#000000", "CB05" = "#0e5c28", "RADM2" = "#f9c500", "MOZART-4" = "#6c254f", "CRIv2" = "#ef6638")

p = ggplot(data.df, aes(x = Temperature, y = Rate.Constant, colour = Mechanism))
p = p + geom_point()
p = p + xlab("Temperature (K)") + ylab("PAN Decomposition Rate Constant")
p = p + ggtitle("PAN thermal decomposition rate constant at each Temperature")
p = p + theme_tufte()
p = p + theme(legend.position = "top")
p = p + theme(plot.title = element_text(face = "bold"))
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(legend.title = element_blank())
p = p + scale_colour_manual(values = my.colours)

CairoPDF(file = "PAN_thermal_decomposition_plot.pdf", height = 7, width = 10)
print(p)
dev.off()
