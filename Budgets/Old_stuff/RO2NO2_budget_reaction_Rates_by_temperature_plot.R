library(ggplot2)
library(dplyr, warn.conflicts=FALSE)
library(Cairo)
library(ggthemes)

d = read.csv(file = "RO2NO2_budget_reaction_rate_constant_data.csv")
reactions = c("PAN = CH3CO3 + NO2", "CH3CO3 + NO2 = PAN")
<<<<<<< HEAD
mechanisms = c("CB05", "CRIv2", "MCMv3.2", "MOZART-4", "RADM2")

get.data = function (mechanism, data) {
    df = data %>% filter(Mechanism == mechanism, Reaction %in% reactions) %>% select(-NOx.Mixing.Ratio,-NOx.Emissions)
=======
mechanisms = c("CB05", "CRIv2", "MOZART-4", "RADM2")

get.data = function (mechanism, data) {
    df = data %>% filter(Mechanism == mechanism, Reaction %in% reactions) %>% select(-NOx.Mixing.Ratio,-NOx.Emissions) #%>% distinct(Temperature, Rate.Constant)
>>>>>>> 6d2581598d330cb9b1ae9af42242f93ea4294504
    return(df)
}
data.list = lapply(mechanisms, get.data, data = d)
data.df = as.data.frame(do.call("rbind", data.list))
<<<<<<< HEAD
data.df$Rate.Constant = as.numeric(as.character(data.df$Rate.Constant))
data.df$Temperature = as.numeric(as.character(data.df$Temperature))
data.df$Mechanism = factor(data.df$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "RADM2", "CB05"))
=======
#data.df$Rate.Constant = as.numeric(as.character(data.df$Rate.Constant))
#data.df$Temperature = as.numeric(as.character(data.df$Temperature))
#data.df$Mechanism = factor(data.df$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "RADM2", "CB05"))
>>>>>>> 6d2581598d330cb9b1ae9af42242f93ea4294504

#cri = data.df %>% filter(Mechanism == "CRIv2")
#mcm = data.df %>% filter(Mechanism == "MCMv3.2")
#cri$Rate.Constant - mcm$Rate.Constant

my.colours = c("MCMv3.2" = "#000000", "CB05" = "#0e5c28", "RADM2" = "#f9c500", "MOZART-4" = "#6c254f", "CRIv2" = "#ef6638")

p = ggplot(data.df, aes(x = Temperature, y = Rate.Constant, colour = Mechanism))
p = p + geom_point()
p = p + facet_wrap( ~ Reaction, scales = "free_y")
p = p + xlab("Temperature (K)") + ylab("Rate Constant")
p = p + ggtitle("RO2NO2 Budget Reactions rate constant at each Temperature")
p = p + theme_tufte()
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(legend.position = "top")
p = p + theme(plot.title = element_text(face = "bold"))
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(legend.title = element_blank())
p = p + scale_colour_manual(values = my.colours)

CairoPDF(file = "RO2NO2_reaction_rate_constants_plot.pdf", height = 7, width = 10)
print(p)
dev.off()
