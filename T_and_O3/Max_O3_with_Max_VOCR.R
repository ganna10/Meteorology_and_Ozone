library(ggplot2)
library(dplyr, warn.conflicts=FALSE)
library(tidyr)
library(Cairo)
library(directlabels)
library(ggthemes)

d = read.csv("out_Temperature_NOx_13102015.csv")
d = tbl_df(d)

filter.temps = function (temp, data) { temp.data = data %>% filter(Temperature == temp)
    max.vocr = max(temp.data$VOCR)
    info.data = c(temp, max.vocr)
    return(info.data)
}

mechanisms = c("MCM", "CB05", "CRI", "MOZART", "RADM2")

get.mechanism.data = function (mechanism, data) {
    filtered = data %>% filter(Mechanism == mechanism) %>% select(Temperature, VOCR)
    filtered.levels = levels(factor(filtered$Temperature))
    max.VOCR.matrix = lapply(filtered.levels, filter.temps, data = filtered)
    max.VOCR.df = as.data.frame(do.call("rbind", max.VOCR.matrix))
    names(max.VOCR.df) = c("Temperature", "Max.VOCR")
    if (mechanism == "MCM") {
        max.VOCR.df$Mechanism = rep("MCMv3.2", length(max.VOCR.df$Temperature))
    } else if (mechanism == "CRI") {
        max.VOCR.df$Mechanism = rep("CRIv2", length(max.VOCR.df$Temperature))
    } else if (mechanism == "MOZART") {
        max.VOCR.df$Mechanism = rep("MOZART-4", length(max.VOCR.df$Temperature))
    } else {
        max.VOCR.df$Mechanism = rep(mechanism, length(max.VOCR.df$Temperature))
    }
    return(max.VOCR.df)
}

max.VOCR.all.data = lapply(mechanisms, get.mechanism.data, data = d)
final.df = as.data.frame(do.call("rbind", max.VOCR.all.data))
final.df$Max.VOCR = as.numeric(as.character(final.df$Max.VOCR))
final.df$Temperature = as.numeric(as.character(final.df$Temperature))
final.df = final.df %>% mutate(Temperature.C = Temperature - 273)
final.df$Mechanism = factor(final.df$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "RADM2", "CB05"))

my.colours = c("MCMv3.2" = "#000000", "CB05" = "#0e5c28", "RADM2" = "#f9c500", "MOZART-4" = "#6c254f", "CRIv2" = "#ef6638")

p = ggplot(final.df, aes(x = Temperature.C, y = Max.VOCR, colour = Mechanism, group = Mechanism))
p = p + geom_point()
p = p + geom_line()
#p = p + stat_smooth(method = glm, family = gaussian(link = "log"))
p = p + theme_tufte()
p = p + ggtitle("Temperature Independent BVOC Emissions, Maximum VOCR at each Temperature")
p = p + theme(plot.title = element_text(face = "bold"))
p = p + theme(axis.line = element_line(colour = "black"))
p = p + ylab("Maximum VOCR (s-1)") + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
p = p + theme(legend.title = element_blank())
p = p + theme(legend.position = c(0.5, 1), legend.justification = c(0.5, 1))
p = p + theme(axis.title = element_text(face = "bold"))
p = p + scale_x_continuous(limits = c(15, 40), expand = c(0, 0.2), breaks = seq(15, 40, 5))
#p = p + scale_y_continuous(limits = c(50, 100), expand = c(0, 0))
p = p + scale_colour_manual(values = my.colours, guide = guide_legend(direction = "horizontal"))

CairoPDF(file = "Max_VOCR_with_Temperature.pdf", width = 10, height = 7)
print(p)
dev.off()
