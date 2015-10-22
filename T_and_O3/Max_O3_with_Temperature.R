# plot maximum O3 at each T and NOx combination. date ddmmyyyy is argument
# Version 0: Jane Coates 19/10/2015

library(ggplot2)
library(dplyr, warn.conflicts=FALSE)
library(tidyr)
library(Cairo)
library(directlabels)
library(ggthemes)

args = commandArgs(trailingOnly = TRUE)

filename = paste0("out_Temperature_NOx_", args[[1]], ".csv")
d = read.csv(filename)
d = tbl_df(d)

filter.temps = function (temp, data) { temp.data = data %>% filter(Temperature == temp)
    max.o3 = max(temp.data$O3)
    info.data = c(temp, max.o3)
    return(info.data)
}

mechanisms = c("CB05")
#mechanisms = c("MCM", "CB05", "CRI", "MOZART", "RADM2")

get.mechanism.data = function (mechanism, data) {
    filtered = data %>% filter(Mechanism == mechanism) %>% select(Temperature, O3)
    filtered.levels = levels(factor(filtered$Temperature))
    max.O3.matrix = lapply(filtered.levels, filter.temps, data = filtered)
    max.O3.df = as.data.frame(do.call("rbind", max.O3.matrix))
    names(max.O3.df) = c("Temperature", "Max.O3")
    if (mechanism == "MCM") {
        max.O3.df$Mechanism = rep("MCMv3.2", length(max.O3.df$Temperature))
    } else if (mechanism == "CRI") {
        max.O3.df$Mechanism = rep("CRIv2", length(max.O3.df$Temperature))
    } else if (mechanism == "MOZART") {
        max.O3.df$Mechanism = rep("MOZART-4", length(max.O3.df$Temperature))
    } else {
        max.O3.df$Mechanism = rep(mechanism, length(max.O3.df$Temperature))
    }
    return(max.O3.df)
}

max.O3.all.data = lapply(mechanisms, get.mechanism.data, data = d)
final.df = as.data.frame(do.call("rbind", max.O3.all.data))
final.df$Max.O3 = as.numeric(as.character(final.df$Max.O3))
final.df$Temperature = as.numeric(as.character(final.df$Temperature))
final.df = final.df %>% mutate(Temperature.C = Temperature - 273)
final.df = final.df %>% mutate(log.Max.O3 = log(Max.O3))
#final.df$Mechanism = factor(final.df$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "RADM2", "CB05"))

my.colours = c("MCMv3.2" = "#000000", "CB05" = "#0e5c28", "RADM2" = "#f9c500", "MOZART-4" = "#6c254f", "CRIv2" = "#ef6638")

lm_eqn = function(m) {

  l <- list(a = format(coef(m)[1], digits = 2),
      b = format(abs(coef(m)[2]), digits = 2),
      r2 = format(summary(m)$r.squared, digits = 3));

  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }

  as.character(as.expression(eq));                 
}

p = ggplot(final.df, aes(x = Temperature.C, y = Max.O3, colour = Mechanism, group = Mechanism))
p = p + geom_point()
p = p + geom_line()
p = p + stat_smooth(method = lm, se = FALSE, formula = y ~ x)
## using linear approximation as this is what is typically reported and the R2 is not that bad
p = p + geom_text(aes(x = 20, y = 4.4, label = lm_eqn(lm(Max.O3 ~ Temperature.C, final.df))), parse = TRUE, show_guide = FALSE)
p = p + theme_tufte()
p = p + ggtitle("Temperature Independent BVOC Emissions, Maximum O3 at each Temperature")
p = p + theme(plot.title = element_text(face = "bold"))
p = p + theme(axis.line = element_line(colour = "black"))
p = p + ylab("Log of Maximum O3 (ppbv)") + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
p = p + theme(legend.title = element_blank())
p = p + theme(legend.position = c(0.5, 1), legend.justification = c(0.5, 1))
p = p + theme(axis.title = element_text(face = "bold"))
p = p + scale_x_continuous(limits = c(15, 40), expand = c(0, 0.2), breaks = seq(15, 40, 5))
#p = p + scale_y_continuous(limits = c(4, 4.6), expand = c(0, 0))
p = p + scale_colour_manual(values = my.colours, guide = guide_legend(direction = "horizontal"))

CairoPDF(file = "Max_O3_with_Temperature.pdf", width = 10, height = 7)
print(p)
dev.off()
