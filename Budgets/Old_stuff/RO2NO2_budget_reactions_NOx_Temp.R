library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(Cairo)
library(ggthemes)

args = commandArgs(trailingOnly = TRUE) #date ddmmyyyy

<<<<<<< HEAD
mechanisms = c("CB05", "CRIv2", "MCMv3.2", "MOZART-4", "RADM2")
=======
mechanisms = c("CB05", "CRIv2", "MOZART-4", "RADM2")
>>>>>>> 6d2581598d330cb9b1ae9af42242f93ea4294504
my.colours = c("MCMv3.2" = "#000000", "CB05" = "#0e5c28", "RADM2" = "#f9c500", "MOZART-4" = "#6c254f", "CRIv2" = "#ef6638")

get.production.data = function(mechanism) {
    filename = paste0(mechanism, "_RO2NO2_budget_", args[[1]], ".txt")
    d = read.csv(filename)
    d = d %>% select(-Max.NOx.Mixing.Ratio, -Net.Reaction.Rate)
    prod = d %>% filter(Rate > 0)
    sum.prod = prod %>% group_by(Mechanism, Temperature, Total.NOx.Emissions) %>% summarise(Total.Production = sum(Rate))
    return(sum.prod)
}

prod.data.list = lapply(mechanisms, get.production.data)
prod.data.df = as.data.frame(do.call("rbind", prod.data.list))
prod.data.df$Temperature = as.numeric(as.character(prod.data.df$Temperature))
prod.data.df = prod.data.df %>% mutate(Temperature.C = Temperature - 273)
<<<<<<< HEAD
prod.data.df$Mechanism = factor(prod.data.df$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "RADM2", "CB05"))
=======
#data.df$Mechanism = factor(data.df$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "RADM2", "CB05"))
>>>>>>> 6d2581598d330cb9b1ae9af42242f93ea4294504

p = ggplot(prod.data.df, aes(x = Total.NOx.Emissions, y = Total.Production, colour = Mechanism))
p = p + geom_point()
p = p + facet_wrap(~ Temperature.C, scales = "free")
p = p + xlab("Total NOx Emissions (molecules cm-3)") + ylab("Net Production Budget (molecules cm-3)")
p = p + ggtitle("RO2NO2 Production Budget with Total NOx emissions at each Temperature")
p = p + theme_tufte()
p = p + theme(legend.position = "top")
p = p + theme(plot.title = element_text(face = "bold"))
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(legend.title = element_blank())
p = p + scale_y_continuous(limits = c(0, 6e9))
p = p + scale_colour_manual(values = my.colours)

CairoPDF(file = "RO2NO2_total_production_budget_facet_Temp.pdf", width = 20, height = 14)
print(p)
dev.off()

get.consumption.data = function(mechanism) {
    filename = paste0(mechanism, "_RO2NO2_budget_", args[[1]], ".txt")
    d = read.csv(filename)
    d = d %>% select(-Max.NOx.Mixing.Ratio, -Net.Reaction.Rate)
    cons = d %>% filter(Rate < 0)
    sum.cons = cons %>% group_by(Mechanism, Temperature, Total.NOx.Emissions) %>% summarise(Total.Consumption = sum(Rate))
    return(sum.cons)
}

cons.data.list = lapply(mechanisms, get.consumption.data)
cons.data.df = as.data.frame(do.call("rbind", cons.data.list))
cons.data.df$Temperature = as.numeric(as.character(cons.data.df$Temperature))
cons.data.df = cons.data.df %>% mutate(Temperature.C = Temperature - 273)
<<<<<<< HEAD
cons.data.df$Mechanism = factor(cons.data.df$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "RADM2", "CB05"))
=======
#data.df$Mechanism = factor(data.df$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "RADM2", "CB05"))
>>>>>>> 6d2581598d330cb9b1ae9af42242f93ea4294504

p = ggplot(cons.data.df, aes(x = Total.NOx.Emissions, y = Total.Consumption, colour = Mechanism))
p = p + geom_point()
p = p + facet_wrap(~ Temperature.C, scales = "free")
p = p + xlab("Total NOx Emissions (molecules cm-3)") + ylab("Net Consumption Budget (molecules cm-3)")
p = p + ggtitle("RO2NO2 Consumption Budget with Total NOx emissions at each Temperature")
p = p + theme_tufte()
p = p + theme(legend.position = "top")
p = p + theme(plot.title = element_text(face = "bold"))
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(legend.title = element_blank())
p = p + scale_y_continuous(limits = c(-6e9, 0))
p = p + scale_colour_manual(values = my.colours)

CairoPDF(file = "RO2NO2_total_consumption_budget_facet_Temp.pdf", width = 20, height = 14)
print(p)
dev.off()

names(prod.data.df) = c("Mechanism", "Temperature", "Total.NOx.Emissions", "Total.Budget", "Temperature.C")
names(cons.data.df) = c("Mechanism", "Temperature", "Total.NOx.Emissions", "Total.Budget", "Temperature.C")
all.data = rbind(prod.data.df, cons.data.df)
p = ggplot(all.data, aes(x = Total.NOx.Emissions, y = Total.Budget, colour = Mechanism))
p = p + geom_point(data = subset(all.data, Total.Budget > 0))
p = p + geom_point(data = subset(all.data, Total.Budget < 0))
p = p + facet_wrap(~ Temperature.C, scales = "free")
p = p + xlab("Total NOx Emissions (molecules cm-3)") + ylab("Budget (molecules cm-3)")
p = p + ggtitle("RO2NO2 Budget with Total NOx emissions at each Temperature")
p = p + theme_tufte()
p = p + theme(legend.position = "top")
p = p + theme(plot.title = element_text(face = "bold"))
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(legend.title = element_blank())
p = p + scale_y_continuous(limits = c(-6e9, 6e9))
p = p + scale_colour_manual(values = my.colours)

CairoPDF(file = "RO2NO2_total_budget_facet_Temp.pdf", width = 20, height = 14)
print(p)
dev.off()
