# compare RO2NO2 reaction rates between mechanisms and Runs
# Jane Coates 11/11/2015

setwd('~/Documents//Analysis//2015_Meteorology_and_Ozone//Budgets')

arrange_data = function (dataframe) {
  data = dataframe %>% arrange(Temperature) %>%
    mutate(Temperature.C = Temperature - 273) %>%
    distinct(Mechanism, Temperature.C, Reaction) %>%
    select(-Temperature)
  return(data)
}

data = read.csv("TI_RO2NO2/RO2NO2_rate_constant_data.csv")
df = arrange_data(data)
df$Run = rep("Temperature Independent\nIsoprene Emissions", length(df$Mechanism))
pan.df = df %>% filter(grepl("PAN", Reaction) & !grepl("PANX|[A-Z]PAN|[0-9]PAN", Reaction) & !grepl("UNITY", Reaction))
pan.df$Rate.Constant = as.double(as.character(pan.df$Rate.Constant))

# get maximum difference in rate constant
#pan.df %>% filter(Reaction == "PAN = CH3CO3 + NO2")  %>% select(Mechanism, Rate.Constant) %>% group_by(Mechanism) %>% mutate(Max = max(Rate.Constant)) %>% distinct(Mechanism, Max)
#pan.df %>% filter(Reaction == "CH3CO3 + NO2 = PAN")  %>% select(Mechanism, Rate.Constant) %>% group_by(Mechanism) %>% mutate(Max = max(Rate.Constant)) %>% distinct(Mechanism, Max)

my.colours = c("MCMv3.2" = "#6c254f", "CRIv2" = "#ef6638", "MOZART-4" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")

p = ggplot(pan.df, aes(x = Temperature.C, y = Rate.Constant, colour = Mechanism))
p = p + geom_point()
p = p + facet_wrap( ~ Reaction, scales = "free")
p = p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("Reaction Rate Constant")
p = p + theme_tufte()
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(axis.title = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))
p = p + scale_colour_manual(values = my.colours)
p
