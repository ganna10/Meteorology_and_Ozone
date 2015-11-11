# analyse O3 Budgets
# Version 0: Jane Coates 09/11/2015

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")

select_data = function (data) {
  df = data %>% select(Mechanism, Temperature, NOx.Emissions, Net.Reaction.Rate) %>%
    arrange(Temperature) %>%
    distinct(Mechanism, Temperature, NOx.Emissions, Net.Reaction.Rate)
  return(df)
}
get_td_data = function (mechanism) {
  data = read.csv(file = paste0("TD_O3/", mechanism, "_O3_budget_10112015.txt"))
  df = select_data(data)
  df$Run = rep("Temperature Dependent\nIsoprene Emissions", length(df$Mechanism))
  return(df)
}

get_ti_data = function (mechanism) {
  data = read.csv(file = paste0("TI_O3/", mechanism, "_O3_budget_07112015.txt"))
  df = select_data(data)
  df$Run = rep("Temperature Independent\nIsoprene Emissions", length(df$Mechanism))
  return(df)
}

mechanisms = c("CB05")
#temperature dependent
td.list = lapply(mechanisms, get_td_data)
td.df = do.call("rbind", td.list)

#temperature independent
# ti.list = lapply(mechanisms, get_ti_data)
# ti.df = do.call("rbind", ti.list)
# 
# df = rbind(ti.df, td.df)

p = ggplot(td.df, aes(x = Temperature, y = Net.Reaction.Rate, colour = Mechanism))
p = p + geom_point()
p = p + facet_wrap( ~ NOx.Emissions)
p = p + theme_tufte()
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(strip.text = element_text(face = "bold"))
p