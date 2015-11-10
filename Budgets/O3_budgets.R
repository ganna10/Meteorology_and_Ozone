# analyse O3 Budgets
# Version 0: Jane Coates 09/11/2015

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")

select_data = function (data) {
  df = data %>% select(Mechanism, Temperature, Total.NOx.Emissions, Reaction, Rate) %>%
    mutate(Reactants = sapply(str_split(Reaction, " = "), "[", 1)) %>%
    select(-Reaction) %>%
    arrange(Temperature) %>% 
    filter(Total.NOx.Emissions == 540678848)
  return(df)
}

get_td_data = function (mechanism) {
  data = read.csv(file = paste0("TD_O3/", mechanism, "_O3_budget_07112015.txt"))
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
tbl_df(td.df)
levels(factor(td.df$Total.NOx.Emissions))
#temperature independent
ti.list = lapply(mechanisms, get_ti_data)
ti.df = do.call("rbind", ti.list)
tbl_df(ti.df)

#df = rbind(ti.df, td.df)

p = ggplot(ti.df, aes(x = Temperature, y = Rate, fill = Reactants))
p = p + geom_bar(data = subset(ti.df, Rate < 0), stat = "identity")
p = p + geom_bar(data = subset(ti.df, Rate > 0), stat = "identity")
p = p + facet_grid(Mechanism ~ Total.NOx.Emissions)
p = p + scale_x_continuous(expand = c(0, 0))
p = p + scale_y_continuous(expand = c(0, 0))
p = p + theme_tufte()
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))
p

