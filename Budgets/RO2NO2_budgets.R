# analyse RO2NO2 Budgets, absolute rates
# Version 0: Jane Coates 09/11/2015

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")

select_data = function (data) {
  df = data %>% select(Mechanism, Temperature, NOx.Condition, Reaction, Rate) %>%
    mutate(Temperature.C = Temperature - 273) %>%
    rowwise() %>% 
    mutate(Reactants = ifelse( is.na(str_match(Reaction, " = ")[1,1]), Reaction, sapply(str_split(Reaction, " = "), "[", 1))) %>%
    arrange(Temperature) %>% 
    select(Mechanism, Temperature.C, Reaction, Rate, NOx.Condition) %>%
    group_by(Mechanism, Temperature.C, Reaction, NOx.Condition) %>%
    summarise(Rate = mean(Rate))
  return(df)
}

get_td_data = function (mechanism) {
  data = read.csv(file = paste0("TD_RO2NO2/", mechanism, "_RO2NO2_budget_with_NOx-Condition_07112015.txt"))
  df = select_data(data)
  df$Run = rep("Temperature Dependent\nIsoprene Emissions", length(df$Mechanism))
  return(df)
}

get_ti_data = function (mechanism) {
  data = read.csv(file = paste0("TI_RO2NO2/", mechanism, "_RO2NO2_budget_with_NOx-Condition_07112015.txt"))
  df = select_data(data)
  df$Run = rep("Temperature Independent\nIsoprene Emissions", length(df$Mechanism))
  return(df)
}

#mechanisms = c("RADM2")
mechanisms = c("CB05", "RADM2", "MOZART-4", "CRIv2", "MCMv3.2")
#temperature dependent
td.list = lapply(mechanisms, get_td_data)
td.df = do.call("rbind", td.list)
tbl_df(td.df)
#temperature independent
ti.list = lapply(mechanisms, get_ti_data)
ti.df = do.call("rbind", ti.list)
tbl_df(ti.df)

#test = ti.df %>% filter(Reaction == "HO2NO2 = HO2 + NO2")
#factor(test$Mechanism)

df = rbind(ti.df, td.df)

p = ggplot(df, aes(x = Temperature.C, y = Rate))
p = p + geom_bar(data = subset(df, Rate < 0), stat = "identity", aes(fill = Reaction))
p = p + geom_bar(data = subset(df, Rate > 0), stat = "identity", aes(fill = Reaction))
p = p + geom_line(data = subset(df, Rate < 0), aes(colour = Reaction, linetype = Run), size = 2,position = "stack")
p = p + geom_line(data = subset(df, Rate > 0), aes(colour = Reaction, linetype = Run), size = 2, position = "stack")
p = p + geom_line(aes(colour = Reaction, linetype = Run), size = 2) #non-stacked
p = p + facet_grid(Mechanism ~ NOx.Condition)
p = p + scale_x_continuous(expand = c(0, 0))
p = p + scale_y_continuous(expand = c(0, 0))
p = p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("Mean Production (molecules cm-3)")
p = p + theme_tufte()
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))
p
