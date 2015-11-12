# analyse CH3CO3 Budgets, absolute rates
# Version 0: Jane Coates 12/11/2015

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")

select_data = function (data) {
  df = data %>% select(Mechanism, Temperature, NOx.Emissions, Reaction, Rate, H2O2, HNO3) %>%
    mutate(Temperature.C = Temperature - 273) %>%
    rowwise() %>% 
    mutate(Reactants = ifelse( is.na(str_match(Reaction, " = ")[1,1]), Reaction, sapply(str_split(Reaction, " = "), "[", 1))) %>%
    arrange(Temperature) %>% 
    rowwise() %>%
    mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
    select(Mechanism, Temperature.C, Reactants, Rate, NOx.Condition) %>%
    group_by(Mechanism, Temperature.C, Reactants, NOx.Condition) %>%
    summarise(Rate = mean(Rate))
  return(df)
}

get_NOx_condition = function (x) {
  if (x > 0.5) {
    condition = "Low-NOx"
  } else if (x < 0.3) {
    condition = "High-NOx"
  } else {
    condition = "Maximal-O3"
  }
  return (condition)
}

get_td_data = function (mechanism) {
  data = read.csv(file = paste0("TD_CH3CO3/", mechanism, "_CH3CO3_budget_11112015.txt"))
  df = select_data(data)
  df$Run = rep("Temperature Dependent\nIsoprene Emissions", length(df$Mechanism))
  return(df)
}

get_ti_data = function (mechanism) {
  data = read.csv(file = paste0("TI_CH3CO3/", mechanism, "_CH3CO3_budget_11112015.txt"))
  df = select_data(data)
  df$Run = rep("Temperature Independent\nIsoprene Emissions", length(df$Mechanism))
  return(df)
}

mechanisms = c("CB05", "MCMv3.2", "CRIv2")
#mechanisms = c("CB05", "RADM2", "MOZART-4", "CRIv2", "MCMv3.2")
#temperature dependent
##### to do -> need to add others by summing reaction rates
td.list = lapply(mechanisms, get_td_data)
td.df = do.call("rbind", td.list)
tbl_df(td.df)
#temperature independent
ti.list = lapply(mechanisms, get_ti_data)
ti.df = do.call("rbind", ti.list)
tbl_df(ti.df)

df = rbind(ti.df, td.df)

p = ggplot(df, aes(x = Temperature.C, y = Rate, colour = Reactants, linetype = Mechanism))
p = p + geom_line(data = subset(df, Rate < 0))
#p = p + geom_bar(data = subset(df, Rate < 0), stat = "identity")
#p = p + geom_bar(data = subset(df, Rate > 0), stat = "identity")
p = p + geom_line(data = subset(df, Rate > 0))
p = p + facet_grid(NOx.Condition ~ Run)
p = p + scale_x_continuous(expand = c(0, 0))
p = p + scale_y_continuous(expand = c(0, 0))
p = p + theme_tufte()
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))
p

#HO2 + NO contribution
df = rbind(ti.df, td.df)
df = df %>% as.data.frame() %>% filter(Reactants == "HO2 + NO")
tbl_df(df)
p = ggplot(df, aes(x = Temperature.C, y = Rate, colour = Mechanism))
p = p + geom_line(size = 2)
p = p + facet_grid(NOx.Condition ~ Run)
p 
