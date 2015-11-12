# analyse RO2NO2 Budgets
# Version 0: Jane Coates 11/11/2015

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")

select_data = function (data) {
  df = data %>% select(Mechanism, Temperature, Net.Reaction.Rate, H2O2, HNO3) %>%
    arrange(Temperature) %>%
    mutate(Temperature.C = Temperature - 273) %>%
    rowwise() %>%
    mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
    select(-Temperature, -H2O2, -HNO3) %>%
    group_by(Mechanism, Temperature.C, NOx.Condition) %>%
    summarise(Mean.Rate = mean(Net.Reaction.Rate))
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

mechanisms = c("CB05", "RADM2", "MOZART-4", "MCMv3.2", "CRIv2")
#mechanisms = c("CB05")
#temperature dependent
td.list = lapply(mechanisms, get_td_data)
td.df = do.call("rbind", td.list)
tbl_df(td.df)

#temperature independent
ti.list = lapply(mechanisms, get_ti_data)
ti.df = do.call("rbind", ti.list)
tbl_df(ti.df)

df = rbind(ti.df, td.df)
my.colours = c("MCMv3.2" = "#6c254f", "CRIv2" = "#ef6638", "MOZART-4" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")

p = ggplot(df, aes(x = Temperature.C, y = Mean.Rate, colour = Mechanism, linetype = Run))
p = p + geom_line(size = 2)
p = p + facet_wrap( ~ NOx.Condition)
p = p + theme_tufte()
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(axis.title = element_text(face = "bold"))
p = p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("Mean Net RO2NO2 Production (molecules cm-3)")
p = p + scale_colour_manual(values = my.colours)
p

#diff from MCM
ti.max = ti.df %>% select(Mechanism, Mean.Rate, NOx.Condition) %>% 
  group_by(Mechanism, NOx.Condition) %>%
  summarise(Max = max(Mean.Rate))
ti.max %>% spread(Mechanism, Max, drop = FALSE) %>%
  gather(Mechanism, Max.Rate, -NOx.Condition, -MCMv3.2) %>%
  mutate(Diff.from.MCM = (Max.Rate - MCMv3.2)/MCMv3.2)

td.max = td.df %>% select(Mechanism, Mean.Rate, NOx.Condition) %>% 
  group_by(Mechanism, NOx.Condition) %>%
  summarise(Max = max(Mean.Rate))
td.max %>% spread(Mechanism, Max, drop = FALSE) %>%
  gather(Mechanism, Max.Rate, -NOx.Condition, -MCMv3.2) %>%
  mutate(Diff.from.MCM = (Max.Rate - MCMv3.2)/MCMv3.2)
