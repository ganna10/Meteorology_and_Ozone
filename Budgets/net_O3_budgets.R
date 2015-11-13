# analyse O3 Budgets
# Version 0: Jane Coates 09/11/2015

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")

select_data = function (data) {
  df = data %>% select(Mechanism, Temperature, NOx.Emissions, Net.Reaction.Rate, H2O2, HNO3) %>%
    mutate(Temperature.C = Temperature - 273) %>%
    arrange(Temperature) %>%
    rowwise() %>%
    mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
    select(Mechanism, Temperature.C, Net.Reaction.Rate, NOx.Condition) %>%
    group_by(Mechanism, Temperature.C, NOx.Condition) %>%
    summarise(Net.Rate = mean(Net.Reaction.Rate))
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
  data = read.csv(file = paste0("TD_O3/", mechanism, "_O3_budget_10112015.txt"))
  df = select_data(data)
  df$Run = rep("Temperature Dependent\nIsoprene Emissions", length(df$Mechanism))
  return(df)
}

get_ti_data = function (mechanism) {
  data = read.csv(file = paste0("TI_O3/", mechanism, "_O3_budget_10112015.txt"))
  df = select_data(data)
  df$Run = rep("Temperature Independent\nIsoprene Emissions", length(df$Mechanism))
  return(df)
}

mechanisms = c("CB05", "RADM2", "MCMv3.2", "CRIv2", "MOZART-4")
#temperature dependent
td.list = lapply(mechanisms, get_td_data)
td.df = do.call("rbind", td.list)

#temperature independent
ti.list = lapply(mechanisms, get_ti_data)
ti.df = do.call("rbind", ti.list)

df = rbind(ti.df, td.df)
my.colours = c("MCMv3.2" = "#6c254f", "CRIv2" = "#ef6638", "MOZART-4" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")

p = ggplot(df, aes(x = Temperature.C, y = Net.Rate, colour = Mechanism, linetype = Run))
p = p + geom_line()
p = p + facet_wrap( ~ NOx.Condition)
p = p + theme_tufte()
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + scale_colour_manual(values = my.colours)
p

CairoPDF(file = "Net_Ox_Budgets.pdf", width = 10, height = 7)
print(p)
dev.off()
