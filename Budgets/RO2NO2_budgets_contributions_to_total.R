# analyse RO2NO2 Budgets, production and consumption separately, plotted as fraction of total Ox production
# Version 0: Jane Coates 09/11/2015

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")

select_data = function (data) {
  df = data %>% select(Mechanism, Temperature, NOx.Condition, Rate, Reaction) %>%
    mutate(Temperature.C = Temperature - 273) %>%
    arrange(Temperature) %>% 
    select(Mechanism, Temperature.C, Reaction, Rate, NOx.Condition) %>%
    group_by(Mechanism, Temperature.C, Reaction, NOx.Condition) %>%
    summarise(Rate = mean(Rate)) %>%
    group_by(Mechanism, Temperature.C, NOx.Condition) %>%
    mutate(Sum = sum(Rate), Fraction = Rate / Sum) %>%
    select(-Rate, -Sum)
  return(df)
}

get_td_prod_data = function (mechanism) {
  data = read.csv(file = paste0("TD_RO2NO2/", mechanism, "_RO2NO2_budget_with_NOx-Condition_07112015.txt"))
  data = data %>% filter(Rate > 0)
  df = select_data(data)
  df$Run = rep("Temperature Dependent\nIsoprene Emissions", length(df$Mechanism))
  return(df)
}

get_td_cons_data = function (mechanism) {
  data = read.csv(file = paste0("TD_RO2NO2/", mechanism, "_RO2NO2_budget_with_NOx-Condition_07112015.txt"))
  data = data %>% filter(Rate < 0)
  df = select_data(data)
  df$Run = rep("Temperature Dependent\nIsoprene Emissions", length(df$Mechanism))
  return(df)
}

get_ti_prod_data = function (mechanism) {
  data = read.csv(file = paste0("TI_RO2NO2/", mechanism, "_RO2NO2_budget_with_NOx-Condition_07112015.txt"))
  data = data %>% filter(Rate > 0)
  df = select_data(data)
  df$Run = rep("Temperature Independent\nIsoprene Emissions", length(df$Mechanism))
  return(df)
}

get_ti_cons_data = function (mechanism) {
  data = read.csv(file = paste0("TI_RO2NO2/", mechanism, "_RO2NO2_budget_with_NOx-Condition_07112015.txt"))
  data = data %>% filter(Rate < 0)
  df = select_data(data)
  df$Run = rep("Temperature Independent\nIsoprene Emissions", length(df$Mechanism))
  return(df)
}

production_plot = function (mechanism, ti.df, td.df) {
  mech.ti = ti.df %>% as.data.frame() %>% filter(Mechanism == mechanism)
  mech.td = td.df %>% as.data.frame() %>% filter(Mechanism == mechanism)
  df = rbind(mech.ti, mech.td)
  title = paste(mechanism, ": Contributions to total RO2NO2 Production Budget")
  
  p = ggplot(df, aes(x = Temperature.C, y = Fraction, colour = Reaction, ymax = 1))
  p = p + geom_line()
  p = p + facet_grid(NOx.Condition ~ Run)
  p = p + scale_y_continuous(labels = percent)
  p = p + ggtitle(title)
  p = p + theme_tufte()
  p = p + theme(axis.line = element_line(colour = "black"))
  p = p + theme(strip.text = element_text(face = "bold"))
  p = p + theme(axis.title = element_text(face = "bold"))
  p = p + theme(plot.title = element_text(face = "bold"))
  p = p + theme(strip.text.y = element_text(angle = 0))
  return(p)
}

consumption_plot = function (mechanism, ti.df, td.df) {
  mech.ti = ti.df %>% as.data.frame() %>% filter(Mechanism == mechanism)
  mech.td = td.df %>% as.data.frame() %>% filter(Mechanism == mechanism)
  df = rbind(mech.ti, mech.td)
  title = paste(mechanism, ": Contributions to total RO2NO2 Consumption Budget")
  
  c = ggplot(df, aes(x = Temperature.C, y = Fraction, colour = Reaction))
  c = c + geom_line()
  c = c + facet_grid(NOx.Condition ~ Run)
  c = c + scale_y_continuous(labels = percent, expand = c(0, 0))
  c = c + scale_x_continuous(expand = c(0, 0))
  c = c + theme_tufte()
  c = c + ggtitle(title)
  c = c + theme(axis.line = element_line(colour = "black"))
  c = c + theme(strip.text = element_text(face = "bold"))
  c = c + theme(plot.title = element_text(face = "bold"))
  c = c + theme(strip.text.y = element_text(angle = 0))
  return(c)
}
  
#mechanisms = c("CB05", "RADM2")
mechanisms = c("CB05", "RADM2", "MOZART-4", "CRIv2", "MCMv3.2")
#temperature dependent
td.prod.list = lapply(mechanisms, get_td_prod_data)
td.prod.df = do.call("rbind", td.prod.list)
tbl_df(td.prod.df) 

td.cons.list = lapply(mechanisms, get_td_cons_data)
td.cons.df = do.call("rbind", td.cons.list)
tbl_df(td.cons.df)

#temperature independent
ti.prod.list = lapply(mechanisms, get_ti_prod_data)
ti.prod.df = do.call("rbind", ti.prod.list)
tbl_df(ti.prod.df)

ti.cons.list = lapply(mechanisms, get_ti_cons_data)
ti.cons.df = do.call("rbind", ti.cons.list)
tbl_df(ti.cons.df)

prod.plot.list = lapply(mechanisms, production_plot, ti.df = ti.prod.df, td.df = td.prod.df)
cons.plot.list = lapply(mechanisms, consumption_plot, ti.df = ti.cons.df, td.df = td.cons.df)
prod.plot.list[[5]]
cons.plot.list[[5]]

#need to fine tune the direct labelling
# p + geom_dl(aes(label = Reactants), method = list(defaultpf.ggplot("line",,,), cex = 0.7))
# p1 = direct.label(p, list("far.from.others.borders", cex = 0.8))
# p1
# p2 = ggplot_gtable(ggplot_build(p1))
# p2$layout$clip[p2$layout$name == "panel"] = "off"
# grid.draw(p2)