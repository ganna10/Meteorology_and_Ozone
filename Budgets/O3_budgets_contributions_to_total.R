# analyse O3 Budgets, production and consumption separately, plotted as fraction of total Ox production
# Version 0: Jane Coates 09/11/2015

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")

select_data = function (data) {
  df = data %>% select(Mechanism, Temperature, NOx.Emissions, Rate, Reaction, H2O2, HNO3) %>%
    mutate(Temperature.C = Temperature - 273) %>%
    rowwise() %>% 
    mutate(Reactants = ifelse( is.na(str_match(Reaction, " = ")[1,1]), Reaction, sapply(str_split(Reaction, " = "), "[", 1))) %>%
    arrange(Temperature) %>% 
    rowwise() %>%
    mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
    select(Mechanism, Temperature.C, Reactants, Rate, NOx.Condition) %>%
    group_by(Mechanism, Temperature.C, Reactants, NOx.Condition) %>%
    summarise(Rate = mean(Rate)) %>%
    group_by(Mechanism, Temperature.C, NOx.Condition) %>%
    mutate(Sum = sum(Rate), Fraction = Rate / Sum) %>%
    select(-Rate, -Sum)
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

get_td_prod_data = function (mechanism) {
  data = read.csv(file = paste0("TD_O3/", mechanism, "_O3_budget_10112015.txt"))
  data = data %>% filter(Rate > 0)
  df = select_data(data)
  df$Run = rep("Temperature Dependent\nIsoprene Emissions", length(df$Mechanism))
  return(df)
}
  
get_td_cons_data = function (mechanism) {
  data = read.csv(file = paste0("TD_O3/", mechanism, "_O3_budget_10112015.txt"))
  data = data %>% filter(Rate < 0)
  df = select_data(data)
  df$Run = rep("Temperature Dependent\nIsoprene Emissions", length(df$Mechanism))
  return(df)
}

get_ti_prod_data = function (mechanism) {
  data = read.csv(file = paste0("TI_O3/", mechanism, "_O3_budget_10112015.txt"))
  data = data %>% filter(Rate > 0)
  df = select_data(data)
  df$Run = rep("Temperature Independent\nIsoprene Emissions", length(df$Mechanism))
  return(df)
}

get_ti_cons_data = function (mechanism) {
  data = read.csv(file = paste0("TI_O3/", mechanism, "_O3_budget_10112015.txt"))
  data = data %>% filter(Rate < 0)
  df = select_data(data)
  df$Run = rep("Temperature Independent\nIsoprene Emissions", length(df$Mechanism))
  return(df)
}

production_plot = function (mechanism, td.df, ti.df) {
  mech.td = td.df %>% as.data.frame() %>% filter(Mechanism == mechanism)
  mech.ti = ti.df %>% as.data.frame() %>% filter(Mechanism == mechanism)
  df = rbind(mech.td, mech.ti)
  title = paste(mechanism, " : Contributions to total Ox Production Budget")
  
  p = ggplot(df, aes(x = Temperature.C, y = Fraction, colour = Reactants, ymax = 1))
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

consumption_plot = function (mechanism, td.df, ti.df) {
  mech.td = td.df %>% as.data.frame() %>% filter(Mechanism == mechanism)
  mech.ti = ti.df %>% as.data.frame() %>% filter(Mechanism == mechanism)
  df = rbind(mech.td, mech.ti)
  title = paste(mechanism, " : Contributions to total Ox Consumption Budget")
  
  p = ggplot(df, aes(x = Temperature.C, y = Fraction, colour = Reactants, ymax = 1))
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

#mechanisms = c("RADM2")
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

prod.plots = lapply(mechanisms, production_plot, td.df = td.prod.df, ti.df = ti.prod.df)
cons.plots = lapply(mechanisms, consumption_plot, td.df = td.cons.df, ti.df = ti.cons.df)

prod.df = rbind(td.prod.df, ti.prod.df)
tbl_df(prod.df)
p = ggplot(prod.df, aes(x = Temperature.C, y = Fraction, colour = Reactants, linetype = Run))
p = p + geom_line()
p = p + facet_grid(NOx.Condition ~ Mechanism)
p = p + ggtitle("Stacked Ox Budgets")
p = p + scale_x_continuous(expand = c(0, 0))
p = p + scale_y_continuous(expand = c(0, 0))
p = p + theme_tufte()
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))
p = p + theme(plot.title = element_text(face = "bold"))
p

CairoPDF(file = "Contributions_Ox_budgets.pdf", width = 7, height = 10)
print(p)
dev.off()

#compare HO2 + NO contribution to Ox budget
# df = rbind(ti.prod.df, td.prod.df)
# df = df %>% as.data.frame() %>% filter(Reactants == "HO2 + NO")
# tbl_df(df)
# p = ggplot(df, aes(x = Temperature.C, y = Fraction, colour = Mechanism))
# p = p + geom_line(size = 2)
# p = p + facet_grid(NOx.Condition ~ Run)
# p 

#direct.label(p, list("last.points", cex = 0.6))
#direct.label(p, "last.qp")

##need to fine tune the direct labelling
#p + geom_dl(aes(label = Reactants), method = list(defaultpf.ggplot("line",,,), cex = 0.7))
#p1 = direct.label(p, list("far.from.others.borders", cex = 0.8))
#p1
#p2 = ggplot_gtable(ggplot_build(p1))
#p2$layout$clip[p2$layout$name == "panel"] = "off"
#grid.draw(p2)