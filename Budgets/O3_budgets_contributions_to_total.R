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

mechanisms = c("RADM2")
#mechanisms = c("CB05", "RADM2", "MOZART-4", "CRIv2", "MCMv3.2")
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

prod.df = rbind(ti.prod.df, td.prod.df)
cons.df = rbind(ti.cons.df, td.cons.df)

#production plot
p = ggplot(data = prod.df, aes(x = Temperature.C, y = Fraction, colour = Reactants, ymax = 1))
p = p + geom_line(position = "stack")
p = p + facet_grid(NOx.Condition ~ Run)
p = p + scale_y_continuous(labels = percent)
p = p + theme_tufte()
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(strip.text = element_text(face = "bold"))
p = p + theme(strip.text.y = element_text(angle = 0))
direct.label(p, list("last.points", cex = 0.6))
direct.label(p, "last.qp")

#need to fine tune the direct labelling
p + geom_dl(aes(label = Reactants), method = list(defaultpf.ggplot("line",,,), cex = 0.7))
p1 = direct.label(p, list("far.from.others.borders", cex = 0.8))
p1
p2 = ggplot_gtable(ggplot_build(p1))
p2$layout$clip[p2$layout$name == "panel"] = "off"
grid.draw(p2)

#consumption plot
c = ggplot(data = subset(cons.df, Run == "Temperature Dependent\nIsoprene Emissions"), aes(x = Temperature.C, y = Fraction, colour = Mechanism))
c = c + geom_line()
c = c + facet_grid(NOx.Condition ~ Reactants)
c = c + scale_y_continuous(labels = percent, expand = c(0, 0))
c = c + scale_x_continuous(expand = c(0, 0))
c = c + theme_tufte()
c = c + theme(axis.line = element_line(colour = "black"))
c = c + theme(strip.text = element_text(face = "bold"))
c = c + theme(strip.text.y = element_text(angle = 0))
c = c + theme(panel.margin = unit(3.5, "mm"))
c


