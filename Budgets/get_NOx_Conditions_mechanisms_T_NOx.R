# output NOx.Condition based on H2O2/HNO3 ratio at each run (NOx.Emissions and T) of the mechanism. Output separate for TD and TI runs
# Version 0: Jane Coates 11/11/2015

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")

select_data = function (data) {
  df = data %>% select(Mechanism, Temperature, NOx.Emissions, H2O2, HNO3) %>%
    arrange(Temperature) %>% 
    rowwise() %>%
    mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
    select(-H2O2, -HNO3) %>%
    distinct(Mechanism, Temperature, NOx.Emissions, NOx.Condition)
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
  return(df)
}
  
get_ti_data = function (mechanism) {
  data = read.csv(file = paste0("TI_O3/", mechanism, "_O3_budget_10112015.txt"))
  df = select_data(data)
  return(df)
}

mechanisms = c("CB05", "RADM2", "MOZART-4", "CRIv2", "MCMv3.2")
#temperature dependent
td.list = lapply(mechanisms, get_td_data)
td.df = do.call("rbind", td.list)
tbl_df(td.df) 

#temperature independent
ti.list = lapply(mechanisms, get_ti_data)
ti.df = do.call("rbind", ti.list)
tbl_df(ti.df)

write.table(td.df, file = "TD_NOx_Conditions_each_T_NOSF.csv", sep = ",", quote = FALSE, row.names = FALSE)
write.table(ti.df, file = "TI_NOx_Conditions_each_T_NOSF.csv", sep = ",", quote = FALSE, row.names = FALSE)
