setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//T_Dep_Indep_Comparisons")
runs <- c("Dependent", "Independent")
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2")
data.list <- lapply(runs, get_all_mixing_ratio_data)
data.df <- do.call("rbind", data.list)
tbl_df(data.df)
data.df$Run[data.df$Run == "Temperature Dependent\nIsoprene Emissions"] <- "TD"
data.df$Run[data.df$Run == "Temperature Independent\nIsoprene Emissions"] <- "TI"

nosf1 <- 188956944

nox <- data.df %>%
  rowwise()  %>% 
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
  select(Mechanism, Temperature, NOx.Emissions, NOx.Condition, Run) %>%
  group_by(Mechanism, Run, NOx.Condition, Temperature) %>%
  summarise(NOx.Emissions = mean(NOx.Emissions)/nosf1) 

print.data.frame(nox %>%
  filter(Run == "TI", Mechanism == "MOZART-4", NOx.Condition == "High-NOx") %>%
  spread(Mechanism, NOx.Emissions))
