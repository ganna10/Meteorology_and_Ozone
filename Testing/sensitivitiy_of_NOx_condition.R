setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Testing/")
runs <- c("Dependent")
mechanisms <- c("CB05")
data.list <- lapply(runs, get_all_mixing_ratio_data)
data.df <- do.call("rbind", data.list)
data.df <- tbl_df(data.df)

test <- data.df %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
  select(Temperature, NOx.Condition, NOx.Emissions, O3) %>%
  arrange(Temperature)

quantiled <- test %>%
  group_by(Temperature) %>%
  filter(between(O3, quantile(O3, 0.5), quantile(O3, 0.95)))
quantiled
summary(test %>% filter(Temperature == 288 & NOx.Condition == "High-NOx"))
