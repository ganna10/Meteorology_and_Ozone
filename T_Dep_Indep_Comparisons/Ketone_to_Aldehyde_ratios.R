# plot Ketone to Aldehyde ratios at each T for different NOx Conditions
# Version 0: Jane Coates 14/11/2015

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//T_Dep_Indep_Comparisons")
td.data <- get_all_mixing_ratio_data("Dependent")
tbl_df(td.data)
ti.data <- get_all_mixing_ratio_data("Independent")
tbl_df(ti.data)

df <- rbind(ti.data, td.data)
test <- df$Ketones / df$Aldehydes
head(test)
data <- get_ratio_data(df, "Ketones", "Aldehydes")
tbl_df(data)

plot_ratios(data)
