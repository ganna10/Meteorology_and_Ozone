# comparing max PO3 in ppb/h model results to analytical model from Pusede:2014
# Version 0: Jane Coates 05/12/2015

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Analytical_Model")
date = "04122015"
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2")

get_PO3_data <- function (Mechanism, Run.Label, Date) {
  filename <- paste0(Run.Label, "/", Mechanism, "_max_PO3_rate_", Date, ".txt")
  data <- read.csv(filename)
  if (Run.Label == "TI") {
    data$Run <- rep("Temperature Independent\nIsoprene Emissions", length(data$Mechanism))
  } else if (Run.Label == "TD") {
    data$Run <- rep("Temperature Dependent\nIsoprene Emissions", length(data$Mechanism))
  }
  return(data)
}

# temperature independent data
ti.list <- lapply(mechanisms, get_PO3_data, Run.Label = "TI", Date = date)
ti.df <- do.call("rbind", ti.list)
tbl_df(ti.df)

# temperature dependent data
td.list <- lapply(mechanisms, get_PO3_data, Run.Label = "TD", Date = date)
td.df <- do.call("rbind", td.list)
tbl_df(td.df)

df <- rbind(ti.df, td.df)
data <- df %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature.C = Temperature - 273, Max.PO3.Rate = Max.PO3.Rate / (60 * 60)) %>%
  select(-H2O2, -HNO3, -Temperature) %>%
  group_by(Mechanism, Temperature.C, NOx.Condition, Run) %>%
  summarise(Max.PO3.Rate = mean(Max.PO3.Rate))

ggplot(data, aes(x = Temperature.C, y = Max.PO3.Rate, colour = Mechanism, linetype = Run)) + geom_line(size = 1) + facet_grid( ~ NOx.Condition) + plot_theme()
