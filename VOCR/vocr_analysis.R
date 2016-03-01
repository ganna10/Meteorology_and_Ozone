# compare maximum VOCR of initial VOCs
# Version 0: Jane Coates 15/2/2016

get_data <- function (mechanism, run.label, date) {
  filename <- paste0(run.label, "_VOCR/", mechanism, "_initial_VOCR_", date, ".txt")
  data <- read.csv(filename)
  data$Run <- rep(run.label, length(data$Mechanism))
  return(data)
}

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//VOCR")
date <- "15022016"
mechanisms <- c("CB05", "RADM2")

# temperature dependent data
td.list <- lapply(mechanisms, get_data, run.label = "TD", date = date)
td.df <- do.call("rbind", td.list)
tbl_df(td.df)

# temperature independent data
ti.list <- lapply(mechanisms, get_data, run.label = "TI", date = date)
ti.df <- do.call("rbind", ti.list)
tbl_df(ti.df)

df <- rbind(td.df, ti.df)
assigned <- df %>%
  rowwise() %>%
  mutate(Temperature.C = Temperature - 273, NOx.Condition = get_NOx_condition(H2O2/HNO3), O3 = O3 * 1e9) %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition) %>%
  summarise(VOCR = mean(VOCR), O3 = mean(O3)) 
%>%
  gather(Item, Value, -Mechanism, -Temperature.C, -Run, -NOx.Condition)
tbl_df(assigned)

p <- ggplot(assigned, aes(x = Temperature.C, y = VOCR, colour = NOx.Condition))
p <- p + geom_line(size = 2)
p <- p + facet_grid(Run ~ Mechanism)
p <- p + plot_theme()
p <- p + scale_y_continuous(limits = c(0, 4))
p
