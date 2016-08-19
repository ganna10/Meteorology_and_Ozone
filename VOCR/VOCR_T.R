# compare total VOCR at each temperature, run, mechanism
# Version 0: Jane Coates 18/8/2016

get_data <- function (mechanism, run.label, date) {
  filename <- paste0(run.label, "_VOCR/", mechanism, "_assigned_initial_VOC_OH_oxidation_rate_", date, ".txt")
  data <- read.csv(filename)
  data$Run <- rep(run.label, length(data$Mechanism))
  return(data)
}

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//VOCR")
date <- "07062016"
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2")

# td
td.list <- lapply(mechanisms, get_data, run.label = "TD", date = date)
td.df <- do.call("rbind", td.list)
td.df <- tbl_df(td.df)

# ti
ti.list <- lapply(mechanisms, get_data, run.label = "TI", date = date)
ti.df <- do.call("rbind", ti.list)
ti.df <- tbl_df(ti.df)
ti.df

df <- rbind(ti.df, td.df)

final.df <- df %>%
  rowwise() %>%
  mutate(Temperature.C = Temperature - 273, NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
  select(Mechanism, Run, NOx.Condition, Temperature.C, Emitted.VOC, Max.VOCR) %>%
  arrange(Temperature.C) %>%
  group_by(Mechanism, Run, NOx.Condition, Temperature.C, Emitted.VOC) %>%
  summarise(Max.VOCR = mean(Max.VOCR)) %>%
  group_by(Mechanism, Run, NOx.Condition, Temperature.C) %>%
  summarise(Max.VOCR = sum(Max.VOCR))
final.df

final.df$Run <- factor(final.df$Run, levels = c("TI", "TD"))

my.colours <- c("MCMv3.2" = "#6c254f", "CRIv2" = "#ef6638", "MOZART-4" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")

p <- ggplot(final.df, aes(x = Temperature.C, y = Max.VOCR, colour = Mechanism))
p <- p + geom_line(size = 1)
p <- p + facet_grid(NOx.Condition ~ Run)
p <- p + plot_theme()
p <- p + scale_colour_manual(values = my.colours)
p
