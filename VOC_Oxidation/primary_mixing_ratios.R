# comparing mixing ratios of emitted species with temperature
# version 0: Jane Coates 3/3/2016

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//VOC_Oxidation")

get_data <- function (mechanism, run, date) {
  filename <- paste0(run, "_primary/", mechanism, "_primary_VOCs_", date, ".txt")
  df <- read.csv(file = filename)
  df$Run <- rep(run, length(df$Mechanism))
  data <- df %>%
    spread(Species, Mixing.Ratio, drop = FALSE) %>%
    gather(Species, Mixing.Ratio, -Mechanism, -Temperature, -NOSF, -Run, -H2O2, -HNO3)
  return(data)
}

date <- "03032016"
mechanisms <- c("MCMv3.2", "CB05", "RADM2", "MOZART-4", "CRIv2")

# ti data
ti.df <- do.call("rbind.data.frame", lapply(mechanisms, get_data, date = date, run = "TI"))
tbl_df(ti.df)

# td data
td.df <- do.call("rbind.data.frame", lapply(mechanisms, get_data, date = date, run = "TD"))
tbl_df(td.df)

df <- rbind(ti.df, td.df)
tbl_df(df)

d <- df %>%
  select(-NOSF) %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature.C = Temperature - 273) %>%
  group_by(Mechanism, Temperature.C, Species, NOx.Condition, Run) %>%
  summarise(Mixing.Ratio = mean(Mixing.Ratio))

p <- ggplot(data = subset(d, Mechanism == "MCMv3.2"), aes(x = Temperature.C, y = Mixing.Ratio, colour = Run))
p <- p + geom_line(size = 2)
p <- p + facet_wrap(Species ~ NOx.Condition)
p <- p + plot_theme()
p

# VOC : Isoprene ratio
isop <- d
isop$Species[isop$Species == "ISO"] <- "ISOP"
isop$Species[isop$Species == "C5H8"] <- "ISOP"

iso.df <- isop %>%
  spread(Species, Mixing.Ratio, drop = FALSE) %>%
  gather(Species, Mixing.Ratio, -Mechanism, -Temperature.C, -NOx.Condition, -Run, -ISOP) %>%
  mutate(Isop.ratio = Mixing.Ratio / ISOP) %>%
  select(-ISOP, -Mixing.Ratio)
tbl_df(iso.df)
iso.df <- na.omit(iso.df)

i <- ggplot(data = subset(iso.df, Mechanism == "MCMv3.2"), aes(x = Temperature.C, y = Isop.ratio, colour = Run))
i <- i + geom_line(size = 2)
i <- i + facet_wrap(Species ~ NOx.Condition)
i <- i + plot_theme()
i

write.table(d, file = "plot_VOC_data.csv", sep = ",", row.names = FALSE, quote = FALSE)
write.table(iso.df, file = "plot_VOC_ISOP_ratio_data.csv", sep = ",", row.names = FALSE, quote = FALSE)
