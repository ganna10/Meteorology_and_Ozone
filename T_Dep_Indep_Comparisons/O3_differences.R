# differences in ozone from 313 to 293K
# Version 0: Jane Coates 26/11/2015

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//T_Dep_Indep_Comparisons")
runs <- c("Dependent", "Independent")
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2")
spc <- "O3"

list <- lapply(runs, get_all_mixing_ratio_data)
df <- do.call("rbind", list)
df <- tbl_df(df)
df$Run[df$Run == "Temperature Dependent\nIsoprene Emissions"] <- "TD"
df$Run[df$Run == "Temperature Independent\nIsoprene Emissions"] <- "TI"

difference <- df %>%
  arrange(Temperature) %>%
  mutate(Temperature.C = Temperature - 273) %>%
  filter(Temperature.C == 20 | Temperature.C == 40) %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature = ifelse(as.character(Temperature.C) == "20", "Reference", "Increased")) %>%
  select(Mechanism, NOx.Condition, Temperature, O3, Run) %>%
  group_by(Mechanism, NOx.Condition, Temperature, Run) %>%
  summarise(O3 = mean(O3)) %>%
  spread(Temperature, O3, drop = FALSE) %>%
  mutate(Difference = (Increased - Reference)/Reference)

ggplot(difference, aes(x = NOx.Condition, y = Difference, shape = Run)) + geom_point(size = 2) + scale_y_continuous(labels = percent) + facet_wrap(~ Mechanism, scales = "free")

difference.from.mcm.293 <- df %>%
  filter(Temperature == 293) %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
  select(Mechanism, O3, Run, NOx.Condition) %>%
  group_by(Mechanism, NOx.Condition, Run) %>%
  summarise(O3 = mean(O3)) %>%
  spread(Mechanism, O3, drop = FALSE) %>%
  gather(Mechanism, O3, -NOx.Condition, -Run, -MCMv3.2) %>%
  mutate(Difference.from.MCM = (MCMv3.2 - O3)/MCMv3.2, Temperature = "Reference")

difference.from.mcm.313 <- df %>%
  filter(Temperature == 313) %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
  select(Mechanism, O3, Run, NOx.Condition) %>%
  group_by(Mechanism, NOx.Condition, Run) %>%
  summarise(O3 = mean(O3)) %>%
  spread(Mechanism, O3, drop = FALSE) %>%
  gather(Mechanism, O3, -NOx.Condition, -Run, -MCMv3.2) %>%
  mutate(Difference.from.MCM = (MCMv3.2 - O3)/MCMv3.2, Temperature = "Increased")

difference.from.mcm <- rbind(difference.from.mcm.293, difference.from.mcm.313)

ggplot(difference.from.mcm, aes(x = NOx.Condition, y = Difference.from.MCM, shape = Run)) + scale_y_continuous(labels = percent) + geom_point(size = 4) + facet_grid(Mechanism ~ Temperature)
