# analyse O3 Budgets, production and consumption separately, plotted as fraction of total Ox production
# Version 0: Jane Coates 09/11/2015
# Version 1: Jane Coates 18/11/2015 visualising by categories
# Version 2: Jane Coates 25/11/2015 separating RO2 into acyl and nonacyl RO2

setwd("~/Documents//Analysis/2015_Meteorology_and_Ozone/Budgets/")
spc <- "O3"
date <- "10112015"
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4","CB05", "RADM2")

get_acyl <- function (mechanism) {
  df <- read.table(file = paste0(mechanism, "_acylRO2.txt"))
  colnames(df) <- c("ARO2")
  df$Mechanism = rep(mechanism, length(df$ARO2))
  return(df)
}

get_nonacyl <- function (mechanism) {
  df <- read.table(file = paste0(mechanism, "_nonacylRO2.txt"))
  colnames(df) <- c("RO2")
  df$Mechanism = rep(mechanism, length(df$RO2))
  return(df)
}

assign_RO2 <- function (x, mechanism) {
  acyl.match <- acyl.df %>%
    filter(Mechanism == mechanism)
  nonacyl.match <- nonacyl.df %>%
    filter(Mechanism == mechanism)
  if (x %in% acyl.match$ARO2) {
    x = "ARO2"
  } else if (x %in% nonacyl.match$RO2) {
    x = "RO2"
  }
  return(x)
}

get_RO2_assigned <- function (mechanism, df) {
  filtered <- df %>%
    filter(Mechanism == mechanism)
  assigned <- filtered %>% 
    mutate(Reaction = sapply(str_split(Reaction, " \\+ "), "[", 1)) %>%
    rowwise() %>%
    mutate(Reaction = assign_RO2(Reaction, mechanism = mechanism))
  return(assigned)
}

acyl.list <- lapply(mechanisms, get_acyl)
acyl.df <- do.call("rbind", acyl.list)
tbl_df(acyl.df)

nonacyl.list <- lapply(mechanisms, get_nonacyl)
nonacyl.df <- do.call("rbind", nonacyl.list)
tbl_df(nonacyl.df)

#temperature dependent
td.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TD", Date = date)
td.df <- do.call("rbind", td.list)
tbl_df(td.df)

#temperature independent
ti.list <- lapply(mechanisms, get_budget_data, Species = spc, Run.Label = "TI", Date = date)
ti.df <- do.call("rbind", ti.list)
tbl_df(ti.df)

df <- rbind(ti.df, td.df)
tbl_df(df)
d <- get_species_budget(dataframe = subset(df, Rate > 0), Reactants = TRUE, Absolute = FALSE)
tbl_df(d)

assigned.list <- lapply(mechanisms, get_RO2_assigned, df = d)
assigned.df <- do.call("rbind", assigned.list)

assigned <- assigned.df %>%
  group_by(Mechanism, Run, Temperature.C, NOx.Condition, Reaction) %>%
  summarise(Fraction = sum(Fraction)) %>% # sum contributions of same reactants but different products
  spread(Reaction, Fraction, drop = FALSE)

assigned[is.na(assigned)] <- 0
tbl_df(assigned)

final <- assigned %>%
  gather(Reaction, Fraction, -Mechanism, -Run, -Temperature.C, -NOx.Condition)

final$NOx.Condition <- factor(final$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))

# difference between T = 40°C and T = 15°C
difference <- final %>%
  filter(Temperature.C == 15 | Temperature.C == 40) %>%
  rowwise() %>%
  mutate(Temperature.C = ifelse(Temperature.C == 15, "Low", "High")) %>%
  spread(Temperature.C, Fraction, drop = FALSE) %>%
  mutate(Difference = (High - Low)/Low) %>%
  select(-High, -Low)

my.colours <- c("MCMv3.2" = "#6c254f", "CRIv2" = "#ef6638", "MOZART-4" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")
p <- ggplot(difference, aes(x = NOx.Condition, y = Difference, colour = Mechanism))
p <- p + geom_point(size = 3)
p <- p + facet_grid(Reaction ~ Run)
p <- p + plot_theme()
p <- p + scale_colour_manual(values = my.colours)
p <- p + theme(legend.title = element_blank(), legend.position = "top")
p
# CairoPDF(file = "Differences_RO2_by_NOx.pdf", width = 10, height = 7)
# print(p)
# dev.off()
td <- final %>%
  filter(Run == "Temperature Dependent\nIsoprene Emissions")
ti <- final %>%
  filter(Run == "Temperature Independent\nIsoprene Emissions")

my.colours = c("HO2" = "#6c254f", "ARO2" = "#2b9eb3", "CH3CO3" = "#ef6638", "RO2" = "#0e5c28")
p <- plot_species_budgets_stacked_area(td, Absolute = FALSE)
p <- p + scale_fill_manual(values = my.colours)
p
CairoPDF(file = "TD_Ox_budgets_Contributions.pdf", width = 10, height = 10)
print(p)
dev.off()

p <- plot_species_budgets_stacked_area(ti, Absolute = FALSE)
p <- p + scale_fill_manual(values = my.colours)
p
CairoPDF(file = "TI_Ox_budgets_Contributions.pdf", width = 10, height = 10)
print(p)
dev.off()
