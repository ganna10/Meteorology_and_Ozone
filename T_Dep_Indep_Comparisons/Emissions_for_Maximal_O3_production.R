# Determine the NO emissions for maximual O3 production
# Version 0: Jane Coates 6/8/2016

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//T_Dep_Indep_Comparisons")
runs <- c("Dependent", "Independent")
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2")
spc <- "O3"

list <- lapply(runs, get_all_mixing_ratio_data)
df <- do.call("rbind", list)
df <- tbl_df(df)
df$Run[df$Run == "Temperature Dependent\nIsoprene Emissions"] <- "TD"
df$Run[df$Run == "Temperature Independent\nIsoprene Emissions"] <- "TI"

emissions.max.o3 <- df %>%
  arrange(Temperature) %>%
  rowwise() %>%
  mutate(Temperature.C = Temperature - 273, NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
  filter(NOx.Condition == "Maximal-O3") %>%
  group_by(Mechanism, Run, Temperature.C) %>%
  summarise(NOx.Emissions = mean(NOx.Emissions))

diff.from.mcm <- emissions.max.o3 %>%
  spread(Mechanism, NOx.Emissions) %>%
  gather(Mechanism, NOx.Emissions, -Run, -Temperature.C, -MCMv3.2) %>%
  rowwise() %>%
  mutate(Rel.Diff = (NOx.Emissions - MCMv3.2) / MCMv3.2) %>%
  select(Run, Temperature.C, Mechanism, Rel.Diff)
diff.from.mcm %>%
  filter(Temperature.C == 20.0)

diff.from.mcm$Run <- factor(diff.from.mcm$Run, levels = c("Temperature Independent\nIsoprene Emissions", "Temperature Dependent\nIsoprene Emissions"))
diff.from.mcm$Mechanism <- factor(diff.from.mcm$Mechanism, levels = c("CRIv2", "MOZART-4", "CB05", "RADM2"))

my.colours <- c("MCMv3.2" = "#6c254f", "CRIv2" = "#ef6638", "MOZART-4" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")

emissions.max.o3$Run <- factor(emissions.max.o3$Run, levels = c("Temperature Independent\nIsoprene Emissions", "Temperature Dependent\nIsoprene Emissions"))
emissions.max.o3$Mechanism <- factor(emissions.max.o3$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2"))

#absolute NOx emissions
plot <- ggplot(emissions.max.o3, aes(x = Temperature.C, y = NOx.Emissions, colour = Mechanism))
plot <- plot + geom_smooth(se = FALSE, size = 1)
plot <- plot + facet_wrap(~ Run) 
plot <- plot + plot_theme() 
plot <- plot + scale_colour_manual(values = my.colours)
plot <- plot + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("NOx Emissions (molecules cm-3)")
plot <- plot + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
plot <- plot + theme(panel.margin = unit(5, "mm"))
plot

# percent diffs
p <- ggplot(diff.from.mcm, aes(x = Temperature.C, y = Rel.Diff, colour = Mechanism))
p <- p + geom_smooth(se = FALSE)
p <- p + facet_wrap(~ Run)
p <- p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("Percent Difference from MCMv3.2")
p <- p + plot_theme()
p <- p + theme(panel.margin.x = unit(3, "mm"))
p <- p + scale_colour_manual(values = my.colours)
p <- p + scale_y_continuous(limits = c(-0.35, 0.10), breaks = seq(-0.3, 0.1, 0.1), labels = percent, expand = c(0, 0))
p <- p + scale_x_continuous(expand = c(0, 0))
p

CairoPDF(file = "NOx_emissions_for_Maximal-O3.pdf", width = 10, height = 7)
print(plot)
dev.off()
