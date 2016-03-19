# O3 vs T, allocated to the 3 NOx-regimes. Overlaid with ERA data and WRF output from different regions. Summer only values.
# Version 0: Jane Coates 18/3/2016

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//ERA_Data/O3-Max8hrMean/")

#mixing data
runs <- c("Dependent", "Independent")
mechanisms <- c("MCMv3.2", "CB05", "RADM2", "MOZART-4", "CRIv2")
data.list <- lapply(runs, get_all_mixing_ratio_data)
data.df <- do.call("rbind", data.list)
tbl_df(data.df)

orig.t.o3 <- data.df %>%
  mutate(Temperature.C = Temperature - 273) %>% 
  rowwise()  %>% 
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
  select(Mechanism, Temperature.C, Max.O3.8hr.av, NOx.Condition, Run) %>%
  group_by(Mechanism, Run, Temperature.C, NOx.Condition) %>%
  summarise(MDA8 = mean(Max.O3.8hr.av))
orig.t.o3$Run[orig.t.o3$Run == "Temperature Dependent\nIsoprene Emissions"] <- "TD"
orig.t.o3$Run[orig.t.o3$Run == "Temperature Independent\nIsoprene Emissions"] <- "TI"
orig.t.o3$NOx.Condition <- factor(orig.t.o3$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))
orig.t.o3$Run <- factor(orig.t.o3$Run, levels = c("TI", "TD"))
orig.t.o3$Type <- rep("Mixing", length(orig.t.o3$Mechanism))
orig.t.o3

# no mixing data
stag.data <- read.csv("MDAO3_Temperature_No_Mixing.csv")
tbl_df(stag.data)

#all box model data
all.data <- rbind(orig.t.o3, stag.data)
colnames(all.data) <- c("Mechanism", "Run", "Temperature.C", "NOx.Condition", "O3", "Type")
all.data

my.colours <- c("MCMv3.2" = "#6c254f", "CRIv2" = "#ef6638", "MOZART-4" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")

#era data
era.data <- read.csv(file = "Germany_O3-T_ERA_data_2007.csv")
wrf.data <- read.csv(file = "Germany_wrf_mozart.csv")

p <- ggplot(all.data, aes(x = Temperature.C, y = O3))
p <- p + facet_grid(NOx.Condition ~ Run)
p <- p + geom_point(data = era.data, , alpha = 0.08)
p <- p + geom_point(data = wrf.data, shape = 0, colour = "#7570b3")
p <- p + geom_line(size = 1, aes(colour = Mechanism, linetype = Type))
p <- p + xlab(expression(bold(paste("Temperature (", degree, "C)")))) + ylab("Maximum 8 h Mean O3 (ppbv)")
p <- p + plot_theme()
p <- p + theme(legend.position = "top", legend.title = element_blank())
p <- p + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
p <- p + theme(panel.margin = unit(5, "mm"))
p <- p + scale_colour_manual(values = my.colours)
p <- p + theme(axis.line.y = element_line(colour = "black"))
p <- p + theme(axis.line.x = element_line(colour = "black"))
p

filename <- paste0("Model_O3-T_vs_Germany_ERA_and_WRF_2007_MOZART_with_Stagnation.pdf")
CairoPDF(file = filename, width = 10, height = 7)
print(p)
dev.off()
