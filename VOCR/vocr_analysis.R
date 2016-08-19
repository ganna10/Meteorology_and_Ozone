# compare maximum VOCR of initial VOCs
# Version 0: Jane Coates 15/2/2016
# Version 1: Jane Coates 7/6/2016 information of individual VOCs

get_data <- function (mechanism, run.label, date) {
  filename <- paste0(run.label, "_VOCR/", mechanism, "_assigned_initial_VOC_OH_oxidation_rate_", date, ".txt")
  data <- read.csv(filename)
  data$Run <- rep(run.label, length(data$Mechanism))
  return(data)
}

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//VOCR")
date <- "07062016"
mechanisms <- c("CB05", "RADM2", "MOZART-4", "CRIv2", "MCMv3.2")

# temperature dependent data
td.list <- lapply(mechanisms, get_data, run.label = "TD", date = date)
td.df <- do.call("rbind", td.list)
tbl_df(td.df)
# 
# # temperature independent data
ti.list <- lapply(mechanisms, get_data, run.label = "TI", date = date)
ti.df <- do.call("rbind", ti.list)
tbl_df(ti.df)

df <- rbind(td.df, ti.df)

assigned <- df %>%
  rowwise() %>%
  mutate(Temperature.C = Temperature - 273, NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition, Group) %>%
  summarise(Max.VOCR = mean(Max.VOCR), Sum.VOCR = mean(Sum.VOCR), Total.Emission.Rate = mean(Total.Emission.Rate)) 
tbl_df(assigned)

# write.table(assigned, file = "VOCR_data.csv", sep = ",", quote = FALSE, row.names = FALSE)
assigned <- read.csv("VOCR_data.csv", header = TRUE)
tbl_df(assigned)

percent.total <- assigned %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition) %>%
  mutate(percent.Max.VOCR = Max.VOCR / sum(Max.VOCR), percent.Sum.VOCR = Sum.VOCR / sum(Sum.VOCR))
tbl_df(percent.total)

percent.total$Group <- factor(percent.total$Group, levels = c("Alkanes", "Alkenes", "Isoprene", "Monoterpenes", "Aromatics", "Aldehydes", "Ketones", "Alcohols", "Ethers", "Esters", "Organic Acids", "Chlorinated"))

# percent.total$Group <- factor(percent.total$Group, levels = c("Ethane", "Propane", "Butanes", "Pentanes", "Higher and Other Alkanes", "Ethene", "Propene", "Higher and Other Alkenes", "Isoprene", "Monoterpenes", "Benzene", "Toluene", "Xylenes", "Trimethylbenzenes", "Other Aromatics", "Formaldehyde", "Aldehydes", "Ketones", "Alcohols", "Ethers", "Esters", "Organic Acids", "Chlorinated"))
percent.total$NOx.Condition <- factor(percent.total$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))
percent.total$Mechanism <- factor(percent.total$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2"))

# ti plots
ti.data <- percent.total %>%
  filter(Run == "TI")

# td plots
td.data <- percent.total %>%
  filter(Run == "TD")

ti.data %>%
  filter(Temperature.C == 20 | Temperature.C == 40) %>%
  mutate(percent.Max.VOCR = percent.Max.VOCR * 100) %>%
  select(Mechanism, NOx.Condition, Group, percent.Max.VOCR) %>%  
  group_by(Mechanism, NOx.Condition) %>%
  top_n(n = 10, wt = percent.Max.VOCR) %>%
#   arrange(NOx.Condition, desc(percent.Max.VOCR)) %>%
  spread(NOx.Condition, percent.Max.VOCR)

my.colours = c("Organic Acids" = "#0352cb", "Alcohols" = "#011e4b", "Chlorinated" = "#ef6638", "Esters" = "#0357d8", "Ethers" = "#2b9eb3", "Alkanes" = "#38103d", "Ketones" = "#02388b", "Aldehydes" = "#0499fc", "Alkenes" = "#f9c500", "Aromatics" = "#216105", "Monoterpenes" = "#f7c56c", "Isoprene" = "#6d6537")

# my.colours = c("Organic Acids" = "#0352cb", "Alcohols" = "#011e4b", "Benzene" = "#0e5c28", "Butanes" = "#6e254f", "Chlorinated" = "#ef6638", "Esters" = "#0357d8", "Ethane" = "#a15daf", "Ethene" = "#dfb100", "Ethers" = "#2b9eb3", "Higher and Other Alkanes" = "#38103d", "Ketones" = "#02388b", "Aldehydes" = "#0499fc", "Higher and Other Alkenes" = "#f9c500", "Other Aromatics" = "#216105", "Others" = "#2b9eb3", "Pentanes" = "#8f2ac9", "Propane" = "#5c1b54", "Propene" = "#796000", "Monoterpenes" = "#b99300", "Toluene" = "#0b6956", "Trimethylbenzenes" = "#109c00", "Xylenes" = "#0c734b", "Isoprene" = "#6d6537", "Formaldehyde" = "#77aecc")

# maximum VOCR plots
ti.p <- ggplot(ti.data %>% arrange(Group), aes(x = Temperature.C, y = percent.Max.VOCR, fill = Group))
ti.p <- ti.p + geom_bar(stat = "identity", width = 1)
ti.p <- ti.p + facet_grid(Mechanism ~ NOx.Condition)
ti.p <- ti.p + plot_theme()
ti.p <- ti.p + scale_y_continuous(labels = percent, expand = c(0, 0))
ti.p <- ti.p + scale_x_continuous(expand = c(0, 0))
ti.p <- ti.p + scale_fill_manual(values = my.colours, limits = rev(levels(factor(percent.total$Group))))
ti.p <- ti.p + ggtitle("Temperature-Independent Isoprene Emissions")
ti.p <- ti.p + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
ti.p <- ti.p + ylab("Maximum VOCR (s-1)")
ti.p <- ti.p + theme(legend.position = "top")
ti.p

td.p <- ggplot(td.data %>% arrange(Group), aes(x = Temperature.C, y = percent.Max.VOCR, fill = Group))
td.p <- td.p + geom_bar(stat = "identity", width = 1)
td.p <- td.p + facet_grid(Mechanism ~ NOx.Condition)
td.p <- td.p + plot_theme()
td.p <- td.p + scale_y_continuous(labels = percent, expand = c(0, 0))
td.p <- td.p + scale_x_continuous(expand = c(0, 0))
td.p <- td.p + scale_fill_manual(values = my.colours, limits = rev(levels(factor(percent.total$Group))))
td.p <- td.p + ggtitle("Temperature-Dependent Isoprene Emissions")
td.p <- td.p + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
td.p <- td.p + ylab("Maximum VOCR (s-1)")
td.p <- td.p + theme(legend.position = "top")
td.p <- td.p + theme(legend.title = element_blank())
td.p <- td.p + guides(fill = guide_legend(label.position = "top", nrow = 2))
td.p

# grid.arrange td and ti plots together
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
my.legend <- g_legend(td.p)
CairoPDF(file = "Max_VOCR_percent_total.pdf", width = 10, height = 7)
print(grid.arrange(my.legend, 
                   arrangeGrob(ti.p + theme(legend.position = "none"), 
                               td.p + theme(axis.title.y = element_blank()) + theme(legend.position = "none"), 
                               nrow = 1), 
                   nrow = 2, 
                   heights = c(1.3, 7)))
dev.off()

# Sum VOCR plots
ti.p <- ggplot(ti.data %>% arrange(Group), aes(x = Temperature.C, y = percent.Sum.VOCR, fill = Group))
ti.p <- ti.p + geom_bar(stat = "identity", width = 1)
ti.p <- ti.p + facet_grid(Mechanism ~ NOx.Condition)
ti.p <- ti.p + plot_theme()
ti.p <- ti.p + scale_y_continuous(labels = percent, expand = c(0, 0))
ti.p <- ti.p + scale_x_continuous(expand = c(0, 0))
ti.p <- ti.p + scale_fill_manual(values = my.colours, limits = rev(levels(factor(percent.total$Group))))
ti.p <- ti.p + ggtitle("Temperature-Independent Isoprene Emissions")
ti.p <- ti.p + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
ti.p <- ti.p + ylab("Percent of Cumulative VOCR")
ti.p <- ti.p + theme(legend.position = "top")
ti.p <- ti.p + theme(panel.margin = unit(3, "mm"))
ti.p

td.p <- ggplot(td.data %>% arrange(Group), aes(x = Temperature.C, y = percent.Sum.VOCR, fill = Group))
td.p <- td.p + geom_bar(stat = "identity", width = 1)
td.p <- td.p + facet_grid(Mechanism ~ NOx.Condition)
td.p <- td.p + plot_theme()
td.p <- td.p + scale_y_continuous(labels = percent, expand = c(0, 0))
td.p <- td.p + scale_x_continuous(expand = c(0, 0))
td.p <- td.p + scale_fill_manual(values = my.colours, limits = rev(levels(factor(percent.total$Group))))
td.p <- td.p + ggtitle("Temperature-Dependent Isoprene Emissions")
td.p <- td.p + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
td.p <- td.p + ylab("Percent of Cumulative VOCR")
td.p <- td.p + theme(legend.position = "top")
td.p <- td.p + theme(legend.title = element_blank())
td.p <- td.p + guides(fill = guide_legend(label.position = "top", nrow = 2))
td.p <- td.p + theme(panel.margin = unit(3, "mm"))
td.p

# grid.arrange td and ti plots together
my.legend <- g_legend(td.p)
CairoPDF(file = "Sum_VOCR_percent.pdf", width = 10, height = 7)
print(grid.arrange(my.legend, 
                   arrangeGrob(ti.p + theme(legend.position = "none"), 
                               td.p + theme(axis.title.y = element_blank()) + theme(legend.position = "none"), 
                               nrow = 1), 
                   nrow = 2, 
                   heights = c(1.3, 7)))
dev.off()

# normalised by total emissions
percent.normalised <- assigned %>%
  rowwise() %>%
  mutate(Max.VOCR.norm = Max.VOCR / Total.Emission.Rate, Sum.VOCR.norm = Sum.VOCR / Total.Emission.Rate) %>%
  select(-Total.Emission.Rate, -Max.VOCR, -Sum.VOCR) %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition) %>%
  mutate(percent.Max.VOCR.norm = Max.VOCR.norm / sum(Max.VOCR.norm), percent.Sum.VOCR.norm = Sum.VOCR.norm / sum(Sum.VOCR.norm))
tbl_df(percent.normalised)
percent.normalised$Group <- factor(percent.normalised$Group, levels = c("Alkanes", "Alkenes", "Isoprene", "Monoterpenes", "Aromatics", "Aldehydes", "Ketones", "Alcohols", "Ethers", "Esters", "Organic Acids", "Chlorinated"))
percent.normalised$NOx.Condition <- factor(percent.normalised$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))
percent.normalised$Mechanism <- factor(percent.normalised$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2"))

filtered <- percent.normalised %>%
  filter(Temperature.C == "20" | Temperature.C == "40") %>%
  filter(NOx.Condition == "Maximal-O3") %>%
  filter(Run == "TD") %>%
  select(-Max.VOCR.norm, -percent.Max.VOCR.norm, -Sum.VOCR.norm) %>%
  mutate(percent.Sum.VOCR.norm = percent.Sum.VOCR.norm * 100)
filtered$Temperature.C[filtered$Temperature.C == "20"] <- "Twenty"
filtered$Temperature.C[filtered$Temperature.C == "40"] <- "Fourty"

analysis <- filtered %>%
  spread(Temperature.C, percent.Sum.VOCR.norm) %>%
  mutate(Difference = Fourty - Twenty) %>%
  group_by(Mechanism, Run, NOx.Condition) %>%
  arrange(desc(Twenty))
print.data.frame(analysis)

# ti plots
ti.data.norm <- percent.normalised %>%
  filter(Run == "TI")

# td plots
td.data.norm <- percent.normalised %>%
  filter(Run == "TD")

# maximum VOCR plots - normalised
ti.p <- ggplot(ti.data.norm %>% arrange(Group), aes(x = Temperature.C, y = percent.Max.VOCR.norm, fill = Group))
ti.p <- ti.p + geom_bar(stat = "identity", width = 1)
ti.p <- ti.p + facet_grid(Mechanism ~ NOx.Condition)
ti.p <- ti.p + plot_theme()
ti.p <- ti.p + scale_y_continuous(labels = percent, expand = c(0, 0))
ti.p <- ti.p + scale_x_continuous(expand = c(0, 0))
ti.p <- ti.p + scale_fill_manual(values = my.colours, limits = rev(levels(factor(percent.total$Group))))
ti.p <- ti.p + ggtitle("Temperature-Independent Isoprene Emissions")
ti.p <- ti.p + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
ti.p <- ti.p + ylab("Maximum VOCR Normalised Total Emission Rate")
ti.p <- ti.p + theme(legend.position = "top")
ti.p

td.p <- ggplot(td.data.norm %>% arrange(Group), aes(x = Temperature.C, y = percent.Max.VOCR.norm, fill = Group))
td.p <- td.p + geom_bar(stat = "identity", width = 1)
td.p <- td.p + facet_grid(Mechanism ~ NOx.Condition)
td.p <- td.p + plot_theme()
td.p <- td.p + scale_y_continuous(labels = percent, expand = c(0, 0))
td.p <- td.p + scale_x_continuous(expand = c(0, 0))
td.p <- td.p + scale_fill_manual(values = my.colours, limits = rev(levels(factor(percent.total$Group))))
td.p <- td.p + ggtitle("Temperature-Dependent Isoprene Emissions")
td.p <- td.p + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
td.p <- td.p + ylab("Maximum VOCR Normalised Total Emission Rate")
td.p <- td.p + theme(legend.position = "top")
td.p <- td.p + theme(legend.title = element_blank())
td.p <- td.p + guides(fill = guide_legend(label.position = "top", nrow = 2))
td.p

# grid.arrange td and ti plots together
my.legend <- g_legend(td.p)
CairoPDF(file = "Max_VOCR_percent_normalised.pdf", width = 10, height = 7)
print(grid.arrange(my.legend, 
                   arrangeGrob(ti.p + theme(legend.position = "none"), 
                               td.p + theme(axis.title.y = element_blank()) + theme(legend.position = "none"), 
                               nrow = 1), 
                   nrow = 2, 
                   heights = c(1.3, 7)))
dev.off()

# sum VOCR plots - normalised
ti.p <- ggplot(ti.data.norm %>% arrange(Group), aes(x = Temperature.C, y = percent.Sum.VOCR.norm, fill = Group))
ti.p <- ti.p + geom_bar(stat = "identity", width = 1)
ti.p <- ti.p + facet_grid(Mechanism ~ NOx.Condition)
ti.p <- ti.p + plot_theme()
ti.p <- ti.p + scale_y_continuous(labels = percent, expand = c(0, 0))
ti.p <- ti.p + scale_x_continuous(expand = c(0, 0))
ti.p <- ti.p + scale_fill_manual(values = my.colours, limits = rev(levels(factor(percent.total$Group))))
ti.p <- ti.p + ggtitle("Temperature-Independent Isoprene Emissions")
ti.p <- ti.p + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
ti.p <- ti.p + ylab("Cumulative VOCR Normalised Total Emission Rate")
ti.p <- ti.p + theme(legend.position = "top")
ti.p <- ti.p + theme(panel.margin = unit(3, "mm"))
ti.p

td.p <- ggplot(td.data.norm %>% arrange(Group), aes(x = Temperature.C, y = percent.Sum.VOCR.norm, fill = Group))
td.p <- td.p + geom_bar(stat = "identity", width = 1)
td.p <- td.p + facet_grid(Mechanism ~ NOx.Condition)
td.p <- td.p + plot_theme()
td.p <- td.p + scale_y_continuous(labels = percent, expand = c(0, 0))
td.p <- td.p + scale_x_continuous(expand = c(0, 0))
td.p <- td.p + scale_fill_manual(values = my.colours, limits = rev(levels(factor(percent.total$Group))))
td.p <- td.p + ggtitle("Temperature-Dependent Isoprene Emissions")
td.p <- td.p + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
td.p <- td.p + ylab("Cumulative VOCR Normalised Total Emission Rate")
td.p <- td.p + theme(legend.position = "top")
td.p <- td.p + theme(legend.title = element_blank())
td.p <- td.p + guides(fill = guide_legend(label.position = "top", nrow = 2))
td.p <- td.p + theme(panel.margin = unit(3, "mm"))
td.p

# grid.arrange td and ti plots together
my.legend <- g_legend(td.p)
CairoPDF(file = "Sum_VOCR_percent_normalised.pdf", width = 10, height = 7)
print(grid.arrange(my.legend, 
                   arrangeGrob(ti.p + theme(legend.position = "none"), 
                               td.p + theme(axis.title.y = element_blank()) + theme(legend.position = "none"), 
                               nrow = 1), 
                   nrow = 2, 
                   heights = c(1.3, 7)))
dev.off()
