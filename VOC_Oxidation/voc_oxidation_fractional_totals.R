# compare oxidation rates of initial VOCs
# Version 0: Jane Coates 2/3/2016

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//VOC_Oxidation/")
assigned <- read.csv(file = "assigned_data.csv")

fractional.loss <- assigned %>%
  select(-Emission.Rate) %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition) %>%
  mutate(Total.Loss = sum(Oxidation.Rate), Fractional.Loss = Oxidation.Rate / Total.Loss) %>%
  select(-Oxidation.Rate, -Total.Loss)
tbl_df(fractional.loss)
fractional.loss$Group <- factor(fractional.loss$Group, levels = c("Alkanes", "Alkenes", "Aromatics", "Isoprene", "Terpenes", "Aldehydes", "Ketones", "Chlorinates", "Others"))
fractional.loss$Mechanism <- factor(fractional.loss$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2"))
fractional.loss$NOx.Condition <- factor(fractional.loss$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))

my.colours <- c("Alkanes" = "#6c254f", "Alkenes" = "#f9c500", "Isoprene" = "#0e5c28", "Terpenes" = "#2b9eb3", "Aromatics" = "#ef6638", "Alcohols" = "#0352cb", "Aldehydes" = "#b569b3", "Ketones" = "#77aecc", "Acids" = "#000000", "Chlorinated" = "#ba8b01", "Others" = "#ae4901")

# td plots
fractional.loss.td <- fractional.loss %>%
  filter(Run == "TD")

td.p <- ggplot(arrange(fractional.loss.td, Group), aes(x = Temperature.C, y = Fractional.Loss, fill = Group))
td.p <- td.p + geom_bar(stat = "identity", width = 1)
td.p <- td.p + facet_grid(Mechanism ~ NOx.Condition)
td.p <- td.p + plot_theme()
td.p <- td.p + scale_fill_manual(values = my.colours)
td.p <- td.p + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
td.p <- td.p + scale_y_continuous(expand = c(0, 0), labels = percent)
td.p <- td.p + theme(legend.position = "top")
td.p <- td.p + theme(legend.key.width = unit(2.5, "cm"))
td.p <- td.p + theme(legend.key.height = unit(0.5, "cm"))
td.p <- td.p + theme(legend.title = element_blank())
td.p <- td.p + ggtitle("Temperature-Dependent Isoprene Emissions")
td.p <- td.p + theme(panel.margin = unit(4, "mm"))
td.p <- td.p + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
td.p <- td.p + ylab("Contribution of VOC Groups to Total Oxidation of VOCs")
td.p <- td.p + guides(fill = guide_legend(label.position = "top", nrow = 1))
td.p

# ti plots
fractional.loss.ti <- fractional.loss %>%
  filter(Run == "TI")

ti.p <- ggplot(arrange(fractional.loss.ti, Group), aes(x = Temperature.C, y = Fractional.Loss, fill = Group))
ti.p <- ti.p + geom_bar(stat = "identity", width = 1)
ti.p <- ti.p + facet_grid(Mechanism ~ NOx.Condition)
ti.p <- ti.p + plot_theme()
ti.p <- ti.p + scale_fill_manual(values = my.colours)
ti.p <- ti.p + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
ti.p <- ti.p + scale_y_continuous(expand = c(0, 0), labels = percent)
ti.p <- ti.p + theme(legend.position = "top")
ti.p <- ti.p + theme(legend.key.width = unit(2.5, "cm"))
ti.p <- ti.p + theme(legend.key.height = unit(0.5, "cm"))
ti.p <- ti.p + theme(legend.title = element_blank())
ti.p <- ti.p + ggtitle("Temperature-Independent Isoprene Emissions")
ti.p <- ti.p + theme(panel.margin = unit(4, "mm"))
ti.p <- ti.p + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
ti.p <- ti.p + ylab("Contribution of VOC Groups to Total Oxidation of VOCs")
ti.p <- ti.p + guides(fill = guide_legend(label.position = "top", nrow = 1))
ti.p

# grid.arrange td and ti plots together
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
my.legend <- g_legend(td.p)
CairoPDF(file = "VOC_fractional_loss.pdf", wI LOVE YOU!!!!idth = 10, height = 7)
print(grid.arrange(my.legend, 
                   arrangeGrob(ti.p + theme(legend.position = "none"), 
                               td.p + theme(axis.title.y = element_blank()) + theme(legend.position = "none"), 
                               nrow = 1), 
                   nrow = 2, 
                   heights = c(0.38, 7)))
dev.off()

# 
# mcm.loss <- fractional.loss %>%
#   filter(Run == "MCMv3.")
# mcm <- ggplot(fractional.loss, aes(x = Temperature.C, y = Fractional.Loss, fill = Run))
# mcm <- mcm + geom_bar(stat = "identity", position = "dodge")
# mcm <- mcm + facet_grid(NOx.Condition ~ Group)
# mcm <- mcm + plot_theme()
# mcm

## total loss
total.loss <- assigned %>%
  select(-Emission.Rate) %>%
  group_by(Mechanism, NOx.Condition, Temperature.C, Run, Group) %>%
  mutate(Total = sum(Oxidation.Rate))
total.loss$Group <- factor(total.loss$Group, levels = c("Alkanes", "Alkenes", "Aromatics", "Terpenes", "Aldehydes", "Ketones", "Chlorinated", "Others", "Isoprene"))
total.loss$Mechanism <- factor(total.loss$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2"))
total.loss$NOx.Condition <- factor(total.loss$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))

td.total <- total.loss %>%
  filter(Run == "TD")
td.plot <- ggplot(td.total %>% filter(NOx.Condition == "High-NOx") %>% arrange(Group), aes(x = Temperature.C, y = Total))
td.plot <- td.plot + geom_area(position = "stack")
td.plot <- td.plot + facet_wrap(Mechanism ~ Group, scales = "free")
td.plot <- td.plot + plot_theme()
td.plot
td.plot <- td.plot + scale_fill_manual(values = my.colours)
td.plot <- td.plot + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
td.plot <- td.plot + scale_y_continuous(limits = c(0, 2e8), breaks = seq(0, 2e8, 5e7), expand = c(0, 0))
td.plot <- td.plot + theme(legend.position = "top")
td.plot <- td.plot + theme(legend.key.width = unit(2.5, "cm"))
td.plot <- td.plot + theme(legend.key.height = unit(0.5, "cm"))
td.plot <- td.plot + theme(legend.title = element_blank())
td.plot <- td.plot + ggtitle("Temperature-Dependent Isoprene Emissions")
td.plot <- td.plot + theme(panel.margin = unit(4, "mm"))
td.plot <- td.plot + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
td.plot <- td.plot + ylab("NMVOC Loss Rate (molecules cm-3)")
td.plot <- td.plot + guides(fill = guide_legend(label.position = "top", nrow = 1))
td.plot <- td.plot + theme(axis.line.y = element_line(colour = "black"))
td.plot

ti.total <- total.loss %>%
  filter(Run == "TI")
ti.plot <- ggplot(ti.total %>% filter(NOx.Condition == "High-NOx") %>% arrange(Group), aes(x = Temperature.C, y = Total))
ti.plot <- ti.plot + geom_area(position = "stack")
ti.plot <- ti.plot + facet_wrap(Mechanism ~ Group, scales = "free")
ti.plot <- ti.plot + plot_theme()
ti.plot <- ti.plot + scale_fill_manual(values = my.colours)
ti.plot
ti.plot <- ti.plot + scale_x_continuous(limits = c(15, 40), breaks = seq(15, 40, 5), expand = c(0, 0))
ti.plot <- ti.plot + scale_y_continuous(limits = c(0, 2e8), breaks = seq(0, 2e8, 5e7), expand = c(0, 0))
ti.plot <- ti.plot + theme(legend.position = "top")
ti.plot <- ti.plot + theme(legend.key.width = unit(2.5, "cm"))
ti.plot <- ti.plot + theme(legend.key.height = unit(0.5, "cm"))
ti.plot <- ti.plot + theme(legend.title = element_blank())
ti.plot <- ti.plot + ggtitle("Temperature-Independent Isoprene Emissions")
ti.plot <- ti.plot + theme(panel.margin = unit(4, "mm"))
ti.plot <- ti.plot + xlab(expression(bold(paste("Temperature (", degree, "C)"))))
ti.plot <- ti.plot + ylab("NMVOC Loss Rate (molecules cm-3)")
ti.plot <- ti.plot + guides(fill = guide_legend(label.position = "top", nrow = 1))
ti.plot <- ti.plot + theme(axis.line.y = element_line(colour = "black"))
ti.plot

my.legend <- g_legend(td.plot)
CairoPDF(file = "VOC_oxidation.pdf", width = 10, height = 7)
print(grid.arrange(my.legend, 
                   arrangeGrob(ti.plot + theme(legend.position = "none"), 
                               td.plot + theme(axis.title.y = element_blank()) + theme(legend.position = "none"), 
                               nrow = 1), 
                   nrow = 2, 
                   heights = c(0.38, 7)))
dev.off()

# emissions
emissions <- assigned %>%
  select(-Oxidation.Rate) 
emissions$Group <- factor(emissions$Group, levels = c("Alkanes", "Alkenes", "Aromatics", "Terpenes", "Aldehydes", "Ketones", "Chlorinated", "Others", "Isoprene"))
emissions$Mechanism <- factor(emissions$Mechanism, levels = c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2"))
emissions$NOx.Condition <- factor(emissions$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))

td.emissions <- emissions %>%
  filter(Run == "TD")
td.emis <- ggplot(td.emissions, aes(x = Temperature.C, y = Emission.Rate, fill = Group))
td.emis <- td.emis + geom_bar(stat = "identity")
td.emis <- td.emis + facet_grid(Mechanism ~ NOx.Condition)
td.emis <- td.emis + plot_theme()
td.emis <- td.emis + scale_fill_manual(values = my.colours)
td.emis

ti.emissions <- emissions %>%
  filter(Run == "TI")
ti.emis <- ggplot(ti.emissions, aes(x = Temperature.C, y = Emission.Rate, fill = Group))
ti.emis <- ti.emis + geom_bar(stat = "identity")
ti.emis <- ti.emis + facet_grid(Mechanism ~ NOx.Condition)
ti.emis <- ti.emis + plot_theme()
ti.emis <- ti.emis + scale_fill_manual(values = my.colours)
ti.emis

# increase in loss rate from 20 to 40C
filtered <- total.loss %>%
  filter(Temperature.C == 20 | Temperature.C == 40)
filtered$Temperature.C[filtered$Temperature.C == 20.0] <- "Twenty"
filtered$Temperature.C[filtered$Temperature.C == 40.0] <- "Fourty"
filtered %>%
  select(-Total) %>%
  spread(Temperature.C, Oxidation.Rate) %>%
  mutate(Increase = Fourty - Twenty)

# linear slopes of loss rates with T
total.loss %>%  group_by(Mechanism, Run, NOx.Condition, Group) %>% 
  do(model = lm(Oxidation.Rate ~ Temperature.C, data = .)) %>% 
  mutate(Slope = summary(model)$coeff[2], Intercept = summary(model)$coeff[1], R2 = summary(model)$r.squared) %>% 
  select(-model, -R2, -Intercept) %>%
  spread(Run,Slope) %>%
  mutate(Diff = TD - TI)
